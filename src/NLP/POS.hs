{-# LANGUAGE OverloadedStrings #-}
-- | This module aims to make tagging text with parts of speech
-- trivially easy.
--
-- If you're new to 'chatter' and POS-tagging, then I
-- suggest you simply try:
--
-- >>> tagger <- defaultTagger
-- >>> tagStr tagger "This is a sample sentence."
-- "This/dt is/bez a/at sample/nn sentence/nn ./."
--
-- Note that we used 'tagStr', instead of 'tag', or 'tagText'.  Many
-- people don't (yet!) use "Data.Text" by default, so there is a
-- wrapper around 'tag' that packs and unpacks the 'String'.  This is
-- innefficient, but it's just to get you started, and 'tagStr' can be
-- very handy when you're debugging an tagger in ghci (or cabal repl).
--
-- 'tag' exposes more details of the tokenization and tagging, since
-- it returns a list of `TaggedSentence`s, but it doesn't print
-- results as nicely.
--
module NLP.POS
  ( tag
  , tagStr
  , tagText
  , train
  , trainStr
  , trainText
  , eval
  , serialize
  , deserialize
  , taggerTable
  , saveTagger
  , loadTagger
  , defaultTagger
  )
where


import           Codec.Compression.GZip      (decompress)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import           Data.List                   (isSuffixOf)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Serialize              (decode, encode)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           System.FilePath             ((</>))

import           NLP.Corpora.Parsing         (readPOS)
import           NLP.Tokenize.Text           (tokenize)
import           NLP.Types                   (POSTagger (..), Sentence,
                                              Tag (..), TaggedSentence,
                                              stripTags, tagUNK)

import qualified NLP.POS.AvgPerceptronTagger as Avg
import qualified NLP.POS.LiteralTagger       as LT
import qualified NLP.POS.UnambiguousTagger   as UT

import           Paths_chatter

defaultTagger :: IO POSTagger
defaultTagger = do
  dir <- getDataDir
  loadTagger (dir </> "data" </> "models" </> "brown-train.model.gz")

-- | The default table of tagger IDs to readTagger functions.  Each
-- tagger packaged with Chatter should have an entry here.  By
-- convention, the IDs use are the fully qualified module name of the
-- tagger package.
taggerTable :: Map ByteString (ByteString -> Maybe POSTagger -> Either String POSTagger)
taggerTable = Map.fromList
  [ (LT.taggerID, LT.readTagger)
  , (Avg.taggerID, Avg.readTagger)
  , (UT.taggerID, UT.readTagger)
  ]

-- | Store a `POSTager' to a file.
saveTagger :: POSTagger -> FilePath -> IO ()
saveTagger tagger file = BS.writeFile file (serialize tagger)

-- | Load a tagger, using the interal `taggerTable`.  If you need to
-- specify your own mappings for new composite taggers, you should use
-- `deserialize`.
--
-- This function checks the filename to determine if the content
-- should be decompressed.  If the file ends with ".gz", then we
-- assume it is a gziped model.
loadTagger :: FilePath -> IO POSTagger
loadTagger file = do
  content <- getContent file
  case deserialize taggerTable content of
    Left err -> error err
    Right tgr -> return tgr
  where
    getContent :: FilePath -> IO ByteString
    getContent f | ".gz" `isSuffixOf` file = fmap (LBS.toStrict . decompress) $ LBS.readFile f
                 | otherwise               = BS.readFile f

serialize :: POSTagger -> ByteString
serialize tagger =
  let backoff = case posBackoff tagger of
                  Nothing -> Nothing
                  Just btgr -> Just $ serialize btgr
  in encode ( posID tagger
            , posSerialize tagger
            , backoff
            )

deserialize :: Map ByteString (ByteString -> Maybe POSTagger -> Either String POSTagger)
            -> ByteString
            -> Either String POSTagger
deserialize table bs = do
  (theID, theTgr, mBackoff) <- decode bs
  backoff <- case mBackoff of
               Nothing  -> Right Nothing
               Just str -> Just `fmap` (deserialize table str)
  case Map.lookup theID table of
    Nothing -> Left ("Could not find ID in POSTagger function map: " ++ show theID)
    Just fn -> fn theTgr backoff

-- | Tag a chunk of input text with part-of-speech tags, using the
-- sentence splitter, tokenizer, and tagger contained in the 'POSTager'.
tag :: POSTagger -> Text -> [TaggedSentence]
tag p txt = let sentences = (posSplitter p) txt
                tokens    = map (posTokenizer p) sentences
            in tagTokens p tokens

tagTokens :: POSTagger -> [Sentence] -> [TaggedSentence]
tagTokens p tokens = let priority = (posTagger p) tokens
                     in case posBackoff p of
                          Nothing  -> priority
                          Just tgr -> combine priority (tagTokens tgr tokens)


-- | Combine the results of POS taggers, using the second param to
-- fill in 'tagUNK' entries, where possible.
combine :: [TaggedSentence] -> [TaggedSentence] -> [TaggedSentence]
combine xs ys = zipWith combineSentences xs ys

combineSentences :: TaggedSentence -> TaggedSentence -> TaggedSentence
combineSentences xs ys = zipWith pickTag xs ys

-- | Returns the first param, unless it is tagged 'tagUNK'.
-- Throws an error if the text does not match.
pickTag :: (Text, Tag) -> (Text, Tag) -> (Text, Tag)
pickTag a@(txt1, t1) b@(txt2, t2) | txt1 /= txt2 = error ("Text does not match: "++ show a ++ " " ++ show b)
                                  | t1 /= tagUNK = (txt1, t1)
                                  | otherwise    = (txt1, t2)

-- | Tag the tokens in a string.
--
-- Returns a space-separated string of tokens, each token suffixed
-- with the part of speech.  For example:
--
-- >>> tag tagger "the dog jumped ."
-- "the/at dog/nn jumped/vbd ./."
--
tagStr :: POSTagger -> String -> String
tagStr tgr = T.unpack . tagText tgr . T.pack

-- | Text version of tagStr
tagText :: POSTagger -> Text -> Text
tagText tgr str = T.intercalate " " $ map toTaggedTok taggedSents
  where
    taggedSents = concat $ tag tgr str

    toTaggedTok :: (Text, Tag) -> Text
    toTaggedTok (tok, Tag c) = tok `T.append` (T.cons '/' c)

-- | Train a tagger on string input in the standard form for POS
-- tagged corpora:
--
-- > trainStr tagger "the/at dog/nn jumped/vbd ./."
--
trainStr :: POSTagger -> String -> IO POSTagger
trainStr tgr = trainText tgr . T.pack

-- | The `Text` version of `trainStr`
trainText :: POSTagger -> Text -> IO POSTagger
trainText p exs = train p (map readPOS $ tokenize exs)

-- | Train a 'POSTagger' on a corpus of sentences.
--
-- This will recurse through the 'POSTagger' stack, training all the
-- backoff taggers as well.  In order to do that, this function has to
-- be generic to the kind of taggers used, so it is not possible to
-- train up a new POSTagger from nothing: 'train' wouldn't know what
-- tagger to create.
--
-- To get around that restriction, you can use the various 'mkTagger'
-- implementations, such as 'NLP.POS.LiteralTagger.mkTagger' or
-- NLP.POS.AvgPerceptronTagger.mkTagger'.  For example:
--
-- > import NLP.POS.AvgPerceptronTagger as APT
-- >
-- > let newTagger = APT.mkTagger APT.emptyPerceptron Nothing
-- > posTgr <- train newTagger trainingExamples
--
train :: POSTagger -> [TaggedSentence] -> IO POSTagger
train p exs = do
  let
    trainBackoff = case posBackoff p of
                     Nothing -> return $ Nothing
                     Just b  -> do tgr <- train b exs
                                   return $ Just tgr
    trainer = posTrainer p
  putStrLn ("train - exs: "++show exs)
  newTgr <- trainer exs
  newBackoff <- trainBackoff
  return (newTgr { posBackoff = newBackoff })

-- | Evaluate a 'POSTager'.
--
-- Measures accuracy over all tags in the test corpus.
--
-- Accuracy is calculated as:
--
-- > |tokens tagged correctly| / |all tokens|
--
eval :: POSTagger -> [TaggedSentence] -> Double
eval tgr oracle = let
  sentences = map stripTags oracle
  results = (posTagger tgr) sentences
  totalTokens = fromIntegral $ sum $ map length oracle

  isMatch :: (Text, Tag) -> (Text, Tag) -> Double
  isMatch (_, rTag) (_, oTag) | rTag == oTag = 1
                              | otherwise    = 0
  in (sum $ zipWith isMatch (concat results) (concat oracle)) / totalTokens
