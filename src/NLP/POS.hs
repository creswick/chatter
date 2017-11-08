{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : NLP.POS
Description : Part-of-Speech tagging facilities.
Copyright   : Rogan Creswick, 2014
Maintainer  : creswick@gmail.com
Stability   : experimental

This module aims to make tagging text with parts of speech
trivially easy.

If you're new to 'chatter' and POS-tagging, then I
suggest you simply try:

>>> tagger <- defaultTagger
>>> tagStr tagger "This is a sample sentence."
"This/dt is/bez a/at sample/nn sentence/nn ./."

Note that we used 'tagStr', instead of 'tag', or 'tagText'.  Many
people don't (yet!) use "Data.Text" by default, so there is a
wrapper around 'tag' that packs and unpacks the 'String'.  This is
innefficient, but it's just to get you started, and 'tagStr' can be
very handy when you're debugging a tagger in ghci (or cabal repl).

'tag' exposes more details of the tokenization and tagging, since
it returns a list of `TaggedSentence`s, but it doesn't print
results as nicely.
-}
module NLP.POS
  ( tag
  , tagStr
  , tagText
  , train
  , trainStr
  , trainText
  , tagTokens
  , eval
  , serialize
  , deserialize
  , taggerTable
  , saveTagger
  , loadTagger
  , defaultTagger
  , conllTagger
  , brownTagger
  )
where

import qualified Control.Exception           as X
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
import           NLP.Types                   ( POSTagger(..), Sentence, POS(..)
                                             , combine, Tag (..), unTS, tsLength
                                             , TaggedSentence(..), stripTags
                                             , printTS)

import qualified NLP.POS.AvgPerceptronTagger as Avg
import qualified NLP.POS.LiteralTagger       as LT
import qualified NLP.POS.UnambiguousTagger   as UT

import qualified NLP.Corpora.Brown as B
import qualified NLP.Corpora.Conll as C

import           Paths_chatter

-- | A basic POS tagger.
defaultTagger :: IO (POSTagger C.Tag)
defaultTagger = conllTagger

-- | A POS tagger that has been trained on the Conll 2000 POS tags.
conllTagger :: IO (POSTagger C.Tag)
conllTagger = do
  dir <- getDataDir
  loadTagger (dir </> "data" </> "models" </> "conll2000.pos.model.gz")

-- | A POS tagger trained on a subset of the Brown corpus.
brownTagger :: IO (POSTagger B.Tag)
brownTagger = do
  dir <- getDataDir
  loadTagger (dir </> "data" </> "models" </> "brown.pos.model.gz")

-- | The default table of tagger IDs to readTagger functions.  Each
-- tagger packaged with Chatter should have an entry here.  By
-- convention, IDs use the fully qualified module name of the
-- tagger package.
taggerTable :: Tag t => Map ByteString
               (ByteString -> Maybe (POSTagger t) -> Either String (POSTagger t))
taggerTable = Map.fromList
  [ (LT.taggerID, LT.readTagger)
  , (Avg.taggerID, Avg.readTagger)
  , (UT.taggerID, UT.readTagger)
  ]

-- | Store a `POSTager' to a file.
saveTagger :: Tag t => POSTagger t -> FilePath -> IO ()
saveTagger tagger file = BS.writeFile file (serialize tagger)

-- | Load a tagger, using the interal `taggerTable`.  If you need to
-- specify your own mappings for new composite taggers, you should use
-- `deserialize`.
--
-- This function checks the filename to determine if the content
-- should be decompressed.  If the file ends with ".gz", then we
-- assume it is a gziped model.
loadTagger :: Tag t => FilePath -> IO (POSTagger t)
loadTagger file = do
  content <- getContent file
  case deserialize taggerTable content of
    Left err -> error ("Could not load tagger from file: "++ file ++" due to error: \n" ++ err)
    Right tgr -> return tgr
  where
    getContent :: FilePath -> IO ByteString
    getContent f | ".gz" `isSuffixOf` file = fmap (LBS.toStrict . decompress) $ LBS.readFile f
                 | otherwise               = BS.readFile f

serialize :: Tag t => POSTagger t -> ByteString
serialize tagger =
  let backoff = case posBackoff tagger of
                  Nothing -> Nothing
                  Just btgr -> Just $ serialize btgr
  in encode ( posID tagger
            , posSerialize tagger
            , backoff
            )

deserialize :: Tag t =>
               Map ByteString
                  (ByteString -> Maybe (POSTagger t) -> Either String (POSTagger t))
            -> ByteString
            -> Either String (POSTagger t)
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
tag :: Tag t => POSTagger t -> Text -> [TaggedSentence t]
tag p txt = let sentences = (posSplitter p) txt
                tokens    = map (posTokenizer p) sentences
            in tagTokens p tokens

tagTokens :: Tag t => POSTagger t -> [Sentence] -> [TaggedSentence t]
tagTokens p tokens = let priority = (posTagger p) tokens
                     in case posBackoff p of
                          Nothing  -> priority
                          Just tgr -> combine priority (tagTokens tgr tokens)


-- | Tag the tokens in a string.
--
-- Returns a space-separated string of tokens, each token suffixed
-- with the part of speech.  For example:
--
-- >>> tag tagger "the dog jumped ."
-- "the/at dog/nn jumped/vbd ./."
--
tagStr :: Tag t => POSTagger t -> String -> String
tagStr tgr = T.unpack . tagText tgr . T.pack

-- | Text version of tagStr
tagText :: Tag t => POSTagger t -> Text -> Text
tagText tgr txt = T.intercalate " " $ map printTS $ tag tgr txt

-- | Train a tagger on string input in the standard form for POS
-- tagged corpora:
--
-- > trainStr tagger "the/at dog/nn jumped/vbd ./."
--
trainStr :: Tag t => POSTagger t -> String -> IO (POSTagger t)
trainStr tgr = trainText tgr . T.pack

-- | The `Text` version of `trainStr`
trainText :: Tag t => POSTagger t -> Text -> IO (POSTagger t)
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
train :: Tag t => POSTagger t -> [TaggedSentence t] -> IO (POSTagger t)
train p exs = do
  let
    trainBackoff = case posBackoff p of
                     Nothing -> return $ Nothing
                     Just b  -> do tgr <- train b exs
                                   return $ Just tgr
    trainer = posTrainer p
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
eval :: Tag t => POSTagger t -> [TaggedSentence t] -> Double
eval tgr oracle = let
  sentences = map stripTags oracle
  results = (posTagger tgr) sentences
  totalTokens = fromIntegral $ sum $ map tsLength oracle

  isMatch :: Tag t => POS t -> POS t -> Double
  isMatch (POS rTag _) (POS oTag _) | rTag == oTag = 1
                                    | otherwise    = 0
  in (sum $ zipWith isMatch (concatMap unTS results) (concatMap unTS oracle)) / totalTokens
