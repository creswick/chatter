{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Parsing for IOB-format corpora.
--
-- Reads in text formatted as:
--
-- <token> <POS tag> <IOB tag>
--
-- Where <IOB tag> is of the format:
--
--  [I-<tag>|B-<tag>|O]
--
-- Since the IOB format is used for various types of information, such
-- as chunks and named-entity classes, the allowable <tag> bits change
-- depending on the corpus.
--
module NLP.Types.IOB where

import Prelude hiding (print)
import Data.Hashable (Hashable(..))
import Data.List (findIndices)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Generics

import Text.PrettyPrint.HughesPJClass (Pretty(..))
import Text.PrettyPrint.HughesPJClass (text)


import NLP.Types.General (Error)
import NLP.Types.Annotations
import NLP.Types.Classes
import NLP.Types.TokenizedSentence
import NLP.Types.TaggedSentence
import NLP.Types.ChunkedSentence

import NLP.Tokenize (runTokenizer, whitespace)

-- | Data type to indicate IOB tags for multi-token annotations.
-- The text tag is left un-parsed at this point.
data IOB = B Text -- ^ Beging marker.
         | I Text -- ^ In tag
         | O      -- ^ Not in a tagged thing.
  deriving (Read, Show, Eq, Generic, Ord)

instance Hashable IOB

-- | Literal representation of IOB data.
data IOBTaggedSentence pos =
  IOBTaggedSentence { iobTagSentence :: TaggedSentence pos
                    , iobAnnotations :: [Annotation (TaggedSentence pos) IOB]
                    } deriving (Read, Show, Eq, Generic, Ord)

instance Hashable pos => Hashable (IOBTaggedSentence pos)

instance AnnotatedText (IOBTaggedSentence pos) where
  getText = getText . iobTagSentence

instance Pretty pos => Pretty (IOBTaggedSentence pos) where
  -- TODO: Write this instance.
  pPrint _ts = text "IOBTaggedSentence - instance needs written"

-- | Parse an IOB-represented corpus into a list of '[ChunkedSentence]'
parseToChunkedSentences :: (POS pos, Chunk chunk) => Text -> Either Error [ChunkedSentence pos chunk]
parseToChunkedSentences txt = do
  iobSentences <- parse txt
  mapM parseToChunkedSentence iobSentences

parseToChunkedSentence :: (POS pos, Chunk chunk) => IOBTaggedSentence pos -> Either Error (ChunkedSentence pos chunk)
parseToChunkedSentence (IOBTaggedSentence tSent iobAnns) = do
  let startIndexes = findIndices isBTag iobAnns
  annotations <- mapM (mkChunkAnnotation tSent iobAnns) startIndexes
  return ChunkedSentence { chunkTagSentence = tSent
                         , chunkAnnotations = annotations
                         }

mkChunkAnnotation :: (Chunk chunk, POS pos)
                  => TaggedSentence pos
                  -> [Annotation (TaggedSentence pos) IOB]
                  -> Int
                  -> Either Error (Annotation (TaggedSentence pos) chunk)
mkChunkAnnotation tSent iobAnns idx = do
  let (ann:anns) = drop idx iobAnns

  theChunk <- iobAnn2Chunk ann
  return Annotation { startIdx = Index idx
                  , len = 1 + (length $ takeWhile isITag anns)
                  , value = theChunk
                  , payload = tSent }


-- | True if the annotation contains a Beginning ('B-') IOB tag.
isBTag :: POS pos => Annotation (TaggedSentence pos) IOB -> Bool
isBTag ann = case value ann of
               B _ -> True
               _   -> False

-- | True if the annotation is an inner ('I-') IOB tag.
isITag :: POS pos => Annotation (TaggedSentence pos) IOB -> Bool
isITag ann = case value ann of
               I _ -> True
               _   -> False

-- | Convert an IOB annotation into a Chunk
iobAnn2Chunk :: (POS pos, Chunk chunk) => Annotation (TaggedSentence pos) IOB -> Either Error chunk
iobAnn2Chunk ann = do
  iobTxt <- case value ann of
              B txt -> Right txt
              I txt -> Right txt
              O     -> Left "Outside of a chunk"
  parseChunk iobTxt

-- | Parse an IOB-tagged corpus into intermediate tagged sentences
-- that are ready for additional parsing into 'ChunkedSentences'
-- or 'NERedSentences'
parse :: POS pos => Text -> Either Error [IOBTaggedSentence pos]
parse input = mapM parseIOB $ parseIOBSentences input

-- | Parse a single sentence from IOB format.
parseIOB :: POS pos => Annotation Text Token -> Either Error (IOBTaggedSentence pos)
parseIOB ann = do
  -- Get the tokens: This should result in a TokenizedSentence with 3*n annotations.
  let tokSent = runTokenizer whitespace $ getText ann -- was rawTokSent
  triples <- resolveTriples (tokAnnotations tokSent) []
  let realTokens = [x | (x,_,_) <- triples]
      posTags = [y | (_,y,_) <- triples]
      iobTags = [z | (_,_,z) <- triples]

      realTokSent = TokenizedSentence { tokText = getText ann
                                      , tokAnnotations = realTokens }
      tagSent = TaggedSentence { tagTokSentence = realTokSent
                               , tagAnnotations = zipWith
                                 (\x pos -> Annotation {
                                               startIdx = Index x
                                             , len = 1
                                             , value = pos
                                             , payload = realTokSent }) [0..] posTags
                               }
      iobSent = IOBTaggedSentence { iobTagSentence = tagSent
                                  , iobAnnotations = zipWith
                                    (\x iob -> Annotation {
                                               startIdx = Index x
                                             , len = 1
                                             , value = iob
                                             , payload = tagSent }) [1..] iobTags
                                  }
  return iobSent

resolveTriples :: POS pos
                => [Annotation Text Token]
                -> [( Annotation Text Token, pos, IOB)]
                -> Either Error [( Annotation Text Token, pos, IOB)]
resolveTriples                       [] acc = Right acc
resolveTriples (tok:rawPos:rawIOB:rest) acc = do
  pos <- parsePOS $ getText rawPos
  iob <- iobBuilder $ getText rawIOB
  recurse <- resolveTriples rest acc
  return ((tok, pos, iob):recurse)
resolveTriples cruft _ = Left ("Could not parse IOB triple from: \"" <> (T.pack $ show cruft) <> "\"")


-- | Shift an annotation by a given distance.
offsetAnnotations :: Int -> Annotation Text b -> Annotation Text b
offsetAnnotations offset ann =
  ann { startIdx = Index (offset + (fromIndex $ startIdx ann))
      }

-- | Parse IOB text into sentences, using the Annotations
-- structure to store *one* sentence per token.
--
parseIOBSentences :: Text -> [Annotation Text Token]
parseIOBSentences input = reverse $ addLast $ T.foldl' fn emptyAcc input
  where
    emptyAcc :: ( Int -- Index
                , Char -- Last char
                , [Char] -- Accumulated tokens.
                , [Annotation Text Token] -- Accumulated annotations
                )
    emptyAcc = (0, '\n', [], [])

    addLast :: (Int, Char, [Char], [Annotation Text Token]) -> [Annotation Text Token]
    addLast (idx, _, tok, acc) = (mkAnnotation (idx - length tok) (reverse tok)):acc

    fn :: (Int, Char, [Char], [Annotation Text Token])
       -> Char
       -> (Int, Char, [Char], [Annotation Text Token])
    fn (idx, lChar, tok, acc) char | char == '\n' && lChar == '\n' =
                                     case tok of -- end of a token; if we have characters, create new one.
                                       [] -> (idx + 1, char, [], acc)
                                       cs -> (idx + 1, char, [], (mkAnnotation (idx - length cs) $ reverse cs):acc)
                                   | otherwise = (idx + 1, char, char:tok, acc)

    mkAnnotation :: Int -> [Char] -> Annotation Text Token
    mkAnnotation sIdx tok = Annotation { startIdx = Index sIdx
                                       , len = length tok
                                       , value = Token $ T.pack tok
                                       , payload = input
                                       }

iobBuilder :: Text -> Either Error IOB
iobBuilder iobTxt | "I-" `T.isPrefixOf` iobTxt = Right (I $ T.drop 2 iobTxt)
                  | "B-" `T.isPrefixOf` iobTxt = Right (B $ T.drop 2 iobTxt)
                  | "O"  `T.isPrefixOf` iobTxt = Right O
                  | otherwise                  = Left ("Could not parse IOB text: \""<>iobTxt<>"\"")
