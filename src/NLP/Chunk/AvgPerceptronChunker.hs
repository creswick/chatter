{-# LANGUAGE OverloadedStrings #-}
-- | Avegeraged Perceptron Chunker
--
module NLP.Chunk.AvgPerceptronChunker
where

import NLP.POS.AvgPerceptron ( Perceptron, Feature(..)
                             , Class(..), predict, update
                             , emptyPerceptron, averageWeights)
import NLP.Types
import NLP.Types.IOB

import Control.Monad (foldM)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.List (zipWith4, foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Either (rights)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Serialize (encode, decode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Random.Shuffle (shuffleM)

data Chunker c t = Chunker
  { chChunker :: [TaggedSentence t] -> [ChunkedSentence c t]
  , chTrainer :: [ChunkedSentence c t] -> IO (Chunker c t)
  , chSerialize :: ByteString
  , chId :: ByteString
  }

chunkerID :: ByteString
chunkerID = "NLP.Chunk.AvgPerceptronChunker"

readChunker :: (ChunkTag c, Tag t) => ByteString -> Either String (Chunker c t)
readChunker bs = do
  model <- decode bs
  return $ mkChunker model

itterations :: Int
itterations = 5

mkChunker :: (ChunkTag c, Tag t) => Perceptron -> Chunker c t
mkChunker per = Chunker { chChunker = chunk per
                        , chTrainer = \exs -> do
                            newPer <- trainInt itterations per exs
                            return $ mkChunker newPer
                        , chSerialize = encode per
                        , chId = chunkerID
                        }

chunk :: (ChunkTag c, Tag t) => Perceptron -> [TaggedSentence t] -> [ChunkedSentence c t]
chunk per corpus = map (chunkSentence per) corpus

chunkSentence :: (ChunkTag c, Tag t) => Perceptron -> TaggedSentence t -> ChunkedSentence c t
chunkSentence per (TaggedSent sent) = let

  chunks = [Class "-START-"] ++ map (predictChunk per) features

  features = zipWith3 (getFeatures sent)
             [0..]
             sent
             (tail chunks) -- only works if startToks is of length 1

  chunkBuilders = map (\(Class c) -> iobBuilder $ T.pack c) $ drop 1 chunks

  in toTree $ rights $ zipWith ($) chunkBuilders sent -- possible hidden failures.

predictChunk :: Perceptron -> Map Feature Int -> Class
predictChunk model feats = fromMaybe (Class "O") $ predict model feats

-- | Turn an IOB result into a tree.
toTree :: (ChunkTag c, Tag t) => [IOBChunk c t] -> ChunkedSentence c t
toTree chunks = ChunkedSent $ toChunkOr chunks

toChunkOr :: (ChunkTag c, Tag t) => [IOBChunk c t] -> [ChunkOr c t]
toChunkOr ((OChunk pos):rest)       = POS_CN pos : toChunkOr rest
toChunkOr ((BChunk pos chunk):rest) = (Chunk_CN (Chunk chunk children)) : toChunkOr theTail
  where
    (ichunks, theTail) = span isIChunk rest

    toPOScn (IChunk pos _) = Just $ POS_CN pos
    toPOScn _              = Nothing

    children = mapMaybe toPOScn ichunks

    isIChunk (IChunk _ _) = True
    isIChunk _            = False

trainInt :: (ChunkTag c, Tag t) =>
            Int -- ^ The number of times to iterate over the training
                -- data, randomly shuffling after each iteration. (@5@
                -- is a reasonable choice.)
         -> Perceptron -- ^ The 'Perceptron' to train.
         -> [ChunkedSentence c t] -- ^ The training data. (A list of @[(Text, Tag)]@'s)
         -> IO Perceptron    -- ^ A trained perceptron.  IO is needed
                             -- for randomization.
trainInt itr per examples = trainCls itr per $ toClassLst $ map unzipChunks examples

toClassLst :: (ChunkTag c, Tag t) => [(TaggedSentence t, [c])] -> [(TaggedSentence t, [Class])]
toClassLst tagged = map (\(x, y)->(x, map (Class . T.unpack . fromChunk) y)) tagged

-- | Copied directly from the AvgPerceptronTagger; should be generalized?
trainCls :: Tag t => Int -> Perceptron -> [(TaggedSentence t, [Class])] -> IO Perceptron
trainCls itr per examples = do
  trainingSet <- shuffleM $ concat $ take itr $ repeat examples
  return $ averageWeights $ foldl' trainSentence per trainingSet

posToClass :: Tag t => POS t -> Class
posToClass = Class . T.unpack . showPOStok

-- | start markers to ensure all features in context are valid,
-- even for the first "real" tokens.
startToks :: Tag t => [POS t]
startToks = [POS startTag (Token "-START-")]

-- | end markers to ensure all features are valid, even for
-- the last "real" tokens.
endToks :: Tag t => [POS t]
endToks = [POS endTag (Token "-END-")]

-- | Train on one sentence.
trainSentence :: Tag t => Perceptron -> (TaggedSentence t, [Class]) -> Perceptron
trainSentence per (TaggedSent sent, ts) = let

  -- This class needs to match the start token.
  tags = [Class "-START-"] ++ ts

  features = zipWith3 (getFeatures sent)
                         [0..] -- index
                         sent  -- words (well, POS values)
                         (tail tags) -- prev1, only works if startToks is of length 1

  fn :: Perceptron -> (Map Feature Int, Class) -> Perceptron
  fn model (feats, truth) = let
    guess = predictChunk model feats
    in update model truth guess $ Map.keys feats

  in foldl' fn per (zip features ts)


-- >>> def npchunk_features(sentence, i, history):
-- ...     word, pos = sentence[i]
-- ...     if i == 0:
-- ...         prevword, prevpos = "<START>", "<START>"
-- ...     else:
-- ...         prevword, prevpos = sentence[i-1]
-- ...     if i == len(sentence)-1:
-- ...         nextword, nextpos = "<END>", "<END>"
-- ...     else:
-- ...         nextword, nextpos = sentence[i+1]
-- ...     return {"pos": pos,
-- ...             "word": word,
-- ...             "prevpos": prevpos,
-- ...             "nextpos": nextpos, [1]
-- ...             "prevpos+pos": "%s+%s" % (prevpos, pos),  [2]
-- ...             "pos+nextpos": "%s+%s" % (pos, nextpos),
-- ...             "tags-since-dt": tags_since_dt(sentence, i)}
getFeatures :: Tag t =>
               [POS t] -- ^ The full sentence that this word is located in.
            -> Int   -- ^ The index of the current word.
            -> POS t -- ^ The current word/tag pair.
            -> Class -- ^ The predicted class of the previous word.
            -> Map Feature Int
getFeatures tagged idx word prev = let
  context = startToks ++ tagged ++ endToks

  i = idx + 1 -- length startToks

  add :: Map Feature Int -> [Text] -> Map Feature Int
  add m args = Map.alter increment (mkFeature $ T.intercalate " " args) m

  increment :: Maybe Int -> Maybe Int
  increment Nothing  = Just 1
  increment (Just w) = Just (w + 1)

  features :: [[Text]]
  features = [ ["pos",         showPOStag word]
             , ["word",        showPOStok word]
             , ["prevpos",     showPOStag (context!!(i-1))]
             , ["prevpos+pos", T.intercalate "+" $ map showPOStag
                                 [ context!!(i-1), word ]
               ]
             , ["pos+nextpos", T.intercalate "+" $ map showPOStag
                                 [word, context!!(i+1) ]
               ]
--             , ["tags-since-dt", ""]
             ]
  in foldl' add Map.empty features

mkFeature :: Text -> Feature
mkFeature txt = Feat $ T.copy txt
