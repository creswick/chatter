{-# LANGUAGE OverloadedStrings #-}
-- | Avegeraged Perceptron Chunker
--
module NLP.Chunk.AvgPerceptronChunker
  ( mkChunker
  , trainInt
  , chunk
  , chunkSentence
  , Chunker(..)
  , chunkerID
  , readChunker
  )
where

import NLP.POS.AvgPerceptron ( Perceptron, Feature(..)
                             , Class(..), predict, update
                             , averageWeights)
import NLP.Types

import Data.ByteString (ByteString)
import Data.List (foldl', group)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Either (rights)
import Data.Maybe (fromMaybe)
import Data.Serialize (encode, decode)
import Data.Text (Text)
import qualified Data.Text as T

import System.Random.Shuffle (shuffleM)

-- | The type of Chunkers, incorporates chunking, training,
-- serilazitaion and unique IDs for deserialization.
data Chunker c t = Chunker
  { chChunker :: [TaggedSentence t] -> [ChunkedSentence c t]
  , chTrainer :: [ChunkedSentence c t] -> IO (Chunker c t)
  , chSerialize :: ByteString
  , chId :: ByteString
  }

-- | The unique ID for this implementation of a 'Chunker'
chunkerID :: ByteString
chunkerID = "NLP.Chunk.AvgPerceptronChunker"

-- | deserialize an 'AvgPerceptronChunker' from a 'ByteString'.
readChunker :: (ChunkTag c, Tag t) => ByteString -> Either String (Chunker c t)
readChunker bs = do
  model <- decode bs
  return $ mkChunker model

itterations :: Int
itterations = 5

-- | Create a chunker from a 'Perceptron'.
mkChunker :: (ChunkTag c, Tag t) => Perceptron -> Chunker c t
mkChunker per = Chunker { chChunker = chunk per
                        , chTrainer = \exs -> do
                            newPer <- trainInt itterations per exs
                            return $ mkChunker newPer
                        , chSerialize = encode per
                        , chId = chunkerID
                        }


-- | Chunk a list of POS-tagged sentence, generating a parse tree.
chunk :: (ChunkTag c, Tag t) => Perceptron -> [TaggedSentence t] -> [ChunkedSentence c t]
chunk per corpus = map (chunkSentence per) corpus

-- | Chunk a single POS-tagged sentence.
chunkSentence :: (ChunkTag c, Tag t) => Perceptron -> TaggedSentence t -> ChunkedSentence c t
chunkSentence per (TaggedSent sent) = let

  chunks = [Class "-START-"] ++ map (predictChunk per) features

  features = zipWith3 (getFeatures sent)
             [0..]
             sent
             chunks

  chunkTags = map (\(Class c) -> parseChunk $ T.pack c) $ drop 1 chunks

  in toTree (rights $ chunkTags) sent -- possible hidden failures.

predictChunk :: Perceptron -> Map Feature Int -> Class
predictChunk model feats =
  let predicted = predict model feats
      theClass = fromMaybe (Class "O") predicted
  in theClass

-- | Turn an IOB result into a tree.
toTree :: (ChunkTag c, Tag t) => [c] -> [POS t] -> ChunkedSentence c t
toTree chunks tags =
  let groups = map (\g -> (head g, length g)) $ group chunks

      groupTags []     _    = []
      groupTags ((g, c):gs) tags = (g, take c tags):(groupTags gs $ drop c tags)

  in ChunkedSent $ concatMap toChunkOr (groupTags groups tags)

toChunkOr :: (ChunkTag c, Tag t) => (c, [POS t]) -> [ChunkOr c t]
toChunkOr (c, tags) | c == notChunk = map POS_CN tags
                    | otherwise     = [Chunk_CN (Chunk c $ map POS_CN tags)]


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
                         tags  -- predicted class of previous word.

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
