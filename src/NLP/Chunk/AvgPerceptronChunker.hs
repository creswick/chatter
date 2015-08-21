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

import NLP.ML.AvgPerceptron ( Perceptron, Feature(..)
                            , Class(..), predict, update
                            , averageWeights)
import NLP.Types

import Data.ByteString (ByteString)
import Data.List (foldl')
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
data Chunker t c = Chunker
  { chChunker :: [TaggedSentence t] -> [ChunkedSentence t c]
  , chTrainer :: [ChunkedSentence t c] -> IO (Chunker t c)
  , chSerialize :: ByteString
  , chId :: ByteString
  }

-- | The unique ID for this implementation of a 'Chunker'
chunkerID :: ByteString
chunkerID = "NLP.Chunk.AvgPerceptronChunker"

-- | deserialize an 'AvgPerceptronChunker' from a 'ByteString'.
readChunker :: (Chunk c, POS t) => ByteString -> Either String (Chunker t c)
readChunker bs = do
  model <- decode bs
  return $ mkChunker model

itterations :: Int
itterations = 5

-- | Create a chunker from a 'Perceptron'.
mkChunker :: (Chunk c, POS t) => Perceptron -> Chunker t c
mkChunker per = Chunker { chChunker = chunk per
                        , chTrainer = \exs -> do
                            newPer <- trainInt itterations per exs
                            return $ mkChunker newPer
                        , chSerialize = encode per
                        , chId = chunkerID
                        }


-- | Chunk a list of POS-tagged sentence, generating a parse tree.
chunk :: (Chunk c, POS t) => Perceptron -> [TaggedSentence t] -> [ChunkedSentence t c]
chunk per corpus = map (chunkSentence per) corpus

-- | Chunk a single POS-tagged sentence.
chunkSentence :: (Chunk c, POS t) => Perceptron -> TaggedSentence t -> ChunkedSentence t c
chunkSentence per sent = let

  -- Classify the list of POS-tagged tokens (in the
  -- TaggedSentence). The tagger operates on individual tokens, so it
  -- creates individual chunk tags on each token as well.  These
  -- should be IOB-style tags, but they are not -- and as such, can't
  -- differentiate between separate, adjacent, chunks of the same
  -- type. The tags are turned into ChunkedSentences below (in the
  -- "body" of this function).
  chunks = map (predictChunk per) features

  -- Features are defined as a mutually-recursive list over the
  -- processed chunks:
  features = zipWith3 (getFeatures sent)
             [0..]
             (tsToPairs sent)
             ((Class "-START-"):chunks)

  -- chunkTags is a list of Chunk values
  chunkTags = map (\(Class c) -> parseChunk $ T.pack c) chunks

  -- loosing data here (discarding lefts!) Not likely to be a problem
  -- unles the training data is in error, though.
  in toChunkedSentence sent (rights $ chunkTags)

predictChunk :: Perceptron -> Map Feature Int -> Class
predictChunk model feats =
  let predicted = predict model feats
      theClass = fromMaybe (Class "O") predicted
  in theClass


trainInt :: (Chunk c, POS t)
         => Int -- ^ The number of times to iterate over the training
                -- data, randomly shuffling after each iteration. (@5@
                -- is a reasonable choice.)
         -> Perceptron -- ^ The 'Perceptron' to train.
         -> [ChunkedSentence t c] -- ^ The training data. (A list of @[(Text, Tag)]@'s)
         -> IO Perceptron    -- ^ A trained perceptron.  IO is needed
                             -- for randomization.
trainInt itr per examples = trainCls itr per $ toClassLst $ map fromChunkedSentence examples

toClassLst :: (Chunk c, POS t) => [(TaggedSentence t, [c])] -> [(TaggedSentence t, [Class])]
toClassLst tagged = map (\(x, y)->(x, map (Class . T.unpack . serializeChunk) y)) tagged

-- | Copied directly from the AvgPerceptronTagger; should be generalized?
trainCls :: POS t => Int -> Perceptron -> [(TaggedSentence t, [Class])] -> IO Perceptron
trainCls itr per examples = do
  trainingSet <- shuffleM $ concat $ take itr $ repeat examples
  return $ averageWeights $ foldl' trainSentence per trainingSet

-- | start markers to ensure all features in context are valid,
-- even for the first "real" tokens.
startToks :: POS pos => [(Token, pos)]
startToks = [(Token "-START-", startPOS)]

-- | end markers to ensure all features are valid, even for
-- the last "real" tokens.
endToks :: POS pos => [(Token, pos)]
endToks = [(Token "-END-", endPOS)]

-- | Train on one sentence.
trainSentence :: POS t => Perceptron -> (TaggedSentence t, [Class]) -> Perceptron
trainSentence per (sent, ts) = let

  -- This class needs to match the start token.
  tags = [Class "-START-"] ++ ts

  features = zipWith3 (getFeatures sent)
                         [0..] -- index
                         (tsToPairs sent)  -- words (well, POS values)
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
getFeatures :: POS pos
            => TaggedSentence pos -- ^ The full sentence that this word is located in.
            -> Int   -- ^ The index of the current word.
            -> (Token, pos) -- ^ The current word/tag pair.
            -> Class -- ^ The predicted class of the previous word.
            -> Map Feature Int
getFeatures tagged idx (word, postag) _prev = let
  context = startToks ++ tsToPairs tagged ++ endToks

  i = idx + 1 -- length startToks

  add :: Map Feature Int -> [Text] -> Map Feature Int
  add m args = Map.alter increment (mkFeature $ T.intercalate " " args) m

  increment :: Maybe Int -> Maybe Int
  increment Nothing  = Just 1
  increment (Just w) = Just (w + 1)

  features :: [[Text]]
  features = [ ["pos",         serializePOS postag]
             , ["word",        getText word]
             , ["prevpos",     serializePOS $ snd (context!!(i-1))]
             , ["prevpos+pos", T.intercalate "+"
                               [ serializePOS $ snd (context!!(i-1))
                               , serializePOS postag]
               ]
             , ["pos+nextpos", T.intercalate "+"
                               [ serializePOS postag
                               , serializePOS $ snd (context!!(i+1))]
               ]
             , ["tags-since-dt", tagsSinceDt $ take idx (map snd $ tsToPairs tagged)]
             ]

  in foldl' add Map.empty features

tagsSinceDt :: POS pos => [pos] -> Text
tagsSinceDt posToks =
  T.intercalate "-" $ tagsSinceHelper $ reverse posToks

tagsSinceHelper :: POS pos => [pos] -> [Text]
tagsSinceHelper []      = []
tagsSinceHelper (t:ts)| isDt t    = []
                      | otherwise = (serializePOS t):(tagsSinceHelper ts)


mkFeature :: Text -> Feature
mkFeature txt = Feat $ T.copy txt
