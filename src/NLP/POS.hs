{-# LANGUAGE OverloadedStrings #-}
module NLP.POS where

import NLP.Corpora.Parsing
import NLP.POS.AvgPerceptron (Class(..))
import qualified NLP.POS.AvgPerceptronTagger as Per
import NLP.POS.AvgPerceptron (emptyPerceptron, Perceptron)

import Data.Text (Text)
import qualified Data.Text as T

itterations :: Int
itterations = 5

-- | Train a new perceptron
--
-- The training corpus should be a collection
-- of sentences, one sentence on each line, and with each token tagged
-- with a part of speech.
--
-- For example, the input:
-- > "The/DT dog/NN jumped/VB ./.\nThe/DT cat/NN slept/VB ./."
-- defines two training sentences.
trainNew :: Text -> IO Perceptron
trainNew rawCorpus = train emptyPerceptron rawCorpus

train :: Perceptron -> Text -> IO Perceptron
train per rawCorpus = do
  let corpora = map readPOS $ T.lines rawCorpus
  Per.train itterations per corpora

tag :: Perceptron -> Text -> [Per.TaggedSentence]
tag per str = Per.tag per $ map T.words $ T.lines str

tagStr :: Perceptron -> Text -> Text
tagStr per str = T.intercalate " " $ map toTaggedTok taggedSents
  where
    taggedSents = concat $ tag per str

    toTaggedTok :: (Text, Class) -> Text
    toTaggedTok (tok, Class c) = tok `T.append` (T.pack ('/':c))