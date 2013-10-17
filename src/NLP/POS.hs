{-# LANGUAGE OverloadedStrings #-}
module NLP.POS where

import NLP.Corpora.Parsing
import qualified NLP.POS.AvgPerceptronTagger as Per
import NLP.POS.AvgPerceptron (emptyPerceptron, Perceptron)

import NLP.Types (TaggedSentence, Tag(..))

import Control.Monad (foldM)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

data POSTagger = POSTagger
    { tagger  :: Text -> [TaggedSentence] -- ^ The initial part-of-speech tagger.
    , backoff :: Maybe POSTagger   -- ^ A tagger to invoke on unknown tokens.
    }

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

-- | Train on a corpus of files.
trainOnFiles :: [FilePath] -> IO Perceptron
trainOnFiles corpora = foldM step emptyPerceptron corpora
  where
    step :: Perceptron -> FilePath -> IO Perceptron
    step per path = do
      content <- T.readFile path
      train per content

-- | Add training examples to a perceptron.
--
-- If you're using multiple input files, this can be useful to improve
-- performance (by folding over the files).  For example, see `trainOnFiles`
train :: Perceptron -> Text -> IO Perceptron
train per rawCorpus = do
  let corpora = map readPOS $ T.lines rawCorpus
  Per.train itterations per corpora

tag :: Perceptron -> Text -> [TaggedSentence]
tag per str = Per.tag per $ map T.words $ T.lines str

-- | Tag the tokens in a string.
--
-- Returns a space-separated string of tokens, each token suffixed
-- with the part of speech.  For example:
--
-- > tag tagger "the dog jumped ."
-- "the/at dog/nn jumped/vbd ./."
--
tagStr :: Perceptron -> String -> String
tagStr per = T.unpack . tagText per . T.pack

-- | Text version of tagStr
tagText :: Perceptron -> Text -> Text
tagText per str = T.intercalate " " $ map toTaggedTok taggedSents
  where
    taggedSents = concat $ tag per str

    toTaggedTok :: (Text, Tag) -> Text
    toTaggedTok (tok, Tag c) = tok `T.append` (T.cons '/' c)