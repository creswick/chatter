module NLP.POS where


import NLP.Corpora.Parsing
import qualified NLP.POS.AvgPerceptronTagger as Per
import NLP.POS.AvgPerceptron (emptyPerceptron, Perceptron)

import Data.Text (Text)
import qualified Data.Text as T

itterations :: Int
itterations = 5

train :: Text -> Perceptron
train rawCorpus = do
  let corpora = map readPOS $ T.lines rawCorpus
  Per.train itterations emptyPerceptron corpora

tag :: Perceptron -> Text -> [Per.TaggedSentence]
tag per str = Per.tag per $ map T.words $ T.lines str
