{-# LANGUAGE OverloadedStrings #-}
module POSTrainer where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

import qualified NLP.POS.AvgPerceptronTagger as Avg
import qualified NLP.POS.UnambiguousTagger as UT
import NLP.POS (saveTagger, train)
import NLP.Types (POSTagger, Error)
import NLP.Types.IOB

import qualified NLP.Corpora.Brown as B
import qualified NLP.Corpora.Conll as C

main :: IO ()
main = do
  args <- getArgs
  let output = last args
      corpora = init args

      avgPerTagger :: POSTagger B.Tag
      avgPerTagger = Avg.mkTagger Avg.emptyPerceptron Nothing

      initTagger :: POSTagger B.Tag
      initTagger   = UT.mkTagger Map.empty (Just avgPerTagger)

  rawCorpus <- mapM T.readFile corpora
  let taggedSentences = B.parseTaggedSentences $ T.concat rawCorpus
  tagger <- train initTagger taggedSentences
  saveTagger tagger output

