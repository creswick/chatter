{-# LANGUAGE OverloadedStrings #-}
module Trainer where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

import qualified NLP.POS.AvgPerceptronTagger as Avg
import qualified NLP.POS.UnambiguousTagger as UT
import NLP.POS (saveTagger, train)
import NLP.Corpora.Parsing
import NLP.Types (POSTagger)

import qualified NLP.Corpora.Brown as B

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
  let taggedCorpora = map readPOS $ concatMap T.lines $ rawCorpus
  tagger <- train initTagger taggedCorpora
  saveTagger tagger output

