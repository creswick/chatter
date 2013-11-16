{-# LANGUAGE OverloadedStrings #-}
module Trainer where

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Serialize (encode)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

import qualified NLP.POS.AvgPerceptronTagger as Avg
import qualified NLP.POS.UnambiguousTagger as UT
import NLP.POS (saveTagger, train)
import NLP.Corpora.Parsing

main :: IO ()
main = do
  args <- getArgs
  let output = last args
      corpora = init args
      avgPerTagger = Avg.mkTagger Avg.emptyPerceptron Nothing
      initTagger   = UT.mkTagger Map.empty (Just avgPerTagger)
  rawCorpus <- mapM T.readFile corpora
  tagger <- train initTagger (map readPOS $ concatMap T.lines $ rawCorpus)
  saveTagger tagger output

