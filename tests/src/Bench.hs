{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module Bench where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.DeepSeq
import Criterion.Main
import Criterion.Config (defaultConfig, Config(..), ljust)
import Criterion (bench, bgroup, Benchmark)

import NLP.POS (tagText)
import NLP.POS.AvgPerceptronTagger (trainNew, mkTagger)
import Corpora
import NLP.Corpora.Email

import qualified NLP.Similarity.VectorSimBench as VS

myConfig :: Config
myConfig = defaultConfig {
              -- Always GC between runs.
              cfgPerformGC = ljust True
            }

main :: IO ()
main = do
--  postagBench <- posTagging
  muc3_1 <- VS.muc3_01
  muc3_2 <- VS.muc3_02
  muc3_3 <- VS.muc3_03
  pTxt <- plugArchiveText
  let len = length pTxt
      plugTxt = take (len `div` 4) pTxt
      plugStr = map T.unpack plugTxt
  deepseq plugStr $ defaultMainWith myConfig (return ())
       [ 
       --  bgroup "POS Tagging" [] -- postagBench
       -- , bgroup "Similarity" $ VS.benchmarks (muc3_1++muc3_2) muc3_3
       ]

-- posTagging :: IO [Benchmark]
-- posTagging = do
--   ca01 <- T.readFile brownCA01
--   ca02 <- T.readFile (brownCAFiles!!1)
--   let ca1_2 = T.unlines [ca01, ca02]
--   return [ bench "Train Brown ca01" $ trainNew ca01
--          , bench "Train & test Brown ca01" $ trainAndTag ca01 "the dog jumped"

--          , bench "Train Brown ca02" $ trainNew ca02
--          , bench "Train & test Brown ca02" $ trainAndTag ca02 "the dog jumped"

--          , bench "Train Brown ca01-02" $ trainNew ca1_2
--          , bench "Train & test Brown ca01-02" $ trainAndTag ca1_2 "the dog jumped"
--          ]

trainAndTag :: Text -> Text -> IO Text
trainAndTag corpus input = do
  tagger <- trainNew corpus
  return $ tagText (mkTagger tagger Nothing) input

