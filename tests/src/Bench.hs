{-# LANGUAGE OverloadedStrings #-}
module Bench where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Criterion.Main (defaultMain)
import Criterion (bench)

import NLP.POS (tagStr, trainNew)
import Corpora

main = do
  ca01 <- T.readFile brownCA01
  ca02 <- T.readFile (brownCAFiles!!1)
  let ca1_2 = T.unlines [ca01, ca02]
  defaultMain [ bench "Train Brown ca01" $ trainNew ca01
              , bench "Train & test Brown ca01" $ trainAndTag ca01 "the dog jumped"

              , bench "Train Brown ca02" $ trainNew ca02
              , bench "Train & test Brown ca02" $ trainAndTag ca02 "the dog jumped"

              , bench "Train Brown ca01-02" $ trainNew ca1_2
              , bench "Train & test Brown ca01-02" $ trainAndTag ca1_2 "the dog jumped"
              ]

trainAndTag :: Text -> Text -> IO Text
trainAndTag corpus input = do
  tagger <- trainNew corpus
  return $ tagStr tagger input

