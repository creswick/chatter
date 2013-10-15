{-# LANGUAGE OverloadedStrings #-}
module Trainer where

import Control.Monad (foldM)
import qualified Data.ByteString as BS
import Data.Serialize (encode)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Environment (getArgs)

import NLP.POS (train)
import qualified NLP.POS.AvgPerceptron as Per
import NLP.POS.AvgPerceptron (Perceptron)


main :: IO ()
main = do
  args <- getArgs
  let output = last args
      corpora = init args

      step :: Perceptron -> FilePath -> IO Perceptron
      step per path = do
        content <- T.readFile path
        train per content

  tagger <- foldM step Per.emptyPerceptron corpora
  BS.writeFile output $ encode tagger
