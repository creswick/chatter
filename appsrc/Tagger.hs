{-# LANGUAGE OverloadedStrings #-}
module Tagger where

import Control.Monad (foldM)
import qualified Data.ByteString as BS
import Data.Serialize (decode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Environment (getArgs)
import System.FilePath ((</>))
import Text.Printf (printf)

import NLP.POS (tagStr, train)
import qualified NLP.POS.AvgPerceptron as Per
import NLP.POS.AvgPerceptron (Perceptron)


main :: IO ()
main = do
  args <- getArgs
  let model = args!!0
      sentence = args!!1
  model <- BS.readFile model
  case decode model of
    Left err -> putStrLn ("Could not load model: "++err)
    Right tagger -> T.putStrLn $ tagStr tagger (T.pack sentence)
