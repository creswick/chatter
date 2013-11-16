{-# LANGUAGE OverloadedStrings #-}
module Tagger where

import qualified Data.ByteString as BS
import Data.Serialize (decode)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Environment (getArgs)

import NLP.POS (tagText, loadTagger)
import NLP.POS.AvgPerceptronTagger (mkTagger)

main :: IO ()
main = do
  args <- getArgs
  let modelFile = args!!0
      sentence  = args!!1
  tagger <- loadTagger modelFile
  T.putStrLn $ tagText tagger (T.pack sentence)

  -- case decode model of
  --   Left err -> putStrLn ("Could not load model: "++err)
  --   Right per -> do let tagger = mkTagger per Nothing
  --                   T.putStrLn $ tagText tagger (T.pack sentence)
