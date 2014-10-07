{-# LANGUAGE OverloadedStrings #-}
module Tagger where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Environment (getArgs)

import NLP.POS (tagText, loadTagger)
import NLP.Types (RawTag, POSTagger)

main :: IO ()
main = do
  args <- getArgs
  let modelFile = args!!0
      sentence  = args!!1
  putStrLn "Loading model..."
  tagger <- (loadTagger modelFile:: IO (POSTagger RawTag))
  putStrLn "...model loaded."
  T.putStrLn $ tagText tagger (T.pack sentence)
