{-# LANGUAGE OverloadedStrings #-}
module Evaluate where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Environment (getArgs)

import NLP.Corpora.Parsing
import NLP.POS (eval, loadTagger)

main :: IO ()
main = do
  args <- getArgs
  let modelFile = args!!0
      corpora = tail args
  putStrLn "Loading model..."
  tagger <- loadTagger modelFile
  putStrLn "...model loaded."
  rawCorpus <- mapM T.readFile corpora
  let taggedCorpora = map readPOS $ concatMap T.lines $ rawCorpus
      result = eval tagger taggedCorpora
  putStrLn ("Result: " ++ show result)
  putStrLn ("Tokens tagged: "++(show $ length $ concat taggedCorpora))
