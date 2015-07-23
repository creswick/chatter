{-# LANGUAGE OverloadedStrings #-}
module NERTrainer where

import System.Environment (getArgs)
import NLP.Chunk (saveChunker)
import qualified NLP.Corpora.WikiNer as W

main :: IO ()
main = do
  args <- getArgs
  let output = last args
      corpora = init args

  chunker <- W.trainChunker corpora
  saveChunker chunker output

