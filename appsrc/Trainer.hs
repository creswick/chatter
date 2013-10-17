{-# LANGUAGE OverloadedStrings #-}
module Trainer where

import qualified Data.ByteString as BS
import Data.Serialize (encode)
import System.Environment (getArgs)

import NLP.POS (trainOnFiles)

main :: IO ()
main = do
  args <- getArgs
  let output = last args
      corpora = init args
  tagger <- trainOnFiles corpora
  BS.writeFile output $ encode tagger

