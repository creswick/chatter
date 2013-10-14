{-# LANGUAGE OverloadedStrings #-}
module Trainer where

import qualified Data.ByteString as BS
import Data.Serialize (encode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Environment (getArgs)
import System.FilePath ((</>))
import Text.Printf (printf)

import NLP.POS (tagStr, train)

main :: IO ()
main = undefined 
-- main = do
  -- args <- getArgs
  -- let output = last args
  --     corpora = init args
  -- fileContents <- mapM T.readFile corpora
  -- let corpus = T.unlines fileContents
  -- tagger <- train corpus
  -- BS.writeFile output $ encode tagger
