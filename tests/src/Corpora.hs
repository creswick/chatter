{-# LANGUAGE OverloadedStrings #-}
module Corpora where

import Data.Text (Text)
import qualified Data.Text as T

-- | A very small (one-sentence) training corpora for tests.
miniCorpora1 :: Text
miniCorpora1 = "the/DT dog/NN jumped/VB ./."

miniCorpora2 :: Text
miniCorpora2 = T.unlines [ "the/DT dog/NN jumped/VB ./."
                         , "a/DT dog/NN barks/VB ./."
                         ]