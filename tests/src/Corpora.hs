{-# LANGUAGE OverloadedStrings #-}
module Corpora where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.FilePath ((</>))
import Text.Printf (printf)

-- | A very small (one-sentence) training corpora for tests.
miniCorpora1 :: Text
miniCorpora1 = "the/DT dog/NN jumped/VB ./."

miniCorpora2 :: Text
miniCorpora2 = T.unlines [ "the/DT dog/NN jumped/VB ./."
                         , "a/DT dog/NN barks/VB ./."
                         ]

brownCorporaDir :: FilePath
brownCorporaDir = "/home/creswick/nltk_data/corpora/brown"

brownCA01 :: FilePath
brownCA01 = brownCAFiles!!0

brownCA :: IO Text
brownCA = do
  let files = brownCAFiles
  contents <- mapM T.readFile files
  return $ T.unlines contents

brownFile :: String -> Int -> String
brownFile cat num = printf (cat++"%02d") num

brownCAFiles :: [FilePath]
brownCAFiles = map (\n->brownCorporaDir </> (brownFile "ca" n)) [1..44]