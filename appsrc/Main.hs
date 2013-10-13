{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.FilePath ((</>))
import Text.Printf (printf)

import NLP.POS (tagStr, train)

input :: Text
input = "the dog jumped"

main :: IO ()
main = do
  ca01 <- T.readFile brownCA01
  ca02 <- T.readFile (brownCAFiles!!1)
  let corpus = T.unlines [ca01, ca02]
  tagger <- train corpus
  T.putStrLn $ tagStr tagger input



brownCorporaDir :: FilePath
brownCorporaDir = "/home/creswick/nltk_data/corpora/brown"

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