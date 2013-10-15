{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (foldM)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.FilePath ((</>))
import Text.Printf (printf)

import qualified NLP.POS.AvgPerceptron as Per
import NLP.POS.AvgPerceptron (Perceptron)
import NLP.POS (tagStr, train)

input :: Text
input = "the dog jumped"

main :: IO ()
main = do
  let step :: Perceptron -> FilePath -> IO Perceptron
      step per path = do
        content <- T.readFile path
        train per content

  tagger <- foldM step Per.emptyPerceptron $ take 15 brownCAFiles
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