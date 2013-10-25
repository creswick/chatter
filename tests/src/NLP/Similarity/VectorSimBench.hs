{-# LANGUAGE OverloadedStrings #-}
module NLP.Similarity.VectorSimBench where

import Data.List.Split (splitWhen)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Criterion (bench, whnf )

import NLP.Tokenize (tokenize)
import NLP.Similarity.VectorSim

benchmarks docs testDocs = let
  corpus = mkCorpus docs
  in [ bench "Doc 1-2 vs 3-4" $ whnf (similarity corpus (concat $ take 2 testDocs))
                                                        ((testDocs!!2) ++ (testDocs!!3))
     , bench "Doc 1-5 vs 6-10" $ whnf (similarity corpus (concat $ take 5 testDocs))
                                                        (concat $ take 5 $ drop 5 testDocs)
     ]

sample1 :: Text
sample1 = "This is a sample document"

sample2 :: Text
sample2 = "This is another sample document"

readMucCorpus :: String -> IO [[Text]]
readMucCorpus file = do
  content <- T.readFile ("./tests/resources/corpora/muc3_4/"++file)
  let
    docMarker :: Text -> Bool
    docMarker txt = "DEV-MUC3-" `T.isPrefixOf` txt

    docLines :: [[Text]]
    docLines = splitWhen docMarker $ T.lines content

    documents :: [Text]
    documents = map T.unlines docLines

  return $ map tokenize documents

muc3_01 :: IO [[Text]]
muc3_01 = readMucCorpus "dev-muc3-0001-0100"

muc3_02 :: IO [[Text]]
muc3_02 = readMucCorpus "dev-muc3-0101-0200"

muc3_03 :: IO [[Text]]
muc3_03 = readMucCorpus "dev-muc3-0201-0300"