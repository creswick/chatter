{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module NLP.Similarity.VectorSimBench where

import Data.List.Split (splitWhen)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Criterion (bench, whnf, Benchmark)

import NLP.Tokenize.Text (tokenize)
import NLP.Similarity.VectorSim
import NLP.Types (mkCorpus, Corpus)

benchmarks :: [[Text]] -> [[Text]] -> [Benchmark]
benchmarks docs testDocs = let
  corpus = mkCorpus docs
  in [ bench "Doc 1-2 vs 3-4" $ whnf (similarity corpus (concat $ take 2 testDocs))
                                                        ((testDocs!!2) ++ (testDocs!!3))
     , bench "Doc 1-5 vs 6-10" $ whnf (similarity corpus (concat $ take 5 testDocs))
                                                        (concat $ take 5 $ drop 5 testDocs)
     , bench "all pairs of 1-5" $ whnf (docsRunAllPairs corpus) (take 5 testDocs)

     , bench "TV all pairs of 1-5" $ whnf (tvDocsRunAllPairs corpus) (take 5 testDocs)
     ]

docsRunAllPairs :: Corpus -> [[Text]] -> Double
docsRunAllPairs _ [] = 0
docsRunAllPairs corpus (d:ds) = let
   firstRow = foldl (\v doc -> v + similarity corpus d doc) 0 ds
   in firstRow + (docsRunAllPairs corpus ds)

tvDocsRunAllPairs :: Corpus -> [[Text]] -> Double
tvDocsRunAllPairs corpus docs = runVectors (map (mkVector corpus) docs)
  where
    runVectors :: [TermVector] -> Double
    runVectors [] = 0
    runVectors (d:ds) = let
      firstRow = foldl (\v doc -> v + tvSim d doc) 0 ds
      in firstRow + (runVectors ds)


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