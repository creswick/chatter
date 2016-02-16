{-# LANGUAGE OverloadedStrings #-}
module NLP.Chunk.AvgPerceptronChunkerTests where

import Test.HUnit      ( (@=?) )
import Test.Tasty ( testGroup, TestTree, withResource )
import Test.QuickCheck.Instances ()
import Test.Tasty.HUnit (testCase, assertFailure)

import Data.Either (rights)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text (Text)

import qualified NLP.Corpora.Conll as C
import NLP.Types

import NLP.ML.AvgPerceptron hiding (train)
import NLP.Chunk.AvgPerceptronChunker
import NLP.Chunk
import NLP.POS hiding (train)

tests :: TestTree
tests = withResource loadChunkers (\_ -> return ()) $ \getChunkers ->
  testGroup "NLP.Chunk.AvgPerceptronChunker"
        [ testGroup "AvgPerceptronChunker - conll" $ map (test_chunk (fst `fmap` getChunkers))
          [ ("The dog jumped.", "[NP The/DT dog/NN] [VP jumped/VBD] ./.")
          , ("A chair.", "[NP A/DT chair/NN] ./.")
          ]
        , testGroup "AvgPerceptionChunker - naive" $ map (test_chunk (snd `fmap` getChunkers))
          [ ("The dog jumped.", "[NP The/DT dog/NN] [VP jumped/VBD] ./.")
          , ("A chair",  "[NP A/DT chair/NN]")
          ]
        ]

  where
    loadChunkers = do
      def <- conllChunker
      naive <- naiveChunker
      return (def, naive)

naiveChunker :: IO (Chunker C.Tag C.Chunk)
naiveChunker = do
  let chunker :: Chunker C.Tag C.Chunk
      chunker = mkChunker emptyPerceptron

      corpus :: [ChunkedSentence C.Tag C.Chunk]
      corpus = rights $ map parseChunkedSentence ["[NP The/DT dog/NN] [VP jumped/VBD] ./."]
  train chunker corpus

test_chunk :: IO (Chunker C.Tag C.Chunk) -> (Text, Text) -> TestTree
test_chunk genChunker (txt, oracleTxt) = testCase (T.unpack txt) $ do
  chk <- genChunker
  tgr <- conllTagger
  let tagged = tag tgr txt
      chunked = chChunker chk tagged

      eOracle :: Either Error (ChunkedSentence C.Tag C.Chunk)
      eOracle = parseChunkedSentence oracleTxt
  ("["<>oracleTxt<>"]") @=? prettyShow chunked
