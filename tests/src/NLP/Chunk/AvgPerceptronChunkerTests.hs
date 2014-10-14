{-# LANGUAGE OverloadedStrings #-}
module NLP.Chunk.AvgPerceptronChunkerTests where

import Test.HUnit      ( (@=?), Assertion )

import Test.Framework ( testGroup, Test, buildTest )
import Test.QuickCheck (Arbitrary(..), listOf, elements, NonEmptyList(..), (==>), Property)
import Test.QuickCheck.Instances ()
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Data.Char (isSpace)
import qualified Data.Text as T
import Data.Text (Text)

import qualified NLP.Corpora.Conll as C
import NLP.Types

import NLP.POS.AvgPerceptron hiding (train)
import NLP.Chunk.AvgPerceptronChunker
import NLP.Chunk
import NLP.POS hiding (train)

tests :: Test
tests = buildTest $ do
  def <- conllChunker
  naive <- naiveChunker
  uniform <- uniformChunker

  return $ testGroup "NLP.Chunk.AvgPerceptronChunker"
        [ testGroup "AvgPerceptronChunker - conll" $ map (test_chunk def)
          [ ("The dog jumped.", [ChunkedSent [ mkChunk C.NP
                                               [ mkChink C.DT "The"
                                               , mkChink C.NN "dog"
                                               ]
                                             , mkChunk C.VP [mkChink C.VBD "jumped"]
                                             , mkChink C.Term "."
                                             ]]
            )
          , ("A chair.",  [ChunkedSent [mkChunk C.NP
                                        [ mkChink C.DT "A"
                                        , mkChink C.NN "chair"
                                        ]
                                       , mkChink C.Term "."
                                       ]])
          -- , ("Confidence in the pound is widely expected to take another sharp dive."
          --   , [ChunkedSent []])
          ]
        , testGroup "AvgPerceptionChunker - naive" $ map (test_chunk naive)
          [ ("The dog jumped.", [ChunkedSent [ mkChunk C.NP
                                               [ mkChink C.DT "The"
                                               , mkChink C.NN "dog"
                                               ]
                                             , mkChunk C.VP [mkChink C.VBD "jumped"]
                                             , mkChink C.Term "."
                                             ]]
            )
          , ("A chair",  [ChunkedSent [mkChunk C.NP
                                        [ mkChink C.DT "A"
                                        , mkChink C.NN "chair"
                                        ]
                                       ]])
          ]
        ]

naiveChunker :: IO (Chunker C.Chunk C.Tag)
naiveChunker = do
  let chunker :: Chunker C.Chunk C.Tag
      chunker = mkChunker emptyPerceptron
  train chunker
         [ChunkedSent [ mkChunk C.NP
                          [ mkChink C.DT "The"
                          , mkChink C.NN "dog"
                          ]
                        , mkChunk C.VP [mkChink C.VBD "jumped"]
                        , mkChink C.Term "."
                        ]
         ]

uniformChunker :: IO (Chunker C.Chunk C.Tag)
uniformChunker = do
  let chunker :: Chunker C.Chunk C.Tag
      chunker = mkChunker emptyPerceptron
  train chunker
         [ChunkedSent [ mkChunk C.NP [ mkChink C.DT "The"]
                      , mkChunk C.NP [ mkChink C.NN "dog"]
                      , mkChunk C.NP [mkChink C.VBD "jumped"]
                      , mkChunk C.NP [mkChink C.Term "."]
                      ]
         ]

test_chunk :: Chunker C.Chunk C.Tag -> (Text, [ChunkedSentence C.Chunk C.Tag]) -> Test
test_chunk chk (txt, oracle) = testCase (T.unpack txt) $ do
  tgr <- conllTagger
  let tagged = tag tgr txt
      chunked = chChunker chk tagged
  oracle @=? chunked


