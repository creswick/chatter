{-# LANGUAGE OverloadedStrings #-}
module NLP.Chunk.AvgPerceptronChunkerTests where

import Test.HUnit      ( (@=?), Assertion )

import Test.Framework ( testGroup, Test )
import Test.QuickCheck (Arbitrary(..), listOf, elements, NonEmptyList(..), (==>), Property)
import Test.QuickCheck.Instances ()
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Data.Char (isSpace)
import qualified Data.Text as T
import Data.Text (Text)

import qualified NLP.Corpora.Conll as C
import NLP.Types

import NLP.Chunk.AvgPerceptronChunker
import NLP.Chunk
import NLP.POS

tests :: Test
tests = testGroup "NLP.Chunk.AvgPerceptronChunker"
        [ testGroup "AvgPerceptronChunker" $ map test_chunk
          [ "The dog jumped."
          , "A chair."
          , "Confidence in the pound is widely expected to take another sharp dive."
          ]
        ]

conllChunker :: IO (Chunker C.Chunk C.Tag)
conllChunker = loadChunker "conll2000.chunk.model"

conllTagger :: IO (POSTagger C.Tag)
conllTagger = loadTagger "conll2000.pos.model"

test_chunk :: Text -> Test
test_chunk txt = testCase (T.unpack txt) $ do
  tgr <- conllTagger
  chk <- conllChunker
  let tagged = tag tgr txt
      chunked = chChunker chk tagged
      oracle = [ChunkedSent []] -- TODO obviously wrong.
  oracle @=? chunked


