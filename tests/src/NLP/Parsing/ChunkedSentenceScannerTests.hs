{-# LANGUAGE OverloadedStrings #-}
module NLP.Parsing.ChunkedSentenceScannerTests

where

import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck ( testProperty, Property, (==>), generate
                             , QuickCheckTests(..), QuickCheckMaxSize(..))
import Test.HUnit      ( (@=?) )
import Test.Tasty.HUnit (testCase)
import Test.QuickCheck.Monadic
import Test.QuickCheck.Modifiers

import Data.List (group)
import Data.Serialize (decode, encode)
import Data.Text (Text)
import qualified Data.Text as T

import NLP.Parsing.ChunkedSentenceScanner

tests :: TestTree
tests = testGroup "NLP.Parsing.ChunkedSentenceScannerTests"
        [ testGroup "specific tests" $
          map mkScannerTest
          [ ( "A[NP bad]test"
            , [ (0, Tok "A"), (1, ChunkStart "NP"), (5, Tok "bad")
              , (8, ChunkEnd), (9, Tok "test")])
--            [(0, Tok "A[NP"), (5, Tok "bad]test")])
          ,  ( "A [NP better] test"
             -- 012345678901234567
             , [ (0, Tok "A"), (2, ChunkStart "NP"), (6, Tok "better")
               , (12, ChunkEnd), (14, Tok "test")])
          ,  ( "[NP A/DT] better"
             -- 01234567890123456
             , [ (0, ChunkStart "NP"), (4, Tok "A"), (5, Pos "DT"), (8, ChunkEnd)
               , (10, Tok "better") ])
          ,  ( "[NP A/DT] better/JJR [NP test/NN]"
             -- 012345678901234567890123456789012
             , [ (0, ChunkStart "NP"), (4, Tok "A"), (5, Pos "DT"), (8, ChunkEnd)
               , (10, Tok "better"), (16, Pos "JJR")
               , (21, ChunkStart "NP"), (25, Tok "test"), (29, Pos "NN"), (32, ChunkEnd)])
          ]
        ]

mkScannerTest :: (String, [(Int, Lexeme)]) -> TestTree
mkScannerTest (input, expected) = testCase input $
  expected @=? alexScanTokens input
