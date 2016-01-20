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
            , [ Tok 0 "A", ChunkStart 1 "NP", Tok 5 "bad"
              , ChunkEnd 8, Tok 9 "test"])
--            [(0, Tok "A[NP"), (5, Tok "bad]test")])
          ,  ( "A [NP better] test"
             -- 012345678901234567
             , [ Tok 0 "A", ChunkStart 2 "NP", Tok 6 "better"
               , ChunkEnd 12, Tok 14 "test"])
          ,  ( "[NP A/DT] better"
             -- 01234567890123456
             , [ ChunkStart 0 "NP", Tok 4 "A", Pos 5 "DT", ChunkEnd 8
               , Tok 10 "better" ])
          ,  ( "[NP A/DT] better/JJR [NP test/NN]"
             -- 012345678901234567890123456789012
             , [ ChunkStart 0 "NP", Tok 4 "A", Pos 5 "DT", ChunkEnd 8
               , Tok 10 "better", Pos 16 "JJR"
               , ChunkStart 21 "NP", Tok 25 "test", Pos 29 "NN", ChunkEnd 32])
          ]
        ]

mkScannerTest :: (String, [Lexeme]) -> TestTree
mkScannerTest (input, expected) = testCase input $
  expected @=? alexScanTokens input
