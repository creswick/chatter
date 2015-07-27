{-# LANGUAGE OverloadedStrings #-}
module NLP.TokenizeTests where

import Test.Tasty.HUnit
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import NLP.Tokenize

import NLP.Types.Annotations
import NLP.POS
import qualified NLP.POS.LiteralTagger as LT

import TestUtils

tests :: TestTree
tests = testGroup "NLP.Tokenze" $ map mkTokTest
        [ ( "This is a test"
          , [ (0, "This")
            , (5, "is")
            , (8, "a")
            , (10, "test")
            ])
        , ("A url: http://google.com"
          , [ (0, "A")
            , (2, "url")
            , (5, ":")
            , (7, "http://google.com")
            ])
        , ("we'll don't."
        --  012345678901
          , [ (0, "we")
            , (2, "'ll")
            , (6, "do")
            , (8, "n't")
            , (11, ".")
            ])
        , ("jumped ."
        --  0123456789
          , [ (0, "jumped")
            , (7, ".")
            ])
        , ("jumped."
        --  0123456789
          , [ (0, "jumped")
            , (6, ".")
            ])
        , (" ., "
          , [(1, ".,")])
        , (" . "
          , [(1, ".")])
        , (" "
          , [])
        , ("  "
          , [])
        , (" \t "
          , [])
        ]

-- TODO: Test 'protectTerms'

mkTokSentence :: Text -> [(Int, Text)] -> TokenizedSentence
mkTokSentence dat toks = TokSentence { tokText = dat
                                     , tokAnnotations = map (mkTxtAnnotation dat) toks
                                     }

mkTxtAnnotation :: Text -> (Int, Text) -> Annotation Text Token
mkTxtAnnotation dat (idx, tok) = Annotation (Index idx) (T.length tok) (Token tok) dat

mkTokTest :: (Text, [(Int, Text)]) -> TestTree
mkTokTest (dat, toks) = let actual = tokenize dat
                            expected = mkTokSentence dat toks
                        in testCase (T.unpack ("\"" <> dat <> "\"")) (actual @?= expected)

