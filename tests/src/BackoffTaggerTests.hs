{-# LANGUAGE OverloadedStrings #-}
module BackoffTaggerTests where

import Test.HUnit      ( (@=?), Assertion )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import NLP.Types
import NLP.POS

import qualified NLP.POS.LiteralTagger as LT

tests :: TestTree
tests = testGroup "Backoff Tagging"
        [ testCase "Simple back-off tagging" testLiteralBackoff
        ]

tagCat :: Map Text RawTag
tagCat = Map.fromList [("cat", RawTag "CAT")]

tagAnimals :: Map Text RawTag
tagAnimals = Map.fromList [("cat", RawTag "NN"), ("dog", RawTag "NN")]

testLiteralBackoff :: Assertion
testLiteralBackoff = let
  tgr = LT.mkTagger tagCat LT.Sensitive (Just $ LT.mkTagger tagAnimals LT.Sensitive Nothing)
  actual = tag tgr "cat dog"
  oracle = [TaggedSent [(POS (RawTag "CAT") "cat"), (POS (RawTag "NN") "dog")]]
  in oracle @=? actual
