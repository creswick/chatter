{-# LANGUAGE OverloadedStrings #-}
module BackoffTaggerTests where

import Test.HUnit      ( (@=?), Assertion )
import Test.Framework ( testGroup, Test )
import Test.Framework.Providers.HUnit (testCase)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import NLP.Types
import NLP.POS

import qualified NLP.POS.LiteralTagger as LT

tests :: Test
tests = testGroup "Backoff Tagging"
        [ testCase "Simple back-off tagging" testLiteralBackoff
        ]

tagCat :: Map Text Tag
tagCat = Map.fromList [("cat", Tag "CAT")]

tagAnimals :: Map Text Tag
tagAnimals = Map.fromList [("cat", Tag "NN"), ("dog", Tag "NN")]

testLiteralBackoff :: Assertion
testLiteralBackoff = let
  tgr = LT.mkTagger tagCat LT.Sensitive (Just $ LT.mkTagger tagAnimals LT.Sensitive Nothing)
  actual = tag tgr "cat dog"
  oracle = [[("cat", Tag "CAT"), ("dog", Tag "NN")]]
  in oracle @=? actual