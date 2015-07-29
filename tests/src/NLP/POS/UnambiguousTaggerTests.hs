{-# LANGUAGE OverloadedStrings #-}
module NLP.POS.UnambiguousTaggerTests where

import Test.HUnit      ( (@=?) )
import Test.QuickCheck ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import NLP.Types
import NLP.POS
import qualified NLP.POS.UnambiguousTagger as UT

tests :: TestTree
tests = testGroup "NLP.POS.UnambiguousTagger"
        [ testProperty "basic tag parsing" prop_emptyAlwaysUnk
        , testGroup "Initial training" $ map (trainAndTagTest emptyTagger)
          [ ("the/dt dog/nn jumped/vb", "a dog"
            , "a/Unk dog/nn")
          , ("the/dt dog/nn jumped/vb jumped/vbx", "a dog jumped"
            , "a/Unk dog/nn jumped/Unk")
          ]
        , testGroup "Retraining" $ map (trainAndTagTest trainedTagger)
          [ ("the/dt dog/nn jumped/vb", "the dog"
            , "the/dt dog/Unk")
          , ("the/dt dog/nn jumped/vb jumped/vbx", "the dog jumped"
            , "the/dt dog/Unk jumped/Unk")
          ]
        ]

emptyTagger :: POSTagger RawTag
emptyTagger = UT.mkTagger Map.empty Nothing

trainedTagger :: POSTagger RawTag
trainedTagger = UT.mkTagger (Map.fromList [("the", RawTag "dt"), ("dog", RawTag "vb")]) Nothing

prop_emptyAlwaysUnk :: String -> Bool
prop_emptyAlwaysUnk input = all (\y -> y == tagUNK) (concatMap getTags $ tag emptyTagger inputTxt)
  where inputTxt = T.pack input

trainAndTagTest :: POS pos => POSTagger pos -> (Text, Text, Text) -> TestTree
trainAndTagTest tgr (exs, input, oracle) = testCase (T.unpack (T.intercalate ": " [exs, input])) $ do
  trained <- trainText tgr exs
  oracle @=? tagText trained input
