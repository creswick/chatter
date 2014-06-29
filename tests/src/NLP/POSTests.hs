{-# LANGUAGE OverloadedStrings #-}
module NLP.POSTests where

import Test.HUnit      ( (@=?), Assertion )
import Test.Framework ( testGroup, Test )
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck.Instances ()
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import NLP.Types
import NLP.POS
import qualified NLP.POS.LiteralTagger as LT

import TestUtils

tests :: Test
tests = testGroup "NLP.POS"
        [ testGroup "Evaluation" $ map (genTestF $ eval mamalTagger)
             [ ("Half", [ [ ("the", Tag "DT"), ("cat", Tag "NN")]
                        , [ ("the", Tag "DT"), ("dog", Tag "NN")] ], 0.5)
             , ("All ", [ [ ("dog", Tag "NN"), ("cat", Tag "NN")] ], 1.0)
             , ("None", [ [ ("the", Tag "DT"), ("couch", Tag "NN")] ], 0)
             ]
        , testGroup "Serialization"
             [ testProperty "1 LiteralTagger" (prop_taggersRoundTrip mamalTagger)
             , testProperty "2 LiteralTaggers" (prop_taggersRoundTrip animalTagger)
             ]
        ]

animalTagger :: POSTagger
animalTagger = LT.mkTagger (Map.fromList [("owl", Tag "NN"), ("flea", Tag "NN")]) LT.Sensitive (Just mamalTagger)

mamalTagger :: POSTagger
mamalTagger = LT.mkTagger (Map.fromList [("cat", Tag "NN"), ("dog", Tag "NN")]) LT.Sensitive Nothing

-- TODO need to make random taggers to really test this...
prop_taggersRoundTrip :: POSTagger -> String -> Bool
prop_taggersRoundTrip tgr input =
  let Right roundTripped = deserialize taggerTable $ serialize tgr
  in tagStr tgr ("cat owl " ++ input) == tagStr roundTripped ("cat owl " ++ input)