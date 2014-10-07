{-# LANGUAGE OverloadedStrings #-}
module NLP.POSTests where

import Test.Framework ( testGroup, Test )
import Test.QuickCheck.Instances ()
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Data.Map as Map

import NLP.Types
import NLP.POS
import qualified NLP.POS.LiteralTagger as LT

import TestUtils

tests :: Test
tests = testGroup "NLP.POS"
        [ testGroup "Evaluation" $ map (genTestF $ eval mamalTagger)
             [ ("Half", [ TS [ ("the", RawTag "DT"), ("cat", RawTag "NN")]
                        , TS [ ("the", RawTag "DT"), ("dog", RawTag "NN")] ], 0.5)
             , ("All ", [ TS [ ("dog", RawTag "NN"), ("cat", RawTag "NN")] ], 1.0)
             , ("None", [ TS [ ("the", RawTag "DT"), ("couch", RawTag "NN")] ], 0)
             ]
        , testGroup "Serialization"
             [ testProperty "1 LiteralTagger" (prop_taggersRoundTrip mamalTagger)
             , testProperty "2 LiteralTaggers" (prop_taggersRoundTrip animalTagger)
             ]
        ]

animalTagger :: POSTagger RawTag
animalTagger = LT.mkTagger (Map.fromList [("owl", RawTag "NN"), ("flea", RawTag "NN")]) LT.Sensitive (Just mamalTagger)

mamalTagger :: POSTagger RawTag
mamalTagger = LT.mkTagger (Map.fromList [("cat", RawTag "NN"), ("dog", RawTag "NN")]) LT.Sensitive Nothing

-- TODO need to make random taggers to really test this...
prop_taggersRoundTrip :: POSTagger RawTag -> String -> Bool
prop_taggersRoundTrip tgr input =
  let Right roundTripped = (deserialize taggerTable $ serialize tgr) :: Either String (POSTagger RawTag)
  in tagStr tgr ("cat owl " ++ input) == tagStr roundTripped ("cat owl " ++ input)
