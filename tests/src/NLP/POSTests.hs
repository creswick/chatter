{-# LANGUAGE OverloadedStrings #-}
module NLP.POSTests where

import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import qualified Data.Map as Map
import Data.Text (Text)

import NLP.Types
import NLP.POS
import NLP.Corpora.Parsing (readPOS)
import qualified NLP.POS.LiteralTagger as LT

import TestUtils

readRawPOS :: Text -> TaggedSentence RawTag
readRawPOS = readPOS

tests :: TestTree
tests = testGroup "NLP.POS"
        [ testGroup "Evaluation" $ map (genTestF $ eval mamalTagger)
             [ ("Half", [ readRawPOS "the/DT cat/NN"
                        , readRawPOS "the/DT dog/NN" ], 0.5)
             , ("All ", [ readRawPOS "dog/NN cat/NN" ], 1.0)
             , ("None", [ readRawPOS "the/DT couch/NN" ], 0)
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
