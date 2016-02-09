{-# LANGUAGE OverloadedStrings #-}
module NLP.POSTests where

import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.HUnit      ( (@=?) )
import Test.Tasty.HUnit (testCase)

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import NLP.Types
import NLP.POS
import NLP.Corpora.Parsing (readPOS, readCorpus)
import qualified NLP.POS.LiteralTagger as LT
import qualified NLP.POS.AvgPerceptronTagger as APT

import TestUtils
import Corpora

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
        , testGroup "Train and tag; Text -> Text"
          [ testGroup "miniCorpora1" $
            map (trainAndTagTextTest miniCorpora1)
             [ ("the dog jumped .", "the/DT dog/NN jumped/VB ./.") ]
          , testGroup "miniCorpora2" $
            map (trainAndTagTextTest miniCorpora2)
             [ ("the dog jumped .", "the/DT dog/NN jumped/VB ./.") ]
          , testGroup "miniCorpora1 - POSTagger train" $
            map (trainAndTagTextTestVTrainer miniCorpora1)
             [ ("the dog jumped .", "the/DT dog/NN jumped/VB ./.") ]
          , testGroup "miniCorpora2 - POSTagger train" $
            map (trainAndTagTextTestVTrainer miniCorpora2)
             [ ("the dog jumped .", "the/DT dog/NN jumped/VB ./.") ]
          , testGroup "miniCorpora2 - POSTagger train, no space punct." $
            map (trainAndTagTextTestVTrainer miniCorpora2)
             [ ("the dog jumped.", "the/DT dog/NN jumped/VB ./.") ]
          ]
        , testGroup "Train and tag; Text -> TaggedSentence"
          [ testGroup "miniCorpora1" $
            map (trainAndTagTest miniCorpora1)
             [ ("the dog jumped .", [readPOS "the/DT dog/NN jumped/VB ./."]) ]
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

trainAndTagTextTestIO :: IO Text -> (Text, Text) -> TestTree
trainAndTagTextTestIO corpora (input, oracle) = testCase (T.unpack input) $ do
  let parser :: Text -> [TaggedSentence RawTag]
      parser = readCorpus
  perceptron <- APT.trainNew parser =<< corpora
  let tagger :: POSTagger RawTag
      tagger = (APT.mkTagger perceptron Nothing)
  oracle @=? tagText tagger input

trainAndTagTextTest :: Text -> (Text, Text) -> TestTree
trainAndTagTextTest corpora (input, oracle) = testCase (T.unpack input) $ do
  let parser :: Text -> [TaggedSentence RawTag]
      parser = readCorpus
  perceptron <- APT.trainNew parser corpora
  let tagger :: POSTagger RawTag
      tagger = (APT.mkTagger perceptron Nothing)
  oracle @=? tagText tagger input

trainAndTagTextTestVTrainer :: Text -> (Text, Text) -> TestTree
trainAndTagTextTestVTrainer corpora (input, oracle) = testCase (T.unpack input) $ do
  let newTagger :: POSTagger RawTag
      newTagger = APT.mkTagger APT.emptyPerceptron Nothing
      examples = map readPOS $ T.lines corpora
  posTgr <- train newTagger examples

  oracle @=? tagText posTgr input

trainAndTagTest :: Text -> (Text, [TaggedSentence RawTag]) -> TestTree
trainAndTagTest corpora (input, oracle) = testCase (T.unpack input) $ do
  let parser :: Text -> [TaggedSentence RawTag]
      parser = readCorpus
  perceptron <- APT.trainNew parser corpora
  let tagger :: POSTagger RawTag
      tagger = (APT.mkTagger perceptron Nothing)
  oracle @=? tag tagger input
