{-# LANGUAGE OverloadedStrings #-}
module NLP.POSTests where

import Test.HUnit      ( (@=?), Assertion )
import Test.Framework ( testGroup, Test )
import Test.Framework.Providers.HUnit (testCase)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import NLP.Types
import NLP.POS
import qualified NLP.POS.LiteralTagger as LT

import TestUtils

tests :: Test
tests = testGroup "NLP.POS"
        [ testGroup "Evaluation" $ map (genTestF $ eval animalTagger)
             [ ("Half", [ [ ("the", Tag "DT"), ("cat", Tag "NN")]
                        , [ ("the", Tag "DT"), ("dog", Tag "NN")] ], 0.5)
             , ("All ",  [ [ ("dog", Tag "NN"), ("cat", Tag "NN")] ], 1.0)
             , ("None", [ [ ("the", Tag "DT"), ("couch", Tag "NN")] ], 0)
             ]
        ]

animalTagger :: POSTagger
animalTagger = LT.mkTagger (Map.fromList [("cat", Tag "NN"), ("dog", Tag "NN")]) Nothing
