{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as T

import Test.HUnit      ( (@=?) )
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.HUnit (testCase)
import Test.Tasty ( TestTree, defaultIngredients, defaultMainWithIngredients
                  , testGroup )
import Test.Tasty.Ingredients (Ingredient )
import Test.Tasty.Runners.AntXML ( antXMLRunner )

import NLP.Types (Tag(..), parseTag, RawTag(..), POSTagger(..))
import NLP.POS (tagText, train)
import NLP.Corpora.Parsing (readPOS)

import qualified AvgPerceptronTests as APT
import qualified BackoffTaggerTests as Backoff
import qualified Data.DefaultMapTests as DefMap
import qualified IntegrationTests as IT
import qualified NLP.Corpora.BrownTests as Brown
import qualified NLP.Corpora.ConllTests as Conll
import qualified NLP.Extraction.ParsecTests as Parsec
import qualified NLP.POS.AvgPerceptronTagger as APT
import qualified NLP.POS.UnambiguousTaggerTests as UT
import qualified NLP.POS.LiteralTaggerTests as LT
import qualified NLP.POSTests as POS
import qualified NLP.Similarity.VectorSimTests as Vec
import qualified NLP.TypesTests as TypeTests
import qualified NLP.Types.IOBTests as IOB
import qualified NLP.Types.TreeTests as Tree
import qualified NLP.Chunk.AvgPerceptronChunkerTests as APC

import Corpora

main :: IO ()
main = defaultMainWithIngredients ingredients tests

ingredients :: [Ingredient]
ingredients = antXMLRunner : defaultIngredients


tests :: TestTree
tests = testGroup "Tests"
        [ testGroup "parseTag" $
          [ testProperty "basic tag parsing" prop_parseTag]
        , testGroup "Train and tag"
          [ testGroup "miniCorpora1" $
            map (trainAndTagTest miniCorpora1)
             [ ("the dog jumped .", "the/DT dog/NN jumped/VB ./.") ]
          , testGroup "miniCorpora2" $
            map (trainAndTagTest miniCorpora1)
             [ ("the dog jumped .", "the/DT dog/NN jumped/VB ./.") ]
          , testGroup "miniCorpora1 - POSTagger train" $
            map (trainAndTagTestVTrainer miniCorpora1)
             [ ("the dog jumped .", "the/DT dog/NN jumped/VB ./.") ]
          , testGroup "miniCorpora2 - POSTagger train" $
            map (trainAndTagTestVTrainer miniCorpora1)
             [ ("the dog jumped .", "the/DT dog/NN jumped/VB ./.") ]
          ]
        , APT.tests
        , Backoff.tests
        , Vec.tests
        , POS.tests
        , UT.tests
        , LT.tests
        , TypeTests.tests
        , DefMap.tests
        , Parsec.tests
        , IT.tests
        , Brown.tests
        , Conll.tests
        , IOB.tests
        , APC.tests
        , Tree.tests
        ]

trainAndTagTestIO :: IO Text -> (Text, Text) -> TestTree
trainAndTagTestIO corpora (input, oracle) = testCase (T.unpack input) $ do
  let parser :: Text -> RawTag
      parser = parseTag
  perceptron <- APT.trainNew parser =<< corpora
  let tagger :: POSTagger RawTag
      tagger = (APT.mkTagger perceptron Nothing)
  oracle @=? tagText tagger input

trainAndTagTest :: Text -> (Text, Text) -> TestTree
trainAndTagTest corpora (input, oracle) = testCase (T.unpack input) $ do
  let parser :: Text -> RawTag
      parser = parseTag
  perceptron <- APT.trainNew parser corpora
  let tagger :: POSTagger RawTag
      tagger = (APT.mkTagger perceptron Nothing)
  oracle @=? tagText tagger input

trainAndTagTestVTrainer :: Text -> (Text, Text) -> TestTree
trainAndTagTestVTrainer corpora (input, oracle) = testCase (T.unpack input) $ do
  let newTagger :: POSTagger RawTag
      newTagger = APT.mkTagger APT.emptyPerceptron Nothing
      examples = map readPOS $ T.lines corpora
  posTgr <- train newTagger examples

  oracle @=? tagText posTgr input

prop_parseTag :: Text -> Bool
prop_parseTag txt = parseTag txt == RawTag txt

genTest :: (Show a, Show b, Eq b) => (a -> b) -> (String, a, b) -> TestTree
genTest fn (descr, input, oracle) =
    testCase (descr++" input: "++show input) assert
        where assert = oracle @=? fn input
