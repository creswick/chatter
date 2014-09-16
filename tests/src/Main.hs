{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Test.HUnit      ( (@=?) )
import Test.QuickCheck ()
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework ( buildTest, testGroup, Test, defaultMain )
-- import Test.Framework.Skip (skip)

import NLP.Types (Tag(..), parseTag)
import NLP.POS (tagText, train)
import NLP.Corpora.Parsing (readPOS)

import qualified NLP.POS.AvgPerceptronTagger as APT
import qualified AvgPerceptronTests as APT
import qualified BackoffTaggerTests as Backoff
import qualified NLP.Similarity.VectorSimTests as Vec
import qualified NLP.POSTests as POS
import qualified NLP.POS.UnambiguousTaggerTests as UT
import qualified NLP.POS.LiteralTaggerTests as LT
import qualified NLP.TypesTests as TypeTests
import qualified Data.DefaultMapTests as DefMap

import Corpora

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testGroup "parseTag" $
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
          -- , skip $ testGroup "brown CA01" $
          --   map (trainAndTagTestFileCorpus brownCA01)
          --    [ ("the dog jumped .", "the/at dog/nn jumped/Unk ./.") ]
          -- , skip $ testGroup "brown CA" $
          --   map (trainAndTagTestIO brownCA)
          --    [ ("the dog jumped .", "the/at dog/nn jumped/vbd ./.") ]
          ]
        , APT.tests
        , Backoff.tests
        , Vec.tests
        , POS.tests
        , UT.tests
        , LT.tests
        , TypeTests.tests
        , DefMap.tests
        ]


trainAndTagTestFileCorpus :: FilePath -> (Text, Text) -> Test
trainAndTagTestFileCorpus file args = buildTest $ do
  corpus <- T.readFile file
  return $ trainAndTagTest corpus args

trainAndTagTestIO :: IO Text -> (Text, Text) -> Test
trainAndTagTestIO corpora (input, oracle) = testCase (T.unpack input) $ do
  tagger <- APT.trainNew =<< corpora
  oracle @=? tagText (APT.mkTagger tagger Nothing) input

trainAndTagTest :: Text -> (Text, Text) -> Test
trainAndTagTest corpora (input, oracle) = testCase (T.unpack input) $ do
  tagger <- APT.trainNew corpora
  oracle @=? tagText (APT.mkTagger tagger Nothing) input

trainAndTagTestVTrainer :: Text -> (Text, Text) -> Test
trainAndTagTestVTrainer corpora (input, oracle) = testCase (T.unpack input) $ do
  let newTagger = APT.mkTagger APT.emptyPerceptron Nothing
      examples = map readPOS $ T.lines corpora
  posTgr <- train newTagger examples

  oracle @=? tagText posTgr input

prop_parseTag :: Text -> Bool
prop_parseTag txt = parseTag txt == Tag txt

genTest :: (Show a, Show b, Eq b) => (a -> b) -> (String, a, b) -> Test
genTest fn (descr, input, oracle) =
    testCase (descr++" input: "++show input) assert
        where assert = oracle @=? fn input