{-# LANGUAGE OverloadedStrings    #-}
module IntegrationTests

where

----------------------------------------------------------------------
import Test.QuickCheck.Instances ()
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework ( testGroup, Test, buildTest )
import Test.HUnit      (Assertion, assertFailure, assertEqual )
----------------------------------------------------------------------
import Data.Text (Text)
import qualified Data.Map as Map
----------------------------------------------------------------------
import NLP.Types
import NLP.POS
import NLP.Corpora.Parsing

import qualified NLP.POS.AvgPerceptronTagger as Avg
import qualified NLP.POS.LiteralTagger       as LT
import qualified NLP.POS.UnambiguousTagger   as UT

import qualified NLP.Corpora.Brown as B
import qualified NLP.Corpora.Conll as C

import TestUtils

tests :: Test
tests = buildTest $ do
  brown <- brownTagger :: IO (POSTagger B.Tag)
  def <- defaultTagger :: IO (POSTagger C.Tag)
  return $ testGroup "Integration Tests"
        [ testGroup "Default Tagger" $
            map (genTest $ tagText def)
              [ ("Simple 1", "The dog jumped.", "The/DT dog/NN jumped/VBD ./.")
              ]
        , testGroup "Brown Tagger" $
            map (genTest $ tagText brown)
              [ ("Simple 1", "The dog jumped.", "The/AT dog/NN jumped/VBD ./.")
              ]
        , testGroup "POS Serialization" $
            map (testSerialization examples)
              [ ("Average Perceptron", Avg.mkTagger Avg.emptyPerceptron Nothing)
              , ("Unambiguous",  UT.mkTagger Map.empty Nothing)
              , ("Literal",  LT.mkTagger Map.empty Sensitive Nothing)
              , ("Unambiguous -> Avg"
                , UT.mkTagger Map.empty
                    (Just $ Avg.mkTagger Avg.emptyPerceptron Nothing))
              ]
        ]


examples :: [Text]
examples = [ "This/dt is/bez a/at test/nn ./."
           , "The/at dog/nn jumped/vbd over/in the/at cat/nn ./."
           , "Where/wrb is/bez the/at conference/nn ?/."
           ]

testSerialization :: [Text]  -- ^ A training corpus.  One sentence per entry.
                  -> ( String    -- ^ The name of the POS tagger.
                     , POSTagger B.Tag) -- ^ An empty (untrained) POS tagger.
                  -> Test
testSerialization training (name, newTagger) = testCase name doTest
  where
    doTest :: Assertion
    doTest = do
      preTagger <- train newTagger $ map readPOS training

      let ePostTagger :: Either String (POSTagger B.Tag)
          ePostTagger = deserialize taggerTable (serialize preTagger)
      case ePostTagger of
        Left err -> assertFailure ("Tagger did not deserialize: "++err)
        Right postTagger -> do
          let pre = map (tagText preTagger) training
              post = map (tagText postTagger) training
          assertEqual "Taggers tagged differently" pre post
