{-# LANGUAGE OverloadedStrings    #-}
module IntegrationTests

where

----------------------------------------------------------------------
import Test.QuickCheck.Instances ()
import Test.HUnit      (Assertion, assertFailure, assertEqual )
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (testCase)
import Test.HUnit      ( (@=?) )
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

genTestM :: (Show b, Show c, Eq c) => IO tgr -> (tgr -> b -> c) -> (String, b, c) -> TestTree
genTestM genTgr fn (descr, input, oracle) =
    testCase (descr++" [input: "++show input++"]") $ do
      tgr <- genTgr
      assert tgr
    where assert tgr = oracle @=? fn tgr input

tests :: TestTree
tests = withResource loadTaggers (\_ -> return ()) $ \getTaggers ->
          testGroup "Integration Tests"
                   [ testGroup "Default Tagger" $
                     map (genTestM (snd `fmap` getTaggers) tagText)
                     [ ("Simple 1", "The dog jumped.", "The/DT dog/NN jumped/VBD ./.")
                     ]
                   , testGroup "Brown Tagger" $
                     map (genTestM (fst `fmap` getTaggers) tagText)
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
  where
    loadTaggers = do
      brown <- brownTagger :: IO (POSTagger B.Tag)
      def <- defaultTagger :: IO (POSTagger C.Tag)
      return (brown, def)

examples :: [Text]
examples = [ "This/dt is/bez a/at test/nn ./."
           , "The/at dog/nn jumped/vbd over/in the/at cat/nn ./."
           , "Where/wrb is/bez the/at conference/nn ?/."
           ]

testSerialization :: [Text]  -- ^ A training corpus.  One sentence per entry.
                  -> ( String    -- ^ The name of the POS tagger.
                     , POSTagger B.Tag) -- ^ An empty (untrained) POS tagger.
                  -> TestTree
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
