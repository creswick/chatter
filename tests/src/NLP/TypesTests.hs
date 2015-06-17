module NLP.TypesTests where

import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Data.Serialize (decode, encode)
import NLP.Types (Corpus(..))


tests :: TestTree
tests = testGroup "NLP.Types"
        [ testGroup "Serialize / Deserialize Tests"
          [ testProperty "Corpus round-trips" prop_corpusSerialize
          ]
        ]

prop_corpusSerialize :: Corpus -> Bool
prop_corpusSerialize c = case (decode . encode) c of
                           Right c' -> c == c'
                           Left _ -> False
