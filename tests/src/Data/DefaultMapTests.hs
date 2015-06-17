module Data.DefaultMapTests where

import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Data.Serialize (decode, encode)

import Data.DefaultMap (DefaultMap(..))

tests :: TestTree
tests = testGroup "NLP.Data.DefaultMapTests"
        [ testGroup "Serialize / Deserialize Tests"
          [ testProperty "DefaultMap round-trips" prop_defMapSerialize
          ]
        ]

prop_defMapSerialize :: DefaultMap String String -> Bool
prop_defMapSerialize c = case (decode . encode) c of
                           Right c' -> c == c'
                           Left _ -> False
