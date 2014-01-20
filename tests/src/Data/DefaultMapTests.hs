module Data.DefaultMapTests where

import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Instances ()
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework ( testGroup, Test )

import Data.Serialize (decode, encode)

import Data.DefaultMap (DefaultMap(..), fromList)

tests :: Test
tests = testGroup "NLP.Data.DefaultMapTests"
        [ testGroup "Serialize / Deserialize Tests"
          [ testProperty "DefaultMap round-trips" prop_defMapSerialize
          ]
        ]

instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (DefaultMap k v) where
  arbitrary = do
      def <- arbitrary
      entries <- arbitrary
      return $ fromList def entries

prop_defMapSerialize :: DefaultMap String String -> Bool
prop_defMapSerialize c = case (decode . encode) c of
                           Right c' -> c == c'
                           Left _ -> False