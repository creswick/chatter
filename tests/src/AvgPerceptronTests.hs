{-# OPTIONS_GHC -fno-warn-orphans #-}
module AvgPerceptronTests where

import Control.Applicative ((<$>))
import Test.QuickCheck ( Arbitrary(..), (==>), Property )
import Test.QuickCheck.Instances ()
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework ( testGroup, Test )

import Data.Serialize (encode, decode)
import Data.Map (Map)
import qualified Data.Map as Map

import NLP.ML.AvgPerceptron

tests :: Test
tests = testGroup "AvgPerceptron"
        [ testGroup "Encoding tests"
          [ testProperty "Features round-trip" prop_featureRoundtrips
          , testProperty "Perceptrons round-trip" prop_perceptronRoundtrips
          , testProperty "Map of features round-trip" prop_mapOfFeaturesRoundTrips
          ]
          , testProperty "Averaging reaches fixed point" prop_avgWeightsFixedpoint
        ]

prop_avgWeightsFixedpoint :: Perceptron -> Property
prop_avgWeightsFixedpoint per = instances per > 1 && Map.size (weights per) > 1 ==>
  (averageWeights per) == ((averageWeights . averageWeights) per)

prop_mapOfFeaturesRoundTrips :: Map Feature Class -> Bool
prop_mapOfFeaturesRoundTrips aMap =
  case (decode . encode) aMap of
    Left  _ -> False
    Right m -> m == aMap

prop_featureRoundtrips :: Feature -> Bool
prop_featureRoundtrips feat =
  case (decode . encode) feat of
    Left  _ -> False
    Right f -> f == feat

prop_perceptronRoundtrips :: Perceptron -> Bool
prop_perceptronRoundtrips per =
  case (decode . encode) per of
    Left  _ -> False
    Right p -> p == per

instance Arbitrary Feature where
  arbitrary = Feat <$> arbitrary

instance Arbitrary Class where
  arbitrary = Class <$> arbitrary

instance Arbitrary Perceptron where
  arbitrary = do ws <- arbitrary
                 ts <- arbitrary
                 times <- arbitrary
                 counts <- arbitrary
                 return Perceptron { weights = ws
                                   , totals = ts
                                   , tstamps = times
                                   , instances = counts
                                   }
