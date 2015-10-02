module Data.DefaultMapTests where

import Control.Applicative ((<$>))
import qualified Data.HashSet as S
import qualified Data.Text as T
import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Data.Function (on)
import Data.List (nubBy)

import Data.Serialize (decode, encode)

import qualified Data.DefaultMap as DM

tests :: TestTree
tests = testGroup "NLP.Data.DefaultMapTests"
        [ testGroup "Serialize / Deserialize Tests"
          [ testProperty "DefaultMap round-trips" prop_defMapSerialize
          , testProperty "DefaultMap elems" prop_elems
          , testProperty "DefaultMap unionWith" prop_unionWith
          , testProperty "DefaultMap map" prop_map
          ]
        ]

prop_defMapSerialize :: DM.DefaultMap String String -> Bool
prop_defMapSerialize c = case (decode . encode) c of
                           Right c' -> c == c'
                           Left _ -> False

prop_elems :: [(T.Text, Int)] -> Bool
prop_elems pairs =
    let uPairs = nubBy ((==) `on` fst) pairs
    in (S.fromList $ snd <$> uPairs) ==
       (S.fromList $ DM.elems $ DM.fromList 0 uPairs)

prop_map :: [(T.Text, Int)] -> Bool
prop_map pairs =
    let uPairs = nubBy ((==) `on` fst) pairs
        f = (+ 4)
    in (S.fromList $ f <$> snd <$> uPairs) ==
       (S.fromList $ DM.elems $ DM.map f $ DM.fromList 0 uPairs)

prop_unionWith :: DM.DefaultMap T.Text Int -> DM.DefaultMap T.Text Int -> Bool
prop_unionWith a b =
    let f = (+)
        result = DM.unionWith f 0 a b
    in and [ f (DM.lookup k a) (DM.lookup k b) == DM.lookup k result
           | k <- S.toList $ S.fromList (DM.keys a ++ DM.keys b)
           ]
