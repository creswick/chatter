{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.DefaultMap
where

import Prelude hiding (lookup)
import Test.QuickCheck (Arbitrary(..))
import Control.DeepSeq (NFData(..), deepseq)
import qualified Data.HashSet as S
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Serialize
import GHC.Generics

-- | Defaulting Map; a Map that returns a default value when queried
-- for a key that does not exist.
data DefaultMap k v =
   DefMap { defDefault :: v
          , defMap :: HashMap k v
          } deriving (Read, Show, Eq, Generic)

instance (Eq k, Hashable k, Serialize k, Serialize v) => Serialize (DefaultMap k v) where
  put (DefMap d theMap) = put d >> put (Map.toList theMap)
  get = DefMap <$> get <*> (Map.fromList <$> get)

instance (NFData k, NFData v, Hashable k) => NFData (DefaultMap k v) where
    rnf (DefMap d m) = d `deepseq` m `deepseq` ()

-- | Create an empty `DefaultMap`
empty :: v -> DefaultMap k v
empty def = DefMap { defDefault = def
                   , defMap = Map.empty }

-- | Query the map for a value.  Returns the default if the key is not
-- found.
lookup :: (Eq k, Hashable k) => k -> DefaultMap k v ->  v
lookup k m = Map.lookupDefault (defDefault m) k (defMap m)

-- | Create a `DefaultMap` from a default value and a list.
fromList :: (Eq k, Hashable k) => v -> [(k, v)] -> DefaultMap k v
fromList def entries = DefMap { defDefault = def
                              , defMap = Map.fromList entries }

-- | Access the keys as a list.
keys :: DefaultMap k a -> [k]
keys m = Map.keys (defMap m)

-- | Access the non-default values as a list.
elems :: DefaultMap k a -> [a]
elems = Map.elems . defMap

-- | Map a function over the values in a map.
map :: (a -> a) -> DefaultMap k a -> DefaultMap k a
map f m = m { defMap = f <$> defMap m }

-- | Fold over the values in the map.
--
-- Note that this *does* not fold
-- over the default value -- this fold behaves in the same way as a
-- standard `Data.Map.foldl`
foldl :: (a -> b -> a) -> a -> DefaultMap k b -> a
foldl fn acc m = Map.foldl' fn acc (defMap m)

-- | Compute the union of two maps using the specified per-value
-- combination function and the specified new map default value.
unionWith :: forall v k . (Eq k, Hashable k)
          => (v -> v -> v)
          -- ^ Combine values with this function
          -> v
          -- ^ The new map's default value
          -> DefaultMap k v
          -- ^ The first map to combine
          -> DefaultMap k v
          -- ^ The second map to combine
          -> DefaultMap k v
unionWith f newDef a b =
    let allKeys = S.unions [ S.fromList $ Map.keys $ defMap a
                           , S.fromList $ Map.keys $ defMap b
                           ]
        mergeKey :: k -> Map.HashMap k v -> Map.HashMap k v
        mergeKey k m = Map.insert k (f (lookup k a) (lookup k b)) m

    in DefMap { defDefault = newDef
              , defMap = S.foldr mergeKey Map.empty allKeys
              }

instance (Arbitrary k, Arbitrary v, Hashable k, Eq k) => Arbitrary (DefaultMap k v) where
  arbitrary = do
      def <- arbitrary
      entries <- arbitrary
      return $ fromList def entries
