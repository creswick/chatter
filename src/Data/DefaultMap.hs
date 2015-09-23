{-# LANGUAGE DeriveGeneric #-}
module Data.DefaultMap
where

import Control.Applicative ((<$>), (<*>))
import Test.QuickCheck (Arbitrary(..))
import Control.DeepSeq (NFData)
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

instance (NFData k, NFData v, Hashable k) => NFData (DefaultMap k v)

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

-- | Fold over the values in the map.
--
-- Note that this *does* not fold
-- over the default value -- this fold behaves in the same way as a
-- standard `Data.Map.foldl`
foldl :: (a -> b -> a) -> a -> DefaultMap k b -> a
foldl fn acc m = Map.foldl' fn acc (defMap m)

instance (Arbitrary k, Arbitrary v, Hashable k, Eq k) => Arbitrary (DefaultMap k v) where
  arbitrary = do
      def <- arbitrary
      entries <- arbitrary
      return $ fromList def entries
