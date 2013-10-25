module Data.DefaultMap
where

import Data.Map (Map)
import qualified Data.Map as Map

-- | Defaulting Map; a Map that returns a default value when queried
-- for a key that does not exist.
data DefaultMap k v = DefMap { defDefault :: v
                             , defMap :: Map k v
                             } deriving (Read, Show, Eq, Ord)


-- | Create an empty `DefaultMap`
empty :: v -> DefaultMap k v
empty def = DefMap { defDefault = def
                   , defMap = Map.empty }

-- | Query the map for a value.  Returns the default if the key is not
-- found.
lookup :: Ord k => k -> DefaultMap k v ->  v
lookup k m = Map.findWithDefault (defDefault m) k (defMap m)

-- | Create a `DefaultMap` from a default value and a list.
fromList :: Ord k => v -> [(k, v)] -> DefaultMap k v
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
foldl fn acc m = Map.foldl fn acc (defMap m)