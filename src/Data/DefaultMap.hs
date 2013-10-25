module Data.DefaultMap
where

import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map

data DefaultMap k v = DefMap { defDefault :: v
                             , defMap :: Map k v
                             } deriving (Read, Show, Eq, Ord)

empty :: v -> DefaultMap k v
empty def = DefMap { defDefault = def
                   , defMap = Map.empty }

lookup :: Ord k => k -> DefaultMap k v ->  v
lookup k m = Map.findWithDefault (defDefault m) k (defMap m)

fromList :: Ord k => v -> [(k, v)] -> DefaultMap k v
fromList def entries = DefMap { defDefault = def
                              , defMap = Map.fromList entries }

keys :: DefaultMap k a -> [k]
keys m = Map.keys (defMap m)

foldl :: (a -> b -> a) -> a -> DefaultMap k b -> a
foldl fn acc m = Map.foldl fn acc (defMap m)