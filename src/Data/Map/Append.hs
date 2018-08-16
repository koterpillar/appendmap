module Data.Map.Append where

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.List.NonEmpty as NE

import Data.Monoid
import Data.Semigroup

newtype AppendMap k v = AppendMap
  { unAppendMap :: Map k v
  }

instance (Ord k, Semigroup v) => Semigroup (AppendMap k v) where
  AppendMap a <> AppendMap b = AppendMap $ Map.unionWith (<>) a b
  sconcat = AppendMap . Map.unionsWith (<>) . NE.toList . fmap unAppendMap

instance (Ord k, Monoid v) => Monoid (AppendMap k v) where
  mempty = AppendMap Map.empty
  mconcat = AppendMap . Map.unionsWith (<>) . fmap unAppendMap
