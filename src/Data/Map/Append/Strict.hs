module Data.Map.Append.Strict where

import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map

import           Data.Monoid        hiding ((<>))
import           Data.Semigroup

import qualified Data.List.NonEmpty as NE

newtype AppendMap k v = AppendMap
  { unAppendMap :: Map k v
  }

instance (Ord k, Semigroup v) => Semigroup (AppendMap k v) where
  AppendMap a <> AppendMap b = AppendMap $ Map.unionWith (<>) a b
  sconcat = AppendMap . Map.unionsWith (<>) . NE.toList . fmap unAppendMap

instance (Ord k, Semigroup v) => Monoid (AppendMap k v) where
  mempty = AppendMap Map.empty
  mappend = (<>)
  mconcat = AppendMap . Map.unionsWith (<>) . fmap unAppendMap
