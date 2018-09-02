module Data.Map.Counter where

import           Data.Map.Append.Strict
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map

import           Data.Monoid

type Counter key = AppendMap key (Sum Int)

mkCounter :: Ord key => key -> Counter key
mkCounter key = AppendMap $ Map.singleton key (Sum 1)

getCounts :: Counter key -> Map key Int
getCounts = Map.map getSum . unAppendMap
