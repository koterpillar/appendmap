{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}

import qualified Data.Map.Append.Lazy   as Lazy
import qualified Data.Map.Lazy          as Lazy

import qualified Data.Map.Append.Strict as Strict
import qualified Data.Map.Strict        as Strict

import           Data.Semigroup

import           Test.Hspec

data AppendMap appendmap map = AppendMap
  { mapFromList :: [(Int, String)] -> map Int String
  , mapEmpty    :: map Int String
  , appendMap   :: forall k v. map k v -> appendmap k v
  , unAppendMap :: forall k v. appendmap k v -> map k v
  }

lazy :: AppendMap Lazy.AppendMap Lazy.Map
lazy =
  AppendMap
    { mapFromList = Lazy.fromList
    , mapEmpty = Lazy.empty
    , appendMap = Lazy.AppendMap
    , unAppendMap = Lazy.unAppendMap
    }

strict :: AppendMap Strict.AppendMap Strict.Map
strict =
  AppendMap
    { mapFromList = Strict.fromList
    , mapEmpty = Strict.empty
    , appendMap = Strict.AppendMap
    , unAppendMap = Strict.unAppendMap
    }

appendMapSpec ::
     ( Show (map Int String)
     , Eq (map Int String)
     , Semigroup (appendmap Int String)
     , Monoid (appendmap Int String)
     )
  => AppendMap appendmap map
  -> Spec
appendMapSpec AppendMap {..} = do
  it "has empty element" $ unAppendMap mempty `shouldBe` mapEmpty
  it "combines elements" $ do
    let one = appendMap $ mapFromList [(1, "hello"), (2, "goodbye")]
    let two = appendMap $ mapFromList [(1, "world"), (3, "again")]
    unAppendMap (one <> two) `shouldBe`
      mapFromList [(1, "helloworld"), (2, "goodbye"), (3, "again")]

main :: IO ()
main =
  hspec $ do
    describe "Data.Map.Append.Lazy" $ appendMapSpec lazy
    describe "Data.Map.Append.Strict" $ appendMapSpec strict
