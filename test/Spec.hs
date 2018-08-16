import qualified Data.Map as Map
import Data.Map.Append

import Data.Semigroup

import Test.Hspec

main :: IO ()
main =
  hspec $
  describe "AppendMap" $ do
    it "has empty element" $
      unAppendMap (mempty :: AppendMap Int String) `shouldBe` Map.empty
    it "combines elements" $ do
      let one = AppendMap $ Map.fromList [(1, "hello"), (2, "goodbye")]
      let two = AppendMap $ Map.fromList [(1, "world"), (3, "again")]
      unAppendMap (one <> two) `shouldBe`
        Map.fromList [(1, "helloworld"), (2, "goodbye"), (3, "again")]
