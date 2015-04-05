module Data.TransitSpec where

import Data.Transit
import Test.Hspec

spec :: Spec
spec =
  describe "tson" $
    it "can encode strings" $
      tson "foo" `shouldBe` jstring "foo"
