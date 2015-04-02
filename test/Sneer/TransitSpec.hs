module Sneer.TransitSpec where

import Sneer.Transit
import Test.Hspec

spec :: Spec
spec =
  describe "tson" $
    it "can encode strings" $
      tson "foo" `shouldBe` jstring "foo"
