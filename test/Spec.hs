module Main where

import Sneer.Transit
import Test.Hspec

main :: IO ()
main = hspec $
  describe "Sneer.Transit" $
    it "can encode strings" $
      tson "foo" `shouldBe` jstring "bar"
