{-# LANGUAGE OverloadedStrings #-}

module Data.Transit.CacheSpec where

import Data.Transit.Cache as TC
import Test.Hspec

spec :: Spec
spec =
  describe "Transit caching" $
    it "caches all ~#tag, keyword and symbol values, and strings if they are longer than 3 characters" $ do
      let cache = foldl (flip TC.insert) TC.empty [ "~#puk", "tst", "test" ]
      TC.lookup "^0" cache `shouldBe` Just "~#puk"
      TC.lookup "^1" cache `shouldBe` Just "test"
      TC.lookup "^2" cache `shouldBe` Nothing
