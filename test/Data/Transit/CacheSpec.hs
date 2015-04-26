{-# LANGUAGE OverloadedStrings #-}

module Data.Transit.CacheSpec where

import Data.Transit.Cache as TC
import Test.Hspec

spec :: Spec
spec =
  describe "Transit caching" $
    it "caches all ~#tag, keyword and symbol values, and strings if they are longer than 3 characters" $ do
      let cache = TC.insert "~#puk" TC.empty
      TC.lookup "^0" cache `shouldBe` Just "~#puk"
