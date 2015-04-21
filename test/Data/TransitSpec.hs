{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Data.TransitSpec where

import qualified Data.Aeson as J
import           Data.Transit
import           Test.Hspec
import           Test.Hspec.SmallCheck
import           Test.SmallCheck.Series

spec :: Spec
spec = do
  describe "fromTransit . toTransit" $ do
    it "can roundtrip strings" $
      property $ \s -> roundtrip s == Just (s :: String)

    it "can roundtrip integers" $
      property $ \i -> roundtrip i == Just (i :: Integer)

  describe "json encoding" $
    it "can roundtrip any transit value" $
      property $ \v -> (J.fromJSON . J.toJSON $ v) == J.Success (v :: Transit)

roundtrip :: (ToTransit a, FromTransit a) => a -> Maybe a
roundtrip = fromTransit . toTransit

instance (Monad m) => Serial m Transit where
  series = cons1 string \/ cons1 integer \/ cons1 TMap

integer :: Integer -> Transit
integer = TNumber . fromIntegral
