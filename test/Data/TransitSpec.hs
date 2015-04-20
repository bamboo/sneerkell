module Data.TransitSpec where

import Data.Transit
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  describe "fromTransit . toTransit" $
    it "can roundtrip strings" $
      property $ \s -> roundtrip s == Just (s :: String)

roundtrip :: (ToTransit a, FromTransit a) => a -> Maybe a
roundtrip = fromTransit . toTransit
