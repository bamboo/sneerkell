{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Data.TransitSpec where

import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import           Data.Transit
import           Data.Word
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

  describe "Transit" $
    it "can be encoded to json" $
      property $ \t -> jsonRoundtrip t == J.Success (t :: Transit)

roundtrip :: (ToTransit a, FromTransit a) => a -> Maybe a
roundtrip = fromTransit . toTransit

jsonRoundtrip :: (J.ToJSON a, J.FromJSON a) => a -> J.Result a
jsonRoundtrip = J.fromJSON . J.toJSON

instance (Monad m) => Serial m Transit where
  series = cons1 string
        \/ cons1 integer
        \/ cons1 keyword
        \/ cons1 TMap
        \/ cons1 TBytes

instance (Monad m) => Serial m BS.ByteString where
  series = cons1 BS.pack

instance (Monad m) => Serial m Word8 where
  series = cons1 (\i -> fromIntegral (i :: Int))

keyword :: NonEmpty Char -> Transit
keyword (NonEmpty cs) = TKeyword $ pack cs

integer :: Integer -> Transit
integer = TNumber . fromIntegral
