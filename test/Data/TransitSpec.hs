{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, OverloadedLists #-}

module Data.TransitSpec where

import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import           Data.Transit
import qualified Data.Vector as V
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

  describe "Transit" $ do
    it "can be encoded to json" $
      property $ \t -> jsonRoundtrip t == J.Success (t :: Transit)

    it "can decode cache references" $
      let json     = "[\"^ \",\"a\",[\"^ \",\"~:value\",1],\"b\",[\"^ \",\"^0\",2]]"
          expected = TMap [(string "a", TMap [(TKeyword "value", integer 1)])
                          ,(string "b", TMap [(TKeyword "value", integer 2)])]
          actual   = J.decode json :: Maybe Transit
      in actual `shouldBe` Just expected

    it "can decode cache references in tag position" $
      let json     = "[\"^ \",\"a\",[\"~#puk\",1],\"b\",[\"^0\",2]]"
          expected = TMap [(string "a", TExtension "puk" (integer 1))
                          ,(string "b", TExtension "puk" (integer 2))]
          actual   = J.decode json :: Maybe Transit
      in actual `shouldBe` Just expected

    -- TODO: test caching of nested map keys to ensure proper ordering
    -- let json = "[\"^ \",\"~:send\",[\"^ \",\"id\",1,\"author\",[\"~#puk\",\"~bAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE=\"],\"audience\",[\"^2\",\"~bAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAI=\"],\"value\",\"42\"]]"


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
        \/ localDepth (const 2) (cons2 extension)

instance (Monad m) => Serial m BS.ByteString where
  series = cons1 BS.pack

instance (Monad m) => Serial m Word8 where
  series = cons1 (\i -> fromIntegral (i :: Int))

instance (Monad m, Serial m a) => Serial m (V.Vector a) where
  series = cons1 V.fromList

keyword :: NonEmpty Char -> Transit
keyword (NonEmpty cs) = TKeyword $ pack cs

extension :: NonEmpty Char -> Transit -> Transit
extension (NonEmpty tag) = TExtension $ pack tag
