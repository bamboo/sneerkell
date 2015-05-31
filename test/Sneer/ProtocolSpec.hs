{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Sneer.ProtocolSpec where

import Data.Transit
import Data.TransitSpec
import Sneer.Keys
import Sneer.Protocol
import Test.Hspec
import Test.Hspec.SmallCheck
import Test.SmallCheck.Series

spec :: Spec
spec =
  describe "Transit" $ do
    it "can roundtrip addresses" $
      property $ \a -> roundtrip a == Just (a :: Address)
    it "can roundtrip tuples" $
      property $ \s -> roundtrip s == Just (s :: Tuple)
    it "can decode Ack" $ do
      let peerPuk = address 1
      let tupleId = 42 :: Integer
      let packet = TMap [(TKeyword "ack", number tupleId)
                        ,(TKeyword "for", toTransit peerPuk)]
      fromTransit packet `shouldBe` Just (Ack peerPuk tupleId)


instance (Monad m) => Serial m Tuple where
  series = localDepth (min 3) $ cons4 Tuple

instance (Monad m) => Serial m Address where
  series = cons1 address

address :: Integer -> Address
address = PubKeyAddress . fromIntegral
