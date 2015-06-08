{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Sneer.ProtocolSpec where

import qualified Data.Aeson.Types as J
import           Data.Transit
import           Data.TransitSpec
import qualified Data.Vector as V
import           Sneer.Keys
import           Sneer.Protocol
import           Test.Hspec
import           Test.Hspec.SmallCheck
import           Test.SmallCheck.Series

spec :: Spec
spec =
  describe "Transit" $ do
    it "can roundtrip addresses" $
      property $ \a -> roundtrip a == Just (a :: Address)

    it "can roundtrip tuples" $
      property $ \s -> roundtrip s == Just (s :: Tuple)

    it "can decode Ack" $
      let peerPuk = address 1
          tupleId = 42 :: Integer
          packet  = TMap [(TKeyword "ack", number tupleId)
                         ,(TKeyword "for", toTransit peerPuk)]
      in fromTransit packet `shouldBe` Just (Ack peerPuk tupleId)

    it "can decode Send" $
      let json = J.Array
                   [J.String "^ "
                   ,J.String "~:send"
                   ,J.Array
                      [J.String "^ "
                      ,J.String "id", J.Number 1.0
                      ,J.String "author", J.Array
                                            [J.String "~#puk"
                                            ,J.String "~bAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE="]
                      ,J.String "audience", J.Array
                                              [J.String "^2"
                                              ,J.String "~bAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAI="]
                      ,J.String "value", J.String "42"]]
          author   = address 1
          audience = address 2
          fields   = V.fromList [(TString "value", TString "42")]
          tuple    = Tuple fields author audience 1
      in untson json `shouldBe` Just (Accept tuple)

instance (Monad m) => Serial m Tuple where
  series = localDepth (min 3) $ cons4 Tuple

instance (Monad m) => Serial m Address where
  series = cons1 address

address :: Integer -> Address
address = PubKeyAddress . fromIntegral
