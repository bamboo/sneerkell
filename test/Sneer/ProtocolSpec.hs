{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Sneer.ProtocolSpec where

import qualified Data.Aeson.Types as J
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import           Data.Transit
import           Data.TransitSpec
import qualified Data.Vector as V
import           Sneer.Keys
import           Sneer.Protocol
import           Test.Hspec
import           Test.Hspec.SmallCheck
import           Test.SmallCheck.Series

singleQuotesToDouble :: T.Text -> T.Text
singleQuotesToDouble = T.replace sq dq
  where
    sq = T.singleton '\''
    dq = T.singleton '"'

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

    it "can decode sub" $
      let json = singleQuotesToDouble "['^ ','~:send',['^ ','id',48,'type','sub','payload',null,'timestamp',1434920895506,'author',['~#puk','~bmigSZ721d57/JpHgS9zdDK8WeTa81scsup1QyXeq0Jo='],'original_id',48,'audience',['^5','~bAAAAAAAAAAAAAAAA6iQMhBpR9g8O7PsKeBcqYVcBvIk='],'criteria',['^ ','^7',['^5','~bmigSZ721d57/JpHgS9zdDK8WeTa81scsup1QyXeq0Jo='],'^4',['^5','~bAAAAAAAAAAAAAAAA6iQMhBpR9g8O7PsKeBcqYVcBvIk='],'^1','message']]]"
          author = address 1
          audience = address 2
          fields = V.empty
          tuple = Tuple fields author audience 1
      in (eitherDecode (T.encodeUtf8 json) >>= eitherUntson) `shouldBe` Right (Accept tuple)


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
