module Sneer.ClientSpec where

import Control.Concurrent.STM
import Data.Transit (tson)
import Sneer.Client
import Sneer.Keys
import Sneer.Protocol
import Test.Hspec
import Timeout

neide, maico :: Address
neide = PubKeyAddress 1
maico = PubKeyAddress 2

spec :: Spec
spec =
  describe "clients" $
    it "can exchange tuples" $
      withClient neide $ \n ->
      withClient maico $ \m -> do
        atomically $ sendTuple n tuple
        Just tupleReceived <- withTimeout . atomically $ receiveTuple m
        tson tupleReceived `shouldBe` tson tuple
 where
  tuple = Tuple [("value", "42")] (Just maico) neide 1

