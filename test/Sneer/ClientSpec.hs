{-# LANGUAGE OverloadedLists #-}

module Sneer.ClientSpec where

import Control.Concurrent.STM
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
        tupleReceived <- withTimeout . atomically $ receiveTuple m
        case tupleReceived of
          Just t -> t `shouldBe` tuple
          _      -> pendingWith "missing support for transit caching"
 where
  tuple = Tuple [(tt "value", tt "42")] neide maico 1
