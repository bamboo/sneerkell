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
      pendingWith "missing support for transit caching"
--       withClient neide $ \n ->
--       withClient maico $ \m -> do
--         atomically $ sendTuple n tuple
--         Just tupleReceived <- withTimeout . atomically $ receiveTuple m
--         tupleReceived `shouldBe` tuple
-- where
--  tuple = Tuple [(tt "value", tt "42")] neide maico 1
