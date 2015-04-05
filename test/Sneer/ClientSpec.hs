module Sneer.ClientSpec where

import Control.Concurrent.Async
import Data.Transit
import Sneer.Client
import Test.Hspec
import Timeout

spec :: Spec
spec =
  describe "client" $
    it "can exchange messages with server" $ do
      subject <- newClient
      pendingAck <- async $ receiveFrom subject
      sendTo subject $ Map [(k "send", tuple)
                           ,(k "from", k "me")
                           ,(k "to",   k "you")
                           ]
      Just _ <- withTimeout $ wait pendingAck
      return ()
 where
  tuple = Map [("id", Number 42)]
  k = Keyword

