module Sneer.ClientSpec where

import Control.Concurrent.Async
import Sneer.Client
import Sneer.Transit
import System.Environment (lookupEnv)
import System.Timeout
import Test.Hspec

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

withTimeout :: IO a -> IO (Maybe a)
withTimeout action = do
  t <- defaultTimeout
  timeoutIn t seconds action

defaultTimeout :: IO Int
defaultTimeout = do
  ci <- lookupEnv "CI"
  return $ case ci of
    Just _  -> 30
    Nothing -> 1

timeoutIn :: Int -> (Int -> Int) -> IO a -> IO (Maybe a)
timeoutIn t unit = timeout (unit t)

seconds :: Int -> Int
seconds = (* 1000000)
