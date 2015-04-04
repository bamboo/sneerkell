module Sneer.ClientSpec where

import Control.Concurrent (forkFinally, threadDelay)
import Control.Concurrent.Async
import Control.Exception (bracket)
import Network (PortID (..), listenOn, accept)
import Network.Socket (shutdown, ShutdownCmd(..))
import Sneer.Client
import Sneer.Transit
import System.Directory (getCurrentDirectory)
import System.FilePath (combine)
import System.IO (Handle, hClose)
import System.Process
import Test.Hspec

spec :: Spec
spec = around_ withSneerServer $
  describe "client" $
    it "can exchange messages with server" $ do
      subject <- newClient
      pendingAck <- async $ receiveFrom subject
      sendTo subject $ Map [(k "send", tuple)
                           ,(k "from", k "me")
                           ,(k "to",   k "you")
                           ]
      Right _ <- waitAtMost (milliseconds 500) pendingAck
      return ()
 where
  tuple = Map [("id", Number 42)]
  k = Keyword

withSneerServer :: IO () -> IO ()
withSneerServer action =
  bracket startServer
          stopServer
          (const action)

startServer :: IO Handle
startServer = do
  socket <- listenOn $ PortNumber harakiriPort
  startServerProcess harakiriPort
  forkFinally
    (threadDelay harakiriConnectionTimeout)
    (\_ -> shutdown socket ShutdownBoth)
  (harakiriSocket, _, _) <- accept socket
  return harakiriSocket
 where
  harakiriPort = 4242
  harakiriConnectionTimeout = seconds 5

startServerProcess  :: (Show a) => a -> IO ProcessHandle
startServerProcess harakiriPort = do
  cd <- getCurrentDirectory
  let serverProc = proc "java" ["-jar", combine cd serverJar, "5555", "8282", show harakiriPort]
  (_, _, _, handle) <- createProcess serverProc {cwd = Just "/tmp"}
  return handle
 where
  serverJar = "../sneer/server/target/uberjar/sneer.server-0.1.0-SNAPSHOT-standalone.jar"

stopServer :: Handle -> IO ()
stopServer = hClose

waitAtMost :: Int -> Async a -> IO (Either () a)
waitAtMost timeout a =
  race (threadDelay timeout)
       (wait a)

milliseconds :: Int -> Int
milliseconds = (* 1000)

seconds :: Int -> Int
seconds = (* 1000000)
