{-# LANGUAGE RecordWildCards #-}

module Network.UdpMessenger
       ( startMessenger
       , stopMessenger
       , Messenger ()
       , JChan
       ) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception (finally)
import           Control.Monad (forever)
import           Data.Aeson (encode, decode)
import qualified Data.Aeson.Types as J
import qualified Data.ByteString.Char8 as BSC8
import           Data.ByteString.Lazy as BSL
import qualified Network.Socket as NS
import           Network.Socket hiding (sendTo, socket)
import qualified Network.Socket.ByteString as NSB

type JChan = TChan J.Value

data Messenger = Messenger { _socket   :: Socket
                           , _sendTask :: Async ()
                           , _recvTask :: Async ()
                           }

startMessenger :: JChan -> JChan -> SockAddr -> IO Messenger
startMessenger packetsOut packetsIn serverAddr = do
  _socket   <- udpSocket
  _sendTask <- async $ sendLoop packetsOut _socket serverAddr
  _recvTask <- async $ recvLoop packetsIn  _socket
  return Messenger{..}

stopMessenger :: Messenger -> IO ()
stopMessenger Messenger{..} =
  finally (sClose _socket) $ do
    cancel _sendTask
    cancel _recvTask

sendLoop :: JChan -> Socket -> SockAddr -> IO ()
sendLoop packetsOut socket serverAddr =
  forever $ do
    packet <- atomically $ readTChan packetsOut
    sendTo socket packet serverAddr

recvLoop :: JChan -> Socket -> IO ()
recvLoop packetsIn socket =
  forever $ do
    maybePacket <- tryRecvFrom socket
    case maybePacket of
      Just packet -> atomically $ writeTChan packetsIn packet
      Nothing     -> return ()

sendTo :: Socket -> J.Value -> SockAddr -> IO Int
sendTo socket msg serverAddr = do
  let bytes = toStrict . encode $ msg
  BSC8.putStrLn bytes
  NSB.sendTo socket bytes serverAddr

tryRecvFrom :: Socket -> IO (Maybe J.Value)
tryRecvFrom socket = do
  (bytes, _) <- NSB.recvFrom socket 1024
  BSC8.putStrLn bytes
  let bytes' = BSL.fromStrict bytes
  return $ decode bytes'

udpSocket :: IO Socket
udpSocket = NS.socket AF_INET Datagram defaultProtocol
