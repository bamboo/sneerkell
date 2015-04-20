{-# LANGUAGE RecordWildCards #-}

module Sneer.Client ( startClient
                    , sendTuple
                    , receiveTuple
                    , stopClient
                    , withClient
                    , Client ()
                    ) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad (forever)
import Data.Transit (tson)
import Network.Socket
import Network.UdpMessenger
import Sneer.Keys
import Sneer.Protocol

type TupleChan = TChan Tuple

data Client = Client { _ownPuk     :: Address
                     , _packetsOut :: JChan
                     , _packetsIn  :: JChan
                     , _tuplesOut  :: TupleChan
                     , _tuplesIn   :: TupleChan
                     , _messenger  :: Messenger
                     , _pingTask   :: Async ()
                     , _sendTask   :: Async ()
                     }

startClient :: Address -> IO Client
startClient ownPuk = do
  let _ownPuk =  ownPuk
  _packetsOut <- newTChanIO
  _packetsIn  <- newTChanIO
  _tuplesOut  <- newTChanIO
  _tuplesIn   <- newTChanIO
  _messenger  <- startMessenger _packetsOut _packetsIn =<< serverAddr
  _pingTask   <- async $ pingLoop _packetsOut _ownPuk
  _sendTask   <- async $ sendLoop _packetsOut _tuplesOut _ownPuk
  _recvTask   <- async $ recvLoop _packetsIn _tuplesIn
  return Client{..}
 where
  serverAddr = SockAddrInet serverPort <$> inet_addr serverHost
  serverHost = "127.0.0.1"
  serverPort = 5555

withClient :: Address -> (Client -> IO a) -> IO a
withClient puk = bracket (startClient puk) stopClient

sendLoop :: JChan -> TupleChan -> Address -> IO ()
sendLoop packetsOut tuplesOut ownPuk =
  forever $ do
    (sentPacket, tuple) <- atomically $ do
      tuple <- readTChan tuplesOut
      case audience tuple of
        Just peerPuk -> do
          let packet = tson $ SendFrom ownPuk peerPuk tuple
          writeTChan packetsOut packet
          return (Just packet, tuple)
        Nothing ->
          return (Nothing, tuple)
    case sentPacket of
      Just packet -> putStrLn $ "OUT: " ++ show packet
      Nothing     -> putStrLn $ "ERROR: audience missing from tuple " ++ show (tson tuple)

recvLoop :: JChan -> TupleChan -> IO ()
recvLoop packetsIn _ =
  forever $ do
    packet <- atomically $ readTChan packetsIn
    putStrLn $ "IN: " ++ show packet

sendTuple :: Client -> Tuple -> STM ()
sendTuple Client{..} = writeTChan _tuplesOut

receiveTuple :: Client -> STM Tuple
receiveTuple Client{..} = readTChan _tuplesIn

stopClient :: Client -> IO ()
stopClient Client{..} = do
  cancel _pingTask
  cancel _sendTask
  stopMessenger _messenger

pingLoop :: JChan -> Address -> IO ()
pingLoop packetsOut ownPuk =
  forever $ do
    atomically $ writeTChan packetsOut ping
    threadDelay $ 30 * seconds
 where
  ping = tson $ PingFrom ownPuk
  seconds = 1000000
