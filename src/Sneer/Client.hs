module Sneer.Client where

import           Control.Applicative
import           Data.Aeson (encode, decode)
import qualified Data.Aeson.Types as J
import qualified Data.ByteString.Char8 as BSC8
import           Data.ByteString.Lazy as BSL
import           Data.Transit (Transitable, transit)
import           Network.Socket hiding (sendTo)
import qualified Network.Socket.ByteString as NSB

newClient :: IO Client
newClient = Client <$> udpSocket <*> serverAddr

sendTo :: (Transitable a) => Client -> a -> IO Int
sendTo (Client sock address) msg = do
  let bytes = toStrict . encode . transit $ msg
  BSC8.putStrLn bytes
  NSB.sendTo sock bytes address

receiveFrom :: Client -> IO (Maybe J.Value)
receiveFrom (Client sock _) = do
  (bytes, _) <- NSB.recvFrom sock 1024
  let bytes' = BSL.fromStrict bytes
  return $ decode bytes'

data Client = Client Socket SockAddr

udpSocket :: IO Socket
udpSocket = socket AF_INET Datagram defaultProtocol

serverAddr :: IO SockAddr
serverAddr = SockAddrInet serverPort <$> inet_addr "127.0.0.1"

serverPort :: PortNumber
serverPort = 5555
