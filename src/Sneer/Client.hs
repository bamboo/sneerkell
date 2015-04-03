module Sneer.Client where

import           Control.Applicative
import           Data.Aeson.Encode (encode)
import qualified Data.ByteString.Char8 as BSC8
import           Data.ByteString.Lazy as BSL
import           Network.Socket hiding (sendTo)
import qualified Network.Socket.ByteString as NSB
import           Sneer.Transit (Transitable, transit)

newClient :: IO Client
newClient = Client <$> udpSocket <*> serverAddr

sendTo :: (Transitable a) => Client -> a -> IO Int
sendTo (Client sock address) msg = do
  let bytes = toStrict . encode . transit $ msg
  BSC8.putStrLn bytes
  NSB.sendTo sock bytes address

data Client = Client Socket SockAddr

udpSocket :: IO Socket
udpSocket = socket AF_INET Datagram defaultProtocol

serverAddr :: IO SockAddr
serverAddr = SockAddrInet serverPort <$> inet_addr "127.0.0.1"

serverPort :: PortNumber
serverPort = 5555
