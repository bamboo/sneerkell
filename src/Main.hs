module Main where

import Network.Haskoin.Crypto
import Network.Socket hiding (sendTo)
import qualified Network.Socket.ByteString as NSB
import Control.Applicative
import Text.Printf(printf)
import qualified Data.ByteString.Char8 as BSC8
import Data.ByteString.Lazy as BSL
import Data.Aeson.Encode(encode)
import Sneer.Transit

main :: IO ()
main = withSocketsDo $ do
  prik <- randomPrivateKey
  let puk = derivePubKey prik
      pukAddr = pubKeyAddr puk
  client <- newClient
  bytesSent <- sendTo client $ Ping pukAddr
  printf "%d bytes sent.\n" bytesSent

randomPrivateKey :: IO PrvKey
randomPrivateKey = withSource devRandom genPrvKey

newClient :: IO Client
newClient = Client <$> udpSocket <*> serverAddr

sendTo :: (Transitable a) => Client -> a -> IO Int
sendTo (Client sock address) msg = do
  let bytes = toStrict . encode . transit $ msg
  BSC8.putStrLn bytes
  NSB.sendTo sock bytes address

data Client = Client Socket SockAddr

data ProtocolMessage = Ping Address

instance Transitable Address where
  transit = Extension "puk" . addrToBase58

instance Transitable ProtocolMessage where
  transit (Ping puk) = Map [(Keyword "from", puk)]

udpSocket :: IO Socket
udpSocket = socket AF_INET Datagram defaultProtocol

serverAddr :: IO SockAddr
serverAddr = SockAddrInet serverPort <$> inet_addr "127.0.0.1"

serverPort :: PortNumber
serverPort = 5555
