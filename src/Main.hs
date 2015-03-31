{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Main where

import Network.Haskoin.Crypto
import Network.Socket hiding (sendTo)
import qualified Network.Socket.ByteString as NSB
import Control.Applicative
import Text.Printf(printf)
import qualified Data.ByteString.Char8 as BSC8
import Data.ByteString.Lazy as BSL
import Data.Aeson.Encode(encode)
import qualified Data.Aeson.Types as J
import Data.Text as T
import qualified Data.Vector as V
import qualified Data.List as L

main :: IO ()
main = withSocketsDo $ do
  prik <- randomPrivateKey
  let puk = derivePubKey prik
  client <- newClient
  bytesSent <- sendTo client $ Ping puk
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

data Transit where
  Extension  :: (Transitable a) => String -> a -> Transit
  Keyword    :: String -> Transit
  Str        :: String -> Transit
  Map        :: (Transitable k, Transitable v) => [(k, v)] -> Transit

instance J.ToJSON Transit where
  toJSON (Str s) = jstring s
  toJSON (Keyword k) = jstring $ "~:" ++ k
  toJSON (Map kvs) = J.Array . V.fromList $ mapMarker : L.concatMap (\(k, v) -> [tson k, tson v]) kvs
  toJSON (Extension tag v) = J.Array $ V.fromList [jstring $ "~#" ++ tag, tson v]

jstring :: String -> J.Value
jstring = J.String . T.pack

mapMarker :: J.Value
mapMarker = J.String "^ "

tson :: (Transitable a) => a -> J.Value
tson = J.toJSON . transit

class Transitable a where
  transit :: a -> Transit

instance Transitable Transit where
  transit = id

instance Transitable String where
  transit = Str

data ProtocolMessage = Ping PubKey

instance Transitable ProtocolMessage where
  transit (Ping puk) = Map [(Keyword "from", puk)]

instance Transitable PubKey where
  transit = Extension "puk" . addrToBase58 . pubKeyAddr

udpSocket :: IO Socket
udpSocket = socket AF_INET Datagram defaultProtocol

serverAddr :: IO SockAddr
serverAddr = SockAddrInet serverPort <$> inet_addr "127.0.0.1"

serverPort :: PortNumber
serverPort = 5555
