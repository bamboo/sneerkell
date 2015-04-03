module Main where

import Network.Haskoin.Crypto
import Sneer.Client
import Sneer.Transit
import Text.Printf (printf)
import Network (withSocketsDo)

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

data ProtocolMessage = Ping Address

instance Transitable Address where
  transit = Extension "puk" . addrToBase58

instance Transitable ProtocolMessage where
  transit (Ping puk) = Map [(Keyword "from", puk)]
