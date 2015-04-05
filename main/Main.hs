module Main where

import Network.Haskoin.Crypto
import Sneer.Client
import Sneer.Protocol
import Text.Printf (printf)
import Network (withSocketsDo)

main :: IO ()
main = withSocketsDo $ do
  prik <- randomPrivateKey
  let puk = derivePubKey prik
      pukAddr = pubKeyAddr puk
  client <- newClient
  bytesSent <- sendTo client $ PingFrom pukAddr
  printf "%d bytes sent.\n" bytesSent

randomPrivateKey :: IO PrvKey
randomPrivateKey = withSource devRandom genPrvKey

