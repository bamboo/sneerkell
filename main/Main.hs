module Main where

import Network (withSocketsDo)
import Sneer.Client
import Sneer.Keys
import Sneer.Protocol
import Text.Printf (printf)

main :: IO ()
main = withSocketsDo $ do
  (_, puk) <- ownKeyAndAddress
  client <- newClient
  bytesSent <- sendTo client $ PingFrom puk
  printf "%d bytes sent.\n" bytesSent
