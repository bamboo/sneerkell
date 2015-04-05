module Main where

import Sneer.Client
import Sneer.Keys
import Sneer.Protocol
import Text.Printf (printf)
import Network (withSocketsDo)

main :: IO ()
main = withSocketsDo $ do
  (_, puk) <- ownKeyAndAddress
  client <- newClient
  bytesSent <- sendTo client $ PingFrom puk
  printf "%d bytes sent.\n" bytesSent
