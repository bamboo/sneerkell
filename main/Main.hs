module Main where

import Network (withSocketsDo)
import Sneer.Client
import Sneer.Keys

main :: IO ()
main = withSocketsDo $ do
  (_, ownPuk) <- ownKeyAndAddress
  withClient ownPuk $ \_ -> do
    putStrLn "Press <ENTER> to quit"
    _ <- getLine
    return ()
