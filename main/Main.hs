module Main where

import Control.Monad (unless)
import Control.Concurrent.STM
import Network (withSocketsDo)
import Sneer.Protocol
import Sneer.Client
import Sneer.Keys

main :: IO ()
main = withSocketsDo $ do
  (_, ownPuk) <- ownKeyAndAddress
  withClient ownPuk $ \client ->
    clientLoop ownPuk client

clientLoop :: Address -> Client -> IO ()
clientLoop ownPuk client = do
  putStrLn $ addressString ownPuk
  putStrLn "Enter a single dot (.) to quit"
  loop
 where
  loop = do
    line <- getLine
    unless (line == ".") $ do
      handleCommand ownPuk client line
      loop

handleCommand :: Address -> Client -> String -> IO ()
handleCommand ownPuk client command = do
  let (peerPuk, inviteCode) = parseInviteLink command
  atomically $ sendTuple client (acceptInviteTuple ownPuk peerPuk inviteCode)
  putStrLn command

acceptInviteTuple :: Address -> Address -> String -> Tuple
acceptInviteTuple = undefined

-- parses links in the form
-- http://sneer.me/public-key?9A281267BDB5779EFF2691E04BDCDD0CAF167936BCD6C72CBA9D50C977AAD09A&invite=f1f9dbc6a7e248f9b905eb5216ed21f0
parseInviteLink :: String -> (Address, InviteCode)
parseInviteLink link = (PubKeyAddress 0, link)

type InviteCode = String
