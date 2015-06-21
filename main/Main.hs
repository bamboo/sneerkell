module Main where

import           Control.Concurrent.STM
import           Control.Monad (unless)
import qualified Data.ByteString.Char8 as BC
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import           Data.Transit
import           Network (withSocketsDo)
import qualified Network.HTTP.Types.URI as URI
import           Sneer.Client
import           Sneer.Keys
import           Sneer.Protocol

main :: IO ()
main = withSocketsDo $ do
  (_, ownPuk) <- ownKeyAndAddress
  withClient ownPuk $ \client ->
    mainLoop ownPuk client

mainLoop :: Address -> Client -> IO ()
mainLoop ownPuk client = do
  printAddress ownPuk
  putStrLn ".<ENTER> to quit"
  loop
 where
  loop = do
    line <- getLine
    unless (line == ".") $ do
      handleInviteLink ownPuk client line
      loop

printAddress :: Address -> IO ()
printAddress = TIO.putStrLn . T.toUpper . addressString

handleInviteLink :: Address -> Client -> String -> IO ()
handleInviteLink ownPuk client link = do
  let (peerPuk, inviteCode) = parseInviteLink link
  let tuple = acceptInviteTuple ownPuk peerPuk inviteCode
  atomically $ sendTuple client tuple

acceptInviteTuple :: Address -> Address -> T.Text -> Tuple
acceptInviteTuple ownPuk contactPuk inviteCode = tuple
  where
    tuple   = Tuple (V.fromList fields) ownPuk contactPuk tupleId
    fields  = [(string "type", string "push")
              ,(string "invite-code", TString inviteCode)]
    tupleId = 42

-- parses links in the form
-- http://sneer.me/public-key?9A281267BDB5779EFF2691E04BDCDD0CAF167936BCD6C72CBA9D50C977AAD09A&invite=f1f9dbc6a7e248f9b905eb5216ed21f0
parseInviteLink :: String -> (Address, T.Text)
parseInviteLink link = (address, inviteCode)
  where
    address = addressFromHex . T.encodeUtf8 . fst . head $ query
    inviteCode = fromJust . snd . last $ query
    query = URI.parseQueryText queryString
    queryString = last . BC.split '?' $ uri
    uri = BC.pack link
