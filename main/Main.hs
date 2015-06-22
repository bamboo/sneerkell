{-# LANGUAGE ViewPatterns #-}

module Main where

import           Control.Concurrent.STM
import           Control.Monad (unless)
import qualified Data.ByteString.Char8 as BC
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
mainLoop ownPuk client = start
  where
    start = do
      printAddress ownPuk
      putStrLn ".<ENTER> to quit"
      loop

    loop = do
      line <- getLine
      unless (line == ".") $ do
        handleInviteLink ownPuk client line
        loop

printAddress :: Address -> IO ()
printAddress = TIO.putStrLn . T.toUpper . addressString

handleInviteLink :: Address -> Client -> String -> IO ()
handleInviteLink ownPuk client link =
  case parseInviteLink link of
    Just (peerPuk, inviteCode) -> do
      let tuple = acceptInviteTuple ownPuk peerPuk inviteCode
      atomically $ sendTuple client tuple
    Nothing                    ->
      return ()

acceptInviteTuple :: Address -> Address -> T.Text -> Tuple
acceptInviteTuple ownPuk contactPuk inviteCode = tuple
  where
    tuple   = Tuple (V.fromList fields) ownPuk contactPuk tupleId
    fields  = [(string "type", string "push")
              ,(string "invite-code", TString inviteCode)]
    tupleId = 42

-- parses links in the form
-- http://sneer.me/public-key?9A281267BDB5779EFF2691E04BDCDD0CAF167936BCD6C72CBA9D50C977AAD09A&invite=f1f9dbc6a7e248f9b905eb5216ed21f0
-- and returns the (Address, InviteCode) pair
parseInviteLink :: String -> Maybe (Address, T.Text)
parseInviteLink link =
  let uri = BC.pack link
      uriParts = BC.split '?' uri
  in case uriParts of
    [_, queryString] -> parseInviteLinkQueryString queryString
    _                -> Nothing

parseInviteLinkQueryString :: BC.ByteString -> Maybe (Address, T.Text)
parseInviteLinkQueryString queryString =
  case URI.parseQueryText queryString of
    [(addressText, Nothing), (unpack -> "invite", Just inviteCode)] -> Just (addressFromText addressText, inviteCode)
    _                                                               -> Nothing

addressFromText :: T.Text -> Address
addressFromText = addressFromHex . T.encodeUtf8
