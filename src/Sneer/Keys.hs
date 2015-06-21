{-# LANGUAGE OverloadedStrings #-}

module Sneer.Keys
       ( ownKeyAndAddress
       , addressString
       , PrvKey
       , Address (..)
       ) where

import           Control.Applicative
import           Data.Text
import           Data.Transit (toBase64)
import           Filesystem
import           Filesystem.Path.CurrentOS
import           Network.Haskoin.Crypto
import           Prelude hiding (FilePath)
import           Sneer.Protocol (addressBytes)
import qualified System.IO.Error as IO

addressString :: Address -> String
addressString = unpack . toBase64 . addressBytes

ownKeyAndAddress :: IO (PrvKey, Address)
ownKeyAndAddress = do
  existing <- existingPrivateKey
  case existing of
    Just prik -> return $ from prik
    Nothing   -> do
      prik <- randomPrivateKey
      savePrivateKey prik
      return $ from prik
 where
  from prik = (prik, addressOf prik)
  addressOf = pubKeyAddr . derivePubKey

existingPrivateKey :: IO (Maybe PrvKey)
existingPrivateKey =
  IO.catchIOError (fromWIFText <$> readPrivateKeyFile)
                  (\e -> if IO.isDoesNotExistError e
                         then return Nothing
                         else IO.ioError e)
 where
  fromWIFText = fromWIF . unpack
  readPrivateKeyFile = privateKeyFile >>= readTextFile

randomPrivateKey :: IO PrvKey
randomPrivateKey = withSource devRandom genPrvKey

privateKeyFile :: IO FilePath
privateKeyFile = (</> "key.wif") <$> dataDirectory

dataDirectory :: IO FilePath
dataDirectory = getAppDataDirectory "sneerkell"

savePrivateKey :: PrvKey -> IO ()
savePrivateKey key = do
  file <- privateKeyFile
  createTree $ parent file
  writeTextFile file wif
 where
  wif = pack $ toWIF key
