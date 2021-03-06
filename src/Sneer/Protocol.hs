{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings, OverloadedLists #-}

module Sneer.Protocol
       ( FromClient(..)
       , FromServer(..)
       , Tuple(..)
       , tt
       , addressBytes
       , addressFromHex
       ) where

import           Control.Applicative ((<$>), (<|>), (<*>))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import           Data.Text (pack)
import qualified Data.Transit as T
import           Data.Vector
import qualified Data.Vector as V
import           Network.Haskoin.Crypto (Address(..))
import           Network.Haskoin.Internals (BigWord (..))
import qualified Network.Haskoin.Util as U

data FromClient = PingFrom  !OwnPuk
                | SendFrom  !OwnPuk !PeerPuk !Tuple
                | AckAccept !OwnPuk !PeerPuk !TupleId
                | AckCTS    !OwnPuk !PeerPuk
                deriving (Eq, Show)

data FromServer = Ack    !PeerPuk !TupleId
                | Nack   !PeerPuk !TupleId
                | Accept !Tuple
                | CTS    !PeerPuk
                deriving (Eq, Show)

type OwnPuk  = Address

type PeerPuk = Address

data Tuple = Tuple { _fields   :: !T.KeyValuePairs
                   , _author   :: !Author
                   , _audience :: !Audience
                   , _id       :: !TupleId
                   } deriving (Eq, Show)

type Audience = Address
type Author   = Address
type TupleId  = Integer

addressBytes :: Address -> ByteString
addressBytes = paddedTo32Bytes . U.integerToBS . addrToInteger

addressFromHex :: ByteString -> Address
addressFromHex = addrFromInteger . U.bsToInteger . fst . Base16.decode

instance T.ToTransit Address where
  toTransit = T.TExtension "puk" . T.toTransit . addressBytes

instance T.FromTransit Address where
  fromTransit (T.TExtension "puk" (T.TBytes bytes)) = Just $ addrFromInteger $ U.bsToInteger bytes
  fromTransit _ = Nothing

instance T.ToTransit ByteString where
  toTransit = T.TBytes

instance T.FromTransit ByteString where
  fromTransit (T.TBytes bytes) = Just bytes
  fromTransit _                = Nothing

paddedTo32Bytes :: ByteString -> ByteString
paddedTo32Bytes bytes = BS.append (BS.replicate padding 0) bytes
 where padding = 32 - BS.length bytes

addrToInteger :: Address -> Integer
addrToInteger (PubKeyAddress (BigWord n)) = n
addrToInteger _                           = error "Unsupported Address type"

addrFromInteger :: Integer -> Address
addrFromInteger = PubKeyAddress . BigWord

instance T.ToTransit FromClient where
  toTransit (PingFrom ownPuk) =
    T.TMap [(k "from", tt ownPuk)]

  toTransit (SendFrom ownPuk peerPuk tuple) =
    T.TMap [(k "from", tt ownPuk)
           ,(k "to",   tt peerPuk)
           ,(k "send", tt tuple)
           ]

  toTransit (AckAccept ownPuk peerPuk tupleId) =
    T.TMap [(k "from", tt ownPuk)
           ,(k "ack", tt peerPuk)
           ,(k "id", T.number tupleId)
           ]

  toTransit (AckCTS ownPuk peerPuk) =
    T.TMap [(k "from", tt ownPuk)
           ,(k "ack",  tt peerPuk)
           ]

instance T.FromTransit FromServer where
  fromTransit (T.TMap kvs) = parseAck kvs <|> parseAccept kvs
  fromTransit _            = Nothing

parseAck :: T.KeyValuePairs -> Maybe FromServer
parseAck kvs =
  Ack <$> tryGetFromTransit forKeyword kvs
      <*> tryGetFromTransit ackKeyword kvs

forKeyword :: T.Transit
forKeyword = k "for"

ackKeyword :: T.Transit
ackKeyword = k "ack"

parseAccept :: T.KeyValuePairs -> Maybe FromServer
parseAccept kvs = Accept <$> tryGetFromTransit sendKeyword kvs

tryGetFromTransit :: T.FromTransit a => T.Transit -> T.KeyValuePairs -> Maybe a
tryGetFromTransit key kvs = tryGet key kvs >>= T.fromTransit

sendKeyword :: T.Transit
sendKeyword = k "send"

tryGet :: (Eq a) => a -> Vector (a, b) -> Maybe b
tryGet key kvs = snd <$> V.find ((== key) . fst) kvs

k :: String -> T.Transit
k = T.TKeyword . pack

tt :: (T.ToTransit a) => a -> T.Transit
tt = T.toTransit

instance T.ToTransit Tuple where
  toTransit Tuple{..} =
    T.TMap $ builtinFields V.++ _fields
   where
    builtinFields = [(idField,       T.number _id)
                    ,(authorField,   tt _author)
                    ,(audienceField, tt _audience)
                    ]

instance T.FromTransit Tuple where
  fromTransit (T.TMap kvs) = do
    _id <- parse idField
    _author <- parse authorField
    _audience <- parse audienceField
    let _fields = V.filter isCustomField kvs
    return Tuple{..}
   where
    parse field = tryGetFromTransit field kvs
  fromTransit _ = Nothing

idField :: T.Transit
idField = T.TString "id"

authorField :: T.Transit
authorField = T.TString "author"

audienceField :: T.Transit
audienceField = T.TString "audience"

isCustomField :: T.KeyValuePair -> Bool
isCustomField (field, _) = field /= idField
                        && field /= authorField
                        && field /= audienceField
