{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Sneer.Protocol where

import           Control.Applicative ((<$>))
import qualified Data.Transit as T
import           Data.Text (pack, unpack)
import           Network.Haskoin.Crypto (Address, addrToBase58, base58ToAddr)

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

data Tuple = Tuple { _fields   :: ![(T.Transit, T.Transit)]
                   , _author   :: !Author
                   , _audience :: !Audience
                   , _id       :: !TupleId
                   } deriving (Eq, Show)

type Audience = Address
type Author   = Address
type TupleId  = Integer

instance T.ToTransit Address where
  toTransit = T.TExtension "puk" . T.toTransit . addrToBase58

instance T.FromTransit Address where
  fromTransit (T.TExtension "puk" (T.TString repr)) = base58ToAddr $ unpack repr
  fromTransit _ = Nothing

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
  fromTransit (T.TMap kvs) = Accept <$> (lookup (k "send") kvs >>= T.fromTransit)
  fromTransit _            = Nothing

k :: String -> T.Transit
k = T.TKeyword . pack

tt :: (T.ToTransit a) => a -> T.Transit
tt = T.toTransit

instance T.ToTransit Tuple where
  toTransit Tuple{..} = T.TMap $ [(T.string "id", T.number _id)
                                 ,(T.string "author", tt _author)
                                 ,(T.string "audience", tt _audience)
                                 ] ++ _fields

instance T.FromTransit Tuple where
  fromTransit (T.TMap kvs) = do
    _id <- parse idField
    _author <- parse authorField
    _audience <- parse audienceField
    let _fields = filter isCustomField kvs
    return Tuple{..}
   where
    parse field = T.fromTransit =<< lookup field kvs
    idField = T.TString "id"
    authorField = T.TString "author"
    audienceField = T.TString "audience"
    isCustomField (field, _) = field `notElem` [idField, authorField, audienceField]

  fromTransit _ = Nothing
