{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Sneer.Protocol where

import           Control.Applicative ((<$>))
import qualified Data.Transit as T
import           Data.Text (pack, unpack)
import           Network.Haskoin.Crypto (Address, addrToBase58, base58ToAddr)

data FromClient = PingFrom  OwnPuk
                | SendFrom  OwnPuk PeerPuk Tuple
                | AckAccept OwnPuk PeerPuk TupleId
                | AckCTS    OwnPuk PeerPuk

data FromServer = Ack    PeerPuk TupleId
                | Nack   PeerPuk TupleId
                | Accept Tuple
                | CTS    PeerPuk

type OwnPuk  = Address

type PeerPuk = Address

data Tuple = Tuple { _fields   :: [(T.Transit, T.Transit)]
                   , _author   :: Author
                   , _audience :: Audience
                   , _id       :: TupleId
                   } deriving (Show, Eq)

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
           ,(k "for", tt peerPuk)
           ,(k "ack", T.number tupleId)
           ]

  toTransit (AckCTS ownPuk peerPuk) =
    T.TMap [(k "from", tt ownPuk)
           ,(k "for",  tt peerPuk)
           ]

instance T.FromTransit FromServer where
  fromTransit (T.TMap kvs) =
    case lookup (k "send") kvs of
      Just tuple -> Accept <$> T.fromTransit tuple
      _          -> Nothing
  fromTransit _ = Nothing

k :: String -> T.Transit
k = T.TKeyword . pack

tt :: (T.ToTransit a) => a -> T.Transit
tt = T.toTransit

instance T.ToTransit Tuple where
  toTransit Tuple{..} = T.TMap [(T.TString "id", T.number _id)]

instance T.FromTransit Tuple where
  fromTransit (T.TMap kvs) = do
    _id <- parse "id"
    _author <- parse "author"
    _audience <- parse "audience"
    let _fields = kvs
    return Tuple{..}
   where
    parse field = T.fromTransit =<< lookup (T.TString field) kvs

  fromTransit _ = Nothing
