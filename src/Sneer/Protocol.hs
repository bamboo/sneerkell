{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Sneer.Protocol where

import qualified Data.Transit as T
import           Data.Text (pack)
import           Network.Haskoin.Crypto (Address, addrToBase58)

data ClientRequest = PingFrom OwnPuk
                   | SendFrom OwnPuk PeerPuk Tuple

data ServerReply   = Ack  PeerPuk TupleId
                   | Nack PeerPuk TupleId

data ServerRequest = Send Tuple
                   | CTS  PeerPuk

data ClientReply   = AckSend OwnPuk PeerPuk TupleId
                   | AckCTS  OwnPuk PeerPuk

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

instance T.ToTransit ClientRequest where
  toTransit (PingFrom ownPuk)               = T.TMap [(k "from", tt ownPuk)]
  toTransit (SendFrom ownPuk peerPuk tuple) = T.TMap [(k "from", tt ownPuk)
                                                     ,(k "to",   tt peerPuk)
                                                     ,(k "send", tt tuple)
                                                     ]

k :: String -> T.Transit
k = T.TKeyword . pack

tt :: (T.ToTransit a) => a -> T.Transit
tt = T.toTransit

instance T.ToTransit Tuple where
  toTransit Tuple{..} = T.TMap [(T.TString "id", T.TNumber $ fromIntegral _id)]
