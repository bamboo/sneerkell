{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs, OverloadedStrings #-}

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

data Tuple where
  Tuple :: (T.ToTransit k, T.ToTransit v) => [(k, v)] -> Maybe Audience -> Author -> TupleId -> Tuple

type Audience = Address
type Author   = Address
type TupleId  = Integer

audience :: Tuple -> Maybe Audience
audience (Tuple _ a _ _) = a

instance T.ToTransit Address where
  toTransit = T.TExtension "puk" . T.toTransit . addrToBase58

instance T.ToTransit ClientRequest where
  toTransit (PingFrom ownPuk)               = T.TMap [(k "from", t ownPuk)]
  toTransit (SendFrom ownPuk peerPuk tuple) = T.TMap [(k "from", t ownPuk)
                                                     ,(k "to",   t peerPuk)
                                                     ,(k "send", t tuple)
                                                     ]

k :: String -> T.Transit
k = T.TKeyword . pack

t :: (T.ToTransit a) => a -> T.Transit
t = T.toTransit

instance T.ToTransit Tuple where
  toTransit (Tuple _fields _audience _author _id) =
    T.TMap [(T.TString "id", T.TNumber $ fromIntegral _id)]
