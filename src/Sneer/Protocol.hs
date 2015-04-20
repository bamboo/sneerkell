{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}

module Sneer.Protocol where

import qualified Data.Transit as T
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
  Tuple :: (T.Transitable k, T.Transitable v) => [(k, v)] -> Maybe Audience -> Author -> TupleId -> Tuple

type Audience = Address
type Author   = Address
type TupleId  = Integer

audience :: Tuple -> Maybe Audience
audience (Tuple _ a _ _) = a

instance T.Transitable Address where
  transit = T.Extension "puk" . addrToBase58

instance T.Transitable ClientRequest where
  transit (PingFrom address) = T.Map [(k "from", address)]
  transit (SendFrom ownPuk peerPuk tuple) =
    T.Map [(k "from", t ownPuk)
          ,(k "to", t peerPuk)
          ,(k "send", t tuple)
          ]
  transit _ = error "not implemented"

k :: String -> T.Transit
k = T.Keyword

t :: (T.Transitable a) => a -> T.Transit
t = T.transit

instance T.Transitable Tuple where
  transit (Tuple _fields _audience _author _id) = T.Map [("id", T.Number $ fromIntegral _id)]
