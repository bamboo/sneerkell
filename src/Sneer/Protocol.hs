{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}

module Sneer.Protocol where

import qualified Data.Transit as T
import           Network.Haskoin.Crypto

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
  Tuple :: (T.Transitable k, T.Transitable v) => Author -> TupleId -> [(k, v)] -> Tuple

type Author  = Address
type TupleId = Integer

instance T.Transitable Address where
  transit = T.Extension "puk" . addrToBase58

instance T.Transitable ClientRequest where
  transit (PingFrom address) = T.Map [(T.Keyword "from", address)]
  transit _ = error "not implemented"
