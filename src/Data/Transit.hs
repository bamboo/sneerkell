{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Transit
       ( Transit(..)
       , ToTransit(..)
       , FromTransit(..)
       , KeyValuePairs
       , KeyValuePair
       , number
       , integer
       , string
       , tson
       , untson
       , J.encode
       , J.decode
       , T.pack
       , T.unpack
       ) where

import           Control.Monad (mzero)
import qualified Data.Aeson as J
import           Data.ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.List as L
import           Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Transit.Parser as P
import           Data.Transit.Value
import qualified Data.Vector as V
import           GHC.Exts (IsList (..))

class ToTransit a where
  toTransit :: a -> Transit

class FromTransit a where
  fromTransit :: Transit -> Maybe a

instance ToTransit String where
  toTransit = string

instance FromTransit String where
  fromTransit (TString s) = Just $ T.unpack s
  fromTransit _           = Nothing

instance ToTransit Integer where
  toTransit = number

instance FromTransit Integer where
  fromTransit (TNumber n) =
    case floatingOrInteger n :: Either Double Integer of
      Right i -> Just i
      _       -> Nothing
  fromTransit _ = Nothing

instance J.ToJSON Transit where
  toJSON (TExtension tag rep) = jarray [J.String $ T.append "~#" tag, J.toJSON rep]
  toJSON (TMap kvs)           = jarray $ mapMarker : L.map J.toJSON (unpairs kvs)
  toJSON (TKeyword k)         = J.String $ T.append "~:" k
  toJSON (TString s)          = J.String s
  toJSON (TNumber n)          = J.Number n
  toJSON (TBytes bs)          = J.String $ T.append "~b" $ toBase64 bs

toBase64 :: ByteString -> T.Text
toBase64 = T.decodeUtf8 . Base64.encode

instance J.FromJSON Transit where
  parseJSON value = case P.toTransit value of
    Nothing -> mzero
    Just t  -> return t

unpairs :: KeyValuePairs -> [Transit]
unpairs = L.concatMap (\(k, v) -> [k, v]) . V.toList

jarray :: [J.Value] -> J.Value
jarray = J.Array . V.fromList

mapMarker :: J.Value
mapMarker = J.String "^ "

tson :: (ToTransit a) => a -> J.Value
tson = J.toJSON . toTransit

untson :: (FromTransit a) => J.Value -> Maybe a
untson v =
  case J.fromJSON v of
    J.Success t -> fromTransit t
    _           -> Nothing

instance IsList (V.Vector a) where
  type Item (V.Vector a) = a
  fromList  = V.fromList
  toList    = V.toList
  fromListN = V.fromListN
