{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Data.Transit where

import qualified Data.Aeson.Types as J
import           Data.ByteString
import qualified Data.List as L
import           Data.Scientific
import qualified Data.Text as T
import qualified Data.Vector as V

type ExtensionTag = T.Text

data Transit = TExtension ExtensionTag Transit
             | TMap [(Transit, Transit)]
             | TKeyword T.Text
             | TString T.Text
             | TNumber Scientific
             | TBytes ByteString
             deriving (Show, Eq)

number :: (Integral a) => a -> Transit
number = TNumber . fromIntegral

class ToTransit a where
  toTransit :: a -> Transit

class FromTransit a where
  fromTransit :: Transit -> Maybe a

instance J.ToJSON Transit where
  toJSON (TExtension tag v) = jarray [J.String $ T.append "~#" tag, J.toJSON v]
  toJSON (TMap kvs)         = jarray $ mapMarker : L.concatMap (\(k, v) -> [J.toJSON k, J.toJSON v]) kvs
  toJSON (TKeyword k)       = J.String $ T.append "~:" k
  toJSON (TString s)        = J.String s
  toJSON (TNumber n)        = J.Number n

instance J.FromJSON Transit where
  parseJSON _ = error "not implemented"

jarray :: [J.Value] -> J.Value
jarray = J.Array . V.fromList

jstring :: String -> J.Value
jstring = J.String . T.pack

mapMarker :: J.Value
mapMarker = jstring "^ "

tson :: (ToTransit a) => a -> J.Value
tson = J.toJSON . toTransit

untson :: (FromTransit a) => J.Value -> Maybe a
untson v =
  case J.fromJSON v of
    J.Success t -> fromTransit t
    _           -> Nothing

instance ToTransit String where
  toTransit = TString . T.pack

instance FromTransit String where
  fromTransit (TString s) = Just $ T.unpack s
  fromTransit _           = Nothing

instance FromTransit Integer where
  fromTransit (TNumber n) =
    case floatingOrInteger n :: Either Double Integer of
      Right i -> Just i
      _       -> Nothing
