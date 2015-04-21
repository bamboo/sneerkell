{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Transit ( Transit(..)
                    , ToTransit(..)
                    , FromTransit(..)
                    , number
                    , string
                    , tson
                    , untson
                    , J.encode
                    , J.decode
                    , T.pack
                    , T.unpack
                    ) where

import           Control.Monad (mzero, guard)
import qualified Data.Aeson as J
import           Data.ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.List as L
import           Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

type ExtensionTag = T.Text

data Transit = TExtension !ExtensionTag !Transit
             | TMap       ![(Transit, Transit)]
             | TKeyword   !T.Text
             | TString    !T.Text
             | TNumber    !Scientific
             | TBytes     !ByteString
             deriving (Eq, Show)

number :: (Integral a) => a -> Transit
number = TNumber . fromIntegral

string :: String -> Transit
string = TString . T.pack

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
  toJSON (TMap kvs)           = jarray $ mapMarker : L.concatMap (\(k, v) -> [J.toJSON k, J.toJSON v]) kvs
  toJSON (TKeyword k)         = J.String $ T.append "~:" k
  toJSON (TString s)          = J.String s
  toJSON (TNumber n)          = J.Number n
  toJSON (TBytes bs)          = J.String $ T.append "~b" $ toBase64 bs

toBase64 :: ByteString -> T.Text
toBase64 = T.decodeUtf8 . Base64.encode

fromBase64 :: T.Text -> ByteString
fromBase64 = Base64.decodeLenient . T.encodeUtf8

instance J.FromJSON Transit where
  parseJSON (J.String (T.stripPrefix "~:" -> Just keyword)) =
    return $ TKeyword keyword
  parseJSON (J.String (T.stripPrefix "~b" -> Just base64)) =
    return $ TBytes $ fromBase64 base64
  parseJSON (J.String s) =
    return $ TString s
  parseJSON (J.Number n) =
    return $ TNumber n
  parseJSON (J.Array xs) = do
    guard $ V.length xs >= 1 && xs V.! 0 == mapMarker
    kvs <- V.mapM J.parseJSON $ V.tail xs
    return $ TMap (pairs $ V.toList kvs)
  parseJSON _ = mzero

pairs :: [a] -> [(a, a)]
pairs (k:v:kvs) = (k, v) : pairs kvs
pairs _         = []

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
