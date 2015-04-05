{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Data.Transit where

import qualified Data.Aeson.Types as J
import           Data.Scientific
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Vector as V

data Transit where
  Extension :: (Transitable a) => String -> a -> Transit
  Map       :: (Transitable k, Transitable v) => [(k, v)] -> Transit
  Keyword   :: String -> Transit
  Str       :: String -> Transit
  Number    :: Scientific -> Transit

class Transitable a where
  transit :: a -> Transit

instance Transitable Transit where
  transit = id

instance J.ToJSON Transit where
  toJSON (Extension tag v) = jarray [jstring $ "~#" ++ tag, tson v]
  toJSON (Map kvs)         = jarray $ mapMarker : L.concatMap (\(k, v) -> [tson k, tson v]) kvs
  toJSON (Keyword k)       = jstring $ "~:" ++ k
  toJSON (Str s)           = jstring s
  toJSON (Number n)        = J.Number n

jarray :: [J.Value] -> J.Value
jarray = J.Array . V.fromList

jstring :: String -> J.Value
jstring = J.String . T.pack

mapMarker :: J.Value
mapMarker = jstring "^ "

tson :: (Transitable a) => a -> J.Value
tson = J.toJSON . transit

instance Transitable String where
  transit = Str
