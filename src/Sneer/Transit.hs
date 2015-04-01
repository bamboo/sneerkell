{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Sneer.Transit where

import qualified Data.Aeson.Types as J
import qualified Data.List as L
import Data.Text as T
import qualified Data.Vector as V

data Transit where
  Extension  :: (Transitable a) => String -> a -> Transit
  Keyword    :: String -> Transit
  Str        :: String -> Transit
  Map        :: (Transitable k, Transitable v) => [(k, v)] -> Transit

class Transitable a where
  transit :: a -> Transit

instance Transitable Transit where
  transit = id

instance J.ToJSON Transit where
  toJSON (Str s) = jstring s
  toJSON (Keyword k) = jstring $ "~:" ++ k
  toJSON (Map kvs) = J.Array . V.fromList $ mapMarker : L.concatMap (\(k, v) -> [tson k, tson v]) kvs
  toJSON (Extension tag v) = J.Array $ V.fromList [jstring $ "~#" ++ tag, tson v]

jstring :: String -> J.Value
jstring = J.String . T.pack

mapMarker :: J.Value
mapMarker = J.String "^ "

tson :: (Transitable a) => a -> J.Value
tson = J.toJSON . transit

instance Transitable String where
  transit = Str
