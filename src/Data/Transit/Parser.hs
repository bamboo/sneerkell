{-# LANGUAGE OverloadedStrings #-}

module Data.Transit.Parser where

import           Control.Applicative ((<|>))
import           Control.Monad (guard, mzero, when)
import           Control.Monad.Trans.State.Lazy
import qualified Data.Aeson as J
import           Data.ByteString
import qualified Data.ByteString.Base64 as Base64
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Transit.Cache as TC
import           Data.Transit.Value
import qualified Data.Vector as V

type TransitParser t = StateT TC.Cache Maybe t

toTransit :: J.Value -> Maybe Transit
toTransit = runParser . parse

runParser :: TransitParser t -> Maybe t
runParser p = fmap fst (runStateT p TC.empty)

parse :: J.Value -> TransitParser Transit
parse (J.Number n) = return $ TNumber n
parse (J.String s) = parseString False s
parse (J.Array xs) = parseMap xs <|> parseExtension xs
parse _            = mzero

parseMap :: V.Vector J.Value -> TransitParser Transit
parseMap xs = do
  guard $ V.length xs >= 1 && (xs V.! 0) == mapMarker
  let kvs = V.tail xs
  let ikvs = V.indexed kvs
  kvs' <- V.mapM parseKeyOrValue ikvs
  return $ TMap (V.fromList . pairs . V.toList $ kvs')

parseKeyOrValue :: (Int, J.Value) -> TransitParser Transit
parseKeyOrValue (idx, value) =
  if even idx
     then parseKey value
     else parse value

parseExtension :: V.Vector J.Value -> TransitParser Transit
parseExtension xs = do
  guard $ V.length xs == 2
  case xs V.! 0 of
    J.String s -> do
      cache <- get
      let s' = fromMaybe s (TC.lookup s cache)
      case T.stripPrefix "~#" s' of
        Just tag -> do
          when (s == s') $ modify $ TC.insert s
          rep <- parse $ xs V.! 1
          return $ TExtension tag rep
        _        -> mzero
    _ -> mzero

parseKey :: J.Value -> TransitParser Transit
parseKey (J.String s) = parseString True s
parseKey v = parse v

parseString :: Bool -> T.Text -> TransitParser Transit
parseString updateCache s = do
  s' <- cacheString updateCache s
  return $ case T.stripPrefix "~:" s' of
    Just keyword -> TKeyword keyword
    Nothing      ->
      case T.stripPrefix "~b" s' of
        Just base64 -> TBytes $ fromBase64 base64
        Nothing     -> TString s'

cacheString :: Bool -> T.Text -> TransitParser T.Text
cacheString updateCache key = do
  cache <- get
  case TC.lookup key cache of
    Just cached -> return cached
    Nothing     -> do
      when updateCache $
        modify $ TC.insert key
      return key

mapMarker :: J.Value
mapMarker = J.String "^ "

pairs :: [a] -> [(a, a)]
pairs (k:v:kvs) = (k, v) : pairs kvs
pairs _         = []

fromBase64 :: T.Text -> ByteString
fromBase64 = Base64.decodeLenient . T.encodeUtf8
