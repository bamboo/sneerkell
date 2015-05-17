module Data.Transit.Value where

import           Data.ByteString
import           Data.Scientific
import qualified Data.Text as T
import qualified Data.Vector as V

type ExtensionTag  = T.Text
type KeyValuePairs = V.Vector KeyValuePair
type KeyValuePair  = (Transit, Transit)

data Transit = TExtension !ExtensionTag !Transit
             | TMap       !KeyValuePairs
             | TKeyword   !T.Text
             | TString    !T.Text
             | TNumber    !Scientific
             | TBytes     !ByteString
             deriving (Eq, Show)

number :: (Integral a) => a -> Transit
number = TNumber . fromIntegral

integer :: Integer -> Transit
integer = TNumber . fromIntegral

string :: String -> Transit
string = TString . T.pack
