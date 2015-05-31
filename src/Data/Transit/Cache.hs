module Data.Transit.Cache
       ( Cache ()
       , empty
       , insert
       , lookup
       ) where

import           Data.Char (chr)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Prelude hiding (lookup)

type Cache = HM.HashMap T.Text T.Text

empty :: Cache
empty = HM.empty

insert :: T.Text -> Cache -> Cache
insert value cache =
  if T.length value <= 3
    then cache
    else
      let index = HM.size cache
          key   = indexToCode index
      in HM.insert key value cache

lookup :: T.Text -> Cache -> Maybe T.Text
lookup = HM.lookup

indexToCode :: Int -> T.Text
indexToCode index = T.append codePrefix $
  if hi == 0
    then charCode lo
    else T.append (charCode hi) (charCode lo)
 where
  (hi, lo)        = index `divMod` cacheCodeDigits
  charCode        = T.singleton . chr . (+ baseCharIndex)
  cacheCodeDigits = 44 :: Int
  baseCharIndex   = 48 :: Int

codePrefix :: T.Text
codePrefix = T.pack "^"
