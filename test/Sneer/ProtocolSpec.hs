{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Sneer.ProtocolSpec where

import Data.TransitSpec
import Sneer.Keys
import Sneer.Protocol
import Test.Hspec
import Test.Hspec.SmallCheck
import Test.SmallCheck.Series

spec :: Spec
spec =
  describe "Tuple" $
    it "can be Transit roundtripped" $
      property $ \s -> roundtrip s == Just (s :: Tuple)

instance (Monad m) => Serial m Tuple where
  series = localDepth (min 3) $ cons4 Tuple

instance (Monad m) => Serial m Address where
  series = cons1 address

address :: Integer -> Address
address = PubKeyAddress . fromIntegral
