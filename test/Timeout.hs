module Timeout (withTimeout) where

import System.Environment (lookupEnv)
import System.Timeout

withTimeout :: IO a -> IO (Maybe a)
withTimeout action = do
  t <- defaultTimeout
  timeoutIn t seconds action

defaultTimeout :: IO Int
defaultTimeout = do
  ci <- lookupEnv "CI"
  return $ case ci of
    Just _  -> 30
    Nothing -> 1

timeoutIn :: Int -> (Int -> Int) -> IO a -> IO (Maybe a)
timeoutIn t unit = timeout (unit t)

seconds :: Int -> Int
seconds = (* 1000000)
