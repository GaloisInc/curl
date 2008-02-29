--------------------------------------------------------------------
-- |
-- Module    : Network.Curl.Debug
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: emertens@galois.com
-- Stability : provisional
-- Portability:
--
-- Debug hooks

module Network.Curl.Debug (debug) where

import System.IO

debugging :: Bool
debugging = False

debug :: String -> IO ()
debug msg
  | debugging     = putStrLn ("DEBUG: " ++ msg) >> hFlush stdout
  | otherwise     = return ()

