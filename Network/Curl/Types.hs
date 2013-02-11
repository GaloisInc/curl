{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, CPP #-}
--------------------------------------------------------------------
-- |
-- Module    : Network.Curl.Types
-- Copyright : (c) Galois Inc 2007-2009
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability: portable
--
-- Basic set of types for the Haskell curl binding, including the
-- @Curl@ handle type which holds the C library stateful connection
-- handle along with a set of cleanup actions tht should be performed
-- upon shutting down the curl session.
--
--------------------------------------------------------------------
module Network.Curl.Types
  ( CurlH, URLString, Port, Long, LLong, Slist_ 
  , Curl, curlPrim, mkCurl, mkCurlWithCleanup
  , OptionMap, shareCleanup, runCleanup, updateCleanup
  ) where

import Network.Curl.Debug

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Concurrent ( addForeignPtrFinalizer )
import Data.Word
import Control.Concurrent
import Data.Maybe(fromMaybe)
import qualified Data.IntMap as M
import Data.IORef
-- import System.IO

data Curl_
type CurlH    = Ptr Curl_

type URLString = String
type Port = Long
type Long = Word32
type LLong = Word64
data Slist_


data Curl = Curl 
  { curlH       :: MVar (ForeignPtr Curl_)  -- libcurl is not thread-safe.
  , curlCleanup :: IORef OptionMap          -- deallocate Haskell curl data 
  }  


-- | Execute a "primitive" curl operation.
-- NOTE: See warnings about the use of 'withForeignPtr'.
curlPrim :: Curl -> (IORef OptionMap -> CurlH -> IO a) -> IO a
curlPrim c f  = withMVar (curlH c) $ \ h ->
                withForeignPtr h   $ f $ curlCleanup c


-- | Allocates a Haskell handle from a C handle.
mkCurl :: CurlH -> IO Curl
mkCurl h = mkCurlWithCleanup h om_empty

-- | Allocates a Haskell handle from a C handle.
mkCurlWithCleanup :: CurlH -> OptionMap -> IO Curl
mkCurlWithCleanup h clean = do
  debug "ALLOC: CURL"
  v2  <- newIORef clean
  fh  <- newForeignPtr_ h 
  v1  <- newMVar fh
  let new_h = Curl { curlH = v1, curlCleanup = v2 }
  
  let fnalizr = do
         debug "FREE: CURL"
         easy_cleanup h
         runCleanup v2
  Foreign.Concurrent.addForeignPtrFinalizer fh fnalizr
  return new_h


-- Admin code for cleaning up marshalled data.
-- Note that these functions assume that they are running atomically,
-- so access to them should be protected by a lock.
--------------------------------------------------------------------------------
runCleanup     :: IORef OptionMap -> IO ()
runCleanup r    = do m <- readIORef r
                     om_cleanup m
                     writeIORef r om_empty 

shareCleanup  :: IORef OptionMap -> IO OptionMap
shareCleanup r  = do old <- readIORef r
                     new <- om_dup old
                     writeIORef r new
                     return new

updateCleanup :: IORef OptionMap -> Int -> IO () -> IO ()
updateCleanup r option act = writeIORef r =<< om_set option act =<< readIORef r



-- Maps that associate curl options with IO actions to
-- perform cleanup for them.
--------------------------------------------------------------------------------
type OptionMap = M.IntMap (IO ())

-- | An empty option map.
om_empty :: OptionMap
om_empty = M.empty

-- | Set the IO action for an option,
-- executing the previvous action, if there was one.
om_set :: Int -> IO () -> OptionMap -> IO OptionMap
om_set opt new_act old_map = 
  do fromMaybe (return ()) old_act
     return new_map
  where
  (old_act,new_map) = M.insertLookupWithKey (\_ a _ -> a) opt new_act old_map

-- | Execute all IO actions in the map.
om_cleanup :: OptionMap -> IO ()
om_cleanup m = sequence_ (M.elems m)

-- | Replace the actions in a map, with actions that
-- will only be executed the second time they are invoked.
om_dup :: OptionMap -> IO OptionMap
om_dup old_map = M.fromList `fmap` mapM dup (M.assocs old_map)
  where dup (x,old_io)  = do new_io <- shareIO old_io
                             return (x,new_io)

-- Share a cleanup action.  When we share cleanup duty between two handles
-- we need to ensure that the first handle to perform the cleanup will do
-- nothing (because the other handle still needs the resources).
shareIO :: IO () -> IO (IO ())
shareIO act = 
  do v <- newMVar False
     let new_act = do b <- takeMVar v
                      if b then act else putMVar v True
     return new_act
--------------------------------------------------------------------------------

{- UNUSED:
-- FFI for inalizers.

-- | Make a finalizer from an IO action.
mkIOfin :: IO a -> IO (FinalizerPtr b)
mkIOfin m = mfix (\ptr -> ioFinalizer (m >> freeHaskellFunPtr ptr))

foreign import ccall "wrapper"
  ioFinalizer :: IO () -> IO (FinalizerPtr a)


-}

foreign import ccall 
  "curl/curl.h curl_easy_cleanup" easy_cleanup :: CurlH -> IO ()

