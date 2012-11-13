{-# LANGUAGE ForeignFunctionInterface #-}
--------------------------------------------------------------------
-- |
-- Module    : Network.Curl.Post
-- Copyright : (c) Galois Inc 2007-2009
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing and marshalling formdata (as part of POST uploads\/submissions.)
-- If you are only looking to submit a sequence of name=value pairs,
-- you are better off using the CurlPostFields constructor; much simpler.
--
--------------------------------------------------------------------
module Network.Curl.Post where

import Network.Curl.Types

import Control.Monad
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Foreign.C.String

type Header = String

data HttpPost
 = HttpPost
     { postName     :: String
     , contentType  :: Maybe String
     , content      :: Content
     , extraHeaders :: [Header]
-- not yet:     , extraEntries :: [HttpPost]
     , showName     :: Maybe String
     } deriving ( Eq, Show )

data Content
 = ContentFile   FilePath
 | ContentBuffer (Ptr CChar) Long -- byte arrays also?
 | ContentString String
   deriving ( Eq, Show )

multiformString :: String -> String -> HttpPost
multiformString x y = 
  HttpPost { postName      = x
           , content       = ContentString y
           , contentType   = Nothing
           , extraHeaders  = []
           , showName      = Nothing
           } 

-- lower-level marshalling code.

sizeof_httppost :: Int
sizeof_httppost = 12 * sizeOf (nullPtr :: Ptr CChar)

marshallPosts :: [HttpPost] -> IO (Ptr HttpPost)
marshallPosts [] = return nullPtr
marshallPosts ps = do
  ms <- mapM marshallPost ps
  case ms of
    [] -> return nullPtr
    (x:xs) -> do
      linkUp x xs
      return x
 where
  linkUp p [] = pokeByteOff p 0 nullPtr
  linkUp p (x:xs) = do
    pokeByteOff p 0 x
    linkUp x xs
  
marshallPost :: HttpPost -> IO (Ptr HttpPost)
marshallPost p = do
  php <- mallocBytes sizeof_httppost
  pokeByteOff php 0 nullPtr
  newCString (postName p) >>= pokeByteOff php (ptrIndex 1)
  pokeByteOff php (ptrIndex 2) (length (postName p))
  case content p of
    ContentFile f -> do
      newCString f >>= pokeByteOff php (ptrIndex 3)
      pokeByteOff php (ptrIndex 4) (length f)
      pokeByteOff php (ptrIndex 5) nullPtr
      pokeByteOff php (ptrIndex 6) nullPtr
      pokeByteOff php (ptrIndex 10) (0x1 :: Long)
    ContentBuffer ptr len -> do
      pokeByteOff php (ptrIndex 3) nullPtr
      pokeByteOff php (ptrIndex 4) nullPtr
      pokeByteOff php (ptrIndex 5) ptr 
      pokeByteOff php (ptrIndex 6) len
      pokeByteOff php (ptrIndex 10) (0x10 :: Long)
    ContentString s -> do
      newCString s >>= pokeByteOff php (ptrIndex 3)
      pokeByteOff php (ptrIndex 4) (length s)
      pokeByteOff php (ptrIndex 5) nullPtr
      pokeByteOff php (ptrIndex 6) nullPtr
      pokeByteOff php (ptrIndex 10) (0x4 :: Long)
  
  cs1 <- case contentType p of
    Nothing -> return nullPtr
    Just s  -> newCString s
  pokeByteOff php (ptrIndex 7) cs1
  cs2 <- mapM newCString (extraHeaders p)
  ip <- foldM curl_slist_append nullPtr cs2
  pokeByteOff php (ptrIndex 8) ip
  pokeByteOff php (ptrIndex 9) nullPtr
  case showName p of
    Nothing -> pokeByteOff php (ptrIndex 11) nullPtr
    Just s  -> newCString s >>= pokeByteOff php (ptrIndex 11)
  return php
 where
  ptrIndex n = n * sizeOf nullPtr


foreign import ccall
  "curl_slist_append" curl_slist_append :: Ptr Slist_ -> CString -> IO (Ptr Slist_)
foreign import ccall
  "curl_slist_free_all" curl_slist_free :: Ptr Slist_ -> IO ()

foreign import ccall
  "curl_formfree" curl_formfree :: Ptr a -> IO ()
  

