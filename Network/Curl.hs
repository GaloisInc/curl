{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
--------------------------------------------------------------------
-- |
-- Module    : Network.Curl
-- Copyright : (c) 2007-2009, Galois Inc 
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability: portable
--
-- A Haskell binding the libcurl library <http://curl.haxx.se/>, a
-- proven and feature-rich library for interacting with HTTP(S)\/FTP
-- servers.
--
-- The binding was initially made against version 7.16.2; libcurl does
-- appear to be considerate in not introducing breaking changes wrt
-- older versions. So, unless you're after the latest features (i.e.,
-- constructors towards the end the Option type), there's a very good
-- chance your code will work against older installations of libcurl.
--
--------------------------------------------------------------------

module Network.Curl
       ( module Network.Curl.Opts
       , module Network.Curl.Easy
       , module Network.Curl.Post
       , module Network.Curl.Info
       , module Network.Curl.Types
       , module Network.Curl.Code

         -- controlled export of this module: 
         -- (ToDo: tighten it up even more)
       , withCurlDo          -- :: IO a -> IO a
       , setopts             -- :: Curl -> [CurlOption] -> IO ()

       , CurlResponse_(..)
       , CurlResponse

          -- get resources and assoc. metadata.
       , curlGet               -- :: URLString -> [CurlOption] -> IO ()
       , curlGetString         -- :: URLString -> [CurlOption] -> IO (CurlCode, String)
       , curlGetResponse       -- :: URLString -> [CurlOption] -> IO CurlResponse
       , perform_with_response -- :: Curl -> IO CurlResponse
       , do_curl        -- :: Curl -> URLString -> [CurlOption] -> IO CurlResponse

       , curlGetString_         -- :: CurlBuffer ty => URLString -> [CurlOption] -> IO (CurlCode, ty)
       , curlGetResponse_       -- :: URLString -> [CurlOption] -> IO (CurlResponse_ a b)
       , perform_with_response_ -- :: Curl -> IO (CurlResponse_ a b)
       , do_curl_               -- :: Curl -> URLString -> [CurlOption] -> IO (CurlResponse_ a b)
       , curlHead_              -- :: URLString
                                -- -> [CurlOption]
                                -- -> IO (String,ty)

          -- probing for gold..
       , curlHead            -- :: URLString
                             -- -> [CurlOption]
                             -- -> IO (String,[(String,String)])

          -- posting requests.
       , curlMultiPost       -- :: URLString -> [CurlOption] -> [HttpPost] -> IO ()
       , curlPost            -- :: URLString -> [String] -> IO ()

          -- 
       , getResponseCode     -- :: Curl -> IO Int

          -- supporting cast
       , setDefaultSSLOpts   -- :: Curl -> URLString -> IO ()
       , callbackWriter      -- :: (String -> IO ()) -> WriteFunction
       , easyWriter          -- :: (String -> IO ()) -> WriteFunction
       , ignoreOutput        -- :: WriteFunction
       , gatherOutput        -- :: IORef [String] -> WriteFunction

       , gatherOutput_      -- :: (CStringLen -> IO ()) -> WriteFunction
       , CurlBuffer(..)
       , CurlHeader(..)

       , method_GET          -- :: [CurlOption]
       , method_HEAD         -- :: [CurlOption]
       , method_POST         -- :: [CurlOption]

       , parseStatusNHeaders
       , parseHeader
          -- ToDo: get rid of (pretty sure I can already...)
       , concRev
       ) where

import Network.Curl.Opts
import Network.Curl.Code
import Network.Curl.Types
import Network.Curl.Post
import Network.Curl.Info
import Network.Curl.Easy

import Foreign.C.String
import Data.IORef
import Data.List(isPrefixOf)
-- import System.IO
import Control.Exception ( finally )

import Data.ByteString ( ByteString, packCStringLen )
import qualified Data.ByteString as BS ( concat )

import qualified Data.ByteString.Lazy as LazyBS ( ByteString, fromChunks )

-- | The @CurlBuffer@ class encodes the representation
-- of response buffers, allowing you to provide your
-- own app-specific buffer reps to be used..or use
-- one of the standard instances (String and ByteStrings.)
--
class CurlBuffer bufferTy where
  newIncoming    :: IO (IO bufferTy, CStringLen -> IO ())
  

-- | The @CurlHeader@ class encodes the representation
-- of response headers. Similar to 'CurlBuffer'.
--
class CurlHeader headerTy where
  newIncomingHeader :: IO (IO (String{-status-},headerTy), CStringLen -> IO ())

instance CurlHeader [(String,String)] where
  newIncomingHeader = do
    ref <- newIORef []
    let readFinalHeader = do
          hss <- readIORef ref
          let (st,hs) = parseStatusNHeaders (concRev [] hss)
          return (st,hs)
    return (readFinalHeader, \ v -> peekCStringLen v >>= \ x -> modifyIORef ref (x:))

instance CurlBuffer String where
  newIncoming = do
    ref <- newIORef []
    let readFinal = readIORef ref >>= return . concat . reverse
    return (readFinal, \ v -> peekCStringLen v >>= \ x -> modifyIORef ref (x:))

instance CurlBuffer ByteString where
  newIncoming = do
    ref <- newIORef []
    let readFinal = readIORef ref >>= return . BS.concat . reverse
    return (readFinal, \ v -> packCStringLen v >>= \ x -> modifyIORef ref (x:))

instance CurlBuffer [ByteString] where
  newIncoming = do
    ref <- newIORef []
    let readFinal = readIORef ref >>= return . reverse
    return (readFinal, \ v -> packCStringLen v >>= \ x -> modifyIORef ref (x:))

instance CurlBuffer LazyBS.ByteString where
  newIncoming = do
    ref <- newIORef []
    let readFinal = readIORef ref >>= return . LazyBS.fromChunks . reverse
    return (readFinal, \ v -> packCStringLen v >>= \ x -> modifyIORef ref (x:))

-- | Should be used once to wrap all uses of libcurl.
-- WARNING: the argument should not return before it
-- is completely done with curl (e.g., no forking or lazy returns)
withCurlDo :: IO a -> IO a
withCurlDo m  = do curl_global_init 3   -- initialize everything
                   finally m curl_global_cleanup

-- | Set a list of options on a Curl handle.
setopts :: Curl -> [CurlOption] -> IO ()
setopts h opts = mapM_ (setopt h) opts


method_GET   :: [CurlOption]
method_GET    = [CurlPost False, CurlNoBody False]

method_POST  :: [CurlOption]
method_POST   = [CurlPost True, CurlNoBody False]

method_HEAD  :: [CurlOption]
method_HEAD   = [CurlPost False, CurlNoBody True] 

-- | 'curlGet' perform a basic GET, dumping the output on stdout.
-- The list of options are set prior performing the GET request.
curlGet :: URLString -> [CurlOption] -> IO ()
curlGet url opts = initialize >>= \ h -> do
  setopt h (CurlFailOnError True)
  setopt h (CurlURL url)
   -- Note: later options may (and should, probably) override these defaults.
  setDefaultSSLOpts h url
  mapM_ (setopt h) opts
  perform h
  return ()

setDefaultSSLOpts :: Curl -> URLString -> IO ()
setDefaultSSLOpts h url
 | "https:" `isPrefixOf` url = do
    -- the default options are pretty dire, really -- turning off
    -- the peer verification checks!
   mapM_ (setopt h)
         [ CurlSSLVerifyPeer False
         , CurlSSLVerifyHost 0
         ]
 | otherwise = return ()

-- | 'curlGetString' performs the same request as 'curlGet', but 
-- returns the response body as a Haskell string.
curlGetString :: URLString
              -> [CurlOption]
              -> IO (CurlCode, String)
curlGetString url opts = initialize >>= \ h -> do
  ref <- newIORef []
   -- Note: later options may (and should, probably) override these defaults.
  setopt h (CurlFailOnError True)
  setDefaultSSLOpts h url
  setopt h (CurlURL url)
  setopt h (CurlWriteFunction (gatherOutput ref))
  mapM_ (setopt h) opts
  rc <- perform h
  lss <- readIORef ref
  return (rc, concat $ reverse lss)

curlGetString_ :: (CurlBuffer ty)
               => URLString
               -> [CurlOption]
               -> IO (CurlCode, ty)
curlGetString_ url opts = initialize >>= \ h -> do
  (finalBody, gatherBody) <- newIncoming
  setopt h (CurlFailOnError True)
  setDefaultSSLOpts h url
  setopt h (CurlURL url)
  setopt h (CurlWriteFunction (gatherOutput_ gatherBody))
  mapM_ (setopt h) opts
  rc <- perform h
  bs  <- finalBody
  return (rc, bs)

type CurlResponse = CurlResponse_ [(String,String)] String

-- | 'CurlResponse_' is a record type encoding all the information
-- embodied in a response to your Curl request. Currently only used
-- to gather up the results of doing a GET in 'curlGetResponse'.
data CurlResponse_ headerTy bodyTy
 = CurlResponse
     { respCurlCode   :: CurlCode
     , respStatus     :: Int
     , respStatusLine :: String
     , respHeaders    :: headerTy
     , respBody       :: bodyTy
     , respGetInfo    :: (Info -> IO InfoValue)
     }


-- | @curlGetResponse url opts@ performs a @GET@, returning all the info
-- it can lay its hands on in the response, a value of type 'CurlResponse'.
-- The representation of the body is overloaded
curlGetResponse_ :: (CurlHeader hdr, CurlBuffer ty)
                 => URLString
                 -> [CurlOption]
                 -> IO (CurlResponse_ hdr ty)
curlGetResponse_ url opts = do
  h <- initialize
   -- Note: later options may (and should, probably) override these defaults.
  setopt  h (CurlFailOnError True)
  setDefaultSSLOpts h url
  setopt  h (CurlURL url)
  mapM_ (setopt h) opts
  -- note that users cannot over-write the body and header handler
  -- which makes sense because otherwise we will return a bogus reposnse.
  perform_with_response_ h 

{-# DEPRECATED curlGetResponse "Switch to using curlGetResponse_" #-}
curlGetResponse :: URLString
                -> [CurlOption]
                -> IO CurlResponse
curlGetResponse url opts = curlGetResponse_ url opts

-- | Perform the actions already specified on the handle.
-- Collects useful information about the returned message.
-- Note that this function sets the
-- 'CurlWriteFunction' and 'CurlHeaderFunction' options.
perform_with_response :: (CurlHeader hdrTy, CurlBuffer bufTy)
                      => Curl
		      -> IO (CurlResponse_ hdrTy bufTy)
perform_with_response h = perform_with_response_ h

{-# DEPRECATED perform_with_response "Consider switching to perform_with_response_" #-}

-- | Perform the actions already specified on the handle.
-- Collects useful information about the returned message.
-- Note that this function sets the
-- 'CurlWriteFunction' and 'CurlHeaderFunction' options.
-- The returned payload is overloaded over the representation of
-- both headers and body via the 'CurlResponse_' type.
perform_with_response_ :: (CurlHeader headerTy, CurlBuffer bodyTy)
                       => Curl
		       -> IO (CurlResponse_ headerTy bodyTy)
perform_with_response_ h = do
   (finalHeader, gatherHeader) <- newIncomingHeader
   (finalBody,   gatherBody)   <- newIncoming

     -- Instead of allocating a separate handler for each
     -- request we could just set this options one and forall
     -- and just clear the IORefs.

   setopt  h (CurlWriteFunction (gatherOutput_ gatherBody))
   setopt  h (CurlHeaderFunction (gatherOutput_ gatherHeader))
   rc      <- perform h
   rspCode <- getResponseCode h
   (st,hs) <- finalHeader
   bs      <- finalBody
   return CurlResponse
       { respCurlCode   = rc
       , respStatus     = rspCode
       , respStatusLine = st
       , respHeaders    = hs
       , respBody       = bs 
       -- note: we're holding onto the handle here..
       -- note: with this interface this is not neccessary.
       , respGetInfo    = getInfo h
       }

-- | Performs a curl request using an exisitng curl handle.
-- The provided URL will overwride any 'CurlURL' options that
-- are provided in the list of options.  See also: 'perform_with_response'.
do_curl :: Curl -> URLString -> [CurlOption] -> IO CurlResponse
do_curl h url opts = do_curl_ h url opts

{-# DEPRECATED do_curl "Consider switching to do_curl_" #-}

do_curl_ :: (CurlHeader headerTy, CurlBuffer bodyTy)
         => Curl
	 -> URLString
	 -> [CurlOption]
	 -> IO (CurlResponse_ headerTy bodyTy)
do_curl_ h url opts = do
   setDefaultSSLOpts h url
   setopts h opts
   setopt h (CurlURL url)
   perform_with_response_ h


-- | Get the headers associated with a particular URL.
-- Returns the status line and the key-value pairs for the headers.
curlHead :: URLString -> [CurlOption] -> IO (String,[(String,String)])
curlHead url opts = initialize >>= \ h -> 
  do ref <- newIORef []
--     setopt h (CurlVerbose True)
     setopt h (CurlURL url)
     setopt h (CurlNoBody True)
     mapM_ (setopt h) opts
     setopt h (CurlHeaderFunction (gatherOutput ref))
     perform h
     lss <- readIORef ref
     return (parseStatusNHeaders (concRev [] lss))

-- | Get the headers associated with a particular URL.
-- Returns the status line and the key-value pairs for the headers.
curlHead_ :: (CurlHeader headers)
          => URLString
	  -> [CurlOption]
	  -> IO (String, headers)
curlHead_ url opts = initialize >>= \ h -> do
  (finalHeader, gatherHeader) <- newIncomingHeader
--  setopt h (CurlVerbose True)
  setopt h (CurlURL url)
  setopt h (CurlNoBody True)
  mapM_ (setopt h) opts
  setopt h (CurlHeaderFunction (gatherOutput_ gatherHeader))
  perform h
  finalHeader


-- utils

concRev :: [a] -> [[a]] -> [a]
concRev acc []     = acc
concRev acc (x:xs) = concRev (x++acc) xs

parseStatusNHeaders :: String -> (String, [(String,String)])
parseStatusNHeaders ys =
  case intoLines [] ys of
   a:as  -> (a,map parseHeader as)
   []    -> ("",[]) 
 where
  intoLines acc "" = addLine acc []
  intoLines acc ('\r':'\n':xs) = addLine acc (intoLines "" xs)
  intoLines acc (x:xs) = intoLines (x:acc) xs
  
  addLine "" ls = ls
  addLine  l ls = (reverse l) : ls
  
parseHeader :: String -> (String,String)
parseHeader xs = 
  case break (':' ==) xs of
   (as,_:bs) -> (as, bs)
   (as,_)    -> (as,"")

-- | 'curlMultiPost' perform a multi-part POST submission.
curlMultiPost :: URLString -> [CurlOption] -> [HttpPost] -> IO ()
curlMultiPost s os ps = initialize >>= \ h -> do
  setopt h (CurlVerbose True)
  setopt h (CurlURL s)
  setopt h (CurlHttpPost ps)
  mapM_ (setopt h) os
  perform h
  return ()


-- | 'curlPost' performs. a common POST operation, namely that
-- of submitting a sequence of name=value pairs.
curlPost :: URLString -> [String] -> IO ()
curlPost s ps = initialize >>= \ h -> do
  setopt h (CurlVerbose True)
  setopt h (CurlPostFields ps)
  setopt h (CurlCookieJar "cookies")
  setopt h (CurlURL s)
  perform h
  return ()

-- Use 'callbackWriter' instead.
{-# DEPRECATED #-}
easyWriter :: (String -> IO ()) -> WriteFunction
easyWriter = callbackWriter

-- | Imports data into the Haskell world and invokes the callback.
callbackWriter :: (String -> IO ()) -> WriteFunction
callbackWriter f pBuf sz szI _ = 
  do let bytes = sz * szI 
     f =<< peekCStringLen (pBuf,fromIntegral bytes)
     return bytes

-- | Imports data into the Haskell world and invokes the callback.
callbackWriter_ :: (CStringLen -> IO ()) -> WriteFunction
callbackWriter_ f pBuf sz szI _ = do
  do let bytes = sz * szI 
     f (pBuf,fromIntegral bytes)
     return bytes

-- | The output of Curl is ignored.  This function
-- does not marshall data into Haskell.
ignoreOutput :: WriteFunction
ignoreOutput _ x y _ = return (x*y)

-- | Add chunks of data to an IORef as they arrive.
gatherOutput :: IORef [String] -> WriteFunction
gatherOutput r = callbackWriter (\ v -> modifyIORef r (v:))

-- | Add chunks of data to an IORef as they arrive.
gatherOutput_ :: (CStringLen -> IO ()) -> WriteFunction
gatherOutput_ f = callbackWriter_ f

getResponseCode :: Curl -> IO Int
getResponseCode c = do
   iv <- getInfo c ResponseCode
   case iv of
     IString s -> 
       case (reads s) of
         ((v,_):_) -> return v
         _ -> fail ("Curl.getResponseCode: not a valid integer string " ++ s)
     IDouble d -> return (round d)
     ILong x   -> return (fromIntegral x)
     IList{}   -> fail ("Curl.getResponseCode: unexpected response code " ++ show iv)

