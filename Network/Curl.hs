--------------------------------------------------------------------
-- |
-- Module    : Curl
-- Copyright : (c) Galois Inc 2007-8
-- License   : BSD3
--
-- Maintainer: emertens@galois.com
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

       , CurlResponse(..)

          -- get resources and assoc. metadata.
       , curlGet             -- :: URLString -> [CurlOption] -> IO ()
       , curlGetString       -- :: URLString -> [CurlOption] -> IO (CurlCode, String)
       , curlGetResponse     -- :: URLString -> [CurlOption] -> IO CurlResponse
       , perform_with_response -- :: Curl -> IO CurlResponse
       , do_curl

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

       , method_GET          -- :: [CurlOption]
       , method_HEAD         -- :: [CurlOption]
       , method_POST         -- :: [CurlOption]

       , parseStatusNHeaders, concRev
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
import System.IO

{- pass along the action you want to perform during its lifetime.
{-# OBSOLETE #-}
withCurl :: (Curl -> IO a) -> IO a
withCurl act = act =<< initialize
-}

-- | Should be used once to wrap all uses of libcurl.
-- WARNING: the argument should not return before it
-- is completely done with curl (e.g., no forking or lazy returns)
withCurlDo :: IO a -> IO a
withCurlDo m  = do curl_global_init 3   -- initialize everything
                   a <- m
                   curl_global_cleanup
                   return a

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

-- | 'CurlResponse' is a record type encoding all the information
-- embodied in a response to your Curl request. Currently only used
-- to gather up the results of doing a GET in 'curlGetResponse'.
data CurlResponse
 = CurlResponse
     { respCurlCode   :: CurlCode
     , respStatus     :: Int
     , respStatusLine :: String
     , respHeaders    :: [(String,String)]
     , respBody       :: String
     , respGetInfo    :: (Info -> IO InfoValue)
     }


-- | 'curlGetResponse' performs a GET, returning all the info
-- it can lay its hands on in the response, a value of type 'CurlResponse'.
curlGetResponse :: URLString
                -> [CurlOption]
                -> IO CurlResponse
curlGetResponse url opts = do
  h <- initialize
  body_ref <- newIORef []
  hdr_ref  <- newIORef []
   -- Note: later options may (and should, probably) override these defaults.
  setopt  h (CurlFailOnError True)
  setDefaultSSLOpts h url
  setopt  h (CurlURL url)
  setopt  h (CurlWriteFunction (gatherOutput body_ref))
  setopt  h (CurlHeaderFunction (gatherOutput hdr_ref))
  mapM_ (setopt h) opts
  -- note that users cannot over-write the body and header handler
  -- which makes sense because otherwise we will return a bogus reposnse.
  perform_with_response h



-- | Perform the actions already specified on the handle.
-- Collects useful information about the returned message.
-- Note that this function sets the
-- 'CurlWriteFunction' and 'CurlHeaderFunction' options.
perform_with_response :: Curl -> IO CurlResponse
perform_with_response h =
  do body_ref <- newIORef []
     hdr_ref <- newIORef []

     -- Insted of allocating a swparate handler for each
     -- request we could just set this options one and forall
     -- and just clear the IORefs.

     setopt  h (CurlWriteFunction (gatherOutput body_ref))
     setopt  h (CurlHeaderFunction (gatherOutput hdr_ref))
     rc       <- perform h
     bss      <- readIORef body_ref
     hss      <- readIORef hdr_ref
     rspCode  <- getResponseCode h
     let (st,hs) = parseStatusNHeaders (concRev [] hss)
     return CurlResponse
       { respCurlCode   = rc
       , respStatus     = rspCode
       , respStatusLine = st
       , respHeaders    = hs
       , respBody       = concRev [] bss
       -- note: we're holding onto the handle here..
       -- note: with this interface this is not neccessary.
       , respGetInfo    = getInfo h
       }

-- | Performs a curl request using an exisitng curl handle.
-- The provided URL will overwride any 'CurlURL' options that
-- are provided in the list of options.  See also: 'perform_with_response'.
do_curl :: Curl -> URLString -> [CurlOption] -> IO CurlResponse
do_curl h url opts =
  do setDefaultSSLOpts h url
     setopts h opts
     setopt h (CurlURL url)
     perform_with_response h


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
{-# OBSOLETE #-}
easyWriter :: (String -> IO ()) -> WriteFunction
easyWriter = callbackWriter

-- | Imports data into the Haskell world and invokes the callback.
callbackWriter :: (String -> IO ()) -> WriteFunction
callbackWriter f pBuf sz szI _ = 
  do let bytes = sz * szI 
     f =<< peekCStringLen (pBuf,fromIntegral bytes)
     return bytes

-- | The output of Curl is ignored.  This function
-- does not marshall data into Haskell.
ignoreOutput :: WriteFunction
ignoreOutput _ x y _ = return (x*y)

-- | Add chunks of data to an IORef as they arrive.
gatherOutput :: IORef [String] -> WriteFunction
gatherOutput r = callbackWriter $ \xs -> do xss <- readIORef r
                                            writeIORef r (xs:xss)

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

