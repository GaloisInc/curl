{-# LANGUAGE ForeignFunctionInterface #-}
--------------------------------------------------------------------
-- |
-- Module    : Network.Curl.Info
-- Copyright : (c) 2007-2009, Galois Inc 
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability: portable
--
-- Accessing the properties of a curl handle's current state\/request.
--
--------------------------------------------------------------------
module Network.Curl.Info 
         ( Info(..)
         , InfoValue(..)
         , getInfo        -- :: Curl -> Info -> IO InfoValue
         ) where

import Network.Curl.Types
import Network.Curl.Code

import Control.Monad
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.C


data Info
 = EffectiveUrl
 | ResponseCode
 | TotalTime
 | NameLookupTime
 | ConnectTime
 | PreTransferTime
 | SizeUpload
 | SizeDownload
 | SpeedDownload
 | SpeedUpload
 | HeaderSize
 | RequestSize
 | SslVerifyResult
 | Filetime
 | ContentLengthDownload
 | ContentLengthUpload
 | StartTransferTime
 | ContentType
 | RedirectTime
 | RedirectCount
 | Private
 | HttpConnectCode
 | HttpAuthAvail
 | ProxyAuthAvail
 | OSErrno
 | NumConnects
 | SslEngines
 | CookieList
 | LastSocket
 | FtpEntryPath
   deriving (Show,Enum,Bounded)

data InfoValue
 = IString String
 | ILong   Long
 | IDouble Double
 | IList   [String]

instance Show InfoValue where
   show k = 
     case k of
       IString s -> s
       ILong l   -> show l
       IDouble d -> show d
       IList ss  -> show ss

{-
stringTag :: Long
stringTag = 0x100000  -- CURLINFO_STRING

longTag :: Long
longTag = 0x200000  -- CURLINFO_LONG

doubleTag :: Long
doubleTag = 0x300000  -- CURLINFO_DOUBLE

slistTag :: Long
slistTag = 0x400000  -- CURLINFO_SLIST
-}

{- unused, unexported
infoMask :: Long
infoMask = 0x0fffff  -- CURLINFO_MASK

infoTypeMask :: Long
infoTypeMask = 0xf00000  -- CURLINFO_TYPEMASK
-}

getInfo :: Curl -> Info -> IO InfoValue
getInfo h i = do
 case i of
   EffectiveUrl -> getInfoStr h (show i) 1
   ResponseCode -> getInfoLong h (show i) 2
   TotalTime    -> getInfoDouble h (show i) 3
   NameLookupTime -> getInfoDouble h (show i) 4
   ConnectTime -> getInfoDouble h (show i) 5
   PreTransferTime -> getInfoDouble h (show i) 6
   SizeUpload -> getInfoDouble h (show i) 7
   SizeDownload -> getInfoDouble h (show i) 8
   SpeedDownload -> getInfoDouble h (show i) 9
   SpeedUpload -> getInfoDouble h (show i) 10
   HeaderSize -> getInfoLong h (show i) 11
   RequestSize -> getInfoLong h (show i) 12
   SslVerifyResult -> getInfoLong h (show i) 13
   Filetime -> getInfoLong h (show i) 14
   ContentLengthDownload -> getInfoDouble h (show i) 15
   ContentLengthUpload   -> getInfoDouble h (show i) 16
   StartTransferTime -> getInfoDouble h (show i) 17
   ContentType -> getInfoStr h (show i) 18
   RedirectTime -> getInfoDouble h (show i) 19
   RedirectCount -> getInfoLong h (show i) 20
   Private -> getInfoStr h (show i) 21
   HttpConnectCode -> getInfoLong h (show i) 22
   HttpAuthAvail -> getInfoLong h (show i) 23
   ProxyAuthAvail -> getInfoLong h (show i) 24
   OSErrno -> getInfoLong h (show i) 25
   NumConnects -> getInfoLong h (show i) 26
   SslEngines -> getInfoSList h (show i) 27
   CookieList -> getInfoSList h (show i) 28
   LastSocket -> getInfoLong h (show i) 29
   FtpEntryPath -> getInfoStr h (show i) 30

getInfoStr :: Curl -> String -> Long -> IO InfoValue
getInfoStr h loc tg =
     alloca $ \ ps -> do
        rc <- curlPrim h $ \_ p -> easy_getinfo_str p tg ps
        case rc of
          0 -> do
             s <- peek ps
             if s == nullPtr
              then return (IString "")
              else liftM IString $ peekCString s
          _ -> fail ("getInfo{"++loc ++ "}: " ++ show (toCode rc))

getInfoLong :: Curl -> String -> Long -> IO InfoValue
getInfoLong h loc tg =
     alloca $ \ pl -> do
        rc <- curlPrim h $ \_ p -> easy_getinfo_long p tg pl
        case rc of
          0 -> do
             l <- peek pl
             return (ILong l)
          _ -> fail ("getInfo{"++loc ++ "}: " ++ show (toCode rc))

getInfoDouble :: Curl -> String -> Long -> IO InfoValue
getInfoDouble h loc tg =
     alloca $ \ pd -> do
        rc <- curlPrim h $ \_ p -> easy_getinfo_double p tg pd
        case rc of
          0 -> do
             d <- peek pd
             return (IDouble d)
          _ -> fail ("getInfo{"++loc ++ "}: " ++ show (toCode rc))

getInfoSList :: Curl -> String -> Long -> IO InfoValue
getInfoSList h loc tg =
     alloca $ \ ps -> do
        rc <- curlPrim h $ \_ p -> easy_getinfo_slist p tg ps
        case rc of
          0 -> do
             p <- peek ps
             ls <- unmarshallList p
             return (IList ls)
          _ -> fail ("getInfo{"++loc ++ "}: " ++ show (toCode rc))
 where
   unmarshallList ptr 
     | ptr == nullPtr = return []
     | otherwise = do
         ps <- peekByteOff ptr 0
         s  <- if ps == nullPtr then return "" else peekCString ps
         nx <- peekByteOff ptr (sizeOf nullPtr)
         ls <- unmarshallList nx
         return (s:ls)

-- FFI decls
foreign import ccall
  "curl_easy_getinfo_long" easy_getinfo_long :: CurlH -> Long -> Ptr Long -> IO CInt

foreign import ccall
  "curl_easy_getinfo_string" easy_getinfo_str  :: CurlH -> Long -> Ptr CString -> IO CInt

foreign import ccall
  "curl_easy_getinfo_double" easy_getinfo_double :: CurlH -> Long -> Ptr Double -> IO CInt

foreign import ccall
  "curl_easy_getinfo_slist" easy_getinfo_slist :: CurlH -> Long -> Ptr (Ptr (Ptr CChar)) -> IO CInt
