--------------------------------------------------------------------
-- |
-- Module    : Network.Curl.Opts
-- Copyright : (c) Galois Inc 2007-2009
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability: portable
--
-- This module contains the various options that specify what happens
-- when we use @perform@ on a @Curl@ handle.
--------------------------------------------------------------------
module Network.Curl.Opts where

import Network.Curl.Types
import Network.Curl.Post
import Data.List

import Foreign.Ptr
import Foreign.C.Types
import Data.Bits

data CurlOption
 = CurlFileObj (Ptr ())  -- ^ external pointer to pass to as 'WriteFunction's last argument.
 | CurlURL  URLString    -- ^ the URL to use for next request; can be the full URL or just the authority\/hostname.
 | CurlPort  Long        -- ^ what port to use.
 | CurlProxy String      -- ^ name of proxy
 | CurlUserPwd String    -- ^ the "user:pass" string to use
 | CurlProxyUserPwd String -- ^ same thing, but for the proxy.
 | CurlRange String      -- ^ byte range to fetch
 | CurlInFile FilePath   -- ^ external pointer to pass to as 'WriteFunction's last argument.
 | CurlErrorBuffer (Ptr CChar) -- ^ buffer for curl to deposit error messages (must at least CURL_ERROR_SIZE bytes long). Uses standard error if not specified.
 | CurlWriteFunction WriteFunction -- ^ callback to handle incoming data.
 | CurlReadFunction  ReadFunction  -- ^ callback for supplying outgoing\/uploaded data.
 | CurlTimeout Long{-secs-}        -- ^ number of seconds before timing out curl operation\/request.
 | CurlInFileSize Long{-bytes-}    -- ^ expected size of uploaded data.
 | CurlPostFields [String]         -- ^ (Multipart) POST data.
 | CurlReferer String              -- ^ Set the Referer: header to the given string.
 | CurlFtpPort String              -- ^ The string to feed to the FTP PORT command.
 | CurlUserAgent String            -- ^ Set the User-Agent: header to the given string.
 | CurlLowSpeed  Long              -- ^ If the bytes per sec drops below the given value, the operation is aborted.
 | CurlLowSpeedTime Long           -- ^ Upper bound for request to complete.
 | CurlResumeFrom Long             -- ^ Byte offset at which the transfer (HTTP or FTP) should start from.
 | CurlCookie String               -- ^ Set the Cookie: header to the given cookie (name=value pairs, semicolon-separated) string.
 | CurlHttpHeaders [String]        -- ^ Embellish the outgoing request with the given list of (formatted) header values.
 | CurlHttpPost  [HttpPost]        -- ^ (Multipart) POST data.
 | CurlSSLCert FilePath            -- ^ file holding your private SSL certificates (default format is PEM).
 | CurlSSLPassword String          -- ^ password to the above file.
 | CurlSSLKeyPassword String       -- ^ an alias for the previous.
 | CurlCRLF Bool                   -- ^ If true, convert Unix newlines into CRLFs when transferring.
 | CurlQuote [String]              -- ^ Sequence of FTP commands to execute prior to the main request.
 | CurlWriteHeader (Ptr ())        -- ^ State \/ pointer argument to pass to WriteFunction callback.
 | CurlCookieFile FilePath         -- ^ Path to file holding initial cookie data; also enables cookie handling.
 | CurlSSLVersion Long             -- ^ What protocol to attempt using (0:default;1:TLS;2:SSLv2;3:SSLv3)
 | CurlTimeCondition TimeCond      -- ^ How to interpret a conditional time value.
 | CurlTimeValue Long              -- ^ Number of secs since Jan 1, 1970. Interpretation is determined by CurlTimeCondition.
 | CurlCustomRequest String        -- ^ String holding alternative request command (WebDAV anyone?)
 {- | CurlStderr String {- XXX: should be FILE* ? -}               -- ^ File object to use for outputting debug info to. -}
 | CurlPostQuote [String]          -- ^ List of commands to issue to FTP server after the main request.
 | CurlWriteInfo String            -- ^ Not sure what this one does; something about passing it to the output function.
 | CurlVerbose Bool                -- ^ Control verbosity
 | CurlHeader Bool                 -- ^ Display outgoing and incoming headers 
 | CurlNoProgress Bool             -- ^ Control progress meter
 | CurlNoBody Bool                 -- ^ Use HEAD instead of GET
 | CurlFailOnError Bool            -- ^ If status response is >= 300, return an error (and no other output).
 | CurlUpload Bool                 -- ^ Control the main dataflow, i.e., True to perform uploads.
 | CurlPost Bool                   -- ^ Issue a POST request.
 | CurlFtpListOnly Bool            -- ^ Switch NLST for FTP directory listings
 | CurlFtpAppend Bool              -- ^ Control if FTP uploads append rather than overwrite files 
 | CurlUseNetRc NetRcOption        -- ^ control how or if a user's.netrc will be consulted for user:password
 | CurlFollowLocation Bool         -- ^ Handle auto-redirects by chasing down Location: values in responses.
 | CurlTransferTextASCII Bool      -- ^ Turn on ASCII transfers for FTP transfers; default is binary (i.e. off).
 | CurlPut Bool                    -- ^ Use PUT to upload data.
 | CurlProgressFunction ProgressFunction  -- ^ callback for showing progress
 | CurlProgressData (Ptr ())       -- ^ state argumentto pass to progress callback.
 | CurlAutoReferer Bool            -- ^ Control if the Referer: field is set upon following Location: redirects
 | CurlProxyPort Long              -- ^ (Numeric) proxy port to use.
 | CurlPostFieldSize Long          -- ^ Size of the POSTed data.
 | CurlHttpProxyTunnel Bool        -- ^ tunnel all HTTP operations through the proxy.
 | CurlInterface String            -- ^ Interface name of outgoing network interface ( network interface, IP address, host name.)
 | CurlKrb4Level String            -- ^ Kerberos security level ("clear", "safe", "confidential", "private" are good values, seemingly.)
 | CurlSSLVerifyPeer Bool          -- ^ Enable the authentication of peer certificate. Default is True.
 | CurlCAInfo FilePath             -- ^ If verifying peer's certificate, use certificates in this file to do so.
 | CurlMaxRedirs Long              -- ^ Maximum number of Location: redirects to chase down before giving up.
 | CurlFiletime Bool               -- ^ Try to determine the modification date of remote document; can be queried for.
 | CurlTelnetOptions [String]      -- ^ List of commands to use for initial telnet negotiations.
 | CurlMaxConnects Long            -- ^ Maximum number of cached active connections.
 | CurlClosePolicy Long            -- ^ No effect (obsolete.)
 | CurlFreshConnect Bool           -- ^ Force the opening up a new connection rather than try to reuse active connections. Default is not to.
 | CurlForbidReuse Bool            -- ^ Do not reuse the connection of next transfer when done.
 | CurlRandomFile FilePath         -- ^ Path to file used to seed (Open)SSL PRNG.
 | CurlEgdSocket FilePath          -- ^ Path to domain socket of EG Daemon.
 | CurlConnectTimeout Long         -- ^ max number of seconds to wait for the initial connection to happen.
 | CurlHeaderFunction WriteFunction -- ^ callback used to handle _incoming_ header data.
 | CurlHttpGet Bool                -- ^ Revert to a GET for the next request.
 | CurlSSLVerifyHost Long          -- ^ Perform Common name checking in peer certificate (1=> existence;2=> matches hostname.)
 | CurlCookieJar FilePath          -- ^ Path to file where additional cookie information will be stored.
 | CurlSSLCipherList String        -- ^ Colon-separated string list of cipher preferences to use for upcoming connection (e.g., "3DES:+RSA")
 | CurlHttpVersion HttpVersion     -- ^ What HTTP version to use, should you want to drop back for some reason.
 | CurlFtpUseEPSV Bool             -- ^ Attempt the use of EPSV before PASV for passive FTP downloads.
 | CurlSSLCertType String          -- ^ The format of your certificates ("PEM", "DER")
 | CurlSSLKey FilePath             -- ^ Filename of private key.
 | CurlSSLKeyType String           -- ^ Format of private key; use "ENG" to load from a crypto engine.
 | CurlSSLEngine String            -- ^ Name of crypto engine to use.
 | CurlSSLEngineDefault            -- ^ Make crypto engine the default for crypto operations.
 | CurlDNSUseGlobalCache Bool      -- ^ Have library uses its MT-unfriendly DNS global cache.
 | CurlDNSCacheTimeout Long        -- ^ Number of seconds to cache results of DNS lookups in memory.
 | CurlPreQuote [String]           -- ^ FTP commands to issue after connection and transfer mode has been set.
 | CurlDebugFunction DebugFunction -- ^ callback to catch and report transfer operations.
 | CurlDebugData (Ptr ())          -- ^ state argument to pass to debug callback.
 | CurlCookieSession Bool          -- ^ Signal the start of a cookie session, ignoring previous session cookies.
 | CurlCAPath FilePath             -- ^ Directory holding CA certificates; used when verifying peer certificate.
 | CurlBufferSize Long             -- ^ Turn (down, presumably) the buffers the received data is chunked up into (and reported to the WriteFunction.) A hint, library is free to ignore.
 | CurlNoSignal Bool               -- ^ Turn off use of signals internally.
 | CurlShare (Ptr ())              -- ^ Share handles are used for sharing data among concurrent Curl objects.
 | CurlProxyType Long              -- ^ What type of proxy to use.
 | CurlEncoding String             -- ^ What to report in the Accept-Encoding: header
 | CurlPrivate (Ptr ())            -- ^ Data associated with a Curl handle.
 | CurlHttp200Aliases String       -- ^ Alternatives to standard 200 OK response strings; whatever it takes, I suppose.
 | CurlUnrestrictedAuth Bool       -- ^ Pass on user:pass when following redirects.
 | CurlFtppUseEPRT Bool            -- ^ For active FTP downloads, try using EPRT command over LPRT.
 | CurlHttpAuth [HttpAuth]         -- ^ State your authentication preferences.
 | CurlSSLCtxFunction SSLCtxtFunction -- ^ callback to handle setting up SSL connections; have the power to abort them.
 | CurlSSLCtxData (Ptr ())         -- ^ state argument to pass into the above callback.
 | CurlFtpCreateMissingDirs Bool   -- ^ Have remote directories be created if not already there
 | CurlProxyAuth [HttpAuth]        -- ^ What preferred authentication schemes to use wrt. proxy.
 | CurlFtpResponseTimeout Long     -- ^ max number of seconds to wait for remote server to ACK commands.
 | CurlIPResolve Long              -- ^ Whether to resolve wrt IPv4 or IPv6.
 | CurlMaxFileSize Long            -- ^ Limit the number of bytes you're willing to download.
 | CurlInFileSizeLarge LLong       -- ^ Wider alternative of option giving upper bound of uploaded content (-1 => unknown.)
 | CurlResumeFromLarge LLong       -- ^ Wider alternative for specifying initial transfer offset.
 | CurlMaxFileSizeLarge LLong      -- ^ Wider alternative for specifying max download size.
 | CurlNetrcFile FilePath          -- ^ Path to user\'s .netrc
 | CurlFtpSSL Long                 -- ^ Try enabling the use of SSL for FTP control connections and\/or transfers.
 | CurlPostFieldSizeLarge LLong    -- ^ Size of data to POST; if unspecified (or -1), curl uses strlen().
 | CurlTCPNoDelay Bool             -- ^ Turn on or off the TCP\/IP NODELAY option.
 | CurlFtpSSLAuth Long             -- ^ Twiddle if TLS or SSL is used.
 | CurlIOCTLFunction (Ptr ())      -- ^ somewhat obscure callback for handling read stream resets.
 | CurlIOCTLData (Ptr ())          -- ^ state argument to the above.
 | CurlFtpAccount String           -- ^ The string to use when server asks for account info.
 | CurlCookieList String           -- ^ Cookie string to pass cookie engine; "ALL" scrubs all cookie info; "SESS" scrubs session ones.
 | CurlIgnoreContentLength Bool    -- ^ If Content-Length: values are troublesome (wrong, perhaps?), use this option to ignore using them as guidance.
 | CurlFtpSkipPASVIP Bool          -- ^ Ignore IP address in 227 responses.
 | CurlFtpFileMethod Long          -- ^ How to navigate to a file on the remote server (single, multiple CWDs).
 | CurlLocalPort Port              -- ^ What local port to use for established connection.
 | CurlLocalPortRange Port         -- ^ Number of attempts at finding local ports (using LocalPort as initial base.)
 | CurlConnectOnly Bool            -- ^ If enabled, perform all steps up until actual transfer.
     -- next three for completeness.
 | CurlConvFromNetworkFunction (Ptr ()) -- ^ callback for doing character translations from network format.
 | CurlConvToNetworkFunction (Ptr ())   -- ^ callback for doing character translations to network format.
 | CurlConvFromUtf8Function (Ptr ())    -- ^ callback for translating UTF8 into host encoding.
 | CurlMaxSendSpeedLarge LLong          -- ^ Specifies throttle value for outgoing data.
 | CurlMaxRecvSpeedLarge LLong          -- ^ Specifies throttle for incoming data.
 | CurlFtpAlternativeToUser String      -- ^ Alternative (to user:pass) for FTP authentication; weird.
 | CurlSockOptFunction (Ptr ())         -- ^ callback that's injected between socket creation and connection.
 | CurlSockOptData (Ptr ())             -- ^ state argument to the above.
 | CurlSSLSessionIdCache Bool           -- ^ Enable the SSL session id cache; default is on, so use this to disable.
 | CurlSSHAuthTypes [SSHAuthType]       -- ^ SSH authentication methods to use.
 | CurlSSHPublicKeyFile FilePath        -- ^ Path to file holding user's SSH public key.
 | CurlSSHPrivateKeyFile FilePath       -- ^ Path to file holding user's SSH private key.
 | CurlFtpSSLCCC Bool                   -- ^ Send CCC command after FTP connection has been authenticated.
 | CurlTimeoutMS Long                   -- ^ Max number of milliseconds that a transfer may take.
 | CurlConnectTimeoutMS Long            -- ^ Max number of milliseconds that a connection attempt may take to complete.
 | CurlHttpTransferDecoding Bool        -- ^ Disable transfer decoding; if disabled, curl will turn off chunking.
 | CurlHttpContentDecoding  Bool        -- ^ Disable content decoding, getting the raw bits.
   -- sync'ed wrt 7.19.2
 | CurlNewFilePerms Long
 | CurlNewDirectoryPerms Long
 | CurlPostRedirect Bool
   -- no support for open socket callbacks/function overrides.
 | CurlSSHHostPublicKeyMD5 String
 | CurlCopyPostFields Bool
 | CurlProxyTransferMode Long
   -- no support for seeking in the input stream.
 | CurlCRLFile       FilePath
 | CurlIssuerCert    FilePath
 | CurlAddressScope  Long
 | CurlCertInfo      Long
 | CurlUserName      String
 | CurlUserPassword  String
 | CurlProxyUser     String
 | CurlProxyPassword String
  

instance Show CurlOption where
  show x = showCurlOption x

data HttpVersion
 = HttpVersionNone
 | HttpVersion10
 | HttpVersion11
   deriving ( Enum,Show )

data TimeCond
 = TimeCondNone
 | TimeCondIfModSince
 | TimeCondIfUnmodSince
 | TimeCondLastMode
   deriving ( Enum, Show )
 
data NetRcOption
 = NetRcIgnored
 | NetRcOptional
 | NetRcRequired
   deriving ( Enum, Show )

data HttpAuth
 = HttpAuthNone
 | HttpAuthBasic
 | HttpAuthDigest
 | HttpAuthGSSNegotiate
 | HttpAuthNTLM
 | HttpAuthAny
 | HttpAuthAnySafe
   deriving ( Enum, Show )

toHttpAuthMask :: [HttpAuth] -> Long
toHttpAuthMask [] = 0
toHttpAuthMask (x:xs) = 
  let vs = toHttpAuthMask xs in
  case x of 
    HttpAuthNone  -> vs
    HttpAuthBasic -> 0x1 .|. vs
    HttpAuthDigest -> 0x2 .|. vs
    HttpAuthGSSNegotiate -> 0x4 .|. vs
    HttpAuthNTLM -> 0x8 .|. vs
    HttpAuthAny -> (complement 0) .|. vs
    HttpAuthAnySafe -> (complement 1) .|. vs


data SSHAuthType
 = SSHAuthAny
 | SSHAuthNone
 | SSHAuthPublickey
 | SSHAuthPassword
 | SSHAuthHost
 | SSHAuthKeyboard
   deriving ( Show )


toSSHAuthMask :: [SSHAuthType] -> Long
toSSHAuthMask [] = 0
toSSHAuthMask (x:xs) = 
  let vs = toSSHAuthMask xs in 
  case x of
    SSHAuthAny -> (complement 0) .|. vs
    SSHAuthNone -> vs
    SSHAuthPublickey -> 1 .|. vs
    SSHAuthPassword -> 2 .|. vs
    SSHAuthHost -> 4 .|. vs
    SSHAuthKeyboard -> 8 .|. vs


type WriteFunction
  = Ptr CChar  --  pointer to external buffer holding data
 -> CInt       --  width (in bytes) of each item
 -> CInt       --  number of items
 -> Ptr ()     --  state argument (file pointer etc.)
 -> IO CInt    --  number of bytes written.

type ReadFunction
  = Ptr CChar  --  pointer to external buffer to fill in.
 -> CInt       --  width (in bytes) of each item
 -> CInt       --  number of items
 -> Ptr ()     --  state argument (file pointer etc.)
 -> IO (Maybe CInt) --  how many bytes was copied into buffer; Nothing => abort.
 
type ReadFunctionPrim
  = Ptr CChar
 -> CInt
 -> CInt
 -> Ptr ()
 -> IO CInt
 

type ProgressFunction
  = Ptr ()  --  state argument
 -> Double  --  expected download totals
 -> Double  --  download totals so far
 -> Double  --  expected upload totals
 -> Double  --  upload totals so far
 -> IO CInt --  not sure; 0 is a good one.

type DebugFunction
  = Curl       --  connection handle
 -> DebugInfo  --  type of call
 -> Ptr CChar  --  data buffer
 -> CInt       --  length of buffer
 -> Ptr ()     --  state argument
 -> IO ()      --  always 0

data DebugInfo
 = InfoText
 | InfoHeaderIn
 | InfoHeaderOut
 | InfoDataIn
 | InfoDataOut
 | InfoSslDataIn
 | InfoSslDataOut
   deriving ( Eq, Enum )

type DebugFunctionPrim
  = CurlH      --  connection handle
 -> CInt       --  type of call
 -> Ptr CChar  --  data buffer
 -> CInt       --  length of buffer
 -> Ptr ()     --  state argument
 -> IO CInt    --  always 0



type SSLCtxtFunction
  = CurlH   --  connection handle
 -> Ptr ()  --  the SSL_CTX handle
 -> Ptr ()  --  state argument
 -> IO CInt

curl_readfunc_abort :: CInt
curl_readfunc_abort = 0x10000000

baseLong :: Int
baseLong = 0

baseObject :: Int
baseObject = 10000

baseFunction :: Int
baseFunction = 20000

baseOffT :: Int
baseOffT = 30000

unmarshallOption :: Unmarshaller a -> CurlOption -> IO a
unmarshallOption um c =
 let 
  l   = (baseLong+)
  o   = (baseObject+)
  f   = (baseFunction+)
  off = (baseOffT+)
 in
 case c of
  CurlFileObj x -> u_ptr um (o 1) x
  CurlURL x   -> u_string um (o 2) x
  CurlPort x  -> u_long   um (l 3) x
  CurlProxy x -> u_string um (o 4) x
  CurlUserPwd x -> u_string um (o 5) x
  CurlProxyUserPwd x -> u_string um (o 6) x
  CurlRange x   -> u_string um (o 7) x
  CurlInFile x  -> u_string um (o 9) x
  CurlErrorBuffer x   -> u_cptr um (o 10) x
  CurlWriteFunction x -> u_writeFun um (f 11) x
  CurlReadFunction x  -> u_readFun  um (f 12) x
  CurlTimeout x ->  u_long um (l 13) x
  CurlInFileSize x -> u_long um (l 14) x
  CurlPostFields x -> u_string um (o 15) (concat $ intersperse "&" x)
  CurlReferer x ->  u_string um (o 16) x
  CurlFtpPort x ->  u_string um (o 17) x
  CurlUserAgent x -> u_string um (o 18) x
  CurlLowSpeed x -> u_long um (l 19) x
  CurlLowSpeedTime x -> u_long um (l 20) x
  CurlResumeFrom x -> u_long um (l 21) x
  CurlCookie x -> u_string um (o 22) x
  CurlHttpHeaders x -> u_strings um (o 23) x
  CurlHttpPost x -> u_posts um (o 24) x
  CurlSSLCert x -> u_string um (o 25) x
  CurlSSLPassword x -> u_string um (o 26) x
  CurlSSLKeyPassword x -> u_string um (o 26) x -- yes, duplicate.
  CurlCRLF x -> u_bool um (l 27) x
  CurlQuote x -> u_strings um (o 28) x
  CurlWriteHeader x -> u_ptr um (o 29) x
  CurlCookieFile x -> u_string um (o 31) x
  CurlSSLVersion x -> u_long um (l 32) x
  CurlTimeCondition x -> u_enum um (l 33) x
  CurlTimeValue x -> u_long um (l 34) x
  CurlCustomRequest x -> u_string um (o 36) x
  -- CurlStderr x -> u_string um (o 37) x
  CurlPostQuote x -> u_strings um (o 39) x
  CurlWriteInfo x -> u_string um (o 40) x
  CurlVerbose x -> u_bool um (l 41) x
  CurlHeader x -> u_bool um (l 42) x
  CurlNoProgress x -> u_bool um (l 43) x
  CurlNoBody x -> u_bool um (l 44) x
  CurlFailOnError x -> u_bool um (l 45) x
  CurlUpload x -> u_bool um (l 46) x
  CurlPost x -> u_bool um (l 47) x
  CurlFtpListOnly x -> u_bool um (l 48) x
  CurlFtpAppend x -> u_bool um (l 50) x
  CurlUseNetRc x -> u_enum um (l 51) x
  CurlFollowLocation x -> u_bool um (l 52) x
  CurlTransferTextASCII x -> u_bool um (l 53) x
  CurlPut x -> u_bool um (l 54) x
  CurlProgressFunction x -> u_progressFun um (f 56) x
  CurlProgressData x -> u_ptr um (o 57) x
  CurlAutoReferer x -> u_bool um (l 58) x
  CurlProxyPort x -> u_long um (l 59) x
  CurlPostFieldSize x -> u_long um (l 60) x
  CurlHttpProxyTunnel x -> u_bool um (l 61) x
  CurlInterface x -> u_string um (o 62) x
  CurlKrb4Level x -> u_string um (o 63) x
  CurlSSLVerifyPeer x -> u_bool um (l 64) x
  CurlCAInfo x -> u_string um (o 65) x
  CurlMaxRedirs x -> u_long um (l 68) x
  CurlFiletime x -> u_bool um (l 69) x
  CurlTelnetOptions x -> u_strings um (o 70) x
  CurlMaxConnects x -> u_long um (l 71) x
  CurlClosePolicy x -> u_long um (l 72) x
  CurlFreshConnect x -> u_bool um (l 74) x
  CurlForbidReuse x -> u_bool um (l 75) x
  CurlRandomFile x -> u_string um (o 76) x
  CurlEgdSocket x -> u_string um (o 77) x
  CurlConnectTimeout x -> u_long um (l 78) x
  CurlHeaderFunction x -> u_writeFun um (f 79) x
  CurlHttpGet x        -> u_bool um (l 80) x
  CurlSSLVerifyHost x  -> u_long um (l 81) x
  CurlCookieJar x -> u_string um (o 82) x
  CurlSSLCipherList x -> u_string um (o 83) x -- a string (or a l-list of them)?
  CurlHttpVersion x -> u_enum um (l 84) x
  CurlFtpUseEPSV x -> u_bool um (l 85) x
  CurlSSLCertType x -> u_string um (o 86) x
  CurlSSLKey x -> u_string um (o 87) x
  CurlSSLKeyType x -> u_string um (o 88) x
  CurlSSLEngine x -> u_string um (o 89) x
  CurlSSLEngineDefault -> u_bool um (l 90) True
  CurlDNSUseGlobalCache x -> u_bool um (l 91) x
  CurlDNSCacheTimeout x -> u_long um (l 92) x
  CurlPreQuote x -> u_strings um (o 93) x
  CurlDebugFunction x -> u_debugFun um (f 94) x
  CurlDebugData x -> u_ptr um (o 95) x
  CurlCookieSession x -> u_bool um (l 96) x
  CurlCAPath x -> u_string um (o 97) x
  CurlBufferSize x -> u_long um (l 98) x
  CurlNoSignal x -> u_bool um (l 99) x
  CurlShare x -> u_ptr um (o 100) x
  CurlProxyType x -> u_enum um (l 101) x
  CurlEncoding x -> u_string um (o 102) x
  CurlPrivate x -> u_ptr um (o 103) x
  CurlHttp200Aliases x -> u_string um (o 104) x -- correct?
  CurlUnrestrictedAuth x -> u_bool um (l 105) x
  CurlFtppUseEPRT x -> u_bool um (l 106) x
  CurlHttpAuth xs -> u_long um (l 107) (toHttpAuthMask xs)
  CurlSSLCtxFunction x -> u_sslctxt um (f 108) x
  CurlSSLCtxData x -> u_ptr um (o 109) x
  CurlFtpCreateMissingDirs x -> u_bool um (l 110) x
  CurlProxyAuth x -> u_long um (l 111) (toHttpAuthMask x)
  CurlFtpResponseTimeout x -> u_long um (l 112) x
  CurlIPResolve x -> u_long um (l 113) x
  CurlMaxFileSize x -> u_long um (l 114) x
  CurlInFileSizeLarge x -> u_llong um (off 115) x
  CurlResumeFromLarge x -> u_llong um (off 116) x
  CurlMaxFileSizeLarge x -> u_llong um (off 117) x
  CurlNetrcFile x -> u_string um (o 118) x
  CurlFtpSSL x -> u_enum um (l 119) x
  CurlPostFieldSizeLarge x -> u_llong um (off 120) x
  CurlTCPNoDelay x -> u_bool um (l 121) x
  CurlFtpSSLAuth x -> u_enum um (l 129) x
  CurlIOCTLFunction x -> u_ioctl_fun um (f 130) x
  CurlIOCTLData x -> u_ptr um (o 131) x
  CurlFtpAccount x -> u_string um (o 134) x
  CurlCookieList x -> u_string um (o 135) x
  CurlIgnoreContentLength x -> u_bool um (l 136) x
  CurlFtpSkipPASVIP x -> u_bool um (l 137) x
  CurlFtpFileMethod x -> u_enum um (l 138) x
  CurlLocalPort x -> u_long um (l 139) x
  CurlLocalPortRange x -> u_long um (l 140) x
  CurlConnectOnly x -> u_bool um (l 141) x
  CurlConvFromNetworkFunction x -> u_convFromNetwork um (f 142) x
  CurlConvToNetworkFunction x -> u_convToNetwork um (f 143) x
  CurlConvFromUtf8Function x -> u_convFromUtf8 um (f 144) x
  CurlMaxSendSpeedLarge x -> u_llong um (off 145) x
  CurlMaxRecvSpeedLarge x -> u_llong um (off 146) x
  CurlFtpAlternativeToUser x -> u_string um (o 147) x
  CurlSockOptFunction x -> u_sockoptFun um (f 148) x
  CurlSockOptData x -> u_ptr um (o 149) x
  CurlSSLSessionIdCache x -> u_bool um (l 150) x
  CurlSSHAuthTypes xs -> u_long um (l 151) (toSSHAuthMask xs)
  CurlSSHPublicKeyFile x -> u_string um (o 152) x
  CurlSSHPrivateKeyFile x -> u_string um (o 153) x
  CurlFtpSSLCCC x -> u_bool um (l 154) x
  CurlTimeoutMS x -> u_long um (l 155) x
  CurlConnectTimeoutMS x -> u_long um (l 156) x
  CurlHttpTransferDecoding x -> u_bool um (l 157) x
  CurlHttpContentDecoding x ->  u_bool um (l 158) x
  CurlNewFilePerms x        -> u_long um (l 159) x
  CurlNewDirectoryPerms x   -> u_long um (l 160) x
  CurlPostRedirect x        -> u_bool um (l 161) x
  CurlSSHHostPublicKeyMD5 x -> u_string um (l 162) x
  CurlCopyPostFields x      -> u_bool um (l 165) x
  CurlProxyTransferMode x   -> u_long um (l 166) x
  CurlCRLFile x             -> u_string um (l 169) x
  CurlIssuerCert x          -> u_string um (l 170) x
  CurlAddressScope x        -> u_long um   (l 171) x
  CurlCertInfo x            -> u_long um   (l 172) x
  CurlUserName x            -> u_string um (l 173) x
  CurlUserPassword x        -> u_string um (l 174) x
  CurlProxyUser x           -> u_string um (l 175) x
  CurlProxyPassword x       -> u_string um (l 176) x

data Unmarshaller a
 = Unmarshaller
     { u_long    :: Int -> Long     -> IO a
     , u_llong   :: Int -> LLong    -> IO a
     , u_string  :: Int -> String   -> IO a
     , u_strings :: Int -> [String] -> IO a
     , u_ptr     :: Int -> Ptr ()   -> IO a
     , u_writeFun :: Int -> WriteFunction -> IO a
     , u_readFun :: Int -> ReadFunction -> IO a
     , u_progressFun :: Int -> ProgressFunction -> IO a
     , u_debugFun :: Int -> DebugFunction -> IO a
     , u_posts    :: Int -> [HttpPost] -> IO a
     , u_sslctxt  :: Int -> SSLCtxtFunction -> IO a
     , u_ioctl_fun :: Int -> Ptr () -> IO a
     , u_convFromNetwork :: Int -> Ptr () -> IO a
     , u_convToNetwork :: Int -> Ptr () -> IO a
     , u_convFromUtf8 :: Int -> Ptr () -> IO a
     , u_sockoptFun  :: Int -> Ptr () -> IO a
     }

verboseUnmarshaller :: Unmarshaller a -> Unmarshaller a
verboseUnmarshaller u =
  let two m f x y = putStrLn m >> f u x y 
      twoS m f x y = putStrLn (m ++ ": " ++ show (x,y)) >> f u x y 
  in u 
    { u_long        = twoS "u_long" u_long
    , u_llong       = twoS "u_llong" u_llong
    , u_string      = twoS "u_string" u_string 
    , u_strings     = twoS "u_strings" u_strings
    , u_ptr         = twoS "u_ptr" u_ptr
    , u_writeFun    = two "u_writeFun" u_writeFun
    , u_readFun     = two "u_readFun" u_readFun
    , u_progressFun = two "u_progressFun" u_progressFun
    , u_debugFun    = two "u_debugFun" u_debugFun
    , u_posts       = two "u_posts" u_posts
    , u_sslctxt     = two "u_sslctxt" u_sslctxt
    , u_ioctl_fun        = two "u_ioctl_fun" u_ioctl_fun
    , u_convFromNetwork  = twoS "u_convFromNetwork" u_convFromNetwork
    , u_convToNetwork    = twoS "u_convToNetwork" u_convToNetwork
    , u_convFromUtf8     = twoS "u_convFromUtf8" u_convFromUtf8
    , u_sockoptFun       = twoS "u_sockoptFun" u_sockoptFun
    }


u_bool :: Unmarshaller a -> Int -> Bool -> IO a
u_bool um x b = u_long um x (if b then 1 else 0)

u_enum :: Enum b => Unmarshaller a -> Int -> b -> IO a
u_enum um x b = u_long um x (fromIntegral $ fromEnum b)

u_cptr :: Unmarshaller a -> Int -> Ptr CChar -> IO a
u_cptr um x p = u_ptr um x (castPtr p)

showCurlOption :: CurlOption -> String
showCurlOption o = 
  case o of
    CurlFileObj p  -> "CurlFileObj " ++ show p
    CurlURL u      -> "CurlURL " ++ show u
    CurlPort p     -> "CurlPort " ++ show p
    CurlProxy s    -> "CurlProxy " ++ show s
    CurlUserPwd p  -> "CurlUserPwd " ++ show p
    CurlProxyUserPwd p -> "CurlProxyUserPwd " ++ show p
    CurlRange p -> "CurlRange " ++ show p
    CurlInFile p -> "CurlInFile " ++ show p
    CurlErrorBuffer p -> "CurlErrorBuffer " ++ show p
    CurlWriteFunction{} -> "CurlWriteFunction <fun>"
    CurlReadFunction{}  -> "CurlReadFunction <fun>"
    CurlTimeout l       -> "CurlTimeout " ++ show l
    CurlInFileSize l    -> "CurlInFileSize " ++ show l
    CurlPostFields p    -> "CurlPostFields " ++ show p
    CurlReferer p       -> "CurlReferer " ++ show p
    CurlFtpPort p       -> "CurlFtpPort " ++ show p
    CurlUserAgent p     -> "CurlUserAgent " ++ show p
    CurlLowSpeed  p     -> "CurlLowSpeed " ++ show p
    CurlLowSpeedTime p  -> "CurlLowSpeedTime " ++ show p
    CurlResumeFrom p    -> "CurlResumeFrom " ++ show p
    CurlCookie p        -> "CurlCookie " ++ show p
    CurlHttpHeaders p   -> "CurlHttpHeaders " ++ show p
    CurlHttpPost p      -> "CurlHttpPost " ++ show p
    CurlSSLCert p       -> "CurlSSLCert " ++ show p
    CurlSSLPassword p   -> "CurlSSLPassword " ++ show p
    CurlSSLKeyPassword p -> "CurlSSLKeyPassword " ++ show p
    CurlCRLF p -> "CurlCRLF " ++ show p
    CurlQuote p -> "CurlQuote " ++ show p
    CurlWriteHeader p -> "CurlWriteHeader " ++ show p
    CurlCookieFile p -> "CurlCookieFile " ++ show p
    CurlSSLVersion p -> "CurlSSLVersion " ++ show p
    CurlTimeCondition p -> "CurlTimeCondition " ++ show p
    CurlTimeValue p -> "CurlTimeValue " ++ show p
    CurlCustomRequest p -> "CurlCustomRequest " ++ show p
    CurlPostQuote p -> "CurlPostQuote " ++ show p
    CurlWriteInfo p -> "CurlWriteInfo " ++ show p
    CurlVerbose p -> "CurlVerbose " ++ show p
    CurlHeader p -> "CurlHeader " ++ show p
    CurlNoProgress p -> "CurlNoProgress " ++ show p
    CurlNoBody p -> "CurlNoBody " ++ show p
    CurlFailOnError p -> "CurlFailOnError " ++ show p
    CurlUpload p -> "CurlUpload " ++ show p
    CurlPost p -> "CurlPost " ++ show p
    CurlFtpListOnly p -> "CurlFtpListOnly " ++ show p
    CurlFtpAppend p -> "CurlFtpAppend " ++ show p
    CurlUseNetRc p -> "CurlUseNetRc " ++ show p
    CurlFollowLocation p -> "CurlFollowLocation " ++ show p
    CurlTransferTextASCII p -> "CurlTransferTextASCII " ++ show p
    CurlPut p -> "CurlPut " ++ show p
    CurlProgressFunction{} -> "CurlProgressFunction <fun>"
    CurlProgressData p -> "CurlProgressData " ++ show p
    CurlAutoReferer p -> "CurlAutoReferer " ++ show p
    CurlProxyPort p -> "CurlProxyPort " ++ show p
    CurlPostFieldSize p -> "CurlPostFieldSize " ++ show p
    CurlHttpProxyTunnel p -> "CurlHttpProxyTunnel " ++ show p
    CurlInterface p -> "CurlInterface " ++ show p
    CurlKrb4Level p -> "CurlKrb4Level " ++ show p
    CurlSSLVerifyPeer p -> "CurlSSLVerifyPeer " ++ show p
    CurlCAInfo p -> "CurlCAInfo " ++ show p
    CurlMaxRedirs p -> "CurlMaxRedirs " ++ show p
    CurlFiletime p -> "CurlFiletime " ++ show p
    CurlTelnetOptions p -> "CurlTelnetOptions " ++ show p
    CurlMaxConnects p -> "CurlMaxConnects " ++ show p
    CurlClosePolicy p -> "CurlClosePolicy " ++ show p
    CurlFreshConnect p -> "CurlFreshConnect " ++ show p
    CurlForbidReuse p -> "CurlForbidReuse " ++ show p
    CurlRandomFile p -> "CurlRandomFile " ++ show p
    CurlEgdSocket p -> "CurlEgdSocket " ++ show p
    CurlConnectTimeout p -> "CurlConnectTimeout " ++ show p
    CurlHeaderFunction{} -> "CurlHeaderFunction <fun>"
    CurlHttpGet p -> "CurlHttpGet " ++ show p
    CurlSSLVerifyHost p -> "CurlSSLVerifyHost " ++ show p
    CurlCookieJar p -> "CurlCookieJar " ++ show p
    CurlSSLCipherList p -> "CurlSSLCipherList " ++ show p
    CurlHttpVersion p -> "CurlHttpVersion " ++ show p
    CurlFtpUseEPSV p -> "CurlFtpUseEPSV " ++ show p
    CurlSSLCertType p -> "CurlSSLCertType " ++ show p
    CurlSSLKey p -> "CurlSSLKey " ++ show p
    CurlSSLKeyType p -> "CurlSSLKeyType " ++ show p
    CurlSSLEngine p -> "CurlSSLEngine " ++ show p
    CurlSSLEngineDefault-> "CurlSSLEngineDefault"
    CurlDNSUseGlobalCache p -> "CurlDNSUseGlobalCache " ++ show p
    CurlDNSCacheTimeout p -> "CurlDNSCacheTimeout " ++ show p
    CurlPreQuote p -> "CurlPreQuote " ++ show p
    CurlDebugFunction{} -> "CurlDebugFunction <fun>"
    CurlDebugData p -> "CurlDebugData " ++ show p
    CurlCookieSession p -> "CurlCookieSession " ++ show p
    CurlCAPath p -> "CurlCAPath " ++ show p
    CurlBufferSize p -> "CurlBufferSize " ++ show p
    CurlNoSignal p -> "CurlNoSignal " ++ show p
    CurlShare p -> "CurlShare " ++ show p
    CurlProxyType p -> "CurlProxyType " ++ show p
    CurlEncoding p -> "CurlEncoding " ++ show p
    CurlPrivate p -> "CurlPrivate " ++ show p
    CurlHttp200Aliases p -> "CurlHttp200Aliases " ++ show p
    CurlUnrestrictedAuth p -> "CurlUnrestrictedAuth " ++ show p
    CurlFtppUseEPRT p -> "CurlFtppUseEPRT " ++ show p
    CurlHttpAuth p -> "CurlHttpAuth " ++ show p
    CurlSSLCtxFunction{} -> "CurlSSLCtxFunction <fun>"
    CurlSSLCtxData p -> "CurlSSLCtxData " ++ show p
    CurlFtpCreateMissingDirs p -> "CurlFtpCreateMissingDirs " ++ show p
    CurlProxyAuth p -> "CurlProxyAuth " ++ show p
    CurlFtpResponseTimeout p -> "CurlFtpResponseTimeout " ++ show p
    CurlIPResolve p -> "CurlIPResolve " ++ show p
    CurlMaxFileSize p -> "CurlMaxFileSize " ++ show p
    CurlInFileSizeLarge p -> "CurlInFileSizeLarge " ++ show p
    CurlResumeFromLarge p -> "CurlResumeFromLarge " ++ show p
    CurlMaxFileSizeLarge p -> "CurlMaxFileSizeLarge " ++ show p
    CurlNetrcFile p -> "CurlNetrcFile " ++ show p
    CurlFtpSSL p -> "CurlFtpSSL " ++ show p
    CurlPostFieldSizeLarge p -> "CurlPostFieldSizeLarge " ++ show p
    CurlTCPNoDelay p -> "CurlTCPNoDelay " ++ show p
    CurlFtpSSLAuth p -> "CurlFtpSSLAuth " ++ show p
    CurlIOCTLFunction p -> "CurlIOCTLFunction " ++ show p
    CurlIOCTLData p -> "CurlIOCTLData " ++ show p
    CurlFtpAccount p -> "CurlFtpAccount " ++ show p
    CurlCookieList p -> "CurlCookieList " ++ show p
    CurlIgnoreContentLength p -> "CurlIgnoreContentLength " ++ show p
    CurlFtpSkipPASVIP p -> "CurlFtpSkipPASVIP " ++ show p
    CurlFtpFileMethod p -> "CurlFtpFileMethod " ++ show p
    CurlLocalPort p -> "CurlLocalPort " ++ show p
    CurlLocalPortRange p -> "CurlLocalPortRange " ++ show p
    CurlConnectOnly p -> "CurlConnectOnly " ++ show p
    CurlConvFromNetworkFunction p -> "CurlConvFromNetworkFunction " ++ show p
    CurlConvToNetworkFunction p -> "CurlConvToNetworkFunction " ++ show p
    CurlConvFromUtf8Function p -> "CurlConvFromUtf8Function " ++ show p
    CurlMaxSendSpeedLarge p -> "CurlMaxSendSpeedLarge " ++ show p
    CurlMaxRecvSpeedLarge p -> "CurlMaxRecvSpeedLarge " ++ show p
    CurlFtpAlternativeToUser p -> "CurlFtpAlternativeToUser " ++ show p
    CurlSockOptFunction p -> "CurlSockOptFunction " ++ show p
    CurlSockOptData p -> "CurlSockOptData " ++ show p
    CurlSSLSessionIdCache p -> "CurlSSLSessionIdCache " ++ show p
    CurlSSHAuthTypes p -> "CurlSSHAuthTypes " ++ show p
    CurlSSHPublicKeyFile p -> "CurlSSHPublicKeyFile " ++ show p
    CurlSSHPrivateKeyFile p -> "CurlSSHPrivateKeyFile " ++ show p
    CurlFtpSSLCCC p -> "CurlFtpSSLCCC " ++ show p
    CurlTimeoutMS p -> "CurlTimeoutMS " ++ show p
    CurlConnectTimeoutMS p -> "CurlConnectTimeoutMS " ++ show p
    CurlHttpTransferDecoding p -> "CurlHttpTransferDecoding " ++ show p
    CurlHttpContentDecoding p -> "CurlHttpContentDecoding " ++ show p
    CurlNewFilePerms l -> "CurlNewFilePerms " ++ show l
    CurlNewDirectoryPerms p -> "CurlNewDirectoryPerms " ++ show p
    CurlPostRedirect p -> "CurlPostRedirect " ++ show p
    CurlSSHHostPublicKeyMD5 p -> "CurlSSHHostPublicKeyMD5 " ++ show p
    CurlCopyPostFields p -> "CurlCopyPostFields " ++ show p
    CurlProxyTransferMode p -> "CurlProxyTransferMode " ++ show p
    CurlCRLFile       p -> "CurlCRLFile " ++ show p
    CurlIssuerCert    p -> "CurlIssuerCert " ++ show p
    CurlAddressScope  p -> "CurlAddressScope " ++ show p
    CurlCertInfo      p -> "CurlCertInfo " ++ show p
    CurlUserName      p -> "CurlUserName " ++ show p
    CurlUserPassword  p -> "CurlUserPassword " ++ show p
    CurlProxyUser     p -> "CurlProxyUser " ++ show p
    CurlProxyPassword p -> "CurlProxyPassword " ++ show p
