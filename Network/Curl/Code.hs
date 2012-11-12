{-# LANGUAGE ForeignFunctionInterface #-}
--------------------------------------------------------------------
-- |
-- Module    : Network.Curl.Code
-- Copyright : (c) Galois Inc 2007-2009, 2011
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing Curl's status codes as a Haskell type.
--
--------------------------------------------------------------------

module Network.Curl.Code where

import Foreign.C.Types

data CurlCode
 = CurlOK
 | CurlUnspportedProtocol
 | CurlFailedInit
 | CurlUrlMalformat
 | CurlUrlMalformatUser
 | CurlCouldntResolveProxy
 | CurlCouldntResolveHost
 | CurlCouldntConnect
 | CurlFtpWeirdServerReply
 | CurlFtpAccessDenied
 | CurlFtpUserPasswordIncorrect
 | CurlFtpWeirdPassReply
 | CurlFtpWeirdUserReply
 | CurlFtpWeirdPASVReply
 | CurlFtpWeird227Format
 | CurlFtpCantGetHost
 | CurlFtpCantReconnect
 | CurlFtpCouldnSetBinary
 | CurlPartialFile
 | CurlFtpCouldntRetrFile
 | CurlFtpWriteError
 | CurlFtpQuoteError
 | CurlHttpReturnedError
 | CurlWriteError
 | CurlMalformatError
 | CurlFtpCouldnStorFile
 | CurlReadError
 | CurlOutOfMemory
 | CurlOperationTimeout
 | CurlFtpCouldntSetAscii
 | CurlFtpPortFailed
 | CurlFtpCouldntUseRest
 | CurlFtpCouldntGetSize
 | CurlHttpRangeError
 | CurlHttpPostError
 | CurlSSLConnectError
 | CurlBadDownloadResume
 | CurlFileCouldntReadFile
 | CurlLDAPCannotBind
 | CurlLDPAPSearchFailed
 | CurlLibraryNotFound
 | CurlFunctionNotFound
 | CurlAbortedByCallback
 | CurlBadFunctionArgument
 | CurlBadCallingOrder
 | CurlInterfaceFailed
 | CurlBadPasswordEntered
 | CurlTooManyRedirects
 | CurlUnknownTelnetOption
 | CurlTelnetOptionSyntax
 | CurlObsolete
 | CurlSSLPeerCertificate
 | CurlGotNothing
 | CurlSSLEngineNotFound
 | CurlSSLEngineSetFailed
 | CurlSendError
 | CurlRecvError
 | CurlShareInUse
 | CurlSSLCertProblem
 | CurlSSLCipher
 | CurlSSLCACert
 | CurlBadContentEncoding
 | CurlLDAPInvalidUrl
 | CurlFilesizeExceeded
 | CurlFtpSSLFailed
 | CurlSendFailRewind
 | CurlSSLEngineInitFailed
 | CurlLoginDenied
 | CurlTFtpNotFound
 | CurlTFtpPerm
 | CurlTFtpDiskFull
 | CurlTFtpIllegal
 | CurlTFtpUnknownId
 | CurlTFtpExists
 | CurlTFtpNoSuchUser
 | CurlConvFailed
 | CurlConvReqd
 | CurlSSLCACertBadFile
 | CurlRemoveFileNotFound
 | CurlSSH
 | CurlSSLShutdownFailed
 | CurlAgain
 | CurlSSLCRLBadFile
 | CurlSSLIssuerError
   deriving ( Eq, Show, Enum )

toCode :: CInt -> CurlCode
toCode x = toEnum (fromIntegral x)
