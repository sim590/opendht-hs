
{-|
  Module      : OpenDHT.Internal.Certificate
  Description : Internal definitions for certificates.
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com
-}

module OpenDHT.Internal.Certificate where

import Data.Functor
import qualified Data.ByteString as BS

import Control.Monad.IO.Class

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Array

import OpenDHT.Types
import OpenDHT.Internal.InfoHash
import OpenDHT.Internal.PublicKey

newtype CCertificate = CCertificate { _certificatePtr :: CCertificatePtr }
type CCertificatePtr = Ptr ()

foreign import ccall "dht_certificate_import" dhtCertificateImportC :: Ptr CUChar -> CUInt -> IO CCertificatePtr

certificatePtrFromBytes :: BS.ByteString -> Dht CCertificate
certificatePtrFromBytes bs = liftIO $ withArray (map CUChar $ BS.unpack bs)
                                    $ \ ptrBytes -> dhtCertificateImportC ptrBytes (fromIntegral $ BS.length bs) <&> CCertificate

foreign import ccall "wr_dht_certificate_get_id" dhtCertificateGetIdC :: CCertificatePtr -> CInfoHashPtr -> IO ()

idFromCertificate :: CCertificate -> Dht InfoHash
idFromCertificate (CCertificate cPtr) = liftIO (withCInfohash strFromInfoHash) <&> InfoHash
  where strFromInfoHash hPtr = do
          dhtCertificateGetIdC cPtr hPtr
          infoHashToString hPtr

foreign import ccall "wr_dht_certificate_get_long_id" dhtCertificateGetLongIdC :: CCertificatePtr -> CPKIdPtr -> IO ()

pkIdFromCertificate :: CCertificate -> Dht PKId
pkIdFromCertificate (CCertificate cPtr) = liftIO (withCPKId strFromPkId) <&> PKId
  where strFromPkId pkidPtr = do
          dhtCertificateGetLongIdC cPtr pkidPtr
          pkIdPtrToString pkidPtr

foreign import ccall "dht_certificate_get_publickey" dhtCertificateGetPublickeyC :: CCertificatePtr -> IO CPublicKeyPtr

publicKeyFromCertificate :: CCertificate -> Dht CPublicKey
publicKeyFromCertificate (CCertificate cPtr) = liftIO (dhtCertificateGetPublickeyC cPtr) <&> CPublicKey

foreign import ccall "dht_certificate_delete" dhtCertificateDeleteC :: CCertificatePtr -> IO ()

deleteCertificate :: CCertificate -> Dht ()
deleteCertificate (CCertificate cPtr) = liftIO $ dhtCertificateDeleteC cPtr

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

