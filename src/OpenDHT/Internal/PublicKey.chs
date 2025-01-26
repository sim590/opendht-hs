
{-|
  Module      : OpenDHT.Internal.PublicKey
  Description : Internal definitions for PublicKey.
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com
-}

module OpenDHT.Internal.PublicKey where

import Data.Functor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc
import Foreign.Storable

import OpenDHT.Types
import OpenDHT.Internal.Blob
import OpenDHT.Internal.InfoHash
import qualified OpenDHT.Internal.Crypto as Crypto

#include <opendht/opendht_c.h>

data CPublicKey    = CPublicKey { _publicKeyPtr :: CPublicKeyPtr }
type CPublicKeyPtr = Ptr ()

newtype PKId  = PKId  { _pkIdString  :: String }
newtype CPKId = CPKId { _pkIdDataPtr :: Ptr CUChar }

{# pointer *dht_pkid as CPKIdPtr -> CPKId #}

instance Storable CPKId where
    sizeOf _            = {# sizeof dht_pkid  #}
    alignment _         = {# alignof dht_pkid #}
    poke p (CPKId cPtr) = {# set dht_pkid->d  #} p cPtr
    peek p              = CPKId <$> {# get dht_pkid->d #} p

pkIdLen :: Int
pkIdLen = 32

foreign import ccall "dht_pkid_print" dhtPkidPrintC :: CPKIdPtr -> IO (Ptr CChar)

pkIdPtrToString :: CPKIdPtr -> IO String
pkIdPtrToString pkidPtr = dhtPkidPrintC pkidPtr >>= peekCString

emptyPkIdWordArray :: [CUChar]
emptyPkIdWordArray = replicate pkIdLen (castCharToCUChar '0')

withCPKId :: (Ptr CPKId -> IO b) -> IO b
withCPKId f = withArray emptyPkIdWordArray $ \ cucharPtr -> with (CPKId cucharPtr) f

foreign import ccall "dht_publickey_import" dhtPublickeyImportC :: Ptr CUChar -> CUInt -> IO CPublicKeyPtr

fromBytes :: BS.ByteString -> MaybeT Dht CPublicKey
fromBytes dataBs = Crypto.fromBytes dataBs Nothing publicKeyImport <&> CPublicKey
  where
    publicKeyImport dataPtr cs _ = dhtPublickeyImportC dataPtr cs

foreign import ccall "dht_publickey_delete" dhtPublickeyDeleteC :: CPublicKeyPtr -> IO ()

delete :: CPublicKey -> Dht ()
delete = liftIO . dhtPublickeyDeleteC . _publicKeyPtr

foreign import ccall "dht_publickey_export" dhtPublickeyExportC :: CPublicKeyPtr -> Ptr CChar -> Ptr CUInt -> IO CInt

export :: CPublicKey -> MaybeT Dht String
export (CPublicKey pPtr) = Crypto.export pPtr Nothing publicKeyExport
  where
    publicKeyExport pPtr' bytesPtr sPtr _ = dhtPublickeyExportC pPtr' bytesPtr sPtr

foreign import ccall "wr_dht_publickey_get_id" dhtPublickeyGetIdC :: CPublicKeyPtr -> CInfoHashPtr -> IO ()

idFromPublicKey :: CPublicKey -> Dht InfoHash
idFromPublicKey (CPublicKey cPtr) = liftIO (withCInfohash strFromInfoHash) <&> InfoHash
  where strFromInfoHash hPtr = do
          dhtPublickeyGetIdC cPtr hPtr
          infoHashToString hPtr

foreign import ccall "wr_dht_publickey_get_long_id" dhtPublickeyGetLongIdC :: CPublicKeyPtr -> CPKIdPtr -> IO ()

pkIdFromPublicKey :: CPublicKey -> Dht PKId
pkIdFromPublicKey (CPublicKey cPtr) = liftIO (withCPKId strFromPkId) <&> PKId
  where strFromPkId pkidPtr = do
          dhtPublickeyGetLongIdC cPtr pkidPtr
          pkIdPtrToString pkidPtr

foreign import ccall "dht_publickey_check_signature"
  dhtPublickeyCheckSignatureC :: CPublicKeyPtr -> Ptr CChar -> CUInt -> Ptr CChar -> CUInt -> IO CBool

checkSignature :: CPublicKey -> BS.ByteString -> BS.ByteString -> Dht Bool
checkSignature (CPublicKey pPtr) dataBs sigBs = liftIO $ withArrays check
  where
    check dataPtr sigPtr = dhtPublickeyCheckSignatureC pPtr dataPtr dataSize sigPtr sigSize <&> toBool
    withArrays f         = withArray dataArray $ \ dataPtr -> withArray sigArray (f dataPtr)
    dataArray            = map (castCharToCChar . BSI.w2c) $ BS.unpack dataBs
    sigArray             = map (castCharToCChar . BSI.w2c) $ BS.unpack sigBs
    dataSize             = fromIntegral $ length dataArray
    sigSize              = fromIntegral $ length sigArray

foreign import ccall "dht_publickey_encrypt" dhtPublickeyEncryptC :: CPublicKeyPtr -> Ptr CChar -> CUInt -> IO CBlobPtr

encrypt :: CPublicKey -> BS.ByteString -> Dht BS.ByteString
encrypt (CPublicKey pPtr) dataBs = do
  let
    dataArray = map (castCharToCChar . BSI.w2c) $ BS.unpack dataBs
    dataSize  = fromIntegral $ length dataArray
  blobPtr        <- liftIO $ withArray dataArray $ \ dataPtr -> dhtPublickeyEncryptC pPtr dataPtr dataSize
  encryptedBytes <- viewBlob blobPtr
  liftIO $ free blobPtr
  return encryptedBytes

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

