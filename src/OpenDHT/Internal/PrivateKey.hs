
{-|
  Module      : OpenDHT.Internal.PrivateKey
  Description : Interna bindings for an OpenDHT PrivateKey
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com
-}

module OpenDHT.Internal.PrivateKey where

import Data.Functor
import qualified Data.ByteString as BS

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Array

import OpenDHT.Types
import OpenDHT.Internal.Blob
import OpenDHT.Internal.PublicKey
import qualified OpenDHT.Internal.Crypto as Crypto

newtype CPrivateKey = CPrivateKey { _privateKeyPtr :: CPrivateKeyPtr }
type CPrivateKeyPtr = Ptr ()

foreign import ccall "dht_privatekey_generate" dhtPrivatekeyGenerateC :: CUInt -> IO CPrivateKeyPtr

generate :: Int -> Dht CPrivateKey
generate len = liftIO $ dhtPrivatekeyGenerateC (fromIntegral len) <&> CPrivateKey

foreign import ccall "dht_privatekey_import" dhtPrivatekeyImportC :: Ptr CUChar -> CUInt -> Ptr CChar -> IO CPrivateKeyPtr

fromBytes :: BS.ByteString -> String -> MaybeT Dht CPrivateKey
fromBytes dataBs password = Crypto.fromBytes dataBs (Just password) privateKeyImport <&> CPrivateKey
  where
    privateKeyImport dataPtr cs (Just passPtr) = dhtPrivatekeyImportC dataPtr cs passPtr
    privateKeyImport _ _ Nothing               = error "PrivateKey.fromBytes: password should have been given."

foreign import ccall "dht_privatekey_export" dhtPrivatekeyExportC :: CPrivateKeyPtr -> Ptr CChar -> Ptr CUInt -> Ptr CChar-> IO CInt

export :: CPrivateKey -> String -> MaybeT Dht String
export (CPrivateKey pPtr) password = Crypto.export pPtr (Just password) privateKeyExport
  where
    privateKeyExport pPtr' bytesPtr sPtr (Just passPtr) = dhtPrivatekeyExportC pPtr' bytesPtr sPtr passPtr
    privateKeyExport _ _ _ Nothing                      = error "PrivateKey.export: password should have been given."

foreign import ccall "dht_privatekey_get_publickey" dhtPrivatekeyGetPublickeyC :: CPrivateKeyPtr -> IO CPublicKeyPtr

getPublicKey :: CPrivateKey -> Dht CPublicKey
getPublicKey (CPrivateKey pPtr) = liftIO $ dhtPrivatekeyGetPublickeyC pPtr <&> CPublicKey

foreign import ccall "dht_privatekey_decrypt" dhtPrivatekeyDecryptC :: CPrivateKeyPtr -> Ptr CChar -> CUInt -> IO CBlobPtr

decrypt :: CPrivateKey -> BS.ByteString -> Dht BS.ByteString
decrypt (CPrivateKey pPtr) dataBs = liftIO decryptData >>= viewBlob
  where
    decryptData = allocaArray s $ \ dataPtr -> dhtPrivatekeyDecryptC pPtr dataPtr cs
    s           = BS.length dataBs
    cs          = fromIntegral s

foreign import ccall "dht_privatekey_delete" dhtPrivatekeyDeleteC :: CPrivateKeyPtr -> IO ()

delete :: CPrivateKey -> Dht ()
delete = liftIO . dhtPrivatekeyDeleteC . _privateKeyPtr

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

