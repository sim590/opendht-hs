
{-|
  Module      : OpenDHT.Blob
  Description : Blob related bindings
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com
-}

{-# LANGUAGE ForeignFunctionInterface #-}

module OpenDHT.Blob ( Blob
                    , viewBlob
                    , deleteBlob
                    ) where

import qualified Data.ByteString as BS

import Control.Monad.IO.Class

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc

import OpenDHT.Types
import OpenDHT.Internal.Blob

{-| This data type holds a pointer to data to be stored or retrieved on the DHT.

  In fact, in OpenDHT, "Blob" is a typdef to "std::vector<uint8_t>".
-}
newtype Blob = Blob { _blobCData :: CBlobPtr }

foreign import ccall "dht_blob_get_data_view" dhtBlobGetDataViewC :: Ptr DataView -> CBlobPtr -> IO ()

{-| Get the string of bytes under the Blob type.
-}
viewBlob :: Blob -> Dht BS.ByteString
viewBlob b = liftIO $ do
  dvPtr <- malloc
  dhtBlobGetDataViewC dvPtr (_blobCData b)
  dv      <- peek dvPtr
  cuchars <- peekArray (fromIntegral $ _dataSize dv) (_data dv)
  free dvPtr
  return $ BS.pack $ map (\ (CUChar w) -> w) cuchars

foreign import ccall "dht_blob_delete" dhtBlobDeleteC :: CBlobPtr -> IO ()

{-| Delete a blob for which OpenDHT holds the pointer to.
-}
deleteBlob :: Blob -> Dht ()
deleteBlob = liftIO . dhtBlobDeleteC . _blobCData

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

