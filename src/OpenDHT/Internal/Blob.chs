
{-|
  Module      : OpenDHT.Internal.Blob
  Description : Internal definitions for blobs.
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com
-}

module OpenDHT.Internal.Blob where

import qualified Data.ByteString as BS
import Control.Monad.IO.Class

import Foreign.Marshal.Array
import Foreign.Marshal.Alloc

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable

import OpenDHT.Types

#include <opendht/opendht_c.h>

{-| Type synonym for C-bindings. Not meant to be used by the library user.
-}
type CBlobPtr = Ptr ()

{-| Internal intermediary type used to retrieve values under Blobs.
-}
data DataView = DataView { _data      :: Ptr CUChar
                         , _dataSize  :: CULong
                         }

instance Storable DataView where
    sizeOf _              = {# sizeof  dht_data_view #}
    alignment _           = {# alignof dht_data_view #}
    poke p (DataView d s) = {# set dht_data_view->data #} p d
                         >> {# set dht_data_view->size #} p s
    peek p                = DataView <$> {# get dht_data_view->data #} p
                                     <*> {# get dht_data_view->size #} p

bytesFromDataView :: DataView -> IO BS.ByteString
bytesFromDataView dv = do
  cuchars <- peekArray (fromIntegral $ _dataSize dv) (_data dv)
  return $ BS.pack $ map (\ (CUChar w) -> w) cuchars

foreign import ccall "wr_dht_blob_get_data" dhtBlobGetDataViewC :: Ptr DataView -> CBlobPtr -> IO ()

{-| Get the string of bytes under the Blob type.
-}
viewBlob :: CBlobPtr -> Dht BS.ByteString
viewBlob b = liftIO $ do
  dvPtr <- malloc
  dhtBlobGetDataViewC dvPtr b
  dv <- peek dvPtr
  free dvPtr
  bytesFromDataView dv

foreign import ccall "dht_blob_delete" dhtBlobDeleteC :: CBlobPtr -> IO ()

{-| Delete a blob for which OpenDHT holds the pointer to.
-}
deleteBlob :: CBlobPtr -> Dht ()
deleteBlob = liftIO . dhtBlobDeleteC

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

