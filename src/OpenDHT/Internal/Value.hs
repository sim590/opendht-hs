
{-|
  Module      : OpenDHT.Internal.Value
  Description : Internal Value related bindings for opendht-c
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com
-}

module OpenDHT.Internal.Value where

import Data.Maybe
import Data.Word
import Data.Functor
import qualified Data.ByteString as BS

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class

import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import OpenDHT.Types
import OpenDHT.Internal.Blob
import OpenDHT.InfoHash
import OpenDHT.PublicKey
import OpenDHT.Internal.InfoHash
import OpenDHT.Internal.PublicKey

type CValuePtr = Ptr ()


data Value = StoredValue { _valueData        :: BS.ByteString -- ^ The data to store onto the DHT.
                         , _valueId          :: Word64        -- ^ The unique identifier of the value taken randomly
                                                              --   from the bits space \(\{0,1\}^{64}\).
                         , _valueOwner       :: PublicKey     -- ^ The owner's public key. If the data was signed, this
                                                              --   field should contain the exported key.
                         , _valueRecipientId :: InfoHash      -- ^ The hash of the public key to which the value is
                                                              --   dedicated when the value is encrypted. Otherwise,
                                                              --   it's the empty InfoHash.
                         , _valueUserType    :: String        -- ^ A user defined field for labelling values.
                         }
           | MetaValue   { _valueId          :: Word64        -- ^
                         , _valueOwner       :: PublicKey     -- ^
                         , _valueRecipientId :: InfoHash      -- ^
                         , _valueUserType    :: String        -- ^
                         }
           | InputValue  { _valueData        :: BS.ByteString -- ^
                         , _valueUserType    :: String        -- ^
                         }

foreign import ccall "dht_value_with_id_new" dhtValueWithIdNewC :: Ptr CUChar -> CULong -> CULong -> IO CValuePtr
foreign import ccall "dht_value_unref" dhtValueUnref            :: CValuePtr -> IO ()

{-| Build an OpenDHT Value from a string of bytes.
-}
withValuePtrFromBytes :: BS.ByteString -> Word64 -> (CValuePtr -> Dht ()) -> Dht ()
withValuePtrFromBytes bs vid f = liftIO $ withArray (map CUChar $ BS.unpack bs) $ \ ptrBytes -> do
  vPtr <- dhtValueWithIdNewC ptrBytes (fromIntegral $ BS.length bs) (fromIntegral vid)
  unDht $ f vPtr
  dhtValueUnref vPtr

foreign import ccall "dht_value_new_from_string" dhtValueNewFromStringC :: Ptr CChar -> IO CValuePtr

{-| Build an OpenDHT Value from a string of characters.
-}
valuePtrFromString :: String -> Dht CValuePtr
valuePtrFromString s = liftIO $ withCString s dhtValueNewFromStringC

-- foreign import ccall "dht_value_ref" dhtValueRefC :: CValuePtr -> IO CValuePtr
foreign import ccall "dht_value_unref" dhtValueUnrefC :: CValuePtr -> IO ()

{-| Delete the C reference to the OpenDHT Value pointed to by the pointer.
-}
unref :: CValuePtr -> Dht ()
unref vPtr = liftIO $ dhtValueUnrefC vPtr

foreign import ccall "wr_dht_value_get_data" dhtValueGetDataC :: Ptr DataView -> CValuePtr -> IO ()

{-| Get the data from the OpenDHT value. This is the actual bytes stored by the
   user.
-}
getValueData :: CValuePtr -> Dht BS.ByteString
getValueData vPtr = liftIO $ do
  dvPtr <- malloc
  dhtValueGetDataC dvPtr vPtr
  dv <- peek dvPtr
  free dvPtr
  bytesFromDataView dv

foreign import ccall "dht_value_get_id" dhtValueGetIdC :: CValuePtr -> IO CULong

{-| Get the id of an OpenDHT value. This field is a metadata.
-}
getValueId :: CValuePtr -> Dht Word64
getValueId = liftIO . (dhtValueGetIdC >=> return . fromIntegral)

foreign import ccall "dht_value_get_owner" dhtValueGetOwnerC :: CValuePtr -> IO CPublicKeyPtr

getValueOwner :: CValuePtr -> Dht CPublicKeyPtr
getValueOwner = liftIO . dhtValueGetOwnerC

foreign import ccall "wr_dht_value_get_recipient" dhtValueGetRecipientC :: CInfoHashPtr -> CValuePtr -> IO ()

{-| Get the recipient of an OpenDHT value. This field is a metadata.
-}
getValueRecipientId :: CValuePtr -> Dht InfoHash
getValueRecipientId vptr = getRecipientHashString <&> InfoHash
  where getRecipientHashString = liftIO $ do
          hPtr <- malloc
          dhtValueGetRecipientC hPtr vptr
          hstr <- infoHashToString hPtr
          free hPtr
          return hstr

getOwnerPublicKey :: CValuePtr -> Dht PublicKey
getOwnerPublicKey vptr = do
  opkPtr <- getValueOwner vptr
  mStr <- runMaybeT $ do
    guard (opkPtr /= nullPtr)
    export (CPublicKey opkPtr)
  return $ ExportedKey $ fromMaybe "" mStr

foreign import ccall "dht_value_get_user_type" dhtValueGetUserTypeC  :: CValuePtr -> IO (Ptr CChar)

{-| Get the user-type of an OpenDHT value. This field is a metadata.
-}
getValueUserType :: CValuePtr -> Dht String
getValueUserType = liftIO . (dhtValueGetUserTypeC >=> peekCString)

foreign import ccall "dht_value_set_user_type" dhtValueSetUserTypeC  :: CValuePtr -> Ptr CChar -> IO ()

{-| Set the user-type of an OpenDHT value.
-}
setValueUserType :: CValuePtr -> String -> Dht ()
setValueUserType vptr s = liftIO $ withCString s $ \ cstr -> dhtValueSetUserTypeC vptr cstr

storedValueFromCValuePtr :: CValuePtr -> Dht Value
storedValueFromCValuePtr vPtr = StoredValue <$> getValueData        vPtr
                                            <*> getValueId          vPtr
                                            <*> getOwnerPublicKey   vPtr
                                            <*> getValueRecipientId vPtr
                                            <*> getValueUserType    vPtr

metaValueFromCValuePtr :: CValuePtr -> Dht Value
metaValueFromCValuePtr vPtr = MetaValue <$> getValueId          vPtr
                                        <*> getOwnerPublicKey   vPtr
                                        <*> getValueRecipientId vPtr
                                        <*> getValueUserType    vPtr

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

