



module OpenDHT.Value ( Value
                        , 
                        ) where

import qualified Data.ByteString as BS

import Control.Monad.IO.Class

import Foreign.Ptr ( Ptr )
import Foreign.C.Types
import Foreign.Marshal.Array

import OpenDHT.Types

-- struct OPENDHT_C_PUBLIC dht_value;
-- typedef struct dht_value dht_value;
-- typedef uint64_t dht_value_id;
-- OPENDHT_C_PUBLIC dht_value* dht_value_new(const uint8_t* data, size_t size);
-- OPENDHT_C_PUBLIC dht_value* dht_value_new_from_string(const char* str);
-- OPENDHT_C_PUBLIC dht_value* dht_value_ref(const dht_value*);
-- OPENDHT_C_PUBLIC void dht_value_unref(dht_value*);
-- OPENDHT_C_PUBLIC dht_data_view dht_value_get_data(const dht_value* data);
-- OPENDHT_C_PUBLIC dht_value_id dht_value_get_id(const dht_value* data);
-- OPENDHT_C_PUBLIC dht_publickey* dht_value_get_owner(const dht_value* data);
-- OPENDHT_C_PUBLIC dht_infohash dht_value_get_recipient(const dht_value* data);
-- OPENDHT_C_PUBLIC const char* dht_value_get_user_type(const dht_value* data);
-- OPENDHT_C_PUBLIC void dht_value_set_user_type(dht_value* data, const char* user_type);


type CValue = Ptr ()
newtype Value = Value { _valuePtr :: CValue }

foreign import ccall "dht_value_new" dhtValueNewC :: Ptr CUChar -> CULong -> IO (CValue)

valueFromBytes :: BS.ByteString -> Dht Value
valueFromBytes bs = liftIO $ do
    let s = BS.length bs
    ptrCValue <- allocaArray s $ \ ptrBytes -> do
        dhtValueNewC ptrBytes (fromIntegral s)
    return $ Value ptrCValue

