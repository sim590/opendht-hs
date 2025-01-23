
{-|
  Module      : OpenDHT.Internal.InfoHash
  Description : Internal definitions for InfoHash.
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com
-}

module OpenDHT.Internal.InfoHash where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Utils

#include <opendht/opendht_c.h>

{-| Type synonym for C-bindings. Not meant to be used by the library user.
-}
{# pointer *dht_infohash as CInfoHashPtr -> CInfoHash #}

-- struct OPENDHT_C_PUBLIC dht_infohash { uint8_t d[HASH_LEN]; };
newtype CInfoHash = CInfoHash { _infoHashData :: Ptr CUChar }

instance Storable CInfoHash where
    sizeOf _                   = {# sizeof dht_infohash  #}
    alignment _                = {# alignof dht_infohash #}
    poke p (CInfoHash cPtr)    = {# set dht_infohash->d  #} p cPtr
    peek p                     = CInfoHash <$> {# get dht_infohash->d #} p

hashHexLen :: Int
hashHexLen = 40

foreign import ccall "dht_infohash_from_hex" dhtInfohashFromHexC :: CInfoHashPtr -> Ptr CChar -> IO ()

foreign import ccall "dht_infohash_print" dhtInfoHashPrintC :: CInfoHashPtr -> IO (Ptr CChar)

{-| Get the string representation of an InfoHash.

   For example:

   > unDht $ do
   >   h <- infoHashFromHex "fae0f12f4f2f5eaaadceff21987ff1f121ff478f"
   >   liftIO $ print $ show h

   would yield the following string:

   > "fae0f12f4f2f5eaaadceff21987ff1f121ff478f"
-}
infoHashToString :: CInfoHashPtr -> IO String
infoHashToString h = dhtInfoHashPrintC h >>= peekCString

emptyInfoHashWordArray :: [CUChar]
emptyInfoHashWordArray = replicate hashHexLen (castCharToCUChar '0')

withCInfohash :: (Ptr CInfoHash -> IO b) -> IO b
withCInfohash f = withArray emptyInfoHashWordArray $ \ cucharPtr -> with (CInfoHash cucharPtr) f

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

