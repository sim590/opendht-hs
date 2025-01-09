
{-|
  Module      : OpenDHT.InfoHash
  Description : InfoHash related bindings for opendht-c
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com

  This module exposes the different functions used to manipulate an OpenDHT
  `InfoHash`. An `InfoHash` encapsulates the result of the computation of the
  SHA-1 function. It's possible to derive such data from either a string, bytes
  or or by picking from random.
-}

module OpenDHT.InfoHash ( InfoHash
                        , emptyInfoHash
                        , randomInfoHash
                        , infoHashFromHex
                        , infoHashFromBytes
                        , infoHashFromString
                        , isZero
                        , infoHashToString
                        ) where

import Data.Functor
import qualified Data.ByteString as BS

import Control.Monad.IO.Class

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Utils
import Foreign.Marshal.Array

import OpenDHT.Types
import OpenDHT.Internal.InfoHash

{-| This type represents a 160-bit string of bytes consistent with the SHA1
   specification.

   It is at the heart of this library and is used as arguments for the get/put
   functions.
-}
newtype InfoHash = InfoHash { _infoHashPtr :: CInfoHash }

gettingInfoHash :: MonadIO f => (CInfoHash -> IO CInfoHash) -> f InfoHash
gettingInfoHash f = liftIO (with () f) <&> InfoHash

foreign import ccall "dht_infohash_zero" dhtInfoHashZeroC :: CInfoHash -> IO ()

{-| Yields the empty InfoHash.

   > unDht (emptyInfoHash >>= infoHashToString)
   > "0000000000000000000000000000000000000000"
-}
emptyInfoHash :: Dht InfoHash
emptyInfoHash = gettingInfoHash zero
  where zero ph = do
          dhtInfoHashZeroC ph
          return ph

foreign import ccall "dht_infohash_random" dhtInfohashRandomC :: CInfoHash -> IO ()

{-| Computes a random hash.

   > unDht (randomInfoHash >>= infoHashToString)
   > "974b3ea26b91ab5c8155a6cca5c2a3c3a7645f58"
-}
randomInfoHash :: Dht InfoHash
randomInfoHash = gettingInfoHash random
  where random ph = do
          dhtInfohashRandomC ph
          return ph

foreign import ccall "dht_infohash_from_hex" dhtInfohashFromHexC :: CInfoHash -> Ptr CChar -> IO ()

{-| Create an InfoHash from a 40 character hexstring.

   > unDht (infoHashFromHex "ffffffffffffffffffffffffffffffffffffffff" >>= infoHashToString)
   > "ffffffffffffffffffffffffffffffffffffffff"
-}
infoHashFromHex :: String -> Dht InfoHash
infoHashFromHex s = gettingInfoHash fromHex
  where fromHex ph = do
          withCString s $ \ cstr -> do
            dhtInfohashFromHexC ph cstr
            return ph

foreign import ccall "dht_infohash_get" dhtInfohashGetC :: CInfoHash -> Ptr CUChar -> CUInt -> IO ()

{-| Computes the SHA-1 sum of a given ByteString and yield the resulting InfoHash.

   > unDht (infoHashFromBytes (Data.ByteString.Char8.pack "0") >>= infoHashToString)
   > "b6589fc6ab0dc82cf12099d1c2d40ab994e8410c"
-}
infoHashFromBytes :: BS.ByteString -> Dht InfoHash
infoHashFromBytes bs = gettingInfoHash fromBs
  where
    fromBs ph = do
      a <- newArray (map CUChar $ BS.unpack bs)
      dhtInfohashGetC ph a (fromIntegral $ BS.length bs)
      return ph

foreign import ccall "dht_infohash_get_from_string" dhtInfohashGetFromStringC :: CInfoHash -> Ptr CChar -> IO ()

{-| Computes the SHA-1 sum of a given String and yield the resulting InfoHash.

  > unDht (infoHashFromString "0" >>= infoHashToString)
  > "b6589fc6ab0dc82cf12099d1c2d40ab994e8410c"
-}
infoHashFromString :: String -> Dht InfoHash
infoHashFromString s = gettingInfoHash fromStr
  where
    fromStr ph = do
      withCString s $ \ cstr -> do
        dhtInfohashGetFromStringC ph cstr
        return ph

foreign import ccall "dht_infohash_is_zero" dhtInfohashIsZeroC :: CInfoHash -> IO CBool

{-| Tells whether infohash is the empty InfoHash.

  > unDht (infoHashFromHex "" >>= isZero)
  > True

  > unDht (infoHashFromString "0" >>= isZero)
  > False

  > unDht (emptyInfoHash >>= isZero)
  > True
-}
isZero :: InfoHash -> Dht Bool
isZero h = liftIO (dhtInfohashIsZeroC (_infoHashPtr h) <&> toBool)

foreign import ccall "dht_infohash_print" dhtInfoHashPrintC :: CInfoHash -> IO (Ptr CChar)

{-| Get the string representation of an InfoHash.

   For example:

   > unDht $ do
   >   h <- infoHashFromHex "fae0f12f4f2f5eaaadceff21987ff1f121ff478f"
   >   liftIO $ print $ infoHashToString h

   would yield the following string:

   > "fae0f12f4f2f5eaaadceff21987ff1f121ff478f"
-}
infoHashToString :: InfoHash -> Dht String
infoHashToString h = liftIO (dhtInfoHashPrintC (_infoHashPtr h) >>= peekCString)

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

