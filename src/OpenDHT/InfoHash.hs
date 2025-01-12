
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
                        ) where

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
newtype InfoHash = InfoHash { _infoHashString :: String }

instance Show InfoHash where
  show = _infoHashString

gettingInfoHash :: MonadIO f => (CInfoHashPtr -> IO String) -> f InfoHash
gettingInfoHash = (InfoHash <$>) . liftIO . with ()

foreign import ccall "dht_infohash_zero" dhtInfoHashZeroC :: CInfoHashPtr -> IO ()

{-| Yields the empty InfoHash.

   > unDht emptyInfoHash
   > 0000000000000000000000000000000000000000
-}
emptyInfoHash :: Dht InfoHash
emptyInfoHash = gettingInfoHash zero
  where zero ph = dhtInfoHashZeroC ph >> infoHashToString ph

foreign import ccall "dht_infohash_random" dhtInfohashRandomC :: CInfoHashPtr -> IO ()

{-| Computes a random hash.

   > unDht randomInfoHash
   > 974b3ea26b91ab5c8155a6cca5c2a3c3a7645f58
-}
randomInfoHash :: Dht InfoHash
randomInfoHash = gettingInfoHash random
  where random ph = dhtInfohashRandomC ph >> infoHashToString ph

{-| Create an InfoHash from a 40 character hexstring.

   > unDht (infoHashFromHex "ffffffffffffffffffffffffffffffffffffffff")
   > ffffffffffffffffffffffffffffffffffffffff
-}
infoHashFromHex :: String -> Dht InfoHash
infoHashFromHex s = gettingInfoHash (withCString s . fromHex)
  where fromHex ph cstr = do
          dhtInfohashFromHexC ph cstr
          infoHashToString ph

foreign import ccall "dht_infohash_get" dhtInfohashGetC :: CInfoHashPtr -> Ptr CUChar -> CUInt -> IO ()

{-| Computes the SHA-1 sum of a given ByteString and yield the resulting InfoHash.

   > unDht (infoHashFromBytes (Data.ByteString.Char8.pack "0"))
   > b6589fc6ab0dc82cf12099d1c2d40ab994e8410c
-}
infoHashFromBytes :: BS.ByteString -> Dht InfoHash
infoHashFromBytes bs = gettingInfoHash fromBs
  where
    fromBs ph = do
      a <- newArray (map CUChar $ BS.unpack bs)
      dhtInfohashGetC ph a (fromIntegral $ BS.length bs)
      infoHashToString ph

foreign import ccall "dht_infohash_get_from_string" dhtInfohashGetFromStringC :: CInfoHashPtr -> Ptr CChar -> IO ()

{-| Computes the SHA-1 sum of a given String and yield the resulting InfoHash.

  > unDht (infoHashFromString "0")
  > b6589fc6ab0dc82cf12099d1c2d40ab994e8410c
-}
infoHashFromString :: String -> Dht InfoHash
infoHashFromString s = gettingInfoHash (withCString s .  fromStr)
  where
    fromStr ph cstr = dhtInfohashGetFromStringC ph cstr >> infoHashToString ph

foreign import ccall "dht_infohash_is_zero" dhtInfohashIsZeroC :: CInfoHashPtr -> IO CBool

{-| Tells whether infohash is the empty InfoHash.

  > unDht (infoHashFromHex "" >>= isZero)
  > True

  > unDht (infoHashFromString "0" >>= isZero)
  > False

  > unDht (emptyInfoHash >>= isZero)
  > True
-}
isZero :: InfoHash -> Dht Bool
isZero h = liftIO $ with () (withCString (_infoHashString h) . iszero)
  where iszero ph cstr = toBool <$> (dhtInfohashFromHexC ph cstr >> dhtInfohashIsZeroC ph)

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

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

