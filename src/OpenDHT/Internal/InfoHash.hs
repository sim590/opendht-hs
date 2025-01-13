
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

{-| Type synonym for C-bindings. Not meant to be used by the library user.
-}
type CInfoHashPtr = Ptr ()

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

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

