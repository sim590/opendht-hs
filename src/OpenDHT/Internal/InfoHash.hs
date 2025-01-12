
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

{-| Type synonym for C-bindings. Not meant to be used by the library user.
-}
type CInfoHashPtr = Ptr ()

foreign import ccall "dht_infohash_from_hex" dhtInfohashFromHexC :: CInfoHashPtr -> Ptr CChar -> IO ()

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

