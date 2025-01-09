
{-|
  Module      : OpenDHT
  Description : Main OpenDHT interface
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com
-}

module OpenDHT ( version
               ) where

import Control.Monad.IO.Class

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import OpenDHT.Types

foreign import ccall "dht_version" dhtVersionC :: IO (Ptr CChar)

{-| Yields the version string of OpenDHT.

   > unDht version
   > "3.2.0"
-}
version :: Dht String
version = liftIO $ dhtVersionC >>= peekCString

-- vim: set sts=2 ts=2 sw=2 tw=120 et :

