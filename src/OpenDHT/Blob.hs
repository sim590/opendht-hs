
{-|
  Module      : OpenDHT.Blob
  Description : Blob related bindings
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com
-}

module OpenDHT.Blob ( Blob
                    ) where

import qualified Data.ByteString as BS

{-| This data type holds a pointer to data to be stored or retrieved on the DHT.

  In fact, in OpenDHT, "Blob" is a typdef to "std::vector<uint8_t>".
-}
type Blob = BS.ByteString

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

