
{-|
  Module      : OpenDHT.PublicKey
  Description : Bindings for the OpenDHT PublicKey
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com
-}

module OpenDHT.PublicKey ( PublicKey (..)
                         ) where

import Data.ByteString

data PublicKey = PublicKey ByteString
               | ExportedKey String

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

