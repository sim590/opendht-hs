
{-|
  Module      : OpenDHT.PrivateKey
  Description : Bindings for the OpenDHT PrivateKey
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com
-}

module OpenDHT.PrivateKey ( PrivateKey (..)
                          ) where

import Data.ByteString

data PrivateKey = PrivateKey ByteString
               | ExportedKey String

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

