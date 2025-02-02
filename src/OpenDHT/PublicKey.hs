
{-|
  Module      : OpenDHT.PublicKey
  Description : Bindings for the OpenDHT PublicKey
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com
-}

{-# LANGUAGE TemplateHaskell #-}

module OpenDHT.PublicKey ( PublicKey (..)
                          , pbkData
                         ) where

import Data.ByteString

import Control.Lens

data PublicKey = PublicKey { _pbkData :: ByteString
                           }
               | ExportedKey String
makeLenses ''PublicKey

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

