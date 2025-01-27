
{-|
  Module      : OpenDHT.PrivateKey
  Description : Bindings for the OpenDHT PrivateKey
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com
-}

{-# LANGUAGE TemplateHaskell #-}

module OpenDHT.PrivateKey ( PrivateKey (..)
                          , pvkData
                          , pvkPassword
                          ) where

import Data.ByteString

import Control.Lens

data PrivateKey = PrivateKey { _pvkData     :: ByteString
                             , _pvkPassword :: String
                             }
                | ExportedKey String
makeLenses ''PrivateKey

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

