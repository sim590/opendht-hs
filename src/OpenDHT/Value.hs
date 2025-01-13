
{-|
  Module      : OpenDHT.Value
  Description : Value related bindings for opendht-c
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com

  This exposes different functions to manipulate an OpenDHT `Value`. This
  datatype encapsulates data either retrieved from or put on the network through
  `DhtRunner` functions.
-}


module OpenDHT.Value ( Value (..)
                     ) where

import qualified Data.ByteString as BS

import OpenDHT.InfoHash
import OpenDHT.PublicKey

data Value = StoredValue { _valueData        :: BS.ByteString
                         , _valueId          :: Int
                         , _valueOwner       :: PublicKey
                         , _valueRecipientId :: InfoHash
                         , _valueUserType    :: String
                         }
           | InputValue { _valueData        :: BS.ByteString
                        , _valueUserType    :: String
                        }

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

