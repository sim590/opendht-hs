
{-|
  Module      : OpenDHT.Internal.Blob
  Description : Internal definitions for blobs.
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com
-}

module OpenDHT.Internal.Blob where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable

#include <opendht/opendht_c.h>

{-| Type synonym for C-bindings. Not meant to be used by the library user.
-}
type CBlobPtr = Ptr ()

{-| Internal intermediary type used to retrieve values under Blobs.
-}
data DataView = DataView { _data      :: Ptr CUChar
                         , _dataSize  :: CULong
                         }

instance Storable DataView where
    sizeOf _    = {# sizeof dht_data_view #}
    alignment _ = {# alignof dht_data_view #}
    poke        = poke
    peek p      = DataView <$> {# get dht_data_view->data #} p
                           <*> {# get dht_data_view->size #} p

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

