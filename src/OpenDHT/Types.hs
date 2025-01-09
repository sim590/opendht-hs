
{-|
  Module      : OpenDHT.Types
  Description : OpenDHT data types
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com

  Global datatypes.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module OpenDHT.Types ( Dht (..)
                     ) where

import Control.Monad.IO.Class

{-| The Dht monad. This is essentially a wrapper around `IO` which is needed to
   interface with the C code.
-}
newtype Dht a = Dht { unDht :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

