
{-|
  Module      : OpenDHT.Internal.Crypto
  Description : Internal crypto bindings
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com
-}

module OpenDHT.Internal.Crypto where

import Data.Foldable
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable

import OpenDHT.Types

type CPassWordPtr      = Ptr CChar
type COutputString     = Ptr CChar
type CInputDataPtr     = Ptr CUChar
type DataImportation a = CInputDataPtr -> CUInt -> Maybe CPassWordPtr -> IO (Ptr a)
type DataExportation a = Ptr a -> COutputString -> Ptr CUInt -> Maybe CPassWordPtr -> IO CInt

withMaybePassword :: Maybe String -> (Maybe CPassWordPtr -> IO a) -> IO a
withMaybePassword mPassword f = do
  mPassPtr <- forM mPassword (liftIO . mallocBytes . length)
  a        <- f mPassPtr
  forM_ mPassPtr free
  return a

fromBytes :: BS.ByteString -> Maybe String -> DataImportation a -> MaybeT Dht (Ptr a)
fromBytes dataBs mPassword dataImport = do
  let
    s  = BS.length dataBs
    cs = fromIntegral s
    withDataAndPass size f = allocaArray size $ \ dataPtr  -> withMaybePassword mPassword
                                              $ \ mPassPtr -> f dataPtr cs mPassPtr
  pPtr <- liftIO $ withDataAndPass s dataImport
  guard (pPtr /= nullPtr)
  return pPtr

export :: Ptr a -> Maybe String -> DataExportation a -> MaybeT Dht String
export pPtr mPassword dataExport = do
  let withSizedArray s f = liftIO $ alloca $ \ sPtr -> do
        poke sPtr (fromIntegral s)
        allocaArray s $ \ bytesPtr -> withMaybePassword mPassword $ \ mPassPtr -> f sPtr bytesPtr mPassPtr
  -- First, test to get the correct value for the size parameter.
  (err, s') <- withSizedArray 0 $ \ sPtr bytesPtr mPassPtr -> do
    err <- dataExport pPtr bytesPtr sPtr mPassPtr
    s'  <- peek sPtr -- According to GNUTls' documentation for the underlying function called by
    return (err, s') -- OpenDHT, gnutls_x509_privkey_export_pkcs8, upon GNUTLS_E_SHORT_MEMORY_BUFFER (-51)
                     -- error the size parameter would be updated with the correct value needed for the
  guard (err == -51) -- function to succeed.
  withSizedArray (fromIntegral s') $ \ sPtr bytesPtr passPtr -> do
    c <- dataExport pPtr bytesPtr sPtr passPtr
    guard (c == 0)
    ccharArray <- peekArray (fromIntegral s') bytesPtr
    return $ map castCCharToChar ccharArray

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

