
{-|
  Module      : OpenDHT.DhtRunner
  Description : DhtRunner main interface
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com

  This encapsulates functions and datatypes for manipulating an OpenDHT node.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module OpenDHT.DhtRunner ( DhtRunner
                         , DhtRunnerM
                         , runDhtRunnerM
                         , run
                         , bootstrap
                         , get
                         ) where

import qualified Data.ByteString as BS

import Control.Monad.Trans.Class
import Control.Monad.Reader

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Utils

import OpenDHT.Types
import OpenDHT.InfoHash
import OpenDHT.Value
import OpenDHT.Internal.Value
import OpenDHT.Internal.DhtRunner
import OpenDHT.Internal.InfoHash

type GetCallback  a = Value -> a -> IO Bool
type DoneCallback a = Bool -> a -> IO ()

type CDhtRunnerPtr = Ptr ()
newtype DhtRunner  = DhtRunner { dhtRunnerPtr :: CDhtRunnerPtr }

data DhtIdentity = DhtIdentity { _privatekey  :: BS.ByteString
                               , _certificate :: BS.ByteString
                               }

data DhtNodeConfig = DhtNodeConfig { _nodeId          :: InfoHash
                                   , _network         :: Int
                                   , _isBootstrap     :: Bool
                                   , _maintainStorage :: Bool
                                   , _persistPath     :: String
                                   }

data DhtSecureConfig = DhtSecureConfig { _nodeConfig :: DhtNodeConfig
                                       , _id         :: DhtIdentity
                                       }

data DhtRunnerConfig = DhtRunnerConfig { _dhtConfig      :: DhtSecureConfig
                                       , _threaded       :: Bool
                                       , _proxyServer    :: String
                                       , _pushNodeId     :: String
                                       , _pushToken      :: String
                                       , _pushTopic      :: String
                                       , _pushPlatform   :: String
                                       , _peerDiscovery  :: Bool
                                       , _peerPublish    :: Bool
                                       , _serverCa       :: BS.ByteString
                                       , _clientIdentity :: DhtIdentity
                                       , _log            :: Bool
                                       }

newtype DhtRunnerM m a = DhtRunnerM { unwrapDhtRunnerM :: ReaderT DhtRunner m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader DhtRunner)

instance MonadTrans DhtRunnerM where
  lift = DhtRunnerM . lift

foreign import ccall "dht_runner_new" dhtRunnerNewC :: IO CDhtRunnerPtr

fromGetCallBack :: Storable t => GetCallback t -> CGetCallback t
fromGetCallBack gcb vPtr userdataPtr = do
  udata <- peek userdataPtr
  v     <- unDht $ storedValueFromCValuePtr vPtr
  fromBool <$> gcb v udata

fromDoneCallback :: Storable t => DoneCallback t -> CDoneCallback t
fromDoneCallback dcb successC userdataPtr = do
  udata <- peek userdataPtr
  dcb (toBool successC) udata

{-| Initialize an OpenDHT node.
-}
initialize :: Dht DhtRunner
initialize = DhtRunner <$> liftIO dhtRunnerNewC

foreign import ccall "dht_runner_delete" dhtRunnerDeleteC :: CDhtRunnerPtr -> IO ()

{-| Delete the underlying DhtRunner C pointer.
-}
delete :: DhtRunner -> Dht ()
delete = liftIO . dhtRunnerDeleteC . dhtRunnerPtr

foreign import ccall "dht_runner_run" dhtRunnerRunC :: CDhtRunnerPtr -> CInt -> IO CInt

{-| Run the OpenDHT node on a given port.
-}
run :: Int -- ^ The port on which to run the DHT node.
    -> DhtRunnerM Dht ()
run port = do
  dhtrunner <- ask
  void $ liftIO $ dhtRunnerRunC (dhtRunnerPtr dhtrunner) (fromIntegral port)

foreign import ccall "dht_runner_bootstrap" dhtRunnerBootstrapC :: CDhtRunnerPtr -> Ptr CChar -> Ptr CChar -> IO ()

{-| Connect to the OpenDHT network before doing any operation.
-}
bootstrap :: String -> String -> DhtRunnerM Dht ()
bootstrap addr port = do
  dhtrunner <- ask
  liftIO $ withCString addr $ \ addrCPtr ->
           withCString port $ \ portCPtr -> dhtRunnerBootstrapC (dhtRunnerPtr dhtrunner) addrCPtr portCPtr

foreign import ccall "dht_runner_get"
  dhtRunnerGetC :: CDhtRunnerPtr -> CInfoHashPtr  -> FunPtr (CGetCallback a) -> FunPtr (CDoneCallback a) -> Ptr a -> IO ()

{-| Get a value pointed by a given hash on the DHT.
-}
get :: Storable userdata
    => InfoHash              -- ^ The hash for which to get data at.
    -> GetCallback userdata  -- ^ The callback invoked for all values retrieved on the DHT for the given hash.
    -> DoneCallback userdata -- ^ The callback invoked when OpenDHT has completed the get request.
    -> userdata              -- ^ Some user data to be passed to callbacks.
    -> DhtRunnerM Dht ()
get h gcb dcb userdata = ask >>= \ dhtrunner -> liftIO $ do
  withCString (show h) $ \ hStrPtr -> withCInfohash $ \ hPtr -> with userdata $ \ userdataPtr -> do
    dhtInfohashFromHexC hPtr hStrPtr
    gcbCWrapped <- wrapGetCallbackC $ fromGetCallBack gcb
    dcbCWrapped <- wrapDoneCallbackC $ fromDoneCallback dcb
    dhtRunnerGetC (dhtRunnerPtr dhtrunner) hPtr gcbCWrapped dcbCWrapped userdataPtr

foreign import ccall "dht_runner_put"
  dhtRunnerPutC :: CDhtRunnerPtr -> CInfoHashPtr -> CValuePtr -> FunPtr (CDoneCallback a) -> Ptr a -> CBool -> IO ()

put :: Storable a
    => Value          -- ^ The value to put on the DHT.
    -> InfoHash       -- ^ The hash under which to store the value.
    -> DoneCallback a -- ^ The callback to invoke when the request is completed (or has failed).
    -> a              -- ^ User data to pass to the callback.
    -> Bool           -- ^ Whether the value should be reannounced automatically after it has expired (after 10 minutes)
    -> DhtRunnerM  Dht ()
put (StoredValue {}) _ _ _ _                           = error "DhtRunner.put needs to be fed an InputValue!"
put (InputValue vbs usertype) h dcb userdata permanent = ask >>= \ dhtrunner -> liftIO $ do
  withCString (show h) $ \ hStrPtr -> withCInfohash $ \ hPtr -> with userdata $ \ userdataPtr -> do
    dhtInfohashFromHexC hPtr hStrPtr
    dcbCWrapped <- wrapDoneCallbackC $ fromDoneCallback dcb
    vPtr <- unDht $ valueFromBytes vbs
    unDht $ setValueUserType vPtr usertype
    dhtRunnerPutC (dhtRunnerPtr dhtrunner) hPtr vPtr dcbCWrapped userdataPtr (fromBool permanent)

runDhtRunnerM :: DhtRunnerM Dht () -> IO ()
runDhtRunnerM runnerAction = unDht $ do
  dhtrunner <- initialize
  runReaderT (unwrapDhtRunnerM runnerAction) dhtrunner
  delete dhtrunner

makeDhtRunnerConfig :: DhtRunnerConfig -> Dht CDhtRunnerConfig
makeDhtRunnerConfig dhtConf = undefined

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

