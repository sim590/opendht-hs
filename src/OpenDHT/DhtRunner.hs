
{-|
  Module      : OpenDHT.DhtRunner
  Description : DhtRunner main interface
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com

  This encapsulates functions and datatypes for manipulating an OpenDHT node. In
  OpenDHT, a node is used through the class DhtRunner. This module exposes this
  class' functions.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module OpenDHT.DhtRunner ( DhtRunner
                         , DhtRunnerM
                         , runDhtRunnerM
                         , GetCallback
                         , ValueCallback
                         , DoneCallback
                         , ShutdownCallback
                         , run
                         , isRunning
                         , bootstrap
                         , get
                         , put
                         , listen
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

{-| Callback invoked whenever a `Value` is retrieved on the DHT during a
   Get (`get`) request. This callback shall return a boolean indicating whether to
   stop the Get request or not.
-}
type GetCallback  a = Value -- ^ A value found for the asked hash.
                   -> a     -- ^ User data passed from the initial call to `get`.
                   -> IO Bool

{-| Callback invoked whenever a `Value` is retrieved on the DHT during a Liste
   (`listen`) request. This callback shall be called once when a value is found
   and once when this same value has expired on the network. Finally, it returns a
   boolean indicating whether to stop the Listen request or not.
-}
type ValueCallback a = Value -- ^ A value found for the asked hash.
                    -> Bool  -- ^ Whether the value is expired or not.
                    -> a     -- ^ User data passed from the initial call to `listen`.
                    -> IO Bool

{-| The generic callback invoked for all asynchronous operations when those
   terminate.
-}
type DoneCallback a = Bool -- ^ A boolean indicating whether the operation was successful or not.
                   -> a    -- ^ User data passed from the initial call to the function.
                   -> IO ()

{-| A callback invoked before the Dhtnode is shutdown.
-}
type ShutdownCallback a = a -- ^ User data passed from the initial call to the function.
                       -> IO ()

type CDhtRunnerPtr = Ptr ()
type COpTokenPtr   = Ptr ()
newtype DhtRunner  = DhtRunner { dhtRunnerPtr :: CDhtRunnerPtr }
newtype OpToken    = OpToken { opTokenValue :: COpTokenPtr }

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

{-| This type wraps all function calls to OpenDHT. It is a transformer wrapping
   `ReaderT` and some other monad (usually `Dht`).

   This type should be used in conjunction with `runDhtRunnerM`.
-}
newtype DhtRunnerM m a = DhtRunnerM { unwrapDhtRunnerM :: ReaderT DhtRunner m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader DhtRunner)

instance MonadTrans DhtRunnerM where
  lift = DhtRunnerM . lift

fromGetCallBack :: Storable t => GetCallback t -> CGetCallback t
fromGetCallBack gcb vPtr userdataPtr = do
  udata <- peek userdataPtr
  v     <- unDht $ storedValueFromCValuePtr vPtr
  fromBool <$> gcb v udata

fromValueCallBack :: Storable t => ValueCallback t -> CValueCallback t
fromValueCallBack vcb vPtr expired userdataPtr = do
  udata <- peek userdataPtr
  v     <- unDht $ storedValueFromCValuePtr vPtr
  fromBool <$> vcb v (toBool expired) udata

fromDoneCallback :: Storable t => DoneCallback t -> CDoneCallback t
fromDoneCallback dcb successC userdataPtr = do
  udata <- peek userdataPtr
  dcb (toBool successC) udata

fromShutdownCallback :: Storable t => ShutdownCallback t -> CShutdownCallback t
fromShutdownCallback scb userdataPtr = do
  udata <- peek userdataPtr
  scb udata

foreign import ccall "dht_runner_new" dhtRunnerNewC :: IO CDhtRunnerPtr

{-| Initialize an OpenDHT node.
-}
initialize :: Dht DhtRunner
initialize = DhtRunner <$> liftIO dhtRunnerNewC

foreign import ccall "dht_runner_delete" dhtRunnerDeleteC :: CDhtRunnerPtr -> IO ()

{-| Delete the underlying DhtRunner C pointer.
-}
delete :: DhtRunner -> Dht ()
delete = liftIO . dhtRunnerDeleteC . dhtRunnerPtr

{-| Starts a DhtRunner session. This initializes the underlying OpenDHT node and
   takes care of freeing it before this function terminates.

   DhtRunner's function calls don't block the thread they're running on.
   Therefore, the user should take care of writing appropriate concurrency code
   for waiting on the underlying node's callback invocation before letting this
   function terminate. In general, the programmer should not let this function
   terminate while DHT operations are still susceptible to take place by the
   application.
-}
runDhtRunnerM :: DhtRunnerM Dht () -> IO ()
runDhtRunnerM runnerAction = unDht $ do
  dhtrunner <- initialize
  runReaderT (unwrapDhtRunnerM runnerAction) dhtrunner
  delete dhtrunner

foreign import ccall "dht_runner_run" dhtRunnerRunC :: CDhtRunnerPtr -> CInt -> IO CInt

{-| Run the OpenDHT node on a given port.
-}
run :: Int -- ^ The port on which to run the DHT node.
    -> DhtRunnerM Dht ()
run port = do
  dhtrunner <- ask
  void $ liftIO $ dhtRunnerRunC (dhtRunnerPtr dhtrunner) (fromIntegral port)

foreign import ccall "dht_runner_is_running" dhtRunnerIsRunningC :: CDhtRunnerPtr -> IO CBool

{-| Indicates whether the underlying node is running. This should yield `True`
   after calling `run`.
-}
isRunning :: DhtRunnerM Dht Bool
isRunning = ask >>= liftIO . (toBool <$>) . dhtRunnerIsRunningC . dhtRunnerPtr

foreign import ccall "dht_runner_bootstrap" dhtRunnerBootstrapC :: CDhtRunnerPtr -> Ptr CChar -> Ptr CChar -> IO ()

{-| Connect to the OpenDHT network before doing any operation.
-}
bootstrap :: String -- ^ The hostname (or IP address) used to bootstrap the connection to the network.
          -> String -- ^ The remote bootstrapping node port to use to connect.
          -> DhtRunnerM Dht ()
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

{-| Put data on the DHT for a given hash.
-}
put :: Storable userdata
    => InfoHash              -- ^ The hash under which to store the value.
    -> Value                 -- ^ The value to put on the DHT.
    -> DoneCallback userdata -- ^ The callback to invoke when the request is completed (or has failed).
    -> userdata              -- ^ User data to pass to the callback.
    -> Bool                  -- ^ Whether the value should be reannounced automatically after it has expired (after 10 minutes)
    -> DhtRunnerM Dht ()
put _ (StoredValue {}) _ _ _                           = error "DhtRunner.put needs to be fed an InputValue!"
put h (InputValue vbs usertype) dcb userdata permanent = ask >>= \ dhtrunner -> liftIO $ do
  withCString (show h) $ \ hStrPtr -> withCInfohash $ \ hPtr -> with userdata $ \ userdataPtr -> do
    dhtInfohashFromHexC hPtr hStrPtr
    dcbCWrapped <- wrapDoneCallbackC $ fromDoneCallback dcb
    vPtr <- unDht $ valueFromBytes vbs
    unDht $ setValueUserType vPtr usertype
    dhtRunnerPutC (dhtRunnerPtr dhtrunner) hPtr vPtr dcbCWrapped userdataPtr (fromBool permanent)

foreign import ccall "dht_runner_listen"
  dhtRunnerListenC :: CDhtRunnerPtr -> CInfoHashPtr -> FunPtr (CValueCallback a) -> FunPtr (CShutdownCallback a) -> Ptr a -> IO (Ptr ())

{-| Initiate a Listen operation for a given hash. The callback will be
   invoked once for every value found and once also when that same value expires
   on the DHT. While the Listen operation is not cancelled (or the node
   shutdown), it goes on. Threfore, values subsequently published will be
   received.
-}
listen :: Storable userdata
       => InfoHash                  -- ^ The hash indicating where to listen to.
       -> ValueCallback userdata    -- ^ The callback to invoke when a value is found or has expired.
       -> ShutdownCallback userdata -- ^ The callback to invoke before the OpenDHT node shuts down.
       -> userdata                  -- ^ User data to pass to the callback.
       -> DhtRunnerM Dht OpToken
listen h vcb scb userdata = ask >>= \ dhtrunner -> liftIO $ do
  withCString (show h) $ \ hStrPtr -> withCInfohash $ \ hPtr -> with userdata $ \ userdataPtr -> do
    dhtInfohashFromHexC hPtr hStrPtr
    vcbCWrapped <- wrapValueCallbackC $ fromValueCallBack vcb
    scbCWrapped <- wrapShutdownCallbackC $ fromShutdownCallback scb
    OpToken <$> dhtRunnerListenC (dhtRunnerPtr dhtrunner) hPtr vcbCWrapped scbCWrapped userdataPtr

makeDhtRunnerConfig :: DhtRunnerConfig -> Dht CDhtRunnerConfig
makeDhtRunnerConfig dhtConf = undefined

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

