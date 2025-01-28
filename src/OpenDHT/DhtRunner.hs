
{-|
  Module      : OpenDHT.DhtRunner
  Description : DhtRunner main interface
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com

  This encapsulates functions and datatypes for manipulating an OpenDHT node. In
  OpenDHT, a node is used through the class @DhtRunner@. This module exposes this
  class' functions.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenDHT.DhtRunner ( DhtRunner
                         , DhtRunnerM
                         , runDhtRunnerM
                         , dhtRunner
                         , listenTokens
                         , OpToken
                         , DhtRunnerConfig (..)
                         , dhtConfig
                         , threaded
                         , proxyServer
                         , pushNodeId
                         , pushToken
                         , pushTopic
                         , pushPlatform
                         , peerDiscovery
                         , peerPublish
                         , serverCa
                         , clientIdentity
                         , logging
                         , DhtSecureConfig (..)
                         , nodeConfig
                         , nodeId
                         , DhtNodeConfig (..)
                         , nodeIdHash
                         , network
                         , isBootstrap
                         , maintainStorage
                         , persistPath
                         , DhtIdentity (..)
                         , privatekey
                         , certificate
                         , GetCallback
                         , ValueCallback
                         , DoneCallback
                         , ShutdownCallback
                         , getNodeIdHash
                         , getPublicKeyID
                         , run
                         , runConfig
                         , isRunning
                         , bootstrap
                         , get
                         , put
                         , cancelPut
                         , listen
                         , cancelListen
                         ) where

import Data.Default
import Data.Word
import Data.Functor
import qualified Data.List as List
import Data.Map ( Map
                )
import qualified Data.Map as Map
import qualified Data.ByteString as BS

import Control.Lens

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.State ( StateT
                           , MonadState
                           , execStateT
                           )
import qualified Control.Monad.State as ST
import Control.Concurrent.MVar

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc

import OpenDHT.Types
import OpenDHT.InfoHash
import OpenDHT.Value
import OpenDHT.PrivateKey
import OpenDHT.Certificate
import OpenDHT.Internal.Value
import OpenDHT.Internal.DhtRunner
import OpenDHT.Internal.InfoHash
import qualified OpenDHT.Internal.Certificate as Certificate
import OpenDHT.Internal.Certificate (CCertificate (..))
import qualified OpenDHT.Internal.PrivateKey as PrivateKey
import OpenDHT.Internal.PrivateKey (CPrivateKey (..))

type CDhtRunnerPtr = Ptr ()
type COpTokenPtr   = Ptr ()

newtype DhtRunner = DhtRunner { _dhtRunnerPtr :: CDhtRunnerPtr }

{-| A token used to track Listen requests.
-}
newtype OpToken = OpToken { _opTokenPtr :: COpTokenPtr }
  deriving Eq

data DhtRunnerState = DhtRunnerState
  { _dhtRunner    :: DhtRunner              -- ^ The DhtRunner.
  , _listenTokens :: Map InfoHash [OpToken] -- ^ Map tracking the different Listen requests for every calls to `listen`
                                            --   according to their respective hash argument.
  }
makeLenses ''DhtRunnerState

data DhtIdentity = DhtIdentity { _privatekey  :: PrivateKey
                               , _certificate :: Certificate
                               }
makeLenses ''DhtIdentity

instance Default DhtIdentity where
  def = DhtIdentity { _privatekey  = PrivateKey BS.empty ""
                    , _certificate = BS.empty
                    }

data DhtNodeConfig = DhtNodeConfig
  { _nodeIdHash      :: Maybe InfoHash -- ^ ID of the underlying DHT node (default: `Nothing`).
                                       --   If not set, it will be randomly generated.
  , _network         :: Word32         -- ^ ID of the network (default: @0@).
  , _isBootstrap     :: Bool           -- ^ Whether to run in bootstrap mode (default: `False`).
  , _maintainStorage :: Bool           -- ^ Whether to help in maintaining storage of
                                       --   permanent values on the network (default: `False`).
  , _persistPath     :: String         -- ^ Path to the file on disk where to store the DHT persisting cache for
                                       --   bootstrapping from last session list of already known nodes (default: empty).
  }
makeLenses ''DhtNodeConfig

instance Default DhtNodeConfig where
  def = DhtNodeConfig { _nodeIdHash      = Nothing
                      , _network         = 0
                      , _isBootstrap     = False
                      , _maintainStorage = False
                      , _persistPath     = ""
                      }

{-| DHT node and security config.
-}
data DhtSecureConfig = DhtSecureConfig { _nodeConfig :: DhtNodeConfig -- ^ Low level DHT config parameters.
                                       , _nodeId     :: DhtIdentity   -- ^ Node identity for handling DhtRunner's secure operations variants.
                                       }
makeLenses ''DhtSecureConfig

instance Default DhtSecureConfig where
  def = DhtSecureConfig def def

data DhtRunnerConfig = DhtRunnerConfig
  { _dhtConfig      :: DhtSecureConfig -- ^ The DHT config and SecureDhtRunner identity.
  , _threaded       :: Bool            -- ^ Whether OpenDHT should run in threaded mode (default: `True`)
  , _proxyServer    :: String          -- ^ The proxy server hostname (default: empty).
  , _pushNodeId     :: String          -- ^ A node id ([push notifications](https://github.com/savoirfairelinux/opendht/wiki/Push-notifications-support))
                                       --   (default: empty). This requires running with the proxy server.
  , _pushToken      :: String          -- ^ Push notification token (default: empty). This requires running with the proxy server.
  , _pushTopic      :: String          -- ^ Push notification topic (default: empty). This requires running with the proxy server.
  , _pushPlatform   :: String          -- ^ Push notification platform (default: empty). This requires running with the proxy server.
  , _peerDiscovery  :: Bool            -- ^ Use multicast to discover nodes announcing themselves (default: `False`).
  , _peerPublish    :: Bool            -- ^ Publish the DHT node through multicast in order to be discoverable by others (default: `False`).
  , _serverCa       :: BS.ByteString   -- ^ Proxy server X.509 certificate (default: empty).
  , _clientIdentity :: DhtIdentity     -- ^ Proxy client certificate and private key structure.
  , _logging        :: Bool            -- ^ Enable logging (default: `False`).
  }
makeLenses ''DhtRunnerConfig

instance Default DhtRunnerConfig where
  def = DhtRunnerConfig { _dhtConfig      = def
                        , _threaded       = True
                        , _proxyServer    = ""
                        , _pushNodeId     = ""
                        , _pushToken      = ""
                        , _pushTopic      = ""
                        , _pushPlatform   = ""
                        , _peerDiscovery  = False
                        , _peerPublish    = False
                        , _serverCa       = BS.empty
                        , _clientIdentity = def
                        , _logging        = False
                        }

{-| This type wraps all function calls to OpenDHT. It is a transformer wrapping
   `StateT` and some other monad (usually `Dht`).

   This type should be used in conjunction with `runDhtRunnerM`.
-}
newtype DhtRunnerM m a = DhtRunnerM { unwrapDhtRunnerM :: StateT DhtRunnerState m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState DhtRunnerState)

instance MonadTrans DhtRunnerM where
  lift = DhtRunnerM . lift

{-| Callback invoked whenever a `Value` is retrieved on the DHT during a
   Get (`get`) request. This callback shall return a boolean indicating whether to
   stop the Get request or not.
-}
type GetCallback  a = Value -- ^ A value found for the asked hash.
                   -> a     -- ^ User data passed from the initial call to `get`.
                   -> IO Bool

{-| Callback invoked whenever a `Value` is retrieved on the DHT during a Listen
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
delete = liftIO . dhtRunnerDeleteC . _dhtRunnerPtr

foreign import ccall "dht_op_token_delete" dhtOpTokenDeleteC :: COpTokenPtr -> IO ()

deleteOpToken :: OpToken -> Dht ()
deleteOpToken = liftIO . dhtOpTokenDeleteC . _opTokenPtr

{-| Starts a DhtRunner session. This initializes the underlying OpenDHT node and
   takes care of freeing it before this function terminates.

   @DhtRunner@'s function calls don't block the thread they're running on.
   Therefore, the user should take care of writing appropriate concurrency code
   for waiting on the underlying node's callback invocation before letting this
   function terminate. In general, the programmer should not let this function
   terminate while DHT operations are still susceptible to occur for the
   application.
-}
runDhtRunnerM :: Storable userdata
              => ShutdownCallback userdata -- ^ A callback to run before shutting down the DHT node.
              -> userdata                  -- ^ User data to pass to the `ShutdownCallback`.
              -> MVar ()                   -- ^ Synchronizing variable used to block while waiting on the
                                           --   `ShutdownCallback` to terminate.
              -> DhtRunnerM Dht ()         -- ^ The `DhtRunnerM` action.
              -> IO ()
runDhtRunnerM scb userdata mv runnerAction = unDht $ do
  dhtrunner <- initialize
  s         <- execStateT (unwrapDhtRunnerM (runnerAction >> shutdown scb userdata)) (DhtRunnerState dhtrunner Map.empty)
  liftIO $ putMVar mv ()
  delete dhtrunner
  forM_ (Map.toList (s^.listenTokens) ^. traverse . _2) deleteOpToken

{-| Boiler plate code for getNodeId, getPublicKeyID.
-}
infohashFromDhtRunner :: (CDhtRunnerPtr -> CInfoHashPtr -> IO ()) -> DhtRunnerM Dht InfoHash
infohashFromDhtRunner f = (use dhtRunner >>= fromDhtRunner) <&> InfoHash
  where fromDhtRunner dhtrunner = liftIO $ withCInfohash $ \ hPtr -> do
          f (_dhtRunnerPtr dhtrunner) hPtr
          infoHashToString hPtr

foreign import ccall "wr_dht_runner_get_node_id" dhtRunnerGetNodeIdC :: CDhtRunnerPtr -> CInfoHashPtr -> IO ()

{-| Get the ID of the underlying DHT node.
-}
getNodeIdHash :: DhtRunnerM Dht InfoHash
getNodeIdHash = infohashFromDhtRunner dhtRunnerGetNodeIdC

foreign import ccall "wr_dht_runner_get_id" dhtRunnerGetIdC :: CDhtRunnerPtr -> CInfoHashPtr -> IO ()

{-| Get the public key ID of the @DhtRunner@.
-}
getPublicKeyID :: DhtRunnerM Dht InfoHash
getPublicKeyID = infohashFromDhtRunner dhtRunnerGetIdC

foreign import ccall "dht_runner_run" dhtRunnerRunC :: CDhtRunnerPtr -> CInt -> IO CInt

{-| Run the OpenDHT node on a given port.
-}
run :: Word16 -- ^ The port on which to run the DHT node. Use @0@ to let the network layer decide.
    -> DhtRunnerM Dht ()
run port = do
  dhtrunner <- use dhtRunner
  void $ liftIO $ dhtRunnerRunC (_dhtRunnerPtr dhtrunner) (fromIntegral port)

withDhtRunnerConfig :: DhtRunnerConfig -> (Ptr CDhtRunnerConfig -> Dht a) -> Dht a
withDhtRunnerConfig dhtConf dhtActionWithConfig = liftIO $ withCString (dhtConf^.proxyServer)
  $ \ proxyServerPtr     -> withCString (dhtConf^.pushNodeId)
  $ \ pushNodeIdPtr      -> withCString (dhtConf^.pushToken)
  $ \ pushTokenPtr       -> withCString (dhtConf^.pushTopic)
  $ \ pushTopicPtr       -> withCString (dhtConf^.pushPlatform)
  $ \ pushPlatformPtr    -> alloca
  $ \ clientIdentityPtr  -> alloca
  $ \ dhtConfigPtr       -> alloca
  $ \ nodeConfigPtr      -> alloca
  $ \ nodeIdPtr          -> withCString (dhtConf^.dhtConfig.nodeConfig.persistPath)
  $ \ persistPathPtr     -> withCInfohash
  $ \ nodeIdHashPtr      -> withCString (show $ dhtConf^.dhtConfig.nodeConfig.nodeIdHash)
  $ \ nodeIdHashStrPtr   -> alloca
  $ \ dhtRunnerConfigPtr -> unDht $ do
    (CCertificate serverCaPtr) <- Certificate.fromBytes $ dhtConf ^. serverCa
    mClientIdentityPvkPtr      <- runMaybeT $ PrivateKey.fromBytes (dhtConf ^. clientIdentity . privatekey . pvkData)
                                                                   (dhtConf ^. clientIdentity . privatekey . pvkPassword)
    (CCertificate clientIdentityCertPtr) <- Certificate.fromBytes (dhtConf ^. clientIdentity . certificate)
    mNodeIdentityPvkPtr                  <- runMaybeT $ PrivateKey.fromBytes (dhtConf ^. dhtConfig . nodeId . privatekey . pvkData)
                                                                             (dhtConf ^. dhtConfig . nodeId . privatekey . pvkPassword)
    (CCertificate nodeIdentityCertPtr) <- Certificate.fromBytes (dhtConf ^. dhtConfig . nodeId . certificate)
    liftIO $ do
      poke dhtConfigPtr $ CDhtSecureConfig { _nodeIdC     = nodeIdPtr
                                           , _nodeConfigC = nodeConfigPtr
                                           }

      poke nodeIdPtr $ CDhtIdentity { _privatekeyC  = maybe nullPtr _privateKeyPtr mNodeIdentityPvkPtr
                                    , _certificateC = nodeIdentityCertPtr
                                    }

      dhtInfohashFromHexC nodeIdHashPtr nodeIdHashStrPtr
      poke nodeConfigPtr $ CDhtNodeConfig { _persistPathC     = persistPathPtr
                                          , _nodeIdHashC      = nodeIdHashPtr
                                          , _networkC         = fromIntegral $ dhtConf ^. dhtConfig . nodeConfig . network
                                          , _maintainStorageC = dhtConf ^. dhtConfig . nodeConfig . maintainStorage
                                          , _isBootstrapC     = dhtConf ^. dhtConfig . nodeConfig . isBootstrap
                                          }

      poke clientIdentityPtr $ CDhtIdentity { _privatekeyC  = maybe nullPtr _privateKeyPtr mClientIdentityPvkPtr
                                            , _certificateC = clientIdentityCertPtr
                                            }

      poke dhtRunnerConfigPtr $ CDhtRunnerConfig { _dhtConfigC      = dhtConfigPtr
                                                 , _threadedC       = dhtConf ^. threaded
                                                 , _proxyServerC    = proxyServerPtr
                                                 , _pushNodeIdC     = pushNodeIdPtr
                                                 , _pushTokenC      = pushTokenPtr
                                                 , _pushTopicC      = pushTopicPtr
                                                 , _pushPlatformC   = pushPlatformPtr
                                                 , _peerDiscoveryC  = dhtConf ^. peerDiscovery
                                                 , _peerPublishC    = dhtConf ^. peerPublish
                                                 , _serverCaC       = serverCaPtr
                                                 , _clientIdentityC = clientIdentityPtr
                                                 , _loggingC        = dhtConf ^. logging
                                                 }
    dhtActionWithConfig dhtRunnerConfigPtr

foreign import ccall "wr_dht_runner_run_config" dhtRunnerRunConfigC :: CDhtRunnerPtr -> CShort -> Ptr CDhtRunnerConfig -> IO CInt

{-| Run the OpenDHT node on a given port according to the specified
   configuration.
-}
runConfig :: Word16          -- ^ The port on which to run the DHT node. Use @0@ to let the network layer decide.
          -> DhtRunnerConfig -- ^ The DhtRunner configuration.
          -> DhtRunnerM Dht ()
runConfig port config = do
  dhtrunner <- use dhtRunner
  lift $ withDhtRunnerConfig config $ \ configPtr -> do
    void $ liftIO $ dhtRunnerRunConfigC (_dhtRunnerPtr dhtrunner) (fromIntegral port) configPtr

foreign import ccall "dht_runner_is_running" dhtRunnerIsRunningC :: CDhtRunnerPtr -> IO CBool

{-| Indicates whether the underlying node is running. This should yield `True`
   after calling `run` or `runConfig`.
-}
isRunning :: DhtRunnerM Dht Bool
isRunning = use dhtRunner >>= liftIO . (toBool <$>) . dhtRunnerIsRunningC . _dhtRunnerPtr

foreign import ccall "dht_runner_bootstrap" dhtRunnerBootstrapC :: CDhtRunnerPtr -> Ptr CChar -> Ptr CChar -> IO ()

{-| Connect to the OpenDHT network before doing any operation.
-}
bootstrap :: String -- ^ The hostname (or IP address) used to bootstrap the connection to the network.
          -> String -- ^ The remote bootstrapping node port to use to connect.
          -> DhtRunnerM Dht ()
bootstrap addr port = do
  dhtrunner <- use dhtRunner
  liftIO $ withCString addr $ \ addrCPtr ->
           withCString port $ \ portCPtr -> dhtRunnerBootstrapC (_dhtRunnerPtr dhtrunner) addrCPtr portCPtr

foreign import ccall "dht_runner_get"
  dhtRunnerGetC :: CDhtRunnerPtr -> CInfoHashPtr  -> FunPtr (CGetCallback a) -> FunPtr (CDoneCallback a) -> Ptr a -> IO ()

{-| Get a `Value` pointed at by a given hash on the DHT.
-}
get :: Storable userdata
    => InfoHash              -- ^ The hash for which to get data at.
    -> GetCallback userdata  -- ^ The callback invoked for all values retrieved on the DHT for the given hash.
    -> DoneCallback userdata -- ^ The callback invoked when OpenDHT has completed the get request.
    -> userdata              -- ^ Some user data to be passed to callbacks.
    -> DhtRunnerM Dht ()
get h gcb dcb userdata = use dhtRunner >>= \ dhtrunner -> liftIO $ do
  withCString (show h) $ \ hStrPtr -> withCInfohash $ \ hPtr -> with userdata $ \ userdataPtr -> do
    dhtInfohashFromHexC hPtr hStrPtr
    gcbCWrapped <- wrapGetCallbackC $ fromGetCallBack gcb
    dcbCWrapped <- wrapDoneCallbackC $ fromDoneCallback dcb
    dhtRunnerGetC (_dhtRunnerPtr dhtrunner) hPtr gcbCWrapped dcbCWrapped userdataPtr

foreign import ccall "dht_runner_put"
  dhtRunnerPutC :: CDhtRunnerPtr -> CInfoHashPtr -> CValuePtr -> FunPtr (CDoneCallback a) -> Ptr a -> CBool -> IO ()

{-| Put a `Value` on the DHT for a given hash.
-}
put :: Storable userdata
    => InfoHash              -- ^ The hash under which to store the value.
    -> Value                 -- ^ The value to put on the DHT.
    -> DoneCallback userdata -- ^ The callback to invoke when the request is completed (or has failed).
    -> userdata              -- ^ User data to pass to the callback.
    -> Bool                  -- ^ Whether the value should be "permanent". A permanent value is
                             --   reannounced automatically after it has expired (after 10 minutes). __NOTE__: This requires
                             --   node to keep running.
    -> DhtRunnerM Dht ()
put _ (StoredValue {}) _ _ _                           = error "DhtRunner.put needs to be fed an InputValue!"
put h (InputValue vbs usertype) dcb userdata permanent = use dhtRunner >>= \ dhtrunner -> liftIO $ do
  withCString (show h) $ \ hStrPtr -> withCInfohash $ \ hPtr -> with userdata $ \ userdataPtr -> do
    dhtInfohashFromHexC hPtr hStrPtr
    dcbCWrapped <- wrapDoneCallbackC $ fromDoneCallback dcb
    vPtr <- unDht $ valuePtrFromBytes vbs
    unDht $ setValueUserType vPtr usertype
    dhtRunnerPutC (_dhtRunnerPtr dhtrunner) hPtr vPtr dcbCWrapped userdataPtr (fromBool permanent)

foreign import ccall "dht_runner_listen"
  dhtRunnerListenC :: CDhtRunnerPtr -> CInfoHashPtr -> FunPtr (CValueCallback a) -> FunPtr (CShutdownCallback a) -> Ptr a -> IO (Ptr ())

foreign import ccall "dht_runner_cancel_put" dhtRunnerCancelPutC :: CDhtRunnerPtr -> CInfoHashPtr -> CULLong -> IO ()

{-| Cancel a Put request.

  This function is useful for cancelling a "permanent" Put request. Therefore,
  if the user puts a permanent value on the network, he should think about
  storing the value ID then.
-}
cancelPut :: InfoHash -- ^ The hash for which the value was first put.
          -> Word64   -- ^ The value ID.
          -> DhtRunnerM Dht ()
cancelPut h vid = use dhtRunner >>= \ dhtrunner -> liftIO $ do
  withCString (show h) $ \ hStrPtr -> withCInfohash $ \ hPtr -> do
    dhtInfohashFromHexC hPtr hStrPtr
    dhtRunnerCancelPutC (_dhtRunnerPtr dhtrunner) hPtr (CULLong vid)

{-| Initiate a Listen operation for a given hash.

   * The `ValueCallback` will be invoked once for every value found and once
   also when each of these same values expire on the DHT.
   * While the Listen operation is not cancelled (or the node shutdown), it goes
   on. Threfore, values subsequently published will be received.
   * When `listen` terminates, an `OpToken` is added to the map of tokens (`listenTokens`)
   for the given hash in the `DhtRunnerState`.
-}
listen :: Storable userdata
       => InfoHash                  -- ^ The hash indicating where to listen to.
       -> ValueCallback userdata    -- ^ The callback to invoke when a value is found or has expired.
       -> ShutdownCallback userdata -- ^ The callback to invoke before the OpenDHT node shuts down.
       -> userdata                  -- ^ User data to pass to the callback.
       -> DhtRunnerM Dht ()
listen h vcb scb userdata = do
  dhtrunner <- use dhtRunner
  tokensMap <- use listenTokens
  t <- liftIO $ do
    withCString (show h) $ \ hStrPtr -> withCInfohash $ \ hPtr -> with userdata $ \ userdataPtr -> do
      dhtInfohashFromHexC hPtr hStrPtr
      vcbCWrapped <- wrapValueCallbackC $ fromValueCallBack vcb
      scbCWrapped <- wrapShutdownCallbackC $ fromShutdownCallback scb
      OpToken <$> dhtRunnerListenC (_dhtRunnerPtr dhtrunner) hPtr vcbCWrapped scbCWrapped userdataPtr
  let mtokens   = tokensMap ^.at h
      newTokens = maybe [t] (t:) mtokens
  listenTokens %= Map.insert h newTokens

foreign import ccall "dht_runner_cancel_listen" dhtRunnerCancelListenC :: CDhtRunnerPtr -> CInfoHashPtr -> COpTokenPtr -> IO ()

{-| Cancel an on-going Listen operation.

   If no Listen request is found for the given arguments, the function returns
   and does nothing.
-}
cancelListen :: InfoHash -- ^ The hash for which the Listen request was previously issued.
             -> OpToken  -- ^ The token associated identifying the exact Listen operation.
             -> MaybeT (DhtRunnerM Dht) ()
cancelListen h t = do
  dhtrunner <- lift $ use dhtRunner
  tokensMap <- lift $ use listenTokens
  tokens    <- MaybeT $ return $ tokensMap ^.at h
  i         <- MaybeT $ return $ List.elemIndex t tokens
  case splitAt i tokens of
    (beg, t':rest) -> do
      liftIO $ withCString (show h) $ \ hStrPtr -> withCInfohash $ \ hPtr -> do
        dhtInfohashFromHexC hPtr hStrPtr
        dhtRunnerCancelListenC (_dhtRunnerPtr dhtrunner) hPtr (_opTokenPtr t')
      lift $ listenTokens %= Map.insert h (beg++rest)
    (_, [])        -> error "cancelListen: the token list should not have been empty."

foreign import ccall "dht_runner_shutdown" dhtRunnerShutdownC :: CDhtRunnerPtr -> FunPtr (CShutdownCallback a) -> Ptr a -> IO ()

{-| Gracefuly shutdown the DHT node. This function should be the last function
   called before exiting the context of `DhtRunnerM Dht`.
-}
shutdown :: Storable userdata
         => ShutdownCallback userdata -- ^ The callback to invoke before the OpenDHT node shuts down.
         -> userdata                  -- ^ User data to pass to the callback.
         -> DhtRunnerM Dht ()
shutdown scb userdata = do
  dhtrunner <- use dhtRunner
  liftIO $ with userdata $ \ userdataPtr -> do
    scbCWrapped <- wrapShutdownCallbackC $ fromShutdownCallback scb
    dhtRunnerShutdownC (_dhtRunnerPtr dhtrunner) scbCWrapped userdataPtr

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

