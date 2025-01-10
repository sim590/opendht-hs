
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
                         , DhtRunnerM (..)
                         , initialize
                         , run
                         , runDhtRunnerM
                         ) where

import qualified Data.ByteString as BS

import Control.Monad.State

import Foreign.Ptr
import Foreign.C.Types

import OpenDHT.Types
import OpenDHT.InfoHash
import OpenDHT.Internal.DhtRunner

type DoneCallBack = Bool -> IO ()
type CDoneCallBack = CBool -> Ptr () -> IO ()
foreign import ccall safe "wrapper" wrapDoneCallBack :: CDoneCallBack -> IO (FunPtr CDoneCallBack)

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

newtype DhtRunnerM a = DhtRunnerM { unwrapDhtRunnerM :: StateT DhtRunner Dht a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState DhtRunner)

foreign import ccall "dht_runner_new" dhtRunnerNewC :: CDhtRunnerPtr

{-| Initialize an OpenDHT node.
-}
initialize :: Dht DhtRunner
initialize = return $ DhtRunner dhtRunnerNewC

foreign import ccall "dht_runner_run" dhtRunnerRunC :: CDhtRunnerPtr -> CInt -> IO CInt

{-| Run the OpenDHT node on a given port.
-}
run :: Int -- ^ The port on which to run the DHT node.
    -> DhtRunnerM ()
run port = do
  dhtrunner <- get
  void $ liftIO $ dhtRunnerRunC (dhtRunnerPtr dhtrunner) (fromIntegral port)

foreign import ccall "dht_runner_bootstrap" dhtRunnerBootstrapC :: CDhtRunnerPtr -> Ptr CChar -> Ptr CChar -> IO ()

-- TODO
bootstrap :: String -> String -> DhtRunnerM ()
bootstrap addr port = undefined

-- TODO: initialiser DhtRunner
runDhtRunnerM :: DhtRunnerM a -> IO a
runDhtRunnerM runnerAction = undefined
-- runDhtRunnerM runnerAction = unDht $ evalStateT (unwrapDhtRunnerM runnerAction) (DhtRunner _)

makeDhtRunnerConfig :: DhtRunnerConfig -> Dht CDhtRunnerConfig
makeDhtRunnerConfig dhtConf = undefined

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

