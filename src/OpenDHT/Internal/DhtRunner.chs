
{-|
  Module      : OpenDHT.Internal.DhtRunner
  Description : Internal definitions for DhtRunner.
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com
-}

module OpenDHT.Internal.DhtRunner where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable

import OpenDHT.Internal.Value
import OpenDHT.Internal.InfoHash

#include <opendht/opendht_c.h>
#include "DhtRunner.h"

type CGetCallback a = CValuePtr -> Ptr a -> IO CBool
foreign import ccall safe "wrapper" wrapGetCallback :: CGetCallback a-> IO (FunPtr (CGetCallback a))

type CDoneCallback a = CBool -> Ptr a -> IO ()
foreign import ccall safe "wrapper" wrapDoneCallback :: CDoneCallback a-> IO (FunPtr (CDoneCallback a))

type CDhtPrivatekey  = ()
type CDhtCertificate = ()

-- struct OPENDHT_PUBLIC dht_identity {
--     dht_privatekey* privatekey;
--     dht_certificate* certificate;
-- };
data CDhtIdentity = CDhtIdentity { _privatekeyC  :: Ptr CDhtPrivatekey
                                 , _certificateC :: Ptr CDhtCertificate
                                 }

instance Storable CDhtIdentity where
    sizeOf _    = {# sizeof dht_identity  #}
    alignment _ = {# alignof dht_identity #}
    poke        = poke
    peek p      = CDhtIdentity <$> {# get dht_identity->privatekey  #} p
                               <*> {# get dht_identity->certificate #} p

-- from: DhtRunner.h
-- --
-- typedef struct {
--     dht_infohash* node_id;
--     uint32_t network;
--     bool is_bootstrap;
--     bool maintain_storage;
--     const char* persist_path;
-- } wr_dht_node_config;
data CDhtNodeConfig = CDhtNodeConfig { _nodeIdC          :: CInfoHashPtr
                                     , _networkC         :: CUInt
                                     , _isBootstrapC     :: Bool
                                     , _maintainStorageC :: Bool
                                     , _persistPathC     :: Ptr CChar
                                     }

instance Storable CDhtNodeConfig where
    sizeOf _    = {# sizeof wr_dht_node_config  #}
    alignment _ = {# alignof wr_dht_node_config #}
    poke        = poke
    peek p      = CDhtNodeConfig <$> peekByteOff p 0
                                 <*> {# get wr_dht_node_config->network          #} p
                                 <*> {# get wr_dht_node_config->is_bootstrap     #} p
                                 <*> {# get wr_dht_node_config->maintain_storage #} p
                                 <*> {# get wr_dht_node_config->persist_path     #} p

-- from: DhtRunner.h
-- --
-- typedef struct {
--     dht_node_config* node_config;
--     dht_identity*    id;
-- } wr_dht_secure_config;
data CDhtSecureConfig = CDhtSecureConfig { _nodeConfigC :: CDhtNodeConfigPtr
                                         , _idC         :: CDhtIdentityPtr
                                         }

{# pointer *dht_node_config as CDhtNodeConfigPtr -> CDhtNodeConfig #}
{# pointer *dht_identity    as CDhtIdentityPtr   -> CDhtIdentity   #}

instance Storable CDhtSecureConfig where
    sizeOf _    = {# sizeof wr_dht_secure_config  #}
    alignment _ = {# alignof wr_dht_secure_config #}
    poke        = poke
    peek p      = CDhtSecureConfig <$> {# get wr_dht_secure_config.node_config #} p
                                   <*> {# get wr_dht_secure_config.id          #} p

-- from: DhtRunner.h
-- --
-- typedef struct {
--     dht_secure_config* dht_config;
--     bool threaded;
--     const char* proxy_server;
--     const char* push_node_id;
--     const char* push_token;
--     const char* push_topic;
--     const char* push_platform;
--     bool peer_discovery;
--     bool peer_publish;
--     dht_certificate* server_ca;
--     dht_identity* client_identity;
--     bool log;
-- } wr_dht_runner_config;
data CDhtRunnerConfig = CDhtRunnerConfig { _dhtConfigC      :: CDhtSecureConfigPtr
                                         , _threadedC       :: Bool
                                         , _proxyServerC    :: Ptr CChar
                                         , _pushNodeIdC     :: Ptr CChar
                                         , _pushTokenC      :: Ptr CChar
                                         , _pushTopicC      :: Ptr CChar
                                         , _pushPlatformC   :: Ptr CChar
                                         , _peerDiscoveryC  :: Bool
                                         , _peerPublishC    :: Bool
                                         , _serverCaC       :: Ptr CDhtCertificate
                                         , _clientIdentityC :: Ptr CDhtIdentity
                                         , _logC            :: Bool
                                         }

{# pointer *dht_secure_config as CDhtSecureConfigPtr -> CDhtSecureConfig #}

instance Storable CDhtRunnerConfig where
    sizeOf _    = {# sizeof wr_dht_runner_config #}
    alignment _ = {# alignof wr_dht_runner_config #}
    poke        = poke
    peek p      = CDhtRunnerConfig <$> {# get wr_dht_runner_config.dht_config       #} p
                                   <*> {# get wr_dht_runner_config->threaded        #} p
                                   <*> {# get wr_dht_runner_config->proxy_server    #} p
                                   <*> {# get wr_dht_runner_config->push_node_id    #} p
                                   <*> {# get wr_dht_runner_config->push_token      #} p
                                   <*> {# get wr_dht_runner_config->push_topic      #} p
                                   <*> {# get wr_dht_runner_config->push_platform   #} p
                                   <*> {# get wr_dht_runner_config->peer_discovery  #} p
                                   <*> {# get wr_dht_runner_config->peer_publish    #} p
                                   <*> {# get wr_dht_runner_config->server_ca       #} p
                                   <*> {# get wr_dht_runner_config.client_identity  #} p
                                   <*> {# get wr_dht_runner_config->log             #} p

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

