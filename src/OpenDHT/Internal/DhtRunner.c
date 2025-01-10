
#include "DhtRunner.h"

wr_dht_node_config* to_wr_dht_node_config(wr_dht_node_config* dst, dht_node_config* const src)
{
    dst->node_id          = &(src->node_id);
    dst->network          = src->network;
    dst->is_bootstrap     = src->is_bootstrap;
    dst->maintain_storage = src->maintain_storage;
    dst->persist_path     = src->persist_path;
    return dst;
}

dht_node_config* from_wr_dht_node_config(dht_node_config* dst, wr_dht_node_config* const src)
{
    dst->node_id          = *src->node_id;
    dst->network          = src->network;
    dst->is_bootstrap     = src->is_bootstrap;
    dst->maintain_storage = src->maintain_storage;
    dst->persist_path     = src->persist_path;
    return dst;
}

wr_dht_secure_config* to_wr_dht_secure_config(wr_dht_secure_config* dst, dht_secure_config* const src)
{
    dst->node_config = &(src->node_config);
    dst->id          = &(src->id);
    return dst;
}

dht_secure_config* from_wr_dht_secure_config(dht_secure_config* dst, wr_dht_secure_config* const src)
{
    dst->node_config = *src->node_config;
    dst->id          = *src->id;
    return dst;
}

wr_dht_runner_config* to_wr_dht_runner_config(wr_dht_runner_config* dst, dht_runner_config* const src)
{
    dst->dht_config      = &(src->dht_config);
    dst->threaded        = src->threaded;
    dst->proxy_server    = src->proxy_server;
    dst->push_node_id    = src->push_node_id;
    dst->push_token      = src->push_token;
    dst->push_topic      = src->push_topic;
    dst->push_platform   = src->push_platform;
    dst->peer_discovery  = src->peer_discovery;
    dst->peer_publish    = src->peer_publish;
    dst->server_ca       = src->server_ca;
    dst->client_identity = &(src->client_identity);
    dst->log             = src->log;
    return dst;
}

dht_runner_config* from_wr_dht_runner_config(dht_runner_config* dst, wr_dht_runner_config* const src)
{
    dst->dht_config      = *src->dht_config;
    dst->threaded        = src->threaded;
    dst->proxy_server    = src->proxy_server;
    dst->push_node_id    = src->push_node_id;
    dst->push_token      = src->push_token;
    dst->push_topic      = src->push_topic;
    dst->push_platform   = src->push_platform;
    dst->peer_discovery  = src->peer_discovery;
    dst->peer_publish    = src->peer_publish;
    dst->server_ca       = src->server_ca;
    dst->client_identity = *src->client_identity;
    dst->log             = src->log;
    return dst;
}

//  vim: set sts=4 ts=4 sw=4 tw=120 et :

