
#include <opendht/opendht_c.h>

// wrapper around dht_node_config
typedef struct {
    dht_infohash* node_id;
    uint32_t      network;
    bool          is_bootstrap;
    bool          maintain_storage;
    const char*   persist_path;
} wr_dht_node_config;

dht_node_config*    from_wr_dht_node_config(dht_node_config* dst, wr_dht_node_config* const src);

// wrapper around dht_secure_config
typedef struct {
    wr_dht_node_config* node_config;
    dht_identity*    id;
} wr_dht_secure_config;

dht_secure_config*    from_wr_dht_secure_config(dht_secure_config* dst, wr_dht_secure_config* const src);

// wrapper around dht_runner_config
typedef struct {
    wr_dht_secure_config* dht_config;
    bool                  threaded;
    const char*           proxy_server;
    const char*           push_node_id;
    const char*           push_token;
    const char*           push_topic;
    const char*           push_platform;
    bool                  peer_discovery;
    bool                  peer_publish;
    dht_certificate*      server_ca;
    dht_identity*         client_identity;
    bool                  log;
} wr_dht_runner_config;

dht_runner_config*    from_wr_dht_runner_config(dht_runner_config* dst, wr_dht_runner_config* const src);

//  vim: set sts=4 ts=4 sw=4 tw=120 et :

