
#include <string.h>

#include <opendht/opendht_c.h>

void wr_dht_publickey_get_id(const dht_publickey* pk, dht_infohash* wr_h) {
    dht_infohash h = dht_publickey_get_id(pk);
    memcpy(wr_h->d, h.d, HASH_LEN);
}

void wr_dht_publickey_get_long_id(const dht_publickey* pk, dht_pkid* wr_pkid) {
    dht_pkid pkid = dht_publickey_get_long_id(pk);
    memcpy(wr_pkid->d, pkid.d, 32);
}

