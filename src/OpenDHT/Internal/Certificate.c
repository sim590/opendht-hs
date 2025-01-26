
#include <string.h>

#include <opendht/opendht_c.h>

void wr_dht_certificate_get_id(const dht_certificate* cert, dht_infohash* wr_h) {
    dht_infohash h = dht_certificate_get_id(cert);
    memcpy(wr_h->d, h.d, HASH_LEN);
}

void wr_dht_certificate_get_long_id(const dht_certificate* cert, dht_pkid* wr_pkid) {
    dht_pkid pkid = dht_certificate_get_long_id(cert);
    memcpy(wr_pkid->d, pkid.d, 32);
}

