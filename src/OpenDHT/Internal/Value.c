
#include <string.h>
#include <opendht/opendht_c.h>

void wr_dht_value_get_data(dht_data_view* dv, const dht_value* data) {
    dht_data_view value_dv = dht_value_get_data(data);
    dv->data = value_dv.data;
    dv->size = value_dv.size;
}

void wr_dht_value_get_recipient(dht_infohash* h, const dht_value* data) {
    dht_infohash value_r_infohash = dht_value_get_recipient(data);
    memcpy(&h->d, &value_r_infohash.d, HASH_LEN);
}

/*  vim: set sts=4 ts=4 sw=4 tw=120 et : */

