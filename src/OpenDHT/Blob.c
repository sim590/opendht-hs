
#include <opendht/opendht_c.h>

void dht_blob_get_data_view(dht_data_view* data_view, const dht_blob* data) {
    dht_data_view data_view_ = dht_blob_get_data(data);
    data_view->data = data_view_.data;
    data_view->size = data_view_.size;
};

/*  vim: set sts=4 ts=4 sw=4 tw=120 et : */

