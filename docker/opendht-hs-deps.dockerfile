
FROM ghcr.io/savoirfairelinux/opendht/opendht-deps:latest
LABEL maintainer="Simon Désaulniers <sim.desaulniers@gmail.com>"
LABEL org.opencontainers.image.source="https://github.com/sim590/opendht-hs"

RUN git clone https://github.com/savoirfairelinux/opendht/ \
    && cd opendht \
    && git checkout v3.3.0 \
    && mkdir build && cd build \
	&& cmake .. -DCMAKE_INSTALL_PREFIX=/usr \
				-DCMAKE_INTERPROCEDURAL_OPTIMIZATION=On \
				-DOPENDHT_C=On \
				-DOPENDHT_PROXY_CLIENT=On \
	&& make -j8 && make install \
	&& cd ../.. && rm -rf opendht

