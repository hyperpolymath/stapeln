# SPDX-License-Identifier: PMPL-1.0-or-later
# ct: Complete variant (all features)

FROM ghcr.io/alire-project/alire:latest AS builder
WORKDIR /build

# Install all dependencies
RUN apt-get update && apt-get install -y \
    gnat-13 \
    gprbuild \
    libcurl4-openssl-dev \
    libssl-dev \
    debootstrap \
    rpm \
    && rm -rf /var/lib/apt/lists/*

COPY . /build
RUN alr build --release -- \
    -XCERRO_FEATURES=complete \
    -XCERRO_BUILD_MODE=Release

# Final image
FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y \
    ca-certificates \
    libcurl4 \
    libssl3 \
    curl \
    debootstrap \
    rpm \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /build/bin/ct /usr/local/bin/
RUN mkdir -p /etc/ct/keys /var/cache/ct /var/lib/ct

ENV CT_VARIANT=standard-complete
ENV CT_VERIFICATION=spark
ENV CT_TRUST_STORE=/etc/ct/trust-store
ENV CT_KEY_DIR=/etc/ct/keys
ENV CT_CACHE_DIR=/var/cache/ct
LABEL org.opencontainers.image.description="Cerro Torre Standard Build (SPARK only) - Complete variant"

ENTRYPOINT ["/usr/local/bin/ct"]
CMD ["--help"]
