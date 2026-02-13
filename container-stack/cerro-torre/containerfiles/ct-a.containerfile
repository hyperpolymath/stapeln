# SPDX-License-Identifier: PMPL-1.0-or-later
# ct-a: Attestation variant (signing + transparency logs)

FROM ghcr.io/alire-project/alire:latest AS builder
WORKDIR /build

# Install crypto dependencies
RUN apt-get update && apt-get install -y \
    gnat-13 \
    gprbuild \
    libcurl4-openssl-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

COPY . /build
RUN alr build --release -- \
    -XCERRO_FEATURES=attestation \
    -XCERRO_BUILD_MODE=Release

# Final image
FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y \
    ca-certificates libcurl4 libssl3 curl \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /build/bin/ct /usr/local/bin/
RUN mkdir -p /etc/ct/keys /var/cache/ct

ENV CT_VARIANT=standard-attestation
ENV CT_VERIFICATION=spark
ENV CT_TRUST_STORE=/etc/ct/trust-store
ENV CT_KEY_DIR=/etc/ct/keys
LABEL org.opencontainers.image.description="Cerro Torre Standard Build (SPARK only) - Attestation variant"

ENTRYPOINT ["/usr/local/bin/ct"]
CMD ["--help"]
