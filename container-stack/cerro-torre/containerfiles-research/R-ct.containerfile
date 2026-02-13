# SPDX-License-Identifier: PMPL-1.0-or-later
# R-ct: Research complete variant (Idris2 + Ada/SPARK verification)

FROM ghcr.io/alire-project/alire:latest AS builder
WORKDIR /build

# Install all dependencies + Idris2
RUN apt-get update && apt-get install -y \
    gnat-13 \
    gprbuild \
    libcurl4-openssl-dev \
    libssl-dev \
    debootstrap \
    rpm \
    idris2 \
    && rm -rf /var/lib/apt/lists/*

COPY . /build

# Build with Idris2 verification enabled
RUN alr build --release -- \
    -XCERRO_FEATURES=complete \
    -XCERRO_BUILD_MODE=Release \
    -XCERRO_VERIFICATION=idris2

# Run comprehensive Idris2 proofs
RUN cd verification/idris && \
    idris2 --check Proofs.idr && \
    idris2 --check CryptoProofs.idr && \
    idris2 --check SignatureProofs.idr && \
    idris2 --check ImporterProofs.idr && \
    idris2 --check Theorems.idr

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

ENV CT_VARIANT=research-complete
ENV CT_VERIFICATION=idris2+spark
ENV CT_TRUST_STORE=/etc/ct/trust-store
ENV CT_KEY_DIR=/etc/ct/keys
ENV CT_CACHE_DIR=/var/cache/ct
LABEL org.opencontainers.image.description="Cerro Torre Research Build (Idris2+SPARK) - Complete variant"

ENTRYPOINT ["/usr/local/bin/ct"]
CMD ["--help"]
