# SPDX-License-Identifier: PMPL-1.0-or-later
# R-ct-a: Research attestation variant (Idris2 + Ada/SPARK verification)

FROM ghcr.io/alire-project/alire:latest AS builder
WORKDIR /build

# Install crypto dependencies + Idris2
RUN apt-get update && apt-get install -y \
    gnat-13 \
    gprbuild \
    libcurl4-openssl-dev \
    libssl-dev \
    idris2 \
    && rm -rf /var/lib/apt/lists/*

COPY . /build

# Build with Idris2 verification enabled
RUN alr build --release -- \
    -XCERRO_FEATURES=attestation \
    -XCERRO_BUILD_MODE=Release \
    -XCERRO_VERIFICATION=idris2

# Run Idris2 proofs (including crypto proofs)
RUN cd verification/idris && \
    idris2 --check Proofs.idr && \
    idris2 --check CryptoProofs.idr && \
    idris2 --check SignatureProofs.idr

# Final image
FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y \
    ca-certificates libcurl4 libssl3 curl \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /build/bin/ct /usr/local/bin/
RUN mkdir -p /etc/ct/keys /var/cache/ct

ENV CT_VARIANT=research-attestation
ENV CT_VERIFICATION=idris2+spark
ENV CT_TRUST_STORE=/etc/ct/trust-store
ENV CT_KEY_DIR=/etc/ct/keys
LABEL org.opencontainers.image.description="Cerro Torre Research Build (Idris2+SPARK) - Attestation variant"

ENTRYPOINT ["/usr/local/bin/ct"]
CMD ["--help"]
