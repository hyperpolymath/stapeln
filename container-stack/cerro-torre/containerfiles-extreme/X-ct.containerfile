# SPDX-License-Identifier: PMPL-1.0-or-later
# X-ct: eXtreme security complete variant (ATS2 + Ada/SPARK verification)

FROM ghcr.io/alire-project/alire:latest AS builder
WORKDIR /build

# Install all dependencies + ATS2
RUN apt-get update && apt-get install -y \
    gnat-13 \
    gprbuild \
    libcurl4-openssl-dev \
    libssl-dev \
    debootstrap \
    rpm \
    ats2-lang \
    && rm -rf /var/lib/apt/lists/*

COPY . /build

# Build with ATS2 linear type verification enabled
RUN alr build --release -- \
    -XCERRO_FEATURES=complete \
    -XCERRO_BUILD_MODE=Release \
    -XCERRO_VERIFICATION=ats2

# Compile and verify comprehensive ATS2 shadow verifier
RUN cd verification/ats && \
    patscc -DATS_MEMALLOC_LIBC -tc shadow_verifier.dats && \
    patscc -DATS_MEMALLOC_LIBC -tc crypto_linear.dats && \
    patscc -DATS_MEMALLOC_LIBC -tc signature_linear.dats && \
    patscc -DATS_MEMALLOC_LIBC -tc importer_linear.dats && \
    patsopt -tc -d shadow_verifier.dats && \
    patsopt -tc -d crypto_linear.dats && \
    patsopt -tc -d signature_linear.dats && \
    patsopt -tc -d importer_linear.dats

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
COPY --from=builder /build/verification/ats/shadow_verifier /usr/local/bin/ct-shadow
RUN mkdir -p /etc/ct/keys /var/cache/ct /var/lib/ct

ENV CT_VARIANT=extreme-complete
ENV CT_VERIFICATION=ats2+spark
ENV CT_SHADOW_VERIFIER=/usr/local/bin/ct-shadow
ENV CT_TRUST_STORE=/etc/ct/trust-store
ENV CT_KEY_DIR=/etc/ct/keys
ENV CT_CACHE_DIR=/var/cache/ct
LABEL org.opencontainers.image.description="Cerro Torre eXtreme Security Build (ATS2+SPARK) - Complete variant"

ENTRYPOINT ["/usr/local/bin/ct"]
CMD ["--help"]
