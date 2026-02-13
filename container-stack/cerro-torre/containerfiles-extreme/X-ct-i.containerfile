# SPDX-License-Identifier: PMPL-1.0-or-later
# X-ct-i: eXtreme security infrastructure variant (ATS2 + Ada/SPARK verification)

FROM ghcr.io/alire-project/alire:latest AS builder
WORKDIR /build

# Install dependencies including ATS2
RUN apt-get update && apt-get install -y \
    gnat-13 gprbuild libcurl4-openssl-dev \
    ats2-lang \
    && rm -rf /var/lib/apt/lists/*

COPY . /build

# Build with ATS2 linear type verification enabled
RUN alr build --release -- \
    -XCERRO_FEATURES=infrastructure \
    -XCERRO_BUILD_MODE=Release \
    -XCERRO_VERIFICATION=ats2

# Compile and verify ATS2 shadow verifier
RUN cd verification/ats && \
    patscc -DATS_MEMALLOC_LIBC -tc shadow_verifier.dats && \
    patsopt -tc -d shadow_verifier.dats

# Final image
FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y \
    ca-certificates libcurl4 curl \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /build/bin/ct /usr/local/bin/
COPY --from=builder /build/verification/ats/shadow_verifier /usr/local/bin/ct-shadow
RUN ct --version && ct-shadow --version

ENV CT_VARIANT=extreme-infrastructure
ENV CT_VERIFICATION=ats2+spark
ENV CT_SHADOW_VERIFIER=/usr/local/bin/ct-shadow
LABEL org.opencontainers.image.description="Cerro Torre eXtreme Security Build (ATS2+SPARK) - Infrastructure variant"

ENTRYPOINT ["/usr/local/bin/ct"]
CMD ["--help"]
