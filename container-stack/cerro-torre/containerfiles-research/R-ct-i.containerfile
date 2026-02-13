# SPDX-License-Identifier: PMPL-1.0-or-later
# R-ct-i: Research infrastructure variant (Idris2 + Ada/SPARK verification)

FROM ghcr.io/alire-project/alire:latest AS builder
WORKDIR /build

# Install dependencies including Idris2
RUN apt-get update && apt-get install -y \
    gnat-13 gprbuild libcurl4-openssl-dev \
    idris2 \
    && rm -rf /var/lib/apt/lists/*

COPY . /build

# Build with Idris2 verification enabled
RUN alr build --release -- \
    -XCERRO_FEATURES=infrastructure \
    -XCERRO_BUILD_MODE=Release \
    -XCERRO_VERIFICATION=idris2

# Run Idris2 proofs
RUN cd verification/idris && idris2 --check Proofs.idr

# Final image
FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y \
    ca-certificates libcurl4 curl \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /build/bin/ct /usr/local/bin/
RUN ct --version

ENV CT_VARIANT=research-infrastructure
ENV CT_VERIFICATION=idris2+spark
LABEL org.opencontainers.image.description="Cerro Torre Research Build (Idris2+SPARK) - Infrastructure variant"

ENTRYPOINT ["/usr/local/bin/ct"]
CMD ["--help"]
