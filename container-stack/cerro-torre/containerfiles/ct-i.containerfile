# SPDX-License-Identifier: PMPL-1.0-or-later
# ct-i: Infrastructure variant (registry + basic packing)

FROM ghcr.io/alire-project/alire:latest AS builder
WORKDIR /build

# Install minimal dependencies
RUN apt-get update && apt-get install -y \
    gnat-13 gprbuild libcurl4-openssl-dev \
    && rm -rf /var/lib/apt/lists/*

COPY . /build
RUN alr build --release -- \
    -XCERRO_FEATURES=infrastructure \
    -XCERRO_BUILD_MODE=Release

# Final image
FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y \
    ca-certificates libcurl4 curl \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /build/bin/ct /usr/local/bin/
RUN ct --version

ENV CT_VARIANT=standard-infrastructure
ENV CT_VERIFICATION=spark
LABEL org.opencontainers.image.description="Cerro Torre Standard Build (SPARK only) - Infrastructure variant"

ENTRYPOINT ["/usr/local/bin/ct"]
CMD ["--help"]
