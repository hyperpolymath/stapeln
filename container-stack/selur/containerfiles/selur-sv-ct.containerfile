# SPDX-License-Identifier: PMPL-1.0-or-later
# Complete verified container stack: svalinn + vordr + cerro-torre

# ============================================================
# Stage 1: Build Vörðr (Rust)
# ============================================================
FROM rust:1.75-bookworm AS vordr-builder
WORKDIR /build/vordr
COPY ../vordr/src/rust /build/vordr
RUN cargo build --release --bin vordr

# ============================================================
# Stage 2: Build Svalinn (Deno/ReScript)
# ============================================================
FROM denoland/deno:1.40 AS svalinn-builder
WORKDIR /build/svalinn
COPY ../svalinn/src /build/svalinn
RUN deno task build

# ============================================================
# Stage 3: Build Selur Bridge (WASM)
# ============================================================
FROM rust:1.75-bookworm AS selur-builder
WORKDIR /build/selur
COPY . /build/selur
RUN rustup target add wasm32-wasi && \
    cargo build --release --target wasm32-wasi

# ============================================================
# Stage 4: Build Cerro Torre (Ada/SPARK)
# ============================================================
FROM ghcr.io/alire-project/alire:latest AS ct-builder
WORKDIR /build/ct

# Install GNAT and dependencies
RUN apt-get update && apt-get install -y \
    gnat-13 \
    gprbuild \
    libcurl4-openssl-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

COPY ../cerro-torre /build/ct
RUN alr build --release

# ============================================================
# Stage 5: Assemble Final Image
# ============================================================
FROM debian:bookworm-slim

# Runtime dependencies
RUN apt-get update && apt-get install -y \
    ca-certificates \
    libcurl4 \
    libssl3 \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Copy all binaries
COPY --from=vordr-builder /build/vordr/target/release/vordr /usr/local/bin/
COPY --from=svalinn-builder /build/svalinn/dist/svalinn /usr/local/bin/
COPY --from=selur-builder /build/selur/target/wasm32-wasi/release/selur.wasm /usr/local/lib/
COPY --from=ct-builder /build/ct/bin/ct /usr/local/bin/

# Copy integration scripts
COPY scripts/selur-svct-launcher /usr/local/bin/
COPY scripts/selur-svct /usr/local/bin/
RUN chmod +x /usr/local/bin/selur-*

# Configuration
ENV VORDR_ENDPOINT=selur://unix:///run/selur-sv.sock
ENV SELUR_WASM=/usr/local/lib/selur.wasm
ENV SVALINN_PORT=8000
ENV CT_TRUST_STORE=/etc/selur/trust-store

# Create directories
RUN mkdir -p /var/lib/selur-sv /etc/selur /var/cache/ct

# Expose ports
EXPOSE 8000

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:8000/healthz || exit 1

CMD ["/usr/local/bin/selur-svct-launcher"]
