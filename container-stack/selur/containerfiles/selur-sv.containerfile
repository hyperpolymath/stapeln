# SPDX-License-Identifier: PMPL-1.0-or-later
# Multi-stage build for selur(svalinn+vordr)

# ============================================================
# Stage 1: Build vordr (Rust)
# ============================================================
FROM rust:1.75-bookworm AS vordr-builder
WORKDIR /build/vordr
COPY ../vordr/src/rust /build/vordr
RUN cargo build --release --bin vordr

# ============================================================
# Stage 2: Build svalinn (Deno/ReScript)
# ============================================================
FROM denoland/deno:1.40 AS svalinn-builder
WORKDIR /build/svalinn
COPY ../svalinn/src /build/svalinn
RUN deno task build

# ============================================================
# Stage 3: Build selur WASM bridge
# ============================================================
FROM rust:1.75-bookworm AS selur-builder
WORKDIR /build/selur
COPY . /build/selur
RUN rustup target add wasm32-wasi && \
    cargo build --release --target wasm32-wasi

# ============================================================
# Stage 4: Assemble welded image
# ============================================================
FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Copy binaries
COPY --from=vordr-builder /build/vordr/target/release/vordr /usr/local/bin/
COPY --from=svalinn-builder /build/svalinn/dist/svalinn /usr/local/bin/
COPY --from=selur-builder /build/selur/target/wasm32-wasi/release/selur.wasm /usr/local/lib/

# Copy launcher script
COPY scripts/selur-launcher /usr/local/bin/
RUN chmod +x /usr/local/bin/selur-launcher

# Configuration
ENV VORDR_ENDPOINT=selur://unix:///run/selur-sv.sock
ENV SELUR_WASM=/usr/local/lib/selur.wasm
ENV SVALINN_PORT=8000

EXPOSE 8000

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:8000/healthz || exit 1

CMD ["/usr/local/bin/selur-launcher"]
