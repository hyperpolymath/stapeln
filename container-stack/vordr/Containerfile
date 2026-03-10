# SPDX-License-Identifier: PMPL-1.0-or-later
# Containerfile for Vordr container runtime binary
#
# Multi-stage build: compile Rust binary, deploy on Wolfi base.
# Only the Rust CLI is included at runtime. Idris2 proofs, Elixir
# orchestration, and Ada/SPARK specs are compile-time verification
# artefacts and are NOT shipped in the container image.

# ── Stage 1: Build ────────────────────────────────────────────────
FROM rust:1.83-slim AS rust-builder

RUN apt-get update && apt-get install -y --no-install-recommends \
        pkg-config libsqlite3-dev ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build

# Copy manifests first for layer caching
COPY src/rust/Cargo.toml src/rust/Cargo.lock ./
# Dummy main to cache dependency compilation
RUN mkdir -p src && echo 'fn main() {}' > src/main.rs \
    && cargo build --release || true \
    && rm -rf src target/release/.fingerprint/vordr-*

# Copy real source
COPY src/rust/ ./

RUN cargo build --release --locked

# ── Stage 2: Runtime ──────────────────────────────────────────────
FROM cgr.dev/chainguard/wolfi-base:latest

LABEL org.opencontainers.image.title="vordr" \
      org.opencontainers.image.description="Formally verified container orchestration engine" \
      org.opencontainers.image.source="https://github.com/hyperpolymath/vordr" \
      org.opencontainers.image.licenses="PMPL-1.0-or-later"

COPY --from=rust-builder /build/target/release/vordr /usr/local/bin/vordr

ENTRYPOINT ["/usr/local/bin/vordr"]
