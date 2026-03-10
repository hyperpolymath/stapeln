# SPDX-License-Identifier: PMPL-1.0-or-later
# Containerfile -- Multi-stage build for Cerro Torre (ct)
#
# Stage 1: Build Ada/SPARK binary and Rust signing utility
# Stage 2: Minimal runtime with ct + ct-sign binaries
#
# Build:  podman build -f Containerfile -t ct:latest .
# Run:    podman run --rm ct:latest --version

# ---------------------------------------------------------------------------
# Stage 1 -- ada-builder
# ---------------------------------------------------------------------------
FROM ubuntu:24.04 AS ada-builder

# Avoid interactive prompts during package install
ENV DEBIAN_FRONTEND=noninteractive

# Install Ada/SPARK toolchain, Alire prerequisites, Rust, and libcurl dev
RUN apt-get update && apt-get install -y --no-install-recommends \
        gnat-14 \
        gprbuild \
        libcurl4-openssl-dev \
        curl \
        ca-certificates \
        git \
        unzip \
        make \
    && rm -rf /var/lib/apt/lists/*

# Install Alire
RUN curl -fsSL https://github.com/alire-project/alire/releases/latest/download/alr-2.0.2-bin-x86_64-linux.zip \
        -o /tmp/alr.zip \
    && unzip /tmp/alr.zip -d /usr/local/bin \
    && rm /tmp/alr.zip \
    && chmod +x /usr/local/bin/alr

# Install Rust toolchain (minimal, stable)
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \
        | sh -s -- -y --default-toolchain stable --profile minimal
ENV PATH="/root/.cargo/bin:${PATH}"

# Copy Ada/SPARK sources and build
WORKDIR /build
COPY alire.toml cerro_torre.gpr ./
COPY src/ src/
COPY config/ config/

RUN alr build

# Build the Rust signing utility
COPY Cargo.toml Cargo.lock ./
COPY src-rust/ src-rust/

RUN cargo build --release

# ---------------------------------------------------------------------------
# Stage 2 -- runtime
# ---------------------------------------------------------------------------
FROM cgr.dev/chainguard/wolfi-base:latest AS runtime

# Install libcurl runtime for HTTP/TLS operations
RUN apk add --no-cache libcurl-openssl4

# Copy the Ada ct binary from the builder stage
COPY --from=ada-builder /build/bin/ct /usr/local/bin/ct

# Copy the Rust ct-sign binary from the builder stage
COPY --from=ada-builder /build/target/release/cerro-sign /usr/local/bin/ct-sign

# Non-root user for runtime
RUN addgroup -S cerro && adduser -S cerro -G cerro
USER cerro

ENTRYPOINT ["ct"]
