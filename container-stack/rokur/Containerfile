# SPDX-License-Identifier: PMPL-1.0-or-later
# Containerfile — Multi-stage build for Rokur secrets management gate.
# Runtime: Deno on Chainguard Wolfi base.

# ---------------------------------------------------------------------------
# Stage 1: Build — install Deno runtime
# ---------------------------------------------------------------------------
FROM cgr.dev/chainguard/wolfi-base:latest AS build

RUN apk add --no-cache curl unzip

# Install Deno (pinned version for reproducibility).
ARG DENO_VERSION=2.2.8
RUN curl -fsSL https://deno.land/install-manual@v${DENO_VERSION}.sh | sh \
    && mv /root/.deno/bin/deno /usr/local/bin/deno

# Pre-cache dependencies by copying deno.json first.
WORKDIR /build
COPY deno.json deno.lock ./
RUN deno cache --lock=deno.lock deno.json || true

# Copy application source.
COPY main.js audit.js rate_limit.js ./
COPY policy/ ./policy/

# Cache all module imports so the runtime image needs no network.
RUN deno cache main.js

# ---------------------------------------------------------------------------
# Stage 2: Runtime — minimal image with only what is needed
# ---------------------------------------------------------------------------
FROM cgr.dev/chainguard/wolfi-base:latest AS runtime

RUN apk add --no-cache libgcc

# Copy Deno binary from build stage.
COPY --from=build /usr/local/bin/deno /usr/local/bin/deno

# Create non-root user for the service.
RUN addgroup -S rokur && adduser -S -G rokur rokur

WORKDIR /app

# Copy application files.
COPY --from=build /build/main.js /build/audit.js /build/rate_limit.js ./
COPY --from=build /build/policy/ ./policy/
COPY --from=build /build/deno.json /build/deno.lock ./

# Copy Deno cache so no network access is required at runtime.
COPY --from=build /root/.cache/deno /home/rokur/.cache/deno
RUN chown -R rokur:rokur /app /home/rokur/.cache

USER rokur

# Rokur listens on port 9090 by default (ROKUR_PORT).
EXPOSE 9090

# Health check against the /health endpoint.
HEALTHCHECK --interval=15s --timeout=5s --start-period=5s --retries=3 \
    CMD deno eval "const r = await fetch('http://127.0.0.1:9090/health'); Deno.exit(r.ok ? 0 : 1)"

# Fail-closed: minimal permissions.  --allow-write is for audit logs only.
ENTRYPOINT ["deno", "run", "--allow-net", "--allow-env", "--allow-read", "--allow-write", "main.js"]
