# SPDX-License-Identifier: PMPL-1.0-or-later
# Justfile - Svalinn Edge Shield (ReScript/Zero Trust)
# Maintainer: Jonathan Jewell <hyperpolymath@protonmail.com>

# --- Default ---
default:
    @just --list

# --- Development ---
dev:
    @echo "🚀 Starting Svalinn dev server (ReScript + Deno)"
    cd src && deno run --allow-net --allow-read --allow-env --watch Main.res

serve:
    @echo "🛡️  Starting Svalinn production server"
    cd src && deno run --allow-net --allow-read --allow-env Main.res.js

# --- Build ---
build-res:
    @echo "🔧 Building ReScript sources"
    cd src && npx rescript build

build:
    @echo "📦 Building compiled binary (Deno)"
    cd src && deno compile --allow-net --allow-read --allow-env -o ../dist/svalinn Main.res.js

build-ui:
    @echo "🎨 Building UI (ReScript)"
    cd ui && npx rescript build

# --- Rootless Container Build (svalinn/vordr) ---
container-build:
    @echo "🐳 Building rootless container (svalinn/vordr)"
    vordr build -t ghcr.io/hyperpolymath/svalinn:latest .
    # Fallback to nerdctl if vordr unavailable
    || nerdctl --namespace=user build -t ghcr.io/hyperpolymath/svalinn:latest .

# --- Tests & Checks ---
test:
    @echo "🧪 Running integration tests"
    deno run --allow-all tests/integration_test.res.mjs

test-e2e:
    @echo "🧪 Running E2E tests (requires Vörðr)"
    ./tests/e2e_test.sh

bench:
    @echo "📊 Running benchmarks"
    deno bench --allow-net benchmarks/gateway_bench.ts

load-test:
    @echo "🔥 Running load tests"
    deno run --allow-net benchmarks/load_test.ts

check:
    @echo "🔍 Type-checking ReScript"
    rescript check

fmt:
    @echo "✨ Formatting code"
    deno fmt
    rescript format -all

lint:
    @echo "📖 Linting (Deno + ReScript)"
    deno lint

# --- Security & Compliance ---
security-audit:
    @echo "🔒 Running security audit"
    ./scripts/security_audit.sh

precommit: fmt lint check test security-audit

# Ethical compliance (PhD research)
ethical-check:
    @echo "📜 Checking ethical compliance (licenses, data privacy)"
    licensee && fossthod check

# SBOM generation (supply chain)
sbom:
    @echo "📋 Generating SBOM"
    cd src && npm run sbom

# WASM proxy rules validation
wasm-check:
    @echo "🌐 Validating WASM proxy rules"
    deno run --allow-read scripts/validate-wasm-rules.ts

# SELinux/AppArmor policy validation
selinux-check:
    @echo "🔒 Validating SELinux policies"
    sesearch -A -C | grep -E "svalinn_t|vordr_t" || true

apparmor-check:
    @echo "🛡️ Validating AppArmor profiles"
    aa-status --enabled && grep "svalinn" /etc/apparmor.d/*

# --- Schema & Specs ---
mcp-schema:
    @echo "📄 Generating MCP schema"
    cd src && deno run --allow-read --allow-write scripts/generate-mcp-schema.ts

validate-schemas:
    @echo "📝 Validating spec schemas"
    cd spec/schemas && \
    for f in *.json; do \
        echo "Validating $f..."; \
        deno run --allow-read jsr:@std/json/validate "$f"; \
    done

# --- Deployment ---
start-all:
    @echo "🚀 Starting Svalinn + UI"
    just serve & just dev-ui

dev-ui:
    @echo "🎨 Serving UI (dev mode)"
    cd ui && npx rescript build -w &
    cd ui && python3 -m http.server 3000 || deno run --allow-net jsr:@std/http/file-server

# --- Release (immutable tags) ---
release VERSION:
    @echo "📦 Releasing {{VERSION}} (immutable)"
    git tag -a "v{{VERSION}}" -m "Release v{{VERSION}}"
    git push origin "v{{VERSION}}"
    # Build and push container (rootless)
    just container-build
    vordr push ghcr.io/hyperpolymath/svalinn:latest
    # Fallback to nerdctl
    || nerdctl --namespace=user push ghcr.io/hyperpolymath/svalinn:latest

# --- CI/CD (cicd-hyper-a) ---
ci:
    @echo "🤖 Running CI checks (cicd-hyper-a)"
    just precommit
    just build-res
    just build-ui
    just sbom
    just ethical-check

# --- Configuration ---
config:
    @echo "📋 Svalinn Configuration:"
    @echo "SVALINN_PORT=${SVALINN_PORT:-8000}"
    @echo "SVALINN_HOST=${SVALINN_HOST:-0.0.0.0}"
    @echo "VORDR_ENDPOINT=${VORDR_ENDPOINT:-http://localhost:8080}"
    @echo "SPEC_VERSION=${SPEC_VERSION:-v0.1.0}"

# --- Fallbacks ---
podman-build:
    @echo "🐳 Fallback: Building with podman (dev only)"
    podman build -t svalinn:latest .

podman-push:
    @echo "🐳 Fallback: Pushing with podman (dev only)"
    podman push svalinn:latest

# --- Cleanup ---
clean:
    @echo "🧹 Cleaning build artifacts"
    rm -rf dist/
    rm -rf src/**/*.bs.js
    rm -rf src/**/*.res.mjs
    rm -rf ui/src/**/*.bs.js
    rm -rf ui/src/**/*.res.mjs
