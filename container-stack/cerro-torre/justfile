# Cerro-Torre - Development Tasks
# SPDX-License-Identifier: PMPL-1.0-or-later
set shell := ["bash", "-uc"]
set dotenv-load := true

project := "Cerro-Torre"
build_mode := env_var_or_default("CERRO_BUILD_MODE", "Development")

# Toolbox prefix - use toolbox on Fedora Kinoite/Silverblue
tb := if `command -v toolbox >/dev/null 2>&1 && echo yes || echo no` == "yes" { "toolbox run" } else { "" }

# Show all recipes
default:
    @just --list --unsorted

# Build the project (Ada + Rust signing CLI)
build:
    alr build
    cargo build --release
    cp target/release/cerro-sign bin/

# Run tests
test:
    alr build
    @echo "Unit tests not yet implemented - see docs/MVP-PLAN.md"

# Clean build artifacts
clean:
    alr clean
    rm -rf obj/ lib/

# Format Ada code (uses gnatpp if available)
fmt:
    @if command -v gnatpp &> /dev/null; then \
        find src -name "*.adb" -o -name "*.ads" | xargs gnatpp -i3 -M100; \
    else \
        echo "gnatpp not found - install GNAT tools for formatting"; \
    fi

# Lint/check Ada code
lint:
    alr build -- -gnatwa -gnatwe

# Run SPARK proofs (requires gnatprove)
prove:
    @if command -v gnatprove &> /dev/null; then \
        gnatprove -P cerro_torre.gpr --level=2; \
    else \
        echo "gnatprove not found - requires SPARK Pro or Community"; \
    fi

# Build and run the CLI
run *args:
    alr run -- {{args}}

# Build the ATS2 shadow verifier
shadow-build:
    @if command -v patscc &> /dev/null; then \
        patscc -O2 -DATS_MEMALLOC_LIBC -o ct-shadow tools/ats-shadow/main.dats; \
    else \
        echo "patscc not found - install ATS2 for shadow verifier"; \
    fi

# ============================================================
# Container Image Variants (3×3 Matrix)
# ============================================================

VERSION := "2.0.0"

# Build all 9 image variants (3 verification levels × 3 feature sets)
build-all-images: build-standard build-research build-extreme

# Build all standard variants (SPARK only)
build-standard: build-ct-i build-ct-a build-ct

# Build all research variants (Idris2 + SPARK)
build-research: build-R-ct-i build-R-ct-a build-R-ct

# Build all extreme variants (ATS2 + SPARK)
build-extreme: build-X-ct-i build-X-ct-a build-X-ct

# ============================================================
# Standard Variants (S- prefix implied, SPARK only)
# ============================================================

# Build ct-i (standard infrastructure: registry ops + basic packing)
build-ct-i:
    @echo "📦 Building ct-i (standard infrastructure)..."
    podman build -f containerfiles/ct-i.containerfile \
      -t ct-i:{{VERSION}} \
      -t ct-i:latest .

# Build ct-a (standard attestation: signing + rekor + sbom)
build-ct-a:
    @echo "🔐 Building ct-a (standard attestation)..."
    podman build -f containerfiles/ct-a.containerfile \
      -t ct-a:{{VERSION}} \
      -t ct-a:latest .

# Build ct (standard complete: all features)
build-ct:
    @echo "🏔️  Building ct (standard complete)..."
    podman build -f containerfiles/ct.containerfile \
      -t ct:{{VERSION}} \
      -t ct:latest .

# ============================================================
# Research Variants (R- prefix, Idris2 + SPARK)
# ============================================================

# Build R-ct-i (research infrastructure)
build-R-ct-i:
    @echo "🔬 Building R-ct-i (research infrastructure)..."
    podman build -f containerfiles-research/R-ct-i.containerfile \
      -t R-ct-i:{{VERSION}} \
      -t R-ct-i:latest .

# Build R-ct-a (research attestation)
build-R-ct-a:
    @echo "🔬🔐 Building R-ct-a (research attestation)..."
    podman build -f containerfiles-research/R-ct-a.containerfile \
      -t R-ct-a:{{VERSION}} \
      -t R-ct-a:latest .

# Build R-ct (research complete)
build-R-ct:
    @echo "🔬🏔️  Building R-ct (research complete)..."
    podman build -f containerfiles-research/R-ct.containerfile \
      -t R-ct:{{VERSION}} \
      -t R-ct:latest .

# ============================================================
# eXtreme Security Variants (X- prefix, ATS2 + SPARK)
# ============================================================

# Build X-ct-i (extreme infrastructure)
build-X-ct-i:
    @echo "⚡ Building X-ct-i (extreme infrastructure)..."
    podman build -f containerfiles-extreme/X-ct-i.containerfile \
      -t X-ct-i:{{VERSION}} \
      -t X-ct-i:latest .

# Build X-ct-a (extreme attestation)
build-X-ct-a:
    @echo "⚡🔐 Building X-ct-a (extreme attestation)..."
    podman build -f containerfiles-extreme/X-ct-a.containerfile \
      -t X-ct-a:{{VERSION}} \
      -t X-ct-a:latest .

# Build X-ct (extreme complete)
build-X-ct:
    @echo "⚡🏔️  Building X-ct (extreme complete)..."
    podman build -f containerfiles-extreme/X-ct.containerfile \
      -t X-ct:{{VERSION}} \
      -t X-ct:latest .

# ============================================================
# Testing
# ============================================================

# Test all variants
test-all: test-standard test-research test-extreme

# Test standard variants
test-standard: test-ct-i test-ct-a test-ct

# Test research variants
test-research: test-R-ct-i test-R-ct-a test-R-ct

# Test extreme variants
test-extreme: test-X-ct-i test-X-ct-a test-X-ct

# Standard tests
test-ct-i:
    @echo "🧪 Testing ct-i (standard)..."
    podman run --rm ct-i:latest pack --help
    podman run --rm ct-i:latest fetch --help

test-ct-a:
    @echo "🧪 Testing ct-a (standard)..."
    podman run --rm ct-a:latest sign --help
    podman run --rm ct-a:latest keygen --help

test-ct:
    @echo "🧪 Testing ct (standard)..."
    podman run --rm ct:latest --version
    podman run --rm ct:latest pack --help

# Research tests
test-R-ct-i:
    @echo "🧪 Testing R-ct-i (research)..."
    podman run --rm R-ct-i:latest pack --help
    podman run --rm R-ct-i:latest --verification-status

test-R-ct-a:
    @echo "🧪 Testing R-ct-a (research)..."
    podman run --rm R-ct-a:latest sign --help
    podman run --rm R-ct-a:latest --verification-status

test-R-ct:
    @echo "🧪 Testing R-ct (research)..."
    podman run --rm R-ct:latest --version
    podman run --rm R-ct:latest --verification-status

# Extreme tests
test-X-ct-i:
    @echo "🧪 Testing X-ct-i (extreme)..."
    podman run --rm X-ct-i:latest pack --help
    podman exec X-ct-i ct-shadow --verify

test-X-ct-a:
    @echo "🧪 Testing X-ct-a (extreme)..."
    podman run --rm X-ct-a:latest sign --help
    podman exec X-ct-a ct-shadow --verify

test-X-ct:
    @echo "🧪 Testing X-ct (extreme)..."
    podman run --rm X-ct:latest --version
    podman exec X-ct ct-shadow --verify

# ============================================================
# Publishing
# ============================================================

# Publish all 9 variants
publish-all: publish-standard publish-research publish-extreme

# Publish standard variants
publish-standard: publish-ct-i publish-ct-a publish-ct

# Publish research variants
publish-research: publish-R-ct-i publish-R-ct-a publish-R-ct

# Publish extreme variants
publish-extreme: publish-X-ct-i publish-X-ct-a publish-X-ct

# Standard publishing
publish-ct-i:
    @echo "📤 Publishing ct-i..."
    podman push ct-i:{{VERSION}} ghcr.io/hyperpolymath/ct-i:{{VERSION}}
    podman push ct-i:latest ghcr.io/hyperpolymath/ct-i:latest

publish-ct-a:
    @echo "📤 Publishing ct-a..."
    podman push ct-a:{{VERSION}} ghcr.io/hyperpolymath/ct-a:{{VERSION}}
    podman push ct-a:latest ghcr.io/hyperpolymath/ct-a:latest

publish-ct:
    @echo "📤 Publishing ct..."
    podman push ct:{{VERSION}} ghcr.io/hyperpolymath/ct:{{VERSION}}
    podman push ct:latest ghcr.io/hyperpolymath/ct:latest

# Research publishing
publish-R-ct-i:
    @echo "📤 Publishing R-ct-i..."
    podman push R-ct-i:{{VERSION}} ghcr.io/hyperpolymath/R-ct-i:{{VERSION}}
    podman push R-ct-i:latest ghcr.io/hyperpolymath/R-ct-i:latest

publish-R-ct-a:
    @echo "📤 Publishing R-ct-a..."
    podman push R-ct-a:{{VERSION}} ghcr.io/hyperpolymath/R-ct-a:{{VERSION}}
    podman push R-ct-a:latest ghcr.io/hyperpolymath/R-ct-a:latest

publish-R-ct:
    @echo "📤 Publishing R-ct..."
    podman push R-ct:{{VERSION}} ghcr.io/hyperpolymath/R-ct:{{VERSION}}
    podman push R-ct:latest ghcr.io/hyperpolymath/R-ct:latest

# Extreme publishing
publish-X-ct-i:
    @echo "📤 Publishing X-ct-i..."
    podman push X-ct-i:{{VERSION}} ghcr.io/hyperpolymath/X-ct-i:{{VERSION}}
    podman push X-ct-i:latest ghcr.io/hyperpolymath/X-ct-i:latest

publish-X-ct-a:
    @echo "📤 Publishing X-ct-a..."
    podman push X-ct-a:{{VERSION}} ghcr.io/hyperpolymath/X-ct-a:{{VERSION}}
    podman push X-ct-a:latest ghcr.io/hyperpolymath/X-ct-a:latest

publish-X-ct:
    @echo "📤 Publishing X-ct..."
    podman push X-ct:{{VERSION}} ghcr.io/hyperpolymath/X-ct:{{VERSION}}
    podman push X-ct:latest ghcr.io/hyperpolymath/X-ct:latest

# ============================================================
# Cleanup
# ============================================================

# Clean all 9 image variants
clean-images: clean-standard clean-research clean-extreme

# Clean standard variants
clean-standard:
    podman rmi ct-i:{{VERSION}} ct-i:latest || true
    podman rmi ct-a:{{VERSION}} ct-a:latest || true
    podman rmi ct:{{VERSION}} ct:latest || true

# Clean research variants
clean-research:
    podman rmi R-ct-i:{{VERSION}} R-ct-i:latest || true
    podman rmi R-ct-a:{{VERSION}} R-ct-a:latest || true
    podman rmi R-ct:{{VERSION}} R-ct:latest || true

# Clean extreme variants
clean-extreme:
    podman rmi X-ct-i:{{VERSION}} X-ct-i:latest || true
    podman rmi X-ct-a:{{VERSION}} X-ct-a:latest || true
    podman rmi X-ct:{{VERSION}} X-ct:latest || true

