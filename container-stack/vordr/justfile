# SPDX-License-Identifier: PMPL-1.0-or-later
# justfile — Task automation for Vörðr
# Usage: just <recipe>

# Default recipe: show available commands
default:
    @just --list

# ============================================================================
# SETUP: Install dependencies and toolchains
# ============================================================================

# Install all dependencies
setup: setup-idris2 setup-rust setup-elixir setup-ada
    @echo "All toolchains installed"

# Install Idris2
setup-idris2:
    @echo "Installing Idris2..."
    @if ! command -v idris2 >/dev/null 2>&1; then \
        echo "Please install Idris2 from https://idris-lang.org"; \
        echo "On Fedora: sudo dnf install idris2"; \
    else \
        echo "Idris2 already installed: $(idris2 --version)"; \
    fi

# Install Rust with RISC-V target
setup-rust:
    @echo "Installing Rust toolchain..."
    @rustup show
    @rustup target add riscv64gc-unknown-linux-gnu
    @cargo install cargo-fuzz || true

# Install Elixir
setup-elixir:
    @echo "Installing Elixir..."
    @if ! command -v elixir >/dev/null 2>&1; then \
        echo "Please install Elixir from https://elixir-lang.org"; \
        echo "On Fedora: sudo dnf install elixir"; \
    else \
        echo "Elixir already installed: $(elixir --version)"; \
    fi

# Install Ada/SPARK (GNAT)
setup-ada:
    @echo "Installing Ada/SPARK toolchain..."
    @if ! command -v gnatmake >/dev/null 2>&1; then \
        echo "Please install GNAT from https://www.adacore.com/download"; \
        echo "On Fedora: sudo dnf install gcc-gnat gprbuild"; \
    else \
        echo "GNAT already installed: $(gnatmake --version | head -1)"; \
    fi

# ============================================================================
# BUILD: Compile all components
# ============================================================================

# Build everything
build: build-idris2 build-rust build-elixir build-ada
    @echo "All components built"

# Build Idris2 verification core
build-idris2:
    @echo "Building Idris2..."
    @if [ -d src/idris2 ] && [ -n "$(ls -A src/idris2/*.idr 2>/dev/null)" ]; then \
        cd src/idris2 && idris2 --build vordr.ipkg; \
    else \
        echo "No Idris2 source files yet"; \
    fi

# Build Rust eBPF and CLI
build-rust:
    @echo "Building Rust..."
    @if [ -f src/rust/Cargo.toml ]; then \
        cd src/rust && cargo build --release; \
    else \
        echo "No Rust Cargo.toml yet"; \
    fi

# Build Elixir orchestrator
build-elixir:
    @echo "Building Elixir..."
    @if [ -f src/elixir/mix.exs ]; then \
        cd src/elixir && mix compile; \
    else \
        echo "No Elixir mix.exs yet"; \
    fi

# Build Ada trust engine
build-ada:
    @echo "Building Ada..."
    @if [ -f src/ada/*.gpr ]; then \
        cd src/ada && gprbuild -P *.gpr; \
    else \
        echo "No Ada project file yet"; \
    fi

# Build for RISC-V target
build-riscv:
    @echo "Cross-compiling for RISC-V..."
    @if [ -f src/rust/Cargo.toml ]; then \
        cd src/rust && cargo build --target riscv64gc-unknown-linux-gnu --release; \
    fi

# Build WASM module
build-wasm:
    @echo "Building WASM..."
    @if [ -d src/idris2 ]; then \
        cd src/idris2 && idris2 --codegen javascript vordr.idr -o vordr.js; \
    fi

# ============================================================================
# TEST: Run test suites
# ============================================================================

# Run all tests
test: test-idris2 test-rust test-elixir
    @echo "All tests passed"

# Test Idris2
test-idris2:
    @echo "Testing Idris2..."
    @if [ -d tests/idris2 ]; then \
        cd tests/idris2 && idris2 --check *.idr; \
    else \
        echo "No Idris2 tests yet"; \
    fi

# Test Rust
test-rust:
    @echo "Testing Rust..."
    @if [ -f src/rust/Cargo.toml ]; then \
        cd src/rust && cargo test; \
    else \
        echo "No Rust tests yet"; \
    fi

# Test Elixir
test-elixir:
    @echo "Testing Elixir..."
    @if [ -f src/elixir/mix.exs ]; then \
        cd src/elixir && mix test; \
    else \
        echo "No Elixir tests yet"; \
    fi

# Run fuzzing
fuzz:
    @echo "Running fuzzing..."
    @if [ -d tests/echidna ]; then \
        cd tests/echidna && cargo +nightly fuzz run fuzz_main -- -max_total_time=300; \
    else \
        echo "No fuzzing targets yet"; \
    fi

# ============================================================================
# VERIFY: Container verification commands
# ============================================================================

# Verify container statically
verify-static image:
    @echo "Verifying {{image}} statically..."
    @./target/release/vordr verify --static --image {{image}}

# Monitor container with eBPF
monitor container:
    @echo "Monitoring {{container}}..."
    @./target/release/vordr monitor --ebpf --container {{container}}

# Generate formal proofs
prove lifecycle:
    @echo "Generating proofs for {{lifecycle}}..."
    @./target/release/vordr prove --lifecycle {{lifecycle}}

# Full verification pipeline
verify-all image:
    @echo "Full verification of {{image}}..."
    @just verify-static {{image}}
    @just prove container_lifecycle.idr

# ============================================================================
# LINT: Code formatting and linting
# ============================================================================

# Format all code
fmt:
    @echo "Formatting..."
    @if [ -f src/rust/Cargo.toml ]; then cd src/rust && cargo fmt; fi
    @if [ -f src/elixir/mix.exs ]; then cd src/elixir && mix format; fi

# Lint all code
lint:
    @echo "Linting..."
    @if [ -f src/rust/Cargo.toml ]; then cd src/rust && cargo clippy; fi
    @if [ -f src/elixir/mix.exs ]; then cd src/elixir && mix credo; fi

# ============================================================================
# MUST: Mandatory verification gates
# ============================================================================

# Run all mandatory checks
must:
    @make -f Mustfile must-all

# Run full checks including proofs
must-full:
    @make -f Mustfile must-full

# ============================================================================
# CLEAN: Remove build artifacts
# ============================================================================

# Clean all build artifacts
clean:
    @echo "Cleaning..."
    @rm -rf target/ _build/ build/
    @if [ -f src/rust/Cargo.toml ]; then cd src/rust && cargo clean; fi
    @if [ -f src/elixir/mix.exs ]; then cd src/elixir && mix clean; fi
    @echo "Clean complete"

# ============================================================================
# DOCS: Documentation generation
# ============================================================================

# Generate documentation
docs:
    @echo "Generating documentation..."
    @if [ -f src/rust/Cargo.toml ]; then cd src/rust && cargo doc --no-deps; fi
    @if [ -f src/elixir/mix.exs ]; then cd src/elixir && mix docs; fi
    @asciidoctor README.adoc -o docs/index.html || true

# ============================================================================
# MCP: MCP adapter commands
# ============================================================================

# Build MCP adapter
build-mcp:
    @echo "Building MCP adapter..."
    @if [ -f src/mcp-adapter/rescript.json ]; then \
        cd src/mcp-adapter && npx rescript; \
    else \
        echo "No MCP adapter yet"; \
    fi

# Run MCP adapter
run-mcp:
    @echo "Running MCP adapter..."
    @cd src/mcp-adapter && deno run --allow-read --allow-write --allow-env src/Main.res.js

# ============================================================================
# PROVE: SPARK formal verification
# ============================================================================

# Run SPARK prover on Ada code
prove-spark:
    @echo "Running SPARK prover..."
    @if [ -d src/ada ]; then \
        cd src/ada && gnatprove -P policy.gpr --level=2; \
    else \
        echo "No Ada code yet"; \
    fi

# ============================================================================
# RELEASE: Version management
# ============================================================================

# Create release
release version:
    @echo "Creating release {{version}}..."
    @git tag -a v{{version}} -m "Release v{{version}}"
    @just build
    @just must-full
    @echo "Release v{{version}} ready"
