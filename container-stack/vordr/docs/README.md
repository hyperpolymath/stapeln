# Vörðr Documentation

**Version:** 0.5.0-dev
**Status:** Phase 1 - 90-92% Complete

---

## Overview

Vörðr (Old Norse: "The Warden") is a high-assurance daemonless container engine with formally verified security components.

**Key Features:**
- Daemonless architecture (no background daemon)
- Multi-language verified stack (Rust + Elixir + Ada/SPARK + Idris2)
- eBPF-based runtime monitoring
- MCP (Model Context Protocol) integration
- OCI-compliant container runtime

---

## Documentation Index

### User Documentation
- [Installation Guide](INSTALLATION.md) *(Coming soon)*
- [User Guide](USER-GUIDE.md) *(Coming soon)*
- [CLI Reference](CLI-REFERENCE.md) - All 16 CLI commands
- [Configuration](CONFIGURATION.md) *(Coming soon)*

### Operator Documentation
- [Deployment Guide](DEPLOYMENT.md) *(Coming soon)*
- [Monitoring with eBPF](MONITORING.md)
- [Security Policies](SECURITY-POLICIES.md)
- [Troubleshooting](TROUBLESHOOTING.md) *(Coming soon)*

### Developer Documentation
- [Architecture Overview](ARCHITECTURE.md)
- [API Reference](api/) - Rustdoc generated
- [Contributing Guide](../CONTRIBUTING.md) *(Coming soon)*
- [Testing Guide](TESTING.md)

### Security Documentation
- [Security Model](SECURITY-MODEL.md)
- [Formal Verification](FORMAL-VERIFICATION.md)
- [Threat Model](THREAT-MODEL.md) *(Coming soon)*
- [Audit Reports](audits/) *(Coming soon)*

---

## Quick Start

### Build from Source

```bash
# Prerequisites: Rust, Elixir, GNAT (optional)
git clone https://github.com/hyperpolymath/vordr.git
cd vordr

# Build Rust components
cd src/rust
cargo build --release

# Binary available at: target/release/vordr
```

### Run Tests

```bash
# Unit and integration tests
cd src/rust
cargo test

# eBPF tests (requires Linux 4.15+, CAP_BPF)
cargo test --features bpf

# Elixir tests
cd ../elixir
mix test

# All tests via justfile
cd ../..
just test
```

### Basic Usage

```bash
# Check system compatibility
vordr doctor

# Pull an image
vordr pull alpine:latest

# Run a container
vordr run --detach --name test alpine:latest sleep 30

# List containers
vordr ps

# Monitor with eBPF
vordr monitor start --policy strict

# Stop monitoring
vordr monitor stop
```

---

## Architecture

Vörðr uses a unique multi-language architecture for defense-in-depth security:

```
┌─────────────────────────────────────────────────────────────┐
│                    CLI (Rust)                                │
│  16 commands: run, ps, exec, monitor, image, network...     │
└────────────────────┬─────────────────────────────────────────┘
                     │
     ┌───────────────┼───────────────┐
     │               │               │
     ▼               ▼               ▼
┌─────────┐   ┌────────────┐   ┌──────────┐
│ Runtime │   │ eBPF       │   │ MCP      │
│ Shim    │   │ Monitor    │   │ Server   │
│ (Rust)  │   │ (Rust)     │   │ (Axum)   │
└────┬────┘   └─────┬──────┘   └────┬─────┘
     │              │               │
     ▼              ▼               │
┌─────────┐   ┌──────────┐         │
│ youki / │   │ Aya eBPF │         │
│ runc    │   │ Probes   │         │
└─────────┘   └──────────┘         │
                                    │
     ┌──────────────────────────────┘
     │
     ▼
┌────────────┐   ┌──────────────┐   ┌───────────┐
│ Elixir     │←→ │ Ada/SPARK    │←→ │ Idris2    │
│ Orchestr.  │   │ Gatekeeper   │   │ Proofs    │
│ (OTP)      │   │ (Verified)   │   │ (Types)   │
└────────────┘   └──────────────┘   └───────────┘
```

**Component Breakdown:**

1. **Rust (CLI + Runtime):**
   - 16 CLI commands
   - Container lifecycle management
   - eBPF monitoring (Aya framework)
   - OCI config builder
   - Registry client
   - State management (SQLite)

2. **Elixir (Orchestration):**
   - GenStateMachine for container state
   - Distributed coordination
   - Fault tolerance (OTP supervision trees)

3. **Ada/SPARK (Gatekeeper):**
   - Formally verified policy enforcement
   - Security boundary validation
   - Capability checking
   - FFI bridge to Rust

4. **Idris2 (Type Proofs):**
   - Dependent type checking
   - State machine correctness proofs
   - Container lifecycle invariants

---

## eBPF Monitoring

Vörðr uses eBPF (Extended Berkeley Packet Filter) for runtime security monitoring without performance overhead.

**Capabilities:**
- Syscall-level monitoring (execve, mount, module load, etc.)
- Network event tracking
- Anomaly detection
- Real-time alerts (webhook integration)

**Policies:**
- **Strict:** Blocks dangerous syscalls (kernel modules, reboot, namespace manipulation)
- **Minimal-Audit:** Only audits process execution
- **Custom:** Define your own syscall filters

**Example:**

```bash
# Start monitoring all containers with strict policy
vordr monitor start --policy strict

# Monitor specific container with webhook alerts
vordr monitor start \
  --container test-app \
  --policy strict \
  --webhook https://alerts.example.com/hooks \
  --sensitivity 0.9

# View live events
vordr monitor events --follow

# Show statistics
vordr monitor stats
```

---

## MCP Integration

Vörðr implements the Model Context Protocol (MCP) for AI assistant integration.

**MCP Server:**

```bash
# Start MCP server
vordr serve --host 0.0.0.0 --port 8080

# Endpoints:
# POST /         - JSON-RPC 2.0 (tools/call, tools/list)
# GET  /health   - Health check
# GET  /tools    - List available tools
```

**Available MCP Tools:**
- `vordr_container_list` - List containers
- `vordr_container_get` - Get container details
- `vordr_container_create` - Create container
- `vordr_container_start` - Start container
- `vordr_container_stop` - Stop container
- `vordr_image_list` - List images
- `vordr_image_pull` - Pull image
- `vordr_health` - Health check

**Integration with Svalinn:**

Svalinn (Phase 2) is the HTTP gateway that sits in front of Vörðr and provides:
- REST API (12+ endpoints)
- Authentication (OAuth2, OIDC, API keys, mTLS)
- Request validation (JSON Schema)
- Policy enforcement (Gatekeeper format)

---

## Testing

**Test Coverage:** 70%+

**Test Types:**
1. **Integration Tests** (44 passing)
   - All 16 CLI commands
   - Error handling
   - Concurrent execution

2. **Unit Tests**
   - eBPF module tests
   - Syscall policy tests
   - Anomaly detection tests

3. **E2E Tests** (requires runtime)
   - Full container lifecycle
   - Network operations
   - Volume management

**Run Tests:**

```bash
# All passing tests (no runtime required)
cargo test

# Include ignored tests (requires youki/runc)
cargo test -- --include-ignored

# With coverage (requires cargo-llvm-cov)
cargo llvm-cov --html

# eBPF tests (requires Linux, CAP_BPF)
cargo test --features bpf
```

---

## Security

**Security Features:**
- Rootless container support
- Capability filtering (via Gatekeeper)
- User namespace enforcement
- Network mode restrictions
- eBPF runtime monitoring
- Formally verified security boundaries (Ada/SPARK)

**Vulnerability Reporting:**

If you discover a security vulnerability, please email: security@svalinnproject.org

**Do NOT** open a public GitHub issue.

---

## Performance

**Benchmarks** *(Preliminary - v0.5.0)*

| Operation | Latency | Throughput |
|-----------|---------|------------|
| Container create | ~50ms | - |
| Container start | ~100ms | - |
| Container stop | ~80ms | - |
| eBPF event processing | <1μs | 1M+ events/s |
| MCP call overhead | ~5ms | - |

*Run your own benchmarks:* `cargo bench`

---

## Roadmap

### Phase 1 (Current - 90-92%)
- ✅ Core container lifecycle
- ✅ eBPF userspace infrastructure
- ✅ Integration tests (70%+)
- ⏳ eBPF kernel programs (aya-bpf)
- ⏳ Documentation completion

### Phase 2 (Complete - 95%)
- ✅ Svalinn HTTP gateway (ReScript/Deno)
- ✅ Authentication (OAuth2, OIDC, mTLS)
- ✅ Policy enforcement
- ✅ MCP client integration

### Phase 3 (Planned)
- Full stack integration (Cerro Torre + Vörðr + Svalinn)
- End-to-end workflow (pack → verify → run)
- Security hardening (SELinux, AppArmor, Seccomp)
- Production deployment

### Phase 4 (Planned)
- selur: Zero-overhead IPC (Ephapax linear types)
- Formal verification (Idris2 proofs)
- Performance optimization

---

## Contributing

Contributions welcome! Please read [CONTRIBUTING.md](../CONTRIBUTING.md) first.

**Areas needing help:**
- eBPF kernel programs (aya-bpf)
- Additional test coverage
- Documentation improvements
- Platform support (macOS, Windows)

---

## License

Vörðr is licensed under PMPL-1.0-or-later.

See [LICENSE](../LICENSE) for details.

---

## Links

- **Repository:** https://github.com/hyperpolymath/vordr
- **Documentation:** https://github.com/hyperpolymath/vordr/tree/main/docs
- **Issue Tracker:** https://github.com/hyperpolymath/vordr/issues
- **Ecosystem Status:** [ECOSYSTEM-STATUS.md](../ECOSYSTEM-STATUS.md)

---

**Last Updated:** 2026-01-25
**Version:** 0.5.0-dev
**Phase:** 1 (90-92% Complete)
