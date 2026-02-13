# selur-compose Development Progress

## Compose Parity Plan

See `docs/COMPOSE-PARITY-PLAN.adoc` for the full parity roadmap with acceptance tests.

**Created:** 2026-01-25
**Status:** MVP Implementation In Progress (68% Complete)

## Phase A Integration (In Progress)

- Wiring Svalinn /api/v2/run payloads in selur-compose
- Added bundle digest (sha256) for imageDigest
- Secrets: file-mount only, dev env injection via SELUR_DEV_SECRETS
- Healthcheck mapping into v2 config
- Added Vörðr MCP client for network/volume lifecycle
- Networks + volumes created before deploy; removed on down
- Restart policy propagated into v2 config (runtime support pending)

## Session 5 - Registry Operations (2026-01-25)

### ✅ Completed

**Commands Implemented (2 new):**
- ✅ `pull` command - Pull .ctp bundles from registry
  - Pull all services or specific services
  - Download from remote registry to local .ctp files
  - ct CLI integration (ct fetch wrapper)
  - Service name → bundle path mapping
  - Error handling and summary reporting
  - ~70 lines

- ✅ `push` command - Push .ctp bundles to registry
  - Push all services or specific services
  - Upload local .ctp bundles to remote registry
  - ct CLI integration (ct push wrapper)
  - Bundle existence checking before push
  - Clear error messages if bundle missing
  - Suggests running build first if needed
  - ~80 lines

**CtClient Usage:**
- Uses existing pull() method for fetching from registry
- Uses existing push() method for uploading to registry
- Both methods already implemented in ct.rs

**Registry Workflow:**
```bash
# Build bundles from source
selur-compose build

# Push to registry
selur-compose push

# On another machine, pull from registry
selur-compose pull

# Deploy pulled bundles
selur-compose up
```

**Progress:**
- Commands: 15/25 → 17/25 (60% → 68%)
- Overall completion: 60% → 68%
- Binary size: 4.2MB (unchanged)

### 📋 Next Steps

**Immediate (Session 6):**
1. Implement `policy` command (policy compliance checks)
2. Implement `sbom` command (show SBOM from bundle)
3. Implement `provenance` command (show build provenance)
4. Reach 80% completion = MVP ready

## Session 4 - Observability (2026-01-25)

### ✅ Completed

**Commands Implemented (3 new):**
- ✅ `top` command - Display running processes
  - Show process list for all or specific services
  - Process information: PID, user, CPU, memory, command
  - Pretty table formatting
  - Vörðr integration for process data
  - ~110 lines

- ✅ `events` command - Stream real-time events
  - Poll-based event streaming (WebSocket/SSE in production)
  - Event types: container lifecycle, state changes
  - Timestamp, type, container, action display
  - Attribute key-value pairs
  - Continuous streaming with Ctrl+C to stop
  - ~70 lines

- ✅ `inspect` command - View detailed service information
  - JSON and pretty output formats
  - Container details: ID, image, state, status
  - Network information with IP addresses
  - Port mappings
  - Environment variables
  - Volume mounts
  - Resource usage (CPU, memory)
  - ~130 lines

**Vörðr Client Extensions:**
- ✅ Added `get_top()` method - returns TopInfo with Process list
- ✅ Added `get_events()` method - poll events by project name
- ✅ Added `inspect_container()` method - detailed container info
- ✅ TopInfo struct with Process details
- ✅ Event struct with id, timestamp, type, attributes
- ✅ ContainerDetails struct with NetworkInfo, PortInfo, MountInfo

**Progress:**
- Commands: 12/25 → 15/25 (48% → 60%)
- Overall completion: 50% → 60%
- Binary size: 4.2MB (unchanged)

### 📋 Next Steps

**Immediate (Session 5):**
1. Implement `pull`/`push` commands (registry operations)
2. Implement `policy` command (policy compliance)
3. Implement `sbom`/`provenance` commands
4. Add integration tests

## Session 3 - Lifecycle Management & Build (2026-01-25)

### ✅ Completed

**Commands Implemented (5 new):**
- ✅ `build` command - Build .ctp bundles from source via ct pack
  - Build context and dockerfile configuration
  - Build arguments (--build-arg KEY=VALUE)
  - Target stage support for multi-stage builds
  - No-cache option (--no-cache)
  - Multi-service building
  - ~120 lines

- ✅ `start` command - Start stopped containers
  - Service filtering
  - Status reporting (running/stopped/not found)
  - Vörðr integration for container start
  - ~100 lines

- ✅ `stop` command - Stop running containers
  - Graceful shutdown with timeout
  - Service filtering
  - Status reporting
  - ~105 lines

- ✅ `restart` command - Restart services
  - Combined stop + start operations
  - Service filtering
  - Timeout support
  - ~25 lines

- ✅ `run` command - One-off command execution
  - Create temporary container with UUID name
  - Execute command and capture output
  - --rm flag for auto-removal
  - Environment variable conversion
  - Port mapping support
  - Exit code propagation
  - ~140 lines

**Compose File Extensions:**
- ✅ Added Build struct (context, dockerfile, args, target)
- ✅ Added build field to Service struct
- ✅ default_dockerfile() helper

**CtClient Improvements:**
- ✅ Changed new() to return Self (not Result)
- ✅ Added run_command() for arbitrary ct commands
- ✅ Fixed all callers (up.rs, verify.rs, build.rs)

**Dependencies:**
- ✅ Added uuid crate (v4 feature) for unique container names

**Progress:**
- Commands: 7/25 → 12/25 (28% → 48%)
- Overall completion: 40% → 50%
- Binary size: 4.2MB (unchanged)

### 📋 Next Steps

**Immediate (Session 4):**
1. Implement `pull`/`push` commands (registry operations)
2. Implement `policy` command (policy compliance checks)
3. Implement `sbom`/`provenance` commands
4. Add integration tests for build/lifecycle commands

## Session 2 - Essential Commands (2026-01-25)

### ✅ Completed

**Commands Implemented (3 new):**
- ✅ `logs` command - View service logs with follow mode and tail support
  - Follow mode (-f) for continuous log streaming
  - Tail option (--tail N) to show last N lines
  - Service filtering
  - Multi-service log aggregation with service name prefixes
  - Vörðr client integration for log retrieval
  - ~150 lines

- ✅ `exec` command - Execute commands in running containers
  - Find running container for service
  - Execute arbitrary commands
  - Capture stdout/stderr separately
  - Propagate exit codes
  - Vörðr client exec API integration
  - ~90 lines

- ✅ `scale` command - Horizontal scaling of services
  - SERVICE=N syntax (e.g., `web=3`)
  - Scale up by creating new replicas
  - Scale down by removing excess replicas
  - Environment variable conversion (EnvValue → String)
  - Port mapping parsing ("8080:80" → PortMapping)
  - Integration with Svalinn deployment API
  - ~180 lines

**Vörðr Client Extensions:**
- ✅ Added `get_logs()` method with tail support
- ✅ Added `exec()` method with ExecOutput response
- ✅ ExecOutput struct (stdout, stderr, exit_code)

**Progress:**
- Commands: 7/25 (28% → up from 16%)
- Overall completion: 40% (up from 30%)
- Binary size: 4.2MB (unchanged)
- All new commands compile and run

### 📋 Next Steps

**Immediate (Session 3):**
1. Implement `build` command (source → .ctp bundle via ct pack)
2. Implement `start`/`stop`/`restart` commands (lifecycle management)
3. Implement `run` command (one-off container execution)
4. Add integration tests for logs/exec/scale

## Session 1 - Foundation & Core Commands (2026-01-25)

### ✅ Completed

**Repository Setup:**
- ✅ Created repository structure
- ✅ PMPL-1.0-or-later license
- ✅ Comprehensive README.adoc (architecture, commands, use cases)
- ✅ ECOSYSTEM.scm (clarifies single-tool integration)
- ✅ META.scm (6 architecture decisions)
- ✅ STATE.scm (project tracking)
- ✅ Cargo.toml with all dependencies

**Core Implementation (4.2MB binary):**
- ✅ CLI skeleton (25 commands defined)
- ✅ TOML parser with validation (src/compose.rs)
- ✅ Dependency graph resolver (src/graph.rs - topological sort)
- ✅ Cerro Torre CLI wrapper (src/ct.rs)
- ✅ Svalinn HTTP client (src/svalinn.rs)
- ✅ Vörðr HTTP client (src/vordr.rs)

**Commands Implemented (17/25 = 68%):**

1. **`up`** - Start all services ✅
2. **`down`** - Stop and remove ✅
3. **`ps`** - List services ✅
4. **`verify`** - Verify signatures ✅
5. **`logs`** - View service logs ✅
6. **`exec`** - Execute in container ✅
7. **`scale`** - Horizontal scaling ✅
8. **`build`** - Build .ctp bundles ✅
9. **`start`** - Start stopped services ✅
10. **`stop`** - Stop running services ✅
11. **`restart`** - Restart services ✅
12. **`run`** - Run one-off commands ✅
13. **`top`** - Display processes ✅
14. **`events`** - Stream events ✅
15. **`inspect`** - View details ✅
16. **`pull`** - Pull from registry ✅
17. **`push`** - Push to registry ✅

(See session details above for implementation specifics)

**Examples:**
- ✅ examples/basic/compose.toml (web app stack)
- ✅ examples/basic/README.adoc (usage guide)

**Testing:**
- ✅ Unit tests (duration parsing, dependency graph)
- ✅ Binary builds successfully (cargo build --release)
- ✅ CLI help/version work

### 🚧 In Progress

**Commands (8/25 remaining):**
- ⏳ `policy` - Check policy compliance
- ⏳ `sbom` - Show SBOM
- ⏳ `provenance` - Show build provenance
- ⏳ `config` - View merged config (already implemented!)

**Integration:**
- ⏳ Real Svalinn API integration (currently HTTP stubs)
- ⏳ Real Vörðr API integration (currently HTTP stubs)
- ⏳ selur zero-copy IPC setup
- ⏳ Volume management
- ⏳ Network configuration
- ⏳ Secret management

**Testing:**
- ⏳ Integration tests with real .ctp bundles
- ⏳ End-to-end tests with Svalinn/Vörðr
- ⏳ CI/CD pipeline

### 📋 Next Steps

**Immediate (Session 2):**
1. Implement `logs` command (tail -f support, service filtering)
2. Implement `exec` command (attach to running container)
3. Implement `scale` command (horizontal scaling)
4. Add integration tests

**Short-term (Sessions 3-4):**
1. Implement `build` command (source → .ctp bundle via ct pack)
2. Implement `pull`/`push` commands (registry operations via ct)
3. Implement `start`/`stop`/`restart` commands
4. Connect to real Svalinn/Vörðr APIs

**Medium-term (Sessions 5-6):**
1. Implement `policy` command (policy validation)
2. Implement `sbom`/`provenance` commands
3. Implement `top`/`events`/`inspect` commands
4. Volume and network management
5. Secret management integration

**Long-term (v1.0):**
1. Complete docker-compose compatibility
2. Kubernetes manifest generation
3. Rolling updates implementation
4. Load balancing
5. Observability (metrics, tracing)
6. Production deployment testing

## Architecture

```
selur-compose (Rust CLI - 4.2MB)
├─→ ct (Cerro Torre)     - Pack/verify .ctp bundles
├─→ Svalinn              - Gateway + policy enforcement
├─→ selur                - Zero-copy IPC bridge
└─→ Vörðr                - Container orchestrator
```

## Statistics

**Code Stats:**
- Total files: 16
- Source lines: ~2,500
- Commands defined: 25
- Commands implemented: 4 (16%)
- Binary size: 4.2MB (release)
- Compile time: ~1m (release)

**Dependencies:**
- clap 4.5 (CLI framework)
- toml 0.8 + serde 1.0 (config)
- tokio 1.36 (async)
- reqwest 0.11 (HTTP)
- petgraph 0.6 (graphs)
- anyhow 1.0 (errors)
- prettytable-rs 0.10 (tables)

## Performance Targets

**vs docker-compose:**
- Startup time: <2s for 5 services (vs 10-15s)
- IPC latency: <100 μs (vs 700-2000 μs)
- Memory: 1MB fixed (vs variable)
- Throughput: 10,000+ req/s per service pair

## Timeline

**v0.1 MVP (4 weeks):**
- [x] Week 1: Foundation + 4 core commands ✅
- [ ] Week 2: Logs, exec, scale, build
- [ ] Week 3: Pull/push, policy, SBOM
- [ ] Week 4: Integration testing, bug fixes

**v0.2 (8 weeks):**
- Health checks
- Scaling
- Rolling updates
- Network isolation
- Secret management

**v1.0 (16 weeks):**
- Complete docker-compose compatibility
- Kubernetes integration
- Production-ready
- Performance benchmarks
- Full test coverage

## License

PMPL-1.0-or-later (Polymath Public Mark License)

## Maintainer

Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

**Co-Authored-By:** Claude Sonnet 4.5 <noreply@anthropic.com>
