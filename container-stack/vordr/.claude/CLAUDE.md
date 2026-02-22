## Machine-Readable Artefacts

The following files in `.machine_readable/` contain structured project metadata:

- `STATE.scm` - Current project state and progress
- `META.scm` - Architecture decisions and development practices
- `ECOSYSTEM.scm` - Position in the ecosystem and related projects
- `AGENTIC.scm` - AI agent interaction patterns
- `NEUROSYM.scm` - Neurosymbolic integration config
- `PLAYBOOK.scm` - Operational runbook

---

# Vörðr AI Assistant Instructions

## Project Overview

**Vörðr** (Old Norse: guardian/watcher, ASCII: `vordr`) is a formally verified container orchestration and verification system.

## Language Policy (STRICT)

### ALLOWED Languages

| Language | Use Case | Location |
|----------|----------|----------|
| **Idris2** | Formal verification, proofs, type-level programming | `src/idris2/` |
| **Rust** | eBPF probes, performance-critical paths, CLI | `src/rust/` |
| **Elixir** | Orchestration, state management, reversibility | `src/elixir/` |
| **Ada/SPARK** | Cryptographic operations, SPARK proofs | `src/ada/` |
| **ReScript** | MCP adapter, browser UI | `adapters/`, `runtime/` |
| **Guile Scheme** | SCM checkpoint files | `*.scm` |
| **Bash** | Build scripts only | `scripts/` |

### BANNED Languages (Per RSR)

| Banned | Replacement | Reason |
|--------|-------------|--------|
| TypeScript | ReScript | RSR compliance |
| Go | Rust | RSR compliance |
| Python | Idris2/Elixir | RSR compliance (except SaltStack) |
| Java/Kotlin | Ada/Rust | RSR compliance |
| Node.js | Deno | RSR compliance |

**Enforcement**: CI/CD rejects PRs with banned languages. Mustfile gates all merges.

## Code Style

### Idris2
- Dependent types are documentation — name them clearly
- Proofs in `src/idris2/proofs/` with literate comments
- SPDX header: `-- SPDX-License-Identifier: PMPL-1.0-or-later`

### Rust
- `rustfmt` + `clippy` mandatory
- No `unsafe` except in eBPF bindings (document why)
- SPDX header: `// SPDX-License-Identifier: PMPL-1.0-or-later`

### Elixir
- `mix format` mandatory
- Prefer `GenServer` over raw processes
- Document public functions with `@doc`

### Ada/SPARK
- SPARK subset only for cryptographic code
- `gnatpp` formatting
- Contracts with `Pre`, `Post`, and `Contract_Cases`

## Architecture Decisions

See `META.scm` for ADRs. Key decisions:

1. **ADR-001**: Multi-language architecture (Idris2 + Rust + Elixir + Ada)
2. **ADR-002**: Bennett-reversible operations for all state changes
3. **ADR-003**: Dependent types for container lifecycle verification
4. **ADR-004**: eBPF for runtime monitoring
5. **ADR-005**: Threshold signatures for multi-stakeholder trust
6. **ADR-006**: RISC-V as primary edge target
7. **ADR-007**: Temporal isolation engine (see `docs/ADR-007-temporal-isolation.md`)
   - Time dilation as primary security primitive (not containers/VMs)
   - BEAM port interceptor swarm (65,535 processes, ~20MB)
   - eBPF + PTP/NTP clock manipulation for undetectable observation
   - CT-scan tomographic mode: distributed across 1,000–100,000 instances
   - Integration with panic-attack abduct subcommand

## Key Files

- `README.adoc` — Main documentation
- `ROADMAP.adoc` — Development roadmap
- `META.scm` — Architecture decisions
- `ECOSYSTEM.scm` — Project ecosystem position
- `STATE.scm` — Current state and progress
- `PLAYBOOK.scm` — Operational procedures
- `AGENTIC.scm` — AI agent boundaries (READ THIS)
- `NEUROSYM.scm` — Neuro-symbolic AI configuration
- `Mustfile` — Mandatory verification gates
- `justfile` — Task automation

## AI Agent Boundaries

**READ `AGENTIC.scm` BEFORE MAKING CHANGES**

### Allowed
- Code generation following language policy
- Test writing and fuzzing
- Documentation updates
- Refactoring with behavior preservation
- Proof suggestions

### Prohibited
- Cryptographic operations
- Deployment actions
- Bypassing Mustfile gates
- Accessing secrets
- Merging without review

## Build Commands

```bash
just setup      # Install toolchains
just build      # Build all components
just test       # Run all tests
just must       # Run mandatory checks
just must-full  # Full checks + proofs + fuzzing
```

## Verification Commands

```bash
just verify-static nginx:latest    # Static verification
just monitor my-container          # eBPF monitoring
just prove container_lifecycle.idr # Generate proofs
```

## Integration Points

- **Svalinn**: OCI hooks for pre-start verification
- **Cerro Torre**: Consumes build attestations
- **Oblibeny**: Orchestration handoff
- **poly-ssg-mcp**: MCP interface exposure
- **panic-attack**: `abduct` subcommand consumes temporal isolation engine
- **selur**: IPC bridge between Rust eBPF and BEAM orchestrator

## Commit Style

```
<type>(<scope>): <subject>

[body]

Co-Authored-By: Claude <noreply@anthropic.com>
```

Types: `feat`, `fix`, `proof`, `refactor`, `docs`, `test`, `chore`
Scopes: `idris2`, `rust`, `elixir`, `ada`, `ci`, `docs`

