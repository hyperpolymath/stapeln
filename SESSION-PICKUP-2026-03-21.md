# Session Pickup — 2026-03-21

## What was done today

### 1. Academic Papers (19 papers, 30,915 lines LaTeX)
All committed, pushed, and merged to main across 17 repos. Each 15-25 pages,
arXiv-style, covering the genuinely novel CS ideas. See each repo root for
`arcvix-*.tex` or `arxiv-*.tex`.

### 2. RGTV + Rokur BoJ Cartridges
- **vault-mcp**: Audit ring buffer (128 entries), command allowlist with prefix
  matching, 5 MCP tool definitions. All Zig tests passing. Merged to main.
- **rokur-mcp**: Container pre-start secrets gate, 5-state gate machine,
  4 MCP tools. All Zig tests passing. Merged to main.

### 3. Credential Guard
- `~/.claude/hooks/credential-guard.sh` — PreToolUse hook blocking AI agent
  access to `.netrc`, `.ssh/id_*`, `gh/hosts.yml`, etc. Active now.
- `config/ai-agent-allowlist.toml` — Default command prefixes for AI agents.
- `svalinn_cli` binary built and in PATH (`~/.local/bin/svalinn_cli`).

### 4. Selur Bridge Wiring
- **Rust host** (`container-stack/selur/src/lib.rs`): Rewrote Bridge to actually
  dispatch WASM requests to Vörðr via HTTP REST. Fixed return value mismatch,
  added VordrConfig, Command enum, StatusCode, ureq HTTP client. 7 tests passing.
- **WASM binary** built (531KB, `zig-out/bin/selur.wasm`).
- **selur-compose** binary built (17/25 commands).

### 5. Svalinn Fixes + Bridge Integration
- Fixed all pre-existing ReScript syntax errors (`|>` → `->`, `return` → `raise`).
- Created `SelurBridge.res` with live Deno WASM FFI bindings.
- Injected bridge into `McpClient.callWithRetry` (transparent HTTP fallback).
- Removed 103 stale build cache files from git tracking.
- Svalinn compiles cleanly: 22 modules, 0 errors.
- All merged to main.

### 6. miniKanren Canvas Rules
- `canvas_rules.ex` — NEW. Rules for visual drag-and-drop validation:
  - Port conflict detection (same host port → critical)
  - Network isolation (DB on same network as web → high)
  - Dependency ordering (missing deps, circular deps → critical)
  - Volume mount safety (sensitive host paths → critical)
  - `validate_canvas/1` — main entry point, returns score + findings + auto-fixes
  - Security score: 100 minus severity deductions

---

## What to pick up next

### Priority 1: Visual Canvas Interaction Layer
**The Build-a-Container Workshop UX.**

The backend is wired. The miniKanren engine is ready. What's missing is the
ReScript frontend that lets users drag, drop, and snap components together.

**Files to work in:** `stapeln/frontend/src/`
**Key tasks:**
- [ ] Component palette sidebar (nginx, postgres, redis, Svalinn, Rokur, custom)
- [ ] Drag-and-drop canvas (Cisco view) with snap-to-grid
- [ ] Connection lines between components (auto-create networks)
- [ ] Real-time security score badge (calls `validate_canvas` on every change)
- [ ] Gap analysis sidebar (findings list with severity badges, auto-fix buttons)
- [ ] Port configuration panel (click a service → edit ports/volumes/env)
- [ ] Export to compose.toml button

**Architecture:** Canvas state → WebSocket → Elixir backend →
`Kanren.CanvasRules.validate_canvas/1` → findings back to frontend →
update score badge + gap sidebar.

### Priority 2: miniKanren Rule Expansion
The canvas_rules.ex covers the basics. Next rules needed:
- [ ] Resource limit validation (CPU/memory bounds, no resource starvation)
- [ ] Image registry policy (only approved registries like Chainguard, ghcr.io)
- [ ] Privilege escalation detection (privileged containers, cap_add)
- [ ] TLS configuration (HTTPS ports without TLS cert → warning)
- [ ] Rokur integration (required secrets check before deploy button activates)

### Priority 3: Simulation Mode
Animate packet flow through the stack before deployment:
- [ ] Click "Simulate" → packets flow along connection lines
- [ ] miniKanren traces the network path and highlights vulnerabilities
- [ ] Ephemeral pinhole visualization (temporary port openings)

### Priority 4: One-Click Deploy
- [ ] Canvas → serialise to compose.toml
- [ ] Call selur-compose up via WebSocket
- [ ] Real-time container state feedback (starting → running → healthy)
- [ ] Rollback button (selur-compose down)

### Priority 5: Remaining Stapeln Holes
- [ ] Cerro Torre: only 1 .rs file — needs .ctp bundle implementation
- [ ] 3 Idris2 proof stubs in selur (bridgeCorrectness, bridgeSafety, noBufferOverflow)
- [ ] Svalinn deprecation warnings (Js.Json.string → JSON.Encode.string)

---

## Repos touched today (all synced to main)

| Repo | Key change |
|------|-----------|
| stapeln | Selur bridge, Svalinn fixes, canvas_rules.ex, paper |
| boj-server | vault-mcp + rokur-mcp cartridges |
| reasonably-good-token-vault | svalinn_cli, allowlist |
| 17 paper repos | 19 arXiv-style papers |

## Standing context

- Credential guard hook is LIVE (blocks .netrc reads)
- svalinn_cli is in PATH
- SELUR_WASM env var activates zero-copy bridge in Svalinn
- Rokur is production-complete (1,047 lines, 50 test steps)
- selur-compose builds but 8 remaining commands are convenience wrappers (not needed)
