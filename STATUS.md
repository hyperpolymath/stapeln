# stapeln Status (Source of Truth)

**Date:** 2026-03-10

## Product Goal

A reasonably IT-capable 12-year-old can help their parents build a secure container stack without prior container knowledge.

## What Works Today

- **UI Prototype (frontend):** 8 views implemented as ReScript modules in `frontend/src/`.
- **TEA Architecture:** State, Msg, Update, View pattern in place via AppIntegrated.res.
- **Security UX Components:** Port config, security inspector, gap analysis, simulation mode modules exist.
- **Import/Export Hooks:** Buttons exist (not wired to durable storage).
- **Backend API (MVP):** Phoenix REST endpoints for stacks and validation are defined.
- **GraphQL API (MVP):** Absinthe schema at `/api/graphql` is defined.
- **Shared API Boundary:** REST/GraphQL route through `backend/lib/stapeln/native_bridge.ex`.
- **ABI/FFI Contract:** Idris2 ABI (`src/abi/*`) has 8 genuine proofs (no believe_me). Zig FFI (`ffi/zig/src/main.zig`) provides CRUD + validate + dispatch.
- **VeriSimDB Integration:** Remote client with JSONL local fallback, configurable timeouts.
- **Runtime Boundary:** `stapeln/backend` is the design/control plane. Container lifecycle orchestration is delegated to `container-stack/svalinn` and `container-stack/vordr`.

## What Is Partial or Scaffolded

- **Dark mode:** Hardcoded to `false` in StackView.res; AppIntegrated passes `isDark` but StackView ignores it.
- **WebSocket integration:** Socket.res exists but no live channel push/receive logic.
- **Auth:** JWT + Plug module present but no token refresh, revocation, or session management.
- **Firewall:** Schema exists but no nftables integration.
- **Database:** Ecto schemas and conditional Repo present but no migrations.
- **Post-Quantum Crypto:** Module scaffolded; no real XMSS implementation.
- **Simulation:** Packet flow UI renders but no real backend simulation engine.

## Preserved Future Work

- **DOM‑mounter track:** Extracted to `/var/mnt/eclipse/repos/stapeln-dom-mounter`.
- This work is not on the critical path for the container‑hater MVP.

## What Is Not Implemented Yet

- **Backend runtime orchestration API:** Not implemented in `stapeln/backend` by design; runtime operations belong to Svalinn/Vordr.
- **Validation Engine Depth:** Rules are basic MVP checks; not yet parity with full security roadmap.
- **Persistence:** No durable save/load outside ad-hoc import/export.
- **Formal Verification Layers:** Idris2 types are now present for ABI contracts, but full proof pipeline is not wired.

## Known Inconsistencies

- Some docs claim a “complete” product; these refer to an internal DOM‑mounter workstream.
- `IMPLEMENTATION-PLAN.md` originated as a legacy `stackur` plan and is now archival context.
- `ROADMAP.adoc` is an alias/deprecation pointer and not the planning source of truth.

## Immediate Focus (Next 4 Weeks)

- Truth-align docs and roadmap.
- Expand backend from MVP to production readiness (durable persistence, broader validation, gRPC/GRC if needed).
- End-user onboarding flow focused on “container haters.”
- Execute the six-stream plan in `docs/EXECUTION-PLAN-2026-02-11.md`.

## Readiness Gate Status

- Current readiness blocker: repo clean gate fails when local edits are present (currently `container-stack/rokur/README.md`).
