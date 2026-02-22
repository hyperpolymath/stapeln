<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- TOPOLOGY.md — Project architecture map and completion dashboard -->
<!-- Last updated: 2026-02-19 -->

# stapeln — Project Topology

## System Architecture

```
                        ┌─────────────────────────────────────────┐
                        │              OPERATOR / ADMIN           │
                        │        (Visual Designer / Drag-and-Drop)│
                        └───────────────────┬─────────────────────┘
                                            │
                                            ▼
                        ┌─────────────────────────────────────────┐
                        │           UI LAYER (RESCRIPT TEA)       │
                        │  ┌───────────┐  ┌───────────────────┐  │
                        │  │ Paragon   │  │ Cisco View        │  │
                        │  │ (Vertical)│  │ (Network Topo)    │  │
                        │  └─────┬─────┘  └────────┬──────────┘  │
                        │  ┌─────▼─────┐  ┌────────▼──────────┐  │
                        │  │ Lago Grey │  │  Simulation       │  │
                        │  │ (Images)  │  │  Mode (WASM)      │  │
                        │  └─────┬─────┘  └────────┬──────────┘  │
                        └────────│─────────────────│──────────────┘
                                 │                 │
                                 ▼                 ▼
                        ┌─────────────────────────────────────────┐
                        │           BACKEND CORE (ELIXIR)         │
                        │    (Phoenix, GraphQL, Orchestration)    │
                        └──────────┬───────────────────┬──────────┘
                                   │                   │
                                   ▼                   ▼
                        ┌───────────────────────┐  ┌────────────────────────────────┐
                        │ REASONING ENGINE      │  │ SECURITY & DATA                │
                        │ - miniKanren (Guile)  │  │ - VeriSimDB (Multimodal)       │
                        │ - OWASP Rules         │  │ - A2ML (Attested Docs)         │
                        │ - Gap Analysis        │  │ - K9-SVC (Nickel Contracts)    │
                        └──────────┬────────────┘  └──────────┬─────────────────────┘
                                   │                          │
                                   └────────────┬─────────────┘
                                                ▼
                        ┌─────────────────────────────────────────┐
                        │           CONTAINER RUNTIME             │
                        │      (Podman, Firewalld, nftables)      │
                        └─────────────────────────────────────────┘

                        ┌─────────────────────────────────────────┐
                        │          REPO INFRASTRUCTURE            │
                        │  Deno Tooling       .machine_readable/  │
                        │  Justfile / Cargo   0-AI-MANIFEST.a2ml  │
                        └─────────────────────────────────────────┘
```

## Completion Dashboard

```
COMPONENT                          STATUS              NOTES
─────────────────────────────────  ──────────────────  ─────────────────────────────────
USER INTERFACES
  Paragon/Cisco/Settings Views      ██████████ 100%    TEA pattern stable
  Lago Grey Designer                ██████████ 100%    Ice formation logic verified
  Simulation Mode                   ████░░░░░░  40%    WASM packet kernel prototyping
  Attack Surface Analyzer           ████░░░░░░  40%    UI sample data only

BACKEND & SECURITY
  Phoenix / GraphQL API             ████████░░  80%    CRUD endpoints active
  miniKanren Engine                 ████░░░░░░  40%    Rules scaffolding verified
  Ephemeral Pinholes                ██████░░░░  60%    Auto-expiry logic stable
  VeriSimDB Integration             ████░░░░░░  40%    Documentation stubs

REPO INFRASTRUCTURE
  Deno Build Tasks                  ██████████ 100%    rescript@11 verified
  .machine_readable/                ██████████ 100%    STATE tracking active
  WCAG 2.3 AAA Compliance           ██████████ 100%    Accessibility verified

─────────────────────────────────────────────────────────────────────────────
OVERALL:                            ████░░░░░░  ~35%   Infrastructure stable, Core maturing
```

## Key Dependencies

```
miniKanren Rules ──► Gap Analysis ───► Paragon View ───► Security Score
     │                   │                 │                 │
     ▼                   ▼                 ▼                 ▼
VeriSimDB ──────► Trace Audit ─────► Cisco View ──────► Simulation
```

## Update Protocol

This file is maintained by both humans and AI agents. When updating:

1. **After completing a component**: Change its bar and percentage
2. **After adding a component**: Add a new row in the appropriate section
3. **After architectural changes**: Update the ASCII diagram
4. **Date**: Update the `Last updated` comment at the top of this file

Progress bars use: `█` (filled) and `░` (empty), 10 characters wide.
Percentages: 0%, 10%, 20%, ... 100% (in 10% increments).
