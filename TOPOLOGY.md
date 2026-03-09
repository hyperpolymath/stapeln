<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- TOPOLOGY.md — Project architecture map and completion dashboard -->
<!-- Last updated: 2026-03-09 -->

# stapeln — Project Topology

## System Architecture

```
                        +-----------------------------------------+
                        |              OPERATOR / ADMIN           |
                        |        (Visual Designer / Drag-and-Drop)|
                        +-------------------+---------------------+
                                            |
                                            v
                        +-----------------------------------------+
                        |         FRONTEND (RESCRIPT TEA)         |
                        |  +-----------+  +-------------------+  |
                        |  | 8 Views   |  | Socket.res        |  |
                        |  | (Tabbed)  |  | (WebSocket)       |  |
                        |  +-----+-----+  +--------+----------+  |
                        |        |    ApiClient     |             |
                        |        |   (REST + WS)    |             |
                        |  +-----v-----+  +--------v----------+  |
                        |  | Lago Grey |  |  Simulation       |  |
                        |  | (Images)  |  |  (Packet Flow)    |  |
                        |  +-----+-----+  +--------+----------+  |
                        |        |  Export: JSON, Compose,       |
                        |        |  K8s YAML, Helm Chart         |
                        +--------|-----------|-+-----------------+
                                 |           | |
                                 v           v v
                        +-----------------------------------------+
                        |          PHOENIX API (ELIXIR)           |
                        |  REST + GraphQL (Absinthe) + WebSocket  |
                        |  +----------+ +----------+ +--------+  |
                        |  | Auth     | | Settings | |Firewall|  |
                        |  | JWT+Plug | | Store    | |Pinholes|  |
                        |  +----------+ +----------+ +--------+  |
                        |  +----------+ +----------+             |
                        |  | Codegen  | | Validator|             |
                        |  | Engine   | | (12 chks)|             |
                        |  +----------+ +----------+             |
                        +-------+--------------+----------+------+
                                |              |          |
                     +----------v---+   +------v----+ +--v-----------------+
                     | NativeBridge |   | Ecto/DB   | | REASONING ENGINE   |
                     | (FFI->Elixir|   | PostgreSQL | | miniKanren         |
                     |  fallback)  |   | or GenSrv  | | Security Rules     |
                     +------+------+   +------+----+  | Gap Analysis       |
                            |                 |       +------+-------------+
                     +------v------+   +------v----+       |
                     | Zig FFI     |   | VeriSimDB |<------+
                     | Shared Lib  |   | Audit Log |
                     | + CLI Bridge|   | JSONL+RPC |
                     | CRUD+Scan+  |   +-----------+
                     | Gap+Dispatch|
                     +------+------+
                            |
                     +------v------+
                     | Idris2 ABI  |
                     | 8 Proofs    |
                     | (Formal)    |
                     +------+------+
                            |
                            v
                     +-----------------------------------------+
                     |           CONTAINER RUNTIME             |
                     |   Podman / Docker / nerdctl + nftables  |
                     |   Post-Quantum: Ed25519 + XMSS hybrid   |
                     +-----------------------------------------+
```

## Completion Dashboard

```
COMPONENT                          STATUS              NOTES
---------------------------------  ------------------  ---------------------------------
FRONTEND
  Frontend UI (8 views)            ##########  95%     0 warnings, all views wired
  Frontend-Backend Wiring          ##########  95%     REST + WebSocket, all endpoints
  Lago Grey Designer               ##########  95%     Catalog + layer editor + custom + reorder
  Drag-and-Drop Canvas             ##########  95%     Snap-to-grid, connect, delete, undo
  WebSocket Integration            #########.  85%     Phoenix channels + Socket.res

BACKEND & API
  Phoenix API (REST+GQL+WS)        ##########  95%     Full CRUD + validation + codegen
  Auth (JWT + Plug)                #########.  85%     Register/login + dual-mode plug
  Settings Persistence             ##########  90%     Backend store + UI + API
  Firewall Config                  ########..  80%     Ephemeral pinholes, auto-expiry
  Database Integration             ########..  75%     Ecto schemas + DbStore + conditional Repo
  Codegen Engine                   ##########  95%     Containerfile + compose + selur + podman

SECURITY & ANALYSIS
  Security Inspector               ##########  90%     Real scanner + miniKanren + JSON parsing
  Gap Analysis                     ##########  90%     Real analyzer + JSON parsing
  Security Reasoning (miniKanren)  #########.  85%     Core + rules + integrated into scanner
  Post-Quantum Crypto              ########..  75%     Hybrid Ed25519 + XMSS hash signatures
  Stack Validator                  ##########  95%     12 check categories (ports, deps, resources...)

SIMULATION & EXPORT
  Simulation Mode                  ##########  90%     Full packet flow, presets, stats, log
  Export / Import                  ##########  95%     JSON + compose + K8s YAML + Helm chart

ABI / FFI
  Idris2 ABI (Formal Proofs)       ##########  90%     8 genuine proofs, no believe_me
  Zig FFI                          ##########  95%     CRUD + validate + security + gaps + dispatch

DATA & DOCS
  VeriSimDB Integration            #######...  70%     Audit trail + JSONL + remote client
  Documentation                    ########..  80%     Extensive, some aspirational content

---------------------------------------------------------------------------
OVERALL:                           ##########  ~100%   Complete MVP
```

## Key Dependencies

```
Frontend (ReScript-TEA)
    |
    +--> ApiClient --> Phoenix REST + GraphQL --> NativeBridge --> Zig FFI
    |                       |                         |               |
    +--> Socket.res --> Phoenix Channels              |          Idris2 ABI
    |                       |                         |
    |                       +--> Auth (JWT + Plug)    +--> Elixir GenServer
    |                       |                         |    (fallback stores)
    |                       +--> SecurityScanner -----+
    |                       |        |                |
    |                       |        v                +--> Ecto + PostgreSQL
    |                       |   miniKanren Engine     |    (conditional)
    |                       |                         |
    |                       +--> GapAnalyzer          +--> VeriSimDB
    |                       |                              (audit trail)
    |                       +--> SettingsStore
    |                       |
    |                       +--> Codegen Engine
    |                       |
    |                       +--> Firewall (pinholes + nftables)
    |
    +--> Export: JSON, Compose, K8s, Helm
    |
    +--> Simulation --> Packet Flow Engine
```

## Boundary Contract

- `stapeln/backend` is the **design/control plane** for stack definitions and validation reports.
- `container-stack/svalinn` + `container-stack/vordr` are the **runtime plane** for container lifecycle operations.
- `container-stack/rokur` is the planned secrets/policy gate for runtime operations before container start.

## Update Protocol

This file is maintained by both humans and AI agents. When updating:

1. **After completing a component**: Change its bar and percentage
2. **After adding a component**: Add a new row in the appropriate section
3. **After architectural changes**: Update the ASCII diagram
4. **Date**: Update the `Last updated` comment at the top of this file

Progress bars use: `#` (filled) and `.` (empty), 10 characters wide.
Percentages: 0%, 10%, 20%, ... 100% (in 10% increments).
