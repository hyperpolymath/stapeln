<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- TOPOLOGY.md — Project architecture map and completion dashboard -->
<!-- Last updated: 2026-03-10 -->

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
  Frontend UI (8 views)            ########..  80%     Views exist; dark mode hardcoded, no durable state sync
  Frontend-Backend Wiring          ######....  60%     REST wired; WS scaffolded but no live events
  Lago Grey Designer               #######...  70%     Catalog + editor; export not fully wired
  Drag-and-Drop Canvas             #######...  70%     Snap-to-grid present; undo/delete partial
  WebSocket Integration            #####.....  50%     Socket.res exists; no channel push/receive logic

BACKEND & API
  Phoenix API (REST+GQL+WS)        ########..  80%     CRUD + validation live; some endpoints thin
  Auth (JWT + Plug)                ######....  60%     Module exists; no session/token refresh/revoke
  Settings Persistence             #######...  70%     GenServer store; no DB persistence yet
  Firewall Config                  #####.....  50%     Schema present; nftables integration absent
  Database Integration             #####.....  50%     Ecto schemas + conditional Repo; no migrations
  Codegen Engine                   ########..  80%     Containerfile + compose output works

SECURITY & ANALYSIS
  Security Inspector               #######...  70%     Scanner module exists; miniKanren rules basic
  Gap Analysis                     #######...  70%     Analyzer module exists; rule coverage limited
  Security Reasoning (miniKanren)  ######....  60%     Core module; small rule set, not deeply tested
  Post-Quantum Crypto              ###.......  30%     Module scaffolded; no real XMSS implementation
  Stack Validator                  ########..  80%     12 check categories defined; some stub-level

SIMULATION & EXPORT
  Simulation Mode                  #######...  70%     Packet flow UI; no real backend simulation
  Export / Import                  #######...  70%     JSON + compose export; K8s/Helm templates only

ABI / FFI
  Idris2 ABI (Formal Proofs)       #########.  90%     8 genuine proofs, no believe_me, 5 postulates
  Zig FFI                          ########..  80%     CRUD + validate + dispatch; CLI bridge partial

DATA & DOCS
  VeriSimDB Integration            ######....  60%     JSONL fallback + remote client; no query UI
  Documentation                    ######....  60%     Extensive docs; many claims aspirational

---------------------------------------------------------------------------
OVERALL:                           #######...  ~65%    Solid MVP skeleton; many features partial
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
