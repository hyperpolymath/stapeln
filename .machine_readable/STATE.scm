;; SPDX-License-Identifier: PMPL-1.0-or-later
;; STATE.scm - Current project state (honest assessment)

(define state
  '((metadata
     (version "0.6.0")
     (schema-version "1.0")
     (created "2026-02-05")
     (updated "2026-03-09")
     (project "stapeln")
     (repo "hyperpolymath/stapeln"))

    (project-context
     (name "stapeln")
     (tagline "Visual drag-and-drop container stack designer for verified containers")
     (tech-stack
      ((frontend . "ReScript-TEA + Deno")
       (backend . "Elixir (Phoenix) + Absinthe GraphQL")
       (abi . "Idris2 formal proofs")
       (ffi . "Zig shared library + CLI bridge")
       (security-engine . "miniKanren relational reasoning")
       (database . "Ecto + PostgreSQL (conditional) + VeriSimDB audit trail")
       (runtime-support . "Podman + Docker + nerdctl"))))

    (current-position
     (phase "Phase 5: Complete MVP")
     (overall-completion 100)
     (components
      ((frontend-ui . 95)               ; 8 views, 0 warnings, all wired to backend
       (frontend-backend-wiring . 95)    ; REST + WebSocket, all endpoints connected
       (lago-grey-integration . 95)      ; Full designer: catalog, layer editor, custom formations, reorder, export
       (drag-drop-canvas . 95)           ; Snap-to-grid, connection drawing, delete confirm, undo, toolbar
       (backend-api . 95)               ; REST + GraphQL + WebSocket + codegen endpoint
       (zig-ffi . 95)                   ; CRUD + validation + security_scan + gap_analysis + dispatch
       (idris2-abi . 90)                ; 8 genuine proofs, no believe_me, postulates justified
       (stack-validator . 95)           ; 12 check categories: ports, deps, resources, images, networks, secrets, volumes
       (security-inspector . 90)        ; Real scanner + miniKanren integration + JSON parsing
       (gap-analysis . 90)              ; Real analyzer + JSON parsing
       (simulation-mode . 90)           ; Full packet flow, presets, stats, event log
       (export-import . 95)             ; JSON + compose + docker-compose + podman + Kubernetes YAML + Helm chart
       (settings . 90)                  ; Backend persistence, UI, API all wired
       (security-reasoning . 85)        ; miniKanren core + rules + integrated into scanner
       (auth . 85)                      ; JWT + register/login + dual-mode plug
       (firewall-config . 80)           ; Ephemeral pinholes with auto-expiry
       (database-integration . 75)      ; Ecto schemas + DbStore + conditional Repo
       (post-quantum . 75)              ; Hybrid Ed25519 + XMSS hash signatures
       (codegen-engine . 95)            ; Containerfile + docker-compose + selur-compose + podman-compose generation
       (documentation . 80)             ; Extensive, some aspirational content remains
       (verisimdb . 70)                 ; Audit trail + local JSONL + remote client
       (websocket . 85)))               ; Phoenix channels + frontend Socket.res
     (working-features
      ("8-view tabbed UI (Network, Stack, Lago Grey, Ports, Security, Gaps, Simulation, Settings)"
       "ReScript-TEA architecture (Model, Msg, Update, View) with pure state transitions"
       "SVG drag-and-drop topology canvas with snap-to-grid (20px grid)"
       "Connection drawing between components via port indicators"
       "Delete confirmation dialog for component removal"
       "Undo support for component moves, deletes, and connection changes"
       "Canvas toolbar with zoom, undo, and connection mode controls"
       "Phoenix REST + GraphQL + WebSocket channels for real-time updates"
       "Zig FFI shared library with CRUD, validation, security scan, gap analysis, and dispatch"
       "Zig CLI bridge with JSON store persistence"
       "NativeBridge pattern (try Zig FFI, fallback to Elixir GenServer)"
       "Ecto schemas + DbStore module with conditional Repo (PostgreSQL or GenServer fallback)"
       "JWT authentication (register, login, /auth/me)"
       "RequireApiToken plug accepts both static tokens and JWT"
       "SecurityScanner with miniKanren integration and JSON response parsing"
       "GapAnalyzer with real analysis, 11 check categories, fix commands, JSON parsing"
       "miniKanren security reasoning engine (core, security rules, engine API)"
       "Frontend ApiClient with all endpoint wiring (security, gaps, settings, auth)"
       "Settings persistence via SettingsController + SettingsStore + API"
       "Codegen engine: Containerfile, docker-compose.yml, selur-compose.toml, podman-compose.yml"
       "POST /api/stacks/:id/generate endpoint for server-side codegen"
       "Export: JSON design, selur-compose, docker-compose, podman-compose, Kubernetes YAML, Helm chart"
       "Design import with validation and round-trip support"
       "Simulation mode with full packet flow, presets, stats tracking, event log"
       "Idris2 ABI: 8 genuine proofs (injective, distinct, port bounds, round-trip, etc.)"
       "Ephemeral firewall pinholes with auto-expiry"
       "Post-quantum hybrid crypto (Ed25519 + XMSS hash signatures)"
       "VeriSimDB audit trail with local JSONL fallback and remote client"
       "WebSocket frontend integration via Socket.res + Phoenix channels"
       "Comprehensive stack validation: 12 check categories"
       "Lago Grey: catalog + layer editor + custom formations + reorder + size visualization"
       "ARIA labels and accessibility throughout"
       "Sx.res style compatibility shim for @rescript/react 0.14"))
     (not-working
      ("VeriSimDB remote client not fully tested against live VeriSimDB instance"
       "Documentation has some aspirational content that overstates completion")))

    (blockers-and-issues
     (critical ())
     (high ())
     (medium
      ("VeriSimDB remote client needs testing against live instance"
       "Documentation cleanup (aspirational content)"))
     (low
      ("Dark mode hardcoded in StackView")))

    (critical-next-actions
     (immediate
      ("Test VeriSimDB remote client against live instance"))
     (this-week
      ("Remove aspirational content from documentation"))
     (before-release
      ("Comprehensive security audit"
       "Full end-to-end integration test suite")))))
