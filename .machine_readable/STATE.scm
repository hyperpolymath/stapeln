;; SPDX-License-Identifier: PMPL-1.0-or-later
;; STATE.scm - Current project state (honest assessment)

(define state
  '((metadata
     (version "0.5.0")
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
     (phase "Phase 4: Near-Complete MVP")
     (overall-completion 92)
     (components
      ((frontend-ui . 90)               ; 8 views, 0 warnings, all wired to backend
       (frontend-backend-wiring . 95)    ; REST + WebSocket, all endpoints connected
       (lago-grey-integration . 60)      ; Visual designer, import/export functional
       (drag-drop-canvas . 40)           ; TopologyView with SVG drag-and-drop works
       (backend-api . 95)                ; REST + GraphQL + WebSocket channels
       (zig-ffi . 60)                    ; bridge_cli functional, shared lib has real CRUD+validation
       (idris2-abi . 90)                 ; 8 genuine proofs, no believe_me, postulates justified
       (stack-validator . 30)            ; Basic MVP checks (empty, duplicates, ports)
       (security-inspector . 90)         ; Real scanner + miniKanren integration + JSON parsing
       (gap-analysis . 90)               ; Real analyzer + JSON parsing
       (simulation-mode . 90)            ; Full packet flow, presets, stats, event log
       (export-import . 70)              ; JSON design + compose file export with real images
       (settings . 90)                   ; Backend persistence, UI, API all wired
       (security-reasoning . 85)         ; miniKanren core + rules + integrated into scanner
       (auth . 85)                       ; JWT + register/login + dual-mode plug
       (firewall-config . 80)            ; Ephemeral pinholes with auto-expiry
       (database-integration . 75)       ; Ecto schemas + DbStore + conditional Repo
       (post-quantum . 75)              ; Hybrid Ed25519 + XMSS hash signatures
       (codegen-engine . 10)             ; compose.toml/docker-compose export only
       (documentation . 80)              ; Extensive, some aspirational content remains
       (verisimdb . 70)                  ; Audit trail + local JSONL + remote client
       (websocket . 85)))                ; Phoenix channels + frontend Socket.res
     (working-features
      ("8-view tabbed UI (Network, Stack, Lago Grey, Ports, Security, Gaps, Simulation, Settings)"
       "ReScript-TEA architecture (Model, Msg, Update, View) with pure state transitions"
       "SVG drag-and-drop topology canvas with zoom/pan"
       "Phoenix REST + GraphQL + WebSocket channels for real-time updates"
       "Zig FFI shared library with real CRUD and validation"
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
       "Design export to JSON with metadata"
       "Compose file export with real container images"
       "Design import with validation and round-trip support"
       "Simulation mode with full packet flow, presets, stats tracking, event log"
       "Idris2 ABI: 8 genuine proofs (injective, distinct, port bounds, round-trip, etc.)"
       "Ephemeral firewall pinholes with auto-expiry"
       "Post-quantum hybrid crypto (Ed25519 + XMSS hash signatures)"
       "VeriSimDB audit trail with local JSONL fallback and remote client"
       "WebSocket frontend integration via Socket.res + Phoenix channels"
       "ARIA labels and accessibility throughout"
       "Sx.res style compatibility shim for @rescript/react 0.14"))
     (not-working
      ("Codegen engine only supports compose.toml/docker-compose (no Containerfile generation)"
       "Stack validator is basic MVP only (empty, duplicates, ports) — no deep analysis"
       "Drag-and-drop canvas needs polish (SVG interaction edge cases)"
       "Lago Grey visual designer incomplete (60%) — import/export works but editor limited"
       "Zig FFI does not cover all backend operations (60%)"
       "VeriSimDB remote client not fully tested against live VeriSimDB instance"
       "Documentation has some aspirational content that overstates completion")))

    (blockers-and-issues
     (critical ())
     (high
      ("Codegen engine needs Containerfile generation for full MVP"))
     (medium
      ("Stack validator needs deeper analysis beyond basic checks"
       "Drag-and-drop canvas SVG interaction edge cases"
       "Lago Grey editor needs more visual editing capabilities"))
     (low
      ("Dark mode hardcoded in StackView"
       "Some deprecation warnings in frontend (Js.Array2.joinWith, Js.Exn.Error)")))

    (critical-next-actions
     (immediate
      ("Expand codegen engine to support Containerfile generation"
       "Deepen stack validator with structural and security checks"))
     (this-week
      ("Test VeriSimDB remote client against live instance"
       "Polish drag-and-drop canvas interactions"))
     (before-release
      ("Comprehensive security audit"
       "Full end-to-end integration test suite"
       "Remove aspirational content from documentation")))))
