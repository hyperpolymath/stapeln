;; SPDX-License-Identifier: PMPL-1.0-or-later
;; STATE.scm - Current project state (honest assessment)

(define state
  '((metadata
     (version "0.4.0")
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
       (runtime-support . "Podman + Docker + nerdctl"))))

    (current-position
     (phase "Phase 3: Feature Complete MVP")
     (overall-completion 75)
     (components
      ((frontend-ui . 80)           ; 8 views render and navigate, TEA wired to backend
       (frontend-backend-wiring . 85) ; ApiClient, SecurityScan, GapAnalysis, Settings all wired
       (lago-grey-integration . 60) ; Visual designer, import/export functional
       (drag-drop-canvas . 40)      ; TopologyView with SVG drag-and-drop works
       (backend-api . 80)           ; REST + GraphQL CRUD + validation + security + gaps
       (zig-ffi . 60)              ; bridge_cli fully functional, shared lib has real CRUD+validation
       (idris2-abi . 90)           ; 8 genuine proofs, no believe_me, postulates justified
       (stack-validator . 30)      ; Basic MVP checks (empty, duplicates, ports)
       (security-inspector . 80)    ; 558-line real scanner, vulnerability detection, grading
       (gap-analysis . 80)         ; 639-line real analyzer, 11 check categories, fix commands
       (simulation-mode . 35)      ; WASM/JS batch packet kernel with backpressure
       (export-import . 70)        ; JSON design + compose file export with real images
       (settings . 80)             ; Full backend persistence via SettingsStore + SettingsController
       (security-reasoning . 70)   ; miniKanren core + security rules + engine API
       (auth . 75)                 ; JWT auth, register/login, UserStore, RequireApiToken dual-mode
       (firewall-config . 0)       ; Not implemented
       (database-integration . 0)  ; In-memory GenServer stores (StackStore, UserStore, SettingsStore)
       (post-quantum . 0)          ; Not started
       (codegen-engine . 10)       ; compose.toml/docker-compose export works
       (documentation . 80)))      ; Extensive, some aspirational content remains
     (working-features
      ("8-view tabbed UI (Network, Stack, Lago Grey, Ports, Security, Gaps, Simulation, Settings)"
       "ReScript-TEA architecture (Model, Msg, Update, View) with pure state transitions"
       "SVG drag-and-drop topology canvas with zoom/pan"
       "Phoenix REST + GraphQL CRUD for stacks with validation"
       "Zig FFI shared library with real CRUD and validation"
       "Zig CLI bridge with JSON store persistence"
       "NativeBridge pattern (try Zig FFI, fallback to Elixir GenServer)"
       "In-memory StackStore, UserStore, SettingsStore GenServers with JSON persistence"
       "JWT authentication (register, login, /auth/me)"
       "RequireApiToken plug accepts both static tokens and JWT"
       "SecurityScanner: 558 lines, vulnerability detection, port analysis, grading A+-F"
       "GapAnalyzer: 639 lines, 11 check categories, fix commands, effort estimates"
       "miniKanren security reasoning engine (core, security rules, engine API)"
       "Frontend ApiClient with all endpoint wiring (security, gaps, settings, auth)"
       "Settings persistence via SettingsController + SettingsStore"
       "Design export to JSON with metadata"
       "Compose file export with real container images"
       "Design import with validation and round-trip support"
       "Simulation mode with WASM/JS batch packet stepping"
       "Idris2 ABI: 8 genuine proofs (injective, distinct, port bounds, round-trip, etc.)"
       "ARIA labels and accessibility throughout"
       "Sx.res style compatibility shim for @rescript/react 0.14"))
     (not-working
      ("No persistent database (Ecto/PostgreSQL) — GenServer stores only"
       "No VeriSimDB integration"
       "No ephemeral pinholes"
       "No post-quantum crypto"
       "SecurityScanResult/GapAnalysisResult still use default init state (JSON parsing TODO)")))

    (blockers-and-issues
     (critical ())
     (high
      ("No persistent database (Ecto/PostgreSQL)"
       "Security/gap scan results not parsed from backend JSON — uses init defaults"))
     (medium
      ("Deprecation warnings in frontend (Js.Array2.joinWith, Js.Exn.Error)"
       "Simulation mode only 35% complete"))
     (low
      ("Dark mode hardcoded in StackView")))

    (critical-next-actions
     (immediate
      ("Parse SecurityScanner/GapAnalyzer JSON responses in Update.res"
       "Add Ecto + PostgreSQL for persistent storage"))
     (this-week
      ("Integrate miniKanren engine into SecurityScanner for enriched findings"
       "Add WebSocket for real-time validation updates"))
     (before-release
      ("VeriSimDB integration for audit trails"
       "Post-quantum crypto"
       "Comprehensive security audit")))))
