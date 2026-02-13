;; SPDX-License-Identifier: PMPL-1.0-or-later
;; STATE.scm - Current project state (honest assessment)

(define state
  '((metadata
     (version "0.3.0")
     (schema-version "1.0")
     (created "2026-02-05")
     (updated "2026-02-13")
     (project "stapeln")
     (repo "hyperpolymath/stapeln"))

    (project-context
     (name "stapeln")
     (tagline "Visual drag-and-drop container stack designer for verified containers")
     (tech-stack
      ((frontend . "ReScript-TEA + Deno")
       (backend . "Elixir (Phoenix) + Absinthe GraphQL")
       (abi . "Idris2 type definitions")
       (ffi . "Zig shared library + CLI bridge")
       (runtime-support . "Podman + Docker + nerdctl"))))

    (current-position
     (phase "Phase 2: Frontend + Backend MVP")
     (overall-completion 35)
     (components
      ((frontend-ui . 50)           ; 8 views render and navigate, TEA pattern working
       (frontend-backend-wiring . 15) ; API client added, SaveStack/LoadStack wired
       (lago-grey-integration . 60) ; Visual designer, import/export functional
       (drag-drop-canvas . 40)      ; TopologyView with SVG drag-and-drop works
       (backend-api . 40)           ; REST + GraphQL CRUD + validation live
       (zig-ffi . 60)              ; bridge_cli fully functional, shared lib has real CRUD+validation
       (idris2-abi . 20)           ; Type definitions only, no proofs
       (stack-validator . 30)      ; Basic MVP checks (empty, duplicates, ports)
       (security-inspector . 30)    ; UI renders with sample data, no backend calls
       (gap-analysis . 30)         ; UI renders with sample data, no backend calls
       (simulation-mode . 35)      ; WASM/JS batch packet kernel with backpressure
       (export-import . 70)        ; JSON design + compose file export with real images
       (settings . 25)             ; Form renders, localStorage save added, no full persistence
       (security-reasoning . 0)    ; miniKanren not implemented (documentation only)
       (firewall-config . 0)       ; Not implemented (documentation only)
       (database-integration . 0)  ; VeriSimDB not connected
       (auth . 0)                  ; No authentication
       (post-quantum . 0)          ; Not started
       (codegen-engine . 10)       ; compose.toml/docker-compose export works
       (documentation . 80)))      ; Extensive but some aspirational content remains
     (working-features
      ("8-view tabbed UI (Network, Stack, Lago Grey, Ports, Security, Gaps, Simulation, Settings)"
       "ReScript-TEA architecture (Model, Msg, Update, View) with pure state transitions"
       "SVG drag-and-drop topology canvas with zoom/pan"
       "Phoenix REST + GraphQL CRUD for stacks with validation"
       "Zig FFI shared library with real CRUD and validation (not stubs)"
       "Zig CLI bridge with JSON store persistence"
       "NativeBridge pattern (try Zig FFI, fallback to Elixir GenServer)"
       "In-memory StackStore GenServer with JSON file persistence"
       "Design export to JSON with metadata"
       "Compose file export with real container images (not TODO stubs)"
       "Design import with validation and round-trip support"
       "Simulation mode with WASM/JS batch packet stepping"
       "SecurityInspector with sample vulnerability data and scoring"
       "GapAnalysis with sample gap detection data"
       "Settings page with localStorage persistence"
       "Frontend-backend API wiring for SaveStack/LoadStack"
       "ARIA labels and accessibility throughout"
       "Idris2 ABI type definitions and FFI declarations"))
     (not-working
      ("RunSecurityScan/RunGapAnalysis buttons call backend but use sample data"
       "Settings form values don't propagate back to model"
       "No authentication (PAM or otherwise)"
       "No database (in-memory only)"
       "No miniKanren security engine"
       "No VeriSimDB integration"
       "No OWASP ModSecurity"
       "No ephemeral pinholes"
       "No post-quantum crypto"
       "Idris2 proofs are declarations only (no actual proofs)")))

    (blockers-and-issues
     (critical
      ("Frontend-backend data model mismatch (components vs services)"))
     (high
      ("SecurityInspector and GapAnalysis operate on hardcoded sample data"
       "No persistent database (Ecto/PostgreSQL)"
       "No authentication system"))
     (medium
      ("dev-server.ts is TypeScript (should be .js)"
       "Old HTML prototypes still in repo"
       "Nested .github/workflows in frontend/"
       "node_modules tracked for ReScript compilation"))
     (low
      ("Dark mode hardcoded in StackView"
       "Old project name 'stackur' referenced in StackView")))

    (critical-next-actions
     (immediate
      ("Connect SecurityInspector to backend validation API"
       "Connect GapAnalysis to backend validation API"
       "Add Ecto + PostgreSQL for persistent storage"
       "Remove old HTML prototypes"))
     (this-week
      ("Implement authentication (PAM or JWT)"
       "Translate frontend component model to backend service model"
       "Add WebSocket for real-time validation updates"))
     (before-release
      ("Implement miniKanren security reasoning engine"
       "Add Idris2 formal proofs (not just declarations)"
       "VeriSimDB integration for audit trails"
       "Post-quantum crypto"
       "Comprehensive security audit")))))
