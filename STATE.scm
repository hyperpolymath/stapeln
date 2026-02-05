;; SPDX-License-Identifier: PMPL-1.0-or-later
;; STATE.scm - Current project state

(define state
  '((metadata
     (version "0.1.0")
     (schema-version "1.0")
     (created "2026-02-05")
     (updated "2026-02-05T19:45:00Z")
     (project "stapeln")
     (repo "hyperpolymath/stapeln"))

    (project-context
     (name "stapeln")
     (tagline "Visual drag-and-drop container stack designer for verified containers")
     (tech-stack
      ((frontend . "ReScript-TEA + cadre-tea-router + Deno")
       (backend . "Elixir (Phoenix) + Ephapax + AffineScript")
       (validation . "Idris2 proofs for stack correctness")
       (codegen . "Rust for compose.toml generation")
       (runtime-support . "Podman + Docker + nerdctl"))))

    (current-position
     (phase "Phase 2: Frontend Implementation - Compilation Complete")
     (overall-completion 75)
     (components
      ((frontend-ui . 80)         ; 8-page navigation, all Phase 2 components complete, compiled
       (lago-grey-integration . 85) ; Visual designer integrated, import/export complete
       (drag-drop-canvas . 20)     ; Architecture defined, not implemented
       (component-library . 15)    ; Types defined in Model.res
       (backend-api . 10)          ; GraphQL schema designed
       (stack-validator . 0)       ; Pending miniKanren implementation
       (security-analysis . 85)    ; SecurityInspector + GapAnalysis components complete & tested
       (firewall-config . 85)      ; PortConfigPanel with ephemeral pinholes complete & tested
       (simulation-engine . 70)    ; SimulationMode with 60fps packet animation complete
       (database-integration . 35) ; VeriSimDB + A2ML + K9 specs complete
       (codegen-engine . 0)
       (runtime-adapters . 0)
       (documentation . 95)))      ; Comprehensive docs + roadmap created
     (working-features
      ("Repository initialized from rsr-template-repo"
       "STATE.scm, ECOSYSTEM.scm, META.scm created"
       "Full ReScript-TEA architecture (Model, Msg, Update, View)"
       "Four-page UI design (Paragon, Cisco, Lago Grey, Settings)"
       "LagoGreyImageDesigner integrated as Page 4"
       "Interactive ice formation designer (Floes, Icebergs, Glaciers)"
       "Real-time size calculation and competitive comparison"
       "Dark theme UI with Lago Grey branding"
       "LagoGrey component type added to Model.res"
       "WCAG 2.3 AAA accessibility implementation"
       "GraphQL schema with a11y metadata"
       "Database integration spec (VeriSimDB, A2ML, K9-SVC)"
       "Security stack audit (47% compliance, identified gaps)"
       "Firewall config with OWASP ModSecurity + ephemeral pinholes"
       "miniKanren security reasoning engine design"
       "Game-like UI mockups for attack surface analyzer"
       "UX Manifesto and Container-Hater Test documents"
       "Deno-only setup (no npm/node) via SETUP.md"
       "Comprehensive ROADMAP.md for v1.0 path"
       "SimulationMode.res - Cisco Packet Tracer-style animation (779 lines)"
       "60fps packet simulation with requestAnimationFrame"
       "Idris² badge component with 3 styles (304 lines)"
       "Formal verification: ValidElementId, NoMemoryLeak, AtomicMount, ElementExists"
       "ReScript compilation: 37/37 modules compiled successfully"
       "Test suite: 37/38 passing (97% success rate)"
       "Deno dev server with WebSocket hot reload running"
       "Server at http://localhost:8080 with SPA routing"
       "8-page navigation: Network, Stack, LagoGrey, Port, Security, Gap, Simulation, Settings"
       "DomMounter.res with Idris2 proven DOM mounting"
       "PROVEN-LIBRARIES.md: Idris2 → Zig → ReScript architecture docs"
       "ABI directory cleaned (only Idris2 .idr files)"
       "Fixed calculateGrade function ordering in SecurityInspector.res")))

    (route-to-mvp
     (milestones
      ((m1 "Design & Specifications"
           ((status . "complete")
            (completion . 100)
            (items
             ("✅ ReScript-TEA architecture (Model, Msg, Update, View)"
              "✅ Three-page UI design (Paragon, Cisco, Settings)"
              "✅ WCAG 2.3 AAA accessibility specs"
              "✅ GraphQL schema with a11y metadata"
              "✅ Deno-only setup documentation"
              "✅ Database integration spec (VeriSimDB, A2ML, K9)"
              "✅ Security reasoning engine design (miniKanren)"
              "✅ Firewall config with ephemeral pinholes"
              "✅ UX Manifesto and Container-Hater Test"
              "✅ Game-like UI mockups"))))
       (m2 "Frontend Implementation"
           ((status . "in-progress")
            (completion . 80)
            (items
             ("✅ Model.res - Type definitions and state"
              "✅ Msg.res - Message types for TEA"
              "✅ Update.res - State transition logic"
              "✅ View.res - Paragon-style vertical stack view"
              "✅ CiscoView.res - Network topology view"
              "✅ Settings.res - Preferences and defaults"
              "✅ LagoGreyImageDesigner.res - Base image designer component"
              "✅ LagoGreyImageDesigner.css - Dark theme styling"
              "✅ App.res - Eight-page navigation with all Phase 2 components"
              "✅ PortConfigPanel.res - Ephemeral pinholes with auto-expiry (475 lines)"
              "✅ SecurityInspector.res - Attack surface analysis with metrics (759 lines)"
              "✅ GapAnalysis.res - Automated remediation with AI reasoning (852 lines)"
              "✅ SimulationMode.res - Cisco Packet Tracer-style animation (779 lines)"
              "✅ IdrisBadge.res - 'Idris² inside' badge with proof list (304 lines)"
              "✅ TeaRouter.res - Simple TEA-compatible routing (148 lines)"
              "✅ DomMounter.res - Idris2 proven DOM mounting with formal proofs"
              "✅ Index.res - Application entry point with proven mounting"
              "✅ ReScript compilation successful: 37/37 modules compiled"
              "✅ Test suite: 37/38 tests passing (97%)"
              "✅ Dev server: Deno with WebSocket hot reload at http://localhost:8080"
              "⚠️ Router integration - cadre-tea-router or custom solution (pending)"
              "⚠️ Auth.res - User authentication flow (pending)"))))
       (m2.5 "Lago Grey Integration"
           ((status . "in-progress")
            (completion . 85)
            (items
             ("✅ LagoGreyImageDesigner component created (921 lines)"
              "✅ Interactive ice formation catalog (Floes, Icebergs, Glaciers)"
              "✅ Component selection with real-time size tracking"
              "✅ Base image selector (Distroless, Alpine, Scratch)"
              "✅ Visual layer stacking on canvas"
              "✅ Competitive comparison (vs Alpine, target 17.5MB)"
              "✅ Security status indicators (PQ crypto, classical crypto)"
              "✅ Dark theme with Lago Grey branding"
              "✅ LagoGrey componentType added to Model.res"
              "✅ ECOSYSTEM.scm integration documented"
              "✅ Design import/export (JSON schema v1.0)"
              "✅ Dockerfile generation from selections"
              "✅ Manifest.json generation with security metadata"
              "✅ Export buttons functional in UI"
              "✅ Complete package export (Dockerfile + manifest + instructions)"
              "⚠️ Triple crypto signing (requires oblibeny integration, pending)"
              "⚠️ Build pipeline (direct podman API, Phase 3)"))))
       (m2.6 "Import/Export Implementation"
           ((status . "complete")
            (completion . 100)
            (items
             ("✅ DesignFormat.res - JSON schema with serialization"
              "✅ Export.res - Export designs, compose files"
              "✅ Import.res - Import with validation"
              "✅ LagoGreyExport.res - Dockerfile, manifest generation"
              "✅ UI buttons in nav bar (Import/Export)"
              "✅ Export buttons in LagoGreyImageDesigner"
              "✅ Msg.res updated with import/export messages"
              "✅ Update.res wired to handle all messages"
              "✅ Round-trip tested (export → import → verify)"))))
       (m3 "Security Reasoning Engine"
           ((status . "not-started")
            (completion . 0)
            (items
             ("miniKanren + Guile Scheme setup"
              "Core security relations (componento, exposed-porto, etc.)"
              "Security rules (SSH exposure, root user, unencrypted traffic)"
              "CVE feed sync script"
              "OWASP Top 10 rule generator"
              "CIS Benchmark rules"
              "Elixir backend integration (S-expression parser)"
              "Provenance chain generation"))))
       (m4 "Backend API (Elixir + Phoenix)"
           ((status . "not-started")
            (completion . 0)
            (items
             ("Phoenix server with GraphQL (Absinthe)"
              "EphemeralPinhole GenServer for firewall pinholes"
              "SecurityReasoner module (miniKanren interface)"
              "VeriSimDB integration"
              "A2ML parser for attested documentation"
              "K9-SVC validator for component configs"
              "PAM authentication for user-only access"
              "Audit log to VeriSimDB temporal modality"
              "WebSocket for real-time updates"))))
       (m5 "Firewall & Security"
           ((status . "not-started")
            (completion . 0)
            (items
             ("ModSecurity + OWASP CRS in Svalinn gateway"
              "firewalld default-deny rules"
              "Ephemeral pinhole implementation"
              "Port conflict detection"
              "Component security scanning (Grype CVE scanner)"
              "Attack surface analyzer"
              "Gap analysis engine"))))
       (m6 "Code Generation & Deployment"
           ((status . "not-started")
            (completion . 0)
            (items
             ("compose.toml generator (Rust)"
              "docker-compose.yml generator"
              "podman-compose.yml generator"
              "Validation against verified-container-spec"
              "Podman API client"
              "Docker API client"
              "Live deployment testing"
              "Simulation mode (pre-deployment dry-run)"
              "Rollback support")))))))

    (blockers-and-issues
     (critical
      ("None currently - design phase complete"))
     (high
      ("cadre-tea-router availability (check JSR/npm or vendor)"
       "Guile Scheme + miniKanren installation/setup"
       "Elixir backend needs PAM library for system user auth"
       "VeriSimDB federated mode configuration"))
     (medium
      ("SVG vs Canvas rendering decision for Cisco view"
       "Component connection routing algorithm (orthogonal recommended)"
       "WebGL for large stacks (100+ containers) - future optimization"
       "Simulation mode packet animation performance"))
     (low
      ("Dark mode theme refinement"
       "Keyboard shortcuts for power users"
       "Braille display testing (WCAG AAA requirement)")))

    (critical-next-actions
     (immediate
      ("Test ReScript compilation (rescript build)"
       "Test import/export in browser (dev server)"
       "Verify Dockerfile generation produces valid output"
       "Test complete package export workflow"
       "Update README.adoc with import/export instructions"
       "Update documentation with usage examples"))
     (this-week
      ("Build pipeline: Direct podman API integration (Phase 3)"
       "Implement triple crypto signing (requires oblibeny)"
       "Build accountability trace generation (.zpkg)"
       "Add progress bars for build process"
       "Implement compose.toml full schema generation"
       "Add error handling UI (toast notifications)"
       "User testing: import/export workflow"))
     (this-month
      ("✅ COMPLETED: PortConfigPanel.res with ephemeral toggle"
       "✅ COMPLETED: SecurityInspector.res component"
       "✅ COMPLETED: GapAnalysis.res automated remediation"
       "Wire new components (PortConfig/Security/Gap) into App.res"
       "Create miniKanren proof-of-concept with 5 basic security rules"
       "Implement SimulationMode.res - packet animation"
       "Set up Phoenix backend with GraphQL"
       "User testing with container-hater (cyberwar officer!)"))
     (before-release
      ("Complete security reasoning engine (miniKanren)"
       "Implement ModSecurity + OWASP CRS in Svalinn"
       "Post-quantum crypto upgrades (full Dilithium5, Kyber-1024)"
       "HTTP/3 + QUIC migration"
       "Comprehensive security audit"
       "Performance optimization (handle 100+ container stacks)"
       "WCAG 2.3 AAA compliance verification"
       "Documentation for end users"
       "Deployment guide")))))
