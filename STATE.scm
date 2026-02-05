;; SPDX-License-Identifier: PMPL-1.0-or-later
;; STATE.scm - Current project state

(define state
  '((metadata
     (version "0.1.0")
     (schema-version "1.0")
     (created "2026-02-05")
     (updated "2026-02-05")
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
     (phase "Phase 0: Repository Setup")
     (overall-completion 5)
     (components
      ((frontend-ui . 0)
       (drag-drop-canvas . 0)
       (component-library . 0)
       (backend-api . 0)
       (stack-validator . 0)
       (codegen-engine . 0)
       (runtime-adapters . 0)
       (documentation . 5)))
     (working-features
      ("Repository initialized from rsr-template-repo"
       "STATE.scm created with project plan"
       "ECOSYSTEM.scm mapping to verified-container-spec ecosystem")))

    (route-to-mvp
     (milestones
      ((m1 "Frontend Setup"
           ((status . "not-started")
            (items
             ("ReScript-TEA project structure"
              "cadre-tea-router integration"
              "Deno configuration (import maps, permissions)"
              "Basic HTML canvas with grid layout"
              "Component palette sidebar"))))
       (m2 "Drag-and-Drop Core"
           ((status . "not-started")
            (items
             ("Drag-and-drop event handlers"
              "Component positioning on canvas"
              "Connection lines between components"
              "Stack visualization (vertical representation)"
              "Snap-to-grid functionality"))))
       (m3 "Component Library"
           ((status . "not-started")
            (items
             ("Cerro Torre component (ct CLI)"
              "Svalinn component (edge gateway)"
              "selur component (IPC bridge)"
              "Vörðr component (orchestrator)"
              "Runtime selectors (Podman/Docker/nerdctl)"
              "Volume and network components"))))
       (m4 "Backend API (Elixir)"
           ((status . "not-started")
            (items
             ("Phoenix server with REST endpoints"
              "Stack validation via Ephapax linear types"
              "AffineScript for constraint checking"
              "Idris2 proofs for dependency ordering"
              "WebSocket for real-time preview"))))
       (m5 "Code Generation (Rust)"
           ((status . "not-started")
            (items
             ("compose.toml generator"
              "docker-compose.yml generator"
              "podman-compose.yml generator"
              "Validation against verified-container-spec"
              "Export to multiple formats"))))
       (m6 "Runtime Integration"
           ((status . "not-started")
            (items
             ("Podman API client"
              "Docker API client"
              "nerdctl CLI wrapper"
              "Live deployment testing"
              "Rollback support")))))))

    (blockers-and-issues
     (critical
      ("Need cadre-tea-router package (check if published to npm/JSR)")
      ("Ephapax-linear not production-ready (use Idris2 + Rust for MVP)")
      ("AffineScript compiler status unclear (use Rust affine types for MVP)"))
     (high
      ("Drag-and-drop with SVG canvas vs HTML5 canvas decision"
       "Component connection routing algorithm (orthogonal vs bezier)"))
     (medium
      ("WebGL for large stacks (100+ containers)"
       "Undo/redo for stack edits"))
     (low
      ("Dark mode theme"
       "Keyboard shortcuts for power users")))

    (critical-next-actions
     (immediate
      ("Set up ReScript project with Deno"
       "Install cadre-tea-router (or vendor if needed)"
       "Create basic TEA app structure (Model, Msg, update, view)"
       "Design component type system in Idris2"))
     (this-week
      ("Implement drag-and-drop prototype"
       "Create component palette UI"
       "Set up Phoenix backend skeleton"
       "Design API endpoints for stack validation"))
     (this-month
      ("Complete drag-and-drop with connection lines"
       "Implement stack validation in Ephapax/Idris2"
       "Create compose.toml codegen in Rust"
       "Test with real selur-compose deployments")))))
