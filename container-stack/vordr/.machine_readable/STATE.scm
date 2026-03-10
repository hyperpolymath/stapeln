;; SPDX-License-Identifier: PMPL-1.0-or-later
;; STATE.scm — Current state, progress, and session tracking for Vordr
;; Format: hyperpolymath/state.scm specification
;; Reference: hyperpolymath/git-hud/STATE.scm
;;
;; Honesty audit: 2026-03-10
;; Previous version over-claimed completion. This revision reflects
;; what actually compiles, runs, or has real implementation behind it.

(define-module (vordr state)
  #:export (metadata
            project-context
            current-position
            route-to-mvp
            blockers-and-issues
            critical-next-actions
            session-history
            ;; Helper functions
            get-completion-percentage
            get-blockers
            get-milestone))

(define metadata
  '((version . "0.1.0")
    (schema-version . "1.0")
    (created . "2025-01-15")
    (updated . "2026-03-10")
    (project . "vordr")
    (repo . "https://gitlab.com/hyperpolymath/vordr")))

(define project-context
  '((name . "Vordr")
    (tagline . "Formally verified container orchestration and verification")
    (tech-stack . ((idris2 . "Formal verification core")
                   (rust . "CLI and eBPF monitoring")
                   (elixir . "Orchestration")
                   (ada . "Cryptographic trust")
                   (zig . "FFI layer")))
    (phase . "early-alpha")))

(define current-position
  '((phase . "v0.1.0-alpha — Scaffolding with partial implementations")
    (overall-completion . 45)

    (components
      ((name . "Repository Structure")
       (completion . 100)
       (status . "complete")
       (notes . "Mustfile, justfile, CI/CD, codemeta.json, SECURITY.md all present"))

      ((name . "Documentation")
       (completion . 60)
       (status . "needs-accuracy-pass")
       (notes . "README, ROADMAP, ARCHITECTURE, API exist but contain inflated status claims"))

      ((name . "Idris2 Proofs")
       (completion . 90)
       (status . "real-but-not-integrated")
       (notes . "Container.idr, Verification.idr, Attestation.idr, SBOM.idr, Proofs.idr contain real dependent-type proofs. Not wired into the Rust runtime or CI."))

      ((name . "Rust CLI")
       (completion . 30)
       (status . "scaffolding")
       (notes . "Cargo project builds. CLI subcommands defined. No actual container runtime calls (clone, unshare, pivot_root, cgroups). Uses C stubs for Ada."))

      ((name . "Rust eBPF")
       (completion . 10)
       (status . "userspace-types-only")
       (notes . "Aya probe definitions and userspace event types exist. Zero kernel-side eBPF programs. No BPF bytecode compiles or loads."))

      ((name . "Elixir Orchestrator")
       (completion . 85)
       (status . "mostly-functional")
       (notes . "GenStateMachine container lifecycle is real. Reversibility journal works. Borg integration and network modules present. Deps had compatibility issues (credo, req/finch); credo updated to ~> 1.8."))

      ((name . "Ada/SPARK Trust")
       (completion . 5)
       (status . "specs-only")
       (notes . "Ada source files exist (threshold_signatures, gatekeeper, container_policy, OCI parser) but have never been compiled with GNAT. No SPARK proofs run. Runtime uses C stubs."))

      ((name . "Zig FFI")
       (completion . 5)
       (status . "placeholder")
       (notes . "ffi/zig/src/main.zig exists with exported C-compatible function signatures. No real container operations behind them."))

      ((name . "Temporal Isolation")
       (completion . 70)
       (status . "surprisingly-complete")
       (notes . "Elixir temporal isolation engine with BEAM port interceptor concept. Time-dilation logic is real. Not integrated end-to-end with eBPF clock manipulation."))

      ((name . "MCP Server")
       (completion . 80)
       (status . "needs-testing")
       (notes . "ReScript MCP adapter with JSON-RPC 2.0 methods defined. Untested against real Svalinn gateway."))

      ((name . "CI/CD")
       (completion . 35)
       (status . "partially-working")
       (notes . "GitLab CI workflows exist. Not all jobs verified passing. Idris2 and Ada jobs likely fail.")))

    (working-features
      "Rust CLI compiles (with C stubs, no real runtime)"
      "Idris2 formal verification types and proof structures"
      "Elixir GenStateMachine container lifecycle"
      "Elixir reversibility journal"
      "Elixir temporal isolation engine (standalone)"
      "ReScript MCP adapter (protocol layer)"
      "Repository structure and documentation skeleton")

    (broken-or-missing
      "No actual container runtime (no namespaces, cgroups, pivot_root)"
      "eBPF kernel programs not implemented (zero BPF bytecode)"
      "Ada/SPARK code never compiled — using C stubs"
      "Zig FFI functions are empty shells"
      "Idris2 proofs not integrated into Rust binary"
      "CI/CD pipelines not fully verified"
      "No integration tests across language boundaries"
      "No end-to-end test of container create→run→stop")))

(define route-to-mvp
  '((milestone-1
     (name . "Repository Complete")
     (target . "v0.0.1")
     (status . "complete")
     (items
       ((item . "Complete directory structure") (done . #t))
       ((item . "Add all SCM files") (done . #t))
       ((item . "Create Mustfile") (done . #t))
       ((item . "Add CI/CD workflows") (done . #t))
       ((item . "Add justfile") (done . #t))
       ((item . "Add codemeta.json") (done . #t))
       ((item . "Add SECURITY.md") (done . #t))))

    (milestone-2
     (name . "Idris2 Skeleton")
     (target . "v0.1.0")
     (status . "complete")
     (notes . "Proofs are real but not runtime-integrated")
     (items
       ((item . "Container state type") (done . #t))
       ((item . "Lifecycle transitions") (done . #t))
       ((item . "Basic proofs") (done . #t))
       ((item . "CLI scaffolding") (done . #t))
       ((item . "SBOM verification") (done . #t))
       ((item . "Advanced proofs") (done . #t))))

    (milestone-3
     (name . "Rust eBPF Skeleton")
     (target . "v0.2.0")
     (status . "incomplete")
     (notes . "Userspace types done, kernel programs not started")
     (items
       ((item . "Aya eBPF setup") (done . #t))
       ((item . "Syscall probes (userspace types)") (done . #t))
       ((item . "Event ringbuffer (types)") (done . #t))
       ((item . "Anomaly detection (types)") (done . #t))
       ((item . "Monitor CLI") (done . #t))
       ((item . "Kernel-side eBPF programs") (done . #f))
       ((item . "BPF bytecode compilation") (done . #f))
       ((item . "Live probe loading") (done . #f))))

    (milestone-4
     (name . "Elixir Orchestrator")
     (target . "v0.3.0")
     (status . "mostly-complete")
     (notes . "Core state machine works; integration with other components pending")
     (items
       ((item . "GenStateMachine") (done . #t))
       ((item . "Reversibility layer") (done . #t))
       ((item . "Borg integration") (done . #t))
       ((item . "Network namespaces") (done . #t))
       ((item . "Telemetry") (done . #t))
       ((item . "Zig FFI NIF integration") (done . #f))
       ((item . "End-to-end test with Rust CLI") (done . #f))))

    (milestone-5
     (name . "Integration MVP")
     (target . "v0.5.0")
     (status . "not-started")
     (notes . "Requires all components to actually talk to each other")
     (items
       ((item . "Ada/SPARK compilation") (done . #f))
       ((item . "Zig FFI real implementation") (done . #f))
       ((item . "Idris2-to-runtime integration") (done . #f))
       ((item . "MCP end-to-end testing") (done . #f))
       ((item . "Full CI/CD green") (done . #f))
       ((item . "Container create→run→stop works") (done . #f))))))

(define blockers-and-issues
  '((critical
      ((id . "VORDR-010")
       (description . "No actual container runtime implementation")
       (type . "missing-feature")
       (notes . "Rust binary has CLI scaffolding but cannot create, run, or stop real containers"))
      ((id . "VORDR-011")
       (description . "Ada/SPARK code never compiled")
       (type . "build-failure")
       (notes . "Need GNAT toolchain in CI; currently bypassed with C stubs")))
    (high
      ((id . "VORDR-012")
       (description . "eBPF kernel programs not implemented")
       (type . "missing-feature")
       (notes . "Only userspace types exist; no BPF bytecode"))
      ((id . "VORDR-013")
       (description . "Zig FFI functions are empty shells")
       (type . "missing-feature")
       (notes . "Exported symbols exist but do nothing")))
    (medium
      ((id . "VORDR-004")
       (description . "RISC-V cross-compilation pipeline")
       (type . "tooling")
       (notes . "Need riscv64gc target for all languages")))
    (low
      ((id . "VORDR-005")
       (description . "WASM compilation for Idris2")
       (type . "research")
       (notes . "Idris2 WASM backend maturity unclear")))))

(define critical-next-actions
  '((immediate
      "Implement real container runtime in Rust (namespaces, cgroups, rootfs)"
      "Compile Ada/SPARK code with GNAT or replace with Rust crypto"
      "Write at least one kernel-side eBPF program")

    (this-week
      "Put real logic behind Zig FFI exported functions"
      "Wire Idris2 proof outputs into Rust build"
      "Fix CI/CD so all jobs pass")

    (this-month
      "End-to-end test: container create, start, stop, remove"
      "Integrate Elixir orchestrator with Rust CLI"
      "Run SPARK prover on Ada code (or decide to drop Ada)"
      "Honest documentation pass on README and ROADMAP")))

(define session-history
  '((session-001
     (date . "2025-01-15")
     (duration . "2 hours")
     (accomplishments
       "Created repository structure"
       "Wrote README.adoc"
       "Wrote ROADMAP.adoc"
       "Created all SCM checkpoint files"
       "Defined architecture decisions in META.scm")
     (next-session
       "Create Mustfile"
       "Add CI/CD workflows"
       "Begin Idris2 type definitions"))
    (session-002
     (date . "2026-01-18")
     (duration . "1 hour")
     (accomplishments
       "Fixed license headers to PMPL-1.0-or-later"
       "Updated GitLab CI with Idris2, Ada/SPARK jobs"
       "Created Container.idr with state machine and dependent types"
       "Created Verification.idr with attestation predicates"
       "Created Attestation.idr with envelope formats"
       "Created Main.idr CLI entry point"
       "Created vordr.ipkg package definition"
       "Added Mustfile verification job to CI")
     (next-session
       "Test Idris2 type checking"
       "Begin Rust eBPF skeleton"
       "Add Elixir GenServer orchestrator"))
    (session-003
     (date . "2026-01-19")
     (duration . "2 hours")
     (accomplishments
       "Created Rust eBPF monitoring skeleton (userspace types only)"
       "Created Elixir orchestrator with GenStateMachine"
       "Implemented reversibility journal"
       "Created supervision tree with DynamicSupervisor"
       "Added telemetry for observability"
       "Created container and reversibility tests")
     (next-session
       "Implement actual eBPF kernel programs"
       "Add Ada/SPARK threshold signatures"
       "Begin integration tests"))
    (session-004
     (date . "2026-01-19")
     (duration . "1 hour")
     (accomplishments
       "Added Ada/SPARK source files (not compiled)"
       "Created ReScript MCP adapter"
       "Added codemeta.json"
       "Created SBOM.idr and Proofs.idr"
       "Added ebpf/probes.rs (userspace definitions)"
       "Created Elixir Borg and Network modules"
       "Added ARCHITECTURE.adoc and API.adoc")
     (notes . "STATE.scm was set to 100% completion at this point — that was inaccurate")
     (next-session
       "Actually compile Ada code"
       "Write real eBPF kernel programs"
       "Integration testing"))
    (session-005
     (date . "2026-01-28")
     (duration . "30 minutes")
     (accomplishments
       "Security audit identified crypto verification gaps"
       "Updated SECURITY.md with current posture"
       "Documented that eBPF kernel monitoring not started"
       "Documented that Idris2 proofs not runtime-integrated")
     (next-session
       "Implement real container runtime"
       "Compile Ada/SPARK or decide to drop it"
       "Wire Idris2 proofs into Rust"))
    (session-006
     (date . "2026-03-10")
     (duration . "30 minutes")
     (accomplishments
       "Honesty audit of STATE.scm — reset completion percentages"
       "Fixed schema.sql SPDX header (was MIT OR AGPL-3.0-or-later)"
       "Updated credo dependency to ~> 1.8 for Elixir 1.19 compat"
       "Created Containerfile (multi-stage Rust build on Wolfi)"
       "Updated README.adoc to reflect actual status")
     (next-session
       "Begin real container runtime implementation"
       "Decide Ada/SPARK future (compile or drop)"))))

;; Helper functions
(define (get-completion-percentage)
  (assoc-ref (assoc-ref current-position 'overall-completion) 'value))

(define (get-blockers priority)
  (assoc-ref blockers-and-issues priority))

(define (get-milestone name)
  (let ((milestones (list (assoc-ref route-to-mvp 'milestone-1)
                          (assoc-ref route-to-mvp 'milestone-2)
                          (assoc-ref route-to-mvp 'milestone-3)
                          (assoc-ref route-to-mvp 'milestone-4)
                          (assoc-ref route-to-mvp 'milestone-5))))
    (find (lambda (m) (string=? (assoc-ref m 'name) name)) milestones)))
