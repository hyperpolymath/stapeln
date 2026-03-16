;; ECOSYSTEM.scm - Overview of the Vordr project ecosystem
;; Media-Type: application/ecosystem+scheme

(ecosystem
  (project-name "Vordr")
  (purpose "Formally verified container orchestration and verification engine")
  (core-languages
    (elixir "Orchestration & MCP server (BEAM fault tolerance, GenStateMachine)")
    (rust "CLI, eBPF monitoring (zero-cost abstractions, runtime integration, syscall interception)")
    (idris2 "Formal verification core (dependent types for proofs)"))
  (removed-languages
    (ada/spark "Previously for trust mechanisms; removed to simplify build and reduce toolchain complexity"))
  (integrations
    (svalinn "Edge gateway, request validation (calls Vordr via MCP/JSON-RPC)")
    (cerro-torre "Provenance-verified builds (produces .ctp bundles verified by Vordr)")
    (oblibeny "Orchestration handoff"))
  (data-layer
    (arangodb "Attestations")
    (dragonfly "Cache")
    (lmdb "Proofs"))
  (tooling
    (cargo "Rust package manager and build tool")
    (mix "Elixir build tool")
    (gprbuild "Ada/SPARK build tool (removed from Rust build process)"))
  (standard-compliance
    ("RSR Compliant" "verified-container-spec Runtime Integration")))