;; SPDX-License-Identifier: PMPL-1.0-or-later
;; META.scm - Architectural decisions and project meta-information for stapeln
;; Media-Type: application/meta+scheme

(define-meta stapeln
  (version "0.3.0")

  (architecture-decisions
    ((adr-001 accepted "2026-02-05"
      "Need a visual container stack designer accessible to non-experts"
      "Build stapeln with ReScript-TEA frontend, Elixir/Phoenix backend, Idris2 ABI + Zig FFI"
      "Combines formal verification (Idris2), performance (Zig), type-safe UI (ReScript-TEA), and concurrent backend (Elixir/BEAM)")
     (adr-002 accepted "2026-02-05"
      "Need real-time security analysis without AI hallucinations"
      "Use miniKanren deterministic reasoning engine for vulnerability detection"
      "Deterministic, explainable, fast (ms not seconds), no GPU needed; requires Guile Scheme setup")
     (adr-003 accepted "2026-02-05"
      "Need a game-like UX so container-haters can build secure stacks"
      "Adopt visual stat bars, real-time scoring, one-click auto-fix, simulation mode"
      "Accessible to 12-year-olds; requires careful UX testing with actual container-haters")
     (adr-004 accepted "2026-02-09"
      "Frontend needs to be accessible"
      "Target WCAG 2.3 AAA compliance with ARIA labels, keyboard navigation, screen reader support"
      "Full accessibility adds development overhead but is a core project value")
     (adr-005 accepted "2026-02-09"
      "Need verified container images"
      "Integrate with hyperpolymath container ecosystem (Cerro Torre, Svalinn, selur, Vordr)"
      "Tight coupling to hyperpolymath tools; provides formal verification and supply chain guarantees")))

  (development-practices
    (code-style
      "ReScript for frontend (TEA architecture), Elixir for backend (Phoenix), "
      "Idris2 for ABI definitions, Zig for FFI implementation. "
      "Avoid TypeScript, Go, Python per RSR.")
    (security
      "All commits signed. "
      "Hypatia neurosymbolic scanning enabled. "
      "OpenSSF Scorecard tracking.")
    (testing
      "Deno tests for validation and file generation. "
      "Elixir tests for backend API. "
      "Zig tests for FFI ABI compliance.")
    (versioning
      "Semantic versioning (semver). "
      "Changelog maintained in CHANGELOG.md.")
    (documentation
      "README.adoc for overview. "
      "STATUS.md for honest current state. "
      "STATE.scm for machine-readable state.")
    (branching
      "Main branch protected. "
      "Feature branches for new work. "
      "PRs required for merges."))

  (design-rationale
    (why-tea
      "The Elm Architecture provides predictable state management, "
      "time-travel debugging, and fully testable pure state transitions.")
    (why-rescript
      "ReScript provides type safety with OCaml-level inference, "
      "compiles to efficient JS, and avoids TypeScript ecosystem complexity.")
    (why-elixir
      "Elixir/BEAM provides fault-tolerant concurrency, hot code upgrades, "
      "and GenServer patterns ideal for ephemeral pinholes and real-time features.")
    (why-idris2-zig
      "Idris2 dependent types prove ABI correctness at compile time. "
      "Zig provides zero-cost C ABI compatibility for FFI implementation.")))
