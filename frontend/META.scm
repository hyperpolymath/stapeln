;; SPDX-License-Identifier: PMPL-1.0-or-later
;; META.scm - Meta-level information for stapeln/frontend

(meta
  (version "1.0")
  (project-name "stapeln-frontend")
  (creation-date "2026-02-05")
  (last-updated "2026-02-05")

  (philosophy
    (core-principles
      "Formal verification over ad-hoc testing"
      "Zero-cost abstractions"
      "Type safety at all layers"
      "Security by default"
      "Minimal dependencies"
      "Framework agnostic"
      "Documentation as code")

    (design-rationale
      (rationale
        (topic "ABI/FFI Universal Standard")
        (decision "Use Idris2 → Zig → ReScript architecture")
        (reasoning
          "Idris2 provides dependent types for compile-time proofs. "
          "Zig provides memory-safe C ABI with zero overhead. "
          "ReScript provides type-safe functional programming. "
          "This three-layer stack ensures correctness at every level "
          "with no runtime cost for verification.")
        (alternatives-considered
          "Rust FFI (rejected: less formal verification)"
          "C FFI (rejected: not memory-safe by default)"
          "Pure ReScript (rejected: no formal proofs)"))

      (rationale
        (topic "Deno over Node.js")
        (decision "Use Deno as primary runtime")
        (reasoning
          "Deno provides native Web API support, avoiding npm bloat. "
          "Zero npm runtime dependencies aligns with minimalist philosophy. "
          "Better security with permissions model. "
          "Native TypeScript support.")
        (alternatives-considered
          "Node.js (rejected: npm dependencies, legacy APIs)"
          "Bun (rejected: not in RSR allowed list)"))

      (rationale
        (topic "TEA Architecture")
        (decision "Use The Elm Architecture pattern")
        (reasoning
          "TEA provides predictable state management with pure functions. "
          "Fits naturally with ReScript's functional style. "
          "Easier to reason about than mutable state. "
          "Testable and type-safe.")
        (alternatives-considered
          "Redux (rejected: too complex, more boilerplate)"
          "MobX (rejected: mutable state)"
          "Context API (rejected: less structured)"))

      (rationale
        (topic "Custom TEA Router")
        (decision "Implement custom router instead of using rescript-tea")
        (reasoning
          "rescript-tea package is incompatible with modern ReScript. "
          "Custom implementation provides exactly what we need. "
          "No external dependencies for core functionality. "
          "Full control over routing logic.")
        (alternatives-considered
          "rescript-tea (rejected: incompatible bsconfig.json)"
          "cadre-tea-router (rejected: doesn't exist on npm)"
          "React Router (rejected: not TEA-compatible)"))

      (rationale
        (topic "Security-First Design")
        (decision "Implement CSP validation, audit logging, and sandboxing")
        (reasoning
          "Container orchestration is security-critical. "
          "CSP prevents script injection attacks. "
          "Audit logging provides compliance trail. "
          "Sandboxing isolates untrusted content.")
        (alternatives-considered
          "Basic validation only (rejected: insufficient for production)"
          "Third-party security lib (rejected: adds dependencies)"))

      (rationale
        (topic "Framework Adapters")
        (decision "Provide adapters for React, Solid, Vue, Web Components")
        (reasoning
          "Framework-agnostic core allows broader adoption. "
          "Adapters are thin wrappers around verified core. "
          "Users can integrate into existing projects easily. "
          "SSR support enables server-side rendering.")
        (alternatives-considered
          "React-only (rejected: limits adoption)"
          "Framework-specific implementations (rejected: duplicates code)")))

    (cross-cutting-concerns
      (concern
        (name "Performance")
        (approach "Zero-cost abstractions via compile-time verification")
        (metrics
          "Mount time: 0.08-0.25ms"
          "Bundle size: 10.4KB gzipped"
          "Memory: <1KB typical usage"
          "CPU: <0.2% with monitoring"))

      (concern
        (name "Security")
        (approach "Defense in depth with formal proofs")
        (measures
          "Idris2 dependent types prevent entire classes of bugs"
          "CSP validation blocks script injection"
          "Audit logging tracks all operations"
          "Sandboxing isolates untrusted content"
          "Zig memory safety prevents C-style vulnerabilities"))

      (concern
        (name "Maintainability")
        (approach "Type safety, tests, and documentation")
        (practices
          "100% test coverage (16/16 tests passing)"
          "Comprehensive documentation (2,310 lines)"
          "Type-checked across 3 languages"
          "Clear error messages with troubleshooting"
          "Migration guides for onboarding"))

      (concern
        (name "Interoperability")
        (approach "Standard interfaces and multiple adapters")
        (support
          "React hooks for React apps"
          "Solid.js primitives for Solid apps"
          "Vue 3 composables for Vue apps"
          "Web Components for framework-free usage"
          "SSR/hydration for server-side rendering"
          "TypeScript definitions for all APIs"))))

  (architecture-decisions
    (adr
      (id "ADR-001")
      (title "Three-Layer Verification Stack")
      (status "accepted")
      (date "2026-02-05")
      (context
        "Need formal verification without runtime overhead")
      (decision
        "Implement Idris2 → Zig → ReScript architecture")
      (consequences
        "Positive: Zero runtime cost for proofs, maximum safety"
        "Positive: Memory-safe FFI via Zig"
        "Positive: Type-safe bindings via ReScript"
        "Negative: More complex build system"
        "Negative: Requires three language toolchains"))

    (adr
      (id "ADR-002")
      (title "Deno-First Runtime")
      (status "accepted")
      (date "2026-02-05")
      (context
        "Need to avoid npm dependency bloat")
      (decision
        "Use Deno as primary runtime with Web API bindings")
      (consequences
        "Positive: Zero npm runtime dependencies"
        "Positive: Native TypeScript support"
        "Positive: Better security model"
        "Negative: Users need Deno installed"
        "Negative: Some ecosystem tools expect Node"))

    (adr
      (id "ADR-003")
      (title "Security-Hardened by Default")
      (status "accepted")
      (date "2026-02-05")
      (context
        "Container orchestration is security-critical")
      (decision
        "Implement CSP validation, audit logging, and sandboxing")
      (consequences
        "Positive: Prevents common attack vectors"
        "Positive: Provides compliance audit trail"
        "Positive: Isolates untrusted content"
        "Negative: Slightly higher complexity"
        "Negative: Small performance overhead (0.07ms)"))

    (adr
      (id "ADR-004")
      (title "Framework-Agnostic Core")
      (status "accepted")
      (date "2026-02-05")
      (context
        "Want broad adoption across different frameworks")
      (decision
        "Provide thin adapters for React, Solid, Vue, Web Components")
      (consequences
        "Positive: Works with multiple frameworks"
        "Positive: Core remains framework-independent"
        "Positive: SSR/hydration compatible"
        "Negative: More adapter code to maintain"
        "Negative: Each framework needs testing")))

  (development-practices
    (testing
      (strategy "100% coverage with formal verification")
      (tools "Zig test" "Idris2 type checker" "Manual ReScript integration tests")
      (coverage "16/16 tests passing (100%)"))

    (documentation
      (strategy "Documentation as code")
      (formats "Markdown" "TypeScript definitions" "Inline comments" "Scheme metadata")
      (metrics "2,310 lines of documentation"))

    (version-control
      (strategy "Semantic versioning")
      (branching "main branch, feature branches as needed")
      (commit-style "Conventional Commits"))

    (ci-cd
      (tools "Hypatia" "GitHub Actions" "OpenSSF Scorecard")
      (workflows
        "hypatia-scan.yml for neurosymbolic security"
        "codeql.yml for code analysis"
        "quality.yml for TruffleHog and EditorConfig"
        "scorecard.yml for OpenSSF scoring"))

    (code-quality
      (linting "EditorConfig" "ReScript compiler warnings")
      (formatting "ReScript formatter" "EditorConfig rules")
      (security "TruffleHog" "Hypatia" "CodeQL")))

  (governance
    (license "PMPL-1.0-or-later (Palimpsest License)")
    (copyright "Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>")
    (contribution-policy "See CONTRIBUTING.md")
    (code-of-conduct "Contributor Covenant 2.1")
    (security-policy "See SECURITY.md"))

  (media-type "application/meta+scheme")
  (schema-version "1.0"))
