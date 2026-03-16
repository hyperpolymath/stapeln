;; SPDX-License-Identifier: PMPL-1.0-or-later
;; ECOSYSTEM.scm - Project ecosystem position

(ecosystem
  (version "1.0")
  (name "stapeln-frontend")
  (type "application")
  (category "container-orchestration-ui")

  (purpose
    "Visual container stack designer with formally verified DOM mounting. "
    "Enables drag-and-drop composition of container stacks with export to "
    "docker-compose.yml, compose.toml (Selur), and Podman configurations.")

  (position-in-ecosystem
    (role "Frontend UI for Stapeln container stack designer")
    (target-users
      "DevOps engineers"
      "System administrators"
      "Container orchestration users"
      "Developers deploying containerized apps")
    (integration-points
      "Exports to docker-compose, Podman, Selur"
      "Integrates with container runtimes"
      "Works with CI/CD pipelines"
      "SSR compatible for documentation sites"))

  (related-projects
    (project
      (name "stapeln-backend")
      (relationship "sibling-component")
      (url "https://github.com/hyperpolymath/stapeln")
      (description "Backend API for Stapeln (future)"))

    (project
      (name "rsr-template-repo")
      (relationship "template-source")
      (url "https://github.com/hyperpolymath/rsr-template-repo")
      (description "RSR template with workflows and security scanning"))

    (project
      (name "hypatia")
      (relationship "ci-cd-dependency")
      (url "https://github.com/hyperpolymath/hypatia")
      (description "Neurosymbolic CI/CD intelligence for security scanning"))

    (project
      (name "gitbot-fleet")
      (relationship "automation-dependency")
      (url "https://github.com/hyperpolymath/gitbot-fleet")
      (description "Bot orchestration for automated fixes"))

    (project
      (name "robot-repo-automaton")
      (relationship "automation-dependency")
      (url "https://github.com/hyperpolymath/robot-repo-automaton")
      (description "Automated repository fixes with confidence thresholds"))

    (project
      (name "docker")
      (relationship "runtime-target")
      (url "https://github.com/docker/cli")
      (description "Docker container runtime target"))

    (project
      (name "podman")
      (relationship "runtime-target")
      (url "https://github.com/containers/podman")
      (description "Podman container runtime target"))

    (project
      (name "selur")
      (relationship "export-target")
      (description "Selur compose.toml format target"))

    (project
      (name "rescript")
      (relationship "language-framework")
      (url "https://github.com/rescript-lang/rescript-compiler")
      (description "ReScript compiler for type-safe functional programming"))

    (project
      (name "idris2")
      (relationship "verification-framework")
      (url "https://github.com/idris-lang/Idris2")
      (description "Idris2 dependent types for formal verification"))

    (project
      (name "zig")
      (relationship "ffi-layer")
      (url "https://github.com/ziglang/zig")
      (description "Zig for memory-safe C ABI FFI implementation"))

    (project
      (name "react")
      (relationship "ui-framework")
      (url "https://github.com/facebook/react")
      (description "React for UI rendering"))

    (project
      (name "deno")
      (relationship "runtime")
      (url "https://github.com/denoland/deno")
      (description "Deno runtime with zero npm dependencies")))

  (unique-features
    "First container stack designer with formally verified DOM mounting"
    "ABI/FFI Universal Standard (Idris2 → Zig → ReScript)"
    "100% test coverage with dependent type proofs"
    "Security hardened with CSP validation and audit logging"
    "Zero npm runtime dependencies (Deno-first)"
    "Framework-agnostic adapters (React, Solid, Vue, Web Components)"
    "SSR/hydration compatible"
    "10.4KB gzipped bundle with full feature set")

  (dependencies
    (compile-time
      "@rescript/react" "ReScript React bindings")
    (runtime
      "Deno" "JavaScript/TypeScript runtime")
    (verification
      "Idris2" "Dependent types for formal proofs")
    (ffi
      "Zig 0.15.2" "Memory-safe C ABI implementation"))

  (consumers
    (consumer
      (name "DevOps teams")
      (use-case "Visual container stack composition"))
    (consumer
      (name "Container platform users")
      (use-case "Export to docker-compose, Podman, Selur"))
    (consumer
      (name "Education/training")
      (use-case "Teaching container orchestration concepts")))

  (similar-projects
    (project
      (name "Portainer")
      (similarity "Container management UI")
      (difference "Stapeln focuses on stack design, not runtime management"))
    (project
      (name "Docker Desktop")
      (similarity "Container UI")
      (difference "Stapeln is web-based, formally verified, export-focused"))
    (project
      (name "Kubernetes Dashboard")
      (similarity "Orchestration UI")
      (difference "Stapeln targets simpler compose-based workflows")))

  (standards-compliance
    "PMPL-1.0-or-later license"
    "RSR (Repo Standards Record) compliant"
    "OpenSSF Scorecard monitored"
    "Hypatia neurosymbolic security scanning"
    "EditorConfig enforced"
    "Git hooks for quality checks"
    "Formal verification via Idris2 dependent types"
    "Memory safety via Zig"
    "Type safety via ReScript")

  (media-type "application/vnd.ecosystem+scm")
  (schema-version "1.0"))
