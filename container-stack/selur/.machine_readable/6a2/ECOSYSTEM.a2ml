;; SPDX-License-Identifier: PMPL-1.0-or-later
;; ECOSYSTEM.scm - Ecosystem relationships for selur
;; Media-Type: application/vnd.ecosystem+scm

(ecosystem
  (version "1.0.0")
  (name "selur")
  (type "library")
  (purpose "Ephapax-linear WASM sealant for zero-copy IPC between Svalinn and Vordr")

  (position-in-ecosystem
    "Core IPC bridge within the stapeln container stack. Selur sits between "
    "Svalinn (ReScript container management frontend) and Vordr (Elixir "
    "container runtime backend), providing zero-copy message passing through "
    "WASM linear memory with Ephapax linear type safety guarantees.")

  (related-projects
    (parent "stapeln" "Container stack - parent project encompassing all components")
    (consumer "svalinn" "ReScript container management bindings - calls selur WASM API")
    (consumer "vordr" "Elixir container runtime NIF - receives bridged requests from selur")
    (sibling "cerro-torre" "Supply-chain-verified container distro - integrated in glued build")
    (dependency "ephapax" "Linear type compiler - not yet available, using manual annotations")
    (dependency "wasmtime" "WASM host runtime - Rust crate, executes Zig WASM module")
    (tool "selur-compose" "Orchestration tool for multi-container selur deployments")
    (tool "hypatia" "Neurosymbolic security scanning for CI/CD"))

  (what-this-is
    "A zero-copy IPC bridge that uses WASM linear memory as a shared region "
    "between the Svalinn frontend and Vordr backend. The Zig runtime manages "
    "memory allocation and request/response serialization within the WASM sandbox. "
    "The Rust host (via wasmtime) bridges WASM to actual system resources. "
    "Ephapax linear types will enforce compile-time memory safety once the "
    "compiler is available.")

  (what-this-is-not
    "Not a container runtime itself - that is Vordr. "
    "Not a container management UI - that is Svalinn. "
    "Not a general-purpose IPC library - specifically designed for the "
    "stapeln container stack protocol and message types."))
