;; SPDX-License-Identifier: PMPL-1.0-or-later
;; META.scm - Architectural decisions and project meta-information for selur
;; Media-Type: application/meta+scheme

(define-meta selur
  (version "1.0.0")

  (architecture-decisions
    ((adr-001 accepted "2026-01-25"
      "Need zero-copy IPC between Svalinn (ReScript frontend) and Vordr (Elixir backend)"
      "Use WASM linear memory as the shared region for zero-copy IPC. The Zig runtime
       manages a flat 1MB memory region that both sides can read/write without copying.
       Requests and responses are serialized directly into linear memory using a binary
       protocol (command byte + length-prefixed payload)."
      "Eliminates serialization overhead for container operations. "
      "Requires careful bounds checking since linear memory has no protection. "
      "WASM sandbox provides isolation without OS-level overhead.")

    (adr-002 accepted "2026-01-25"
      "Need compile-time memory safety guarantees for the IPC bridge"
      "Adopt Ephapax linear types for compile-time enforcement of memory ownership.
       Each allocated region has exactly one owner at any time. Transferring a region
       consumes the source reference, preventing use-after-free and double-free at
       compile time rather than runtime."
      "Eliminates entire classes of memory bugs before code runs. "
      "Ephapax compiler not yet available - using manual annotations as interim. "
      "Idris2 formal proofs verify the linear type properties hold.")

    (adr-003 accepted "2026-01-25"
      "Need to expose selur bridge to both ReScript and Elixir consumers"
      "Provide multi-language bindings: ReScript bindings for Svalinn (compiles to JS,
       calls WASM), Elixir NIF bindings for Vordr (native code, calls Rust host).
       Both binding layers present idiomatic APIs in their respective languages while
       the Rust core and Zig WASM module handle the actual IPC."
      "Each consumer gets a natural API in their language. "
      "Two binding layers to maintain. "
      "NIF bindings require careful error handling to avoid crashing the BEAM VM.")

    (adr-004 accepted "2026-01-25"
      "Need a host runtime to load and execute the Zig WASM module"
      "Use Rust with wasmtime as the host runtime. Rust manages the wasmtime instance,
       provides imported functions the WASM module can call, and bridges between the
       WASM linear memory and actual system resources (network, filesystem) that WASM
       cannot access directly."
      "wasmtime is mature, well-maintained, and has excellent Rust integration. "
      "Rust host can enforce additional security policies beyond WASM sandbox. "
      "Adds wasmtime as a significant dependency."))

  (development-practices
    (code-style
      "Rust: standard rustfmt, 4-space indent. "
      "Zig: 4-space indent, explicit error handling. "
      "ReScript: standard formatter. "
      "Elixir: mix format. "
      "Idris2: 2-space indent.")
    (security
      "All commits signed. "
      "Hypatia neurosymbolic scanning enabled. "
      "Fuzz testing for WASM memory operations. "
      "Bounds checking on all linear memory access.")
    (testing
      "Unit tests per language binding. "
      "Fuzz tests for binary protocol parsing. "
      "Criterion benchmarks for IPC throughput. "
      "Integration tests with Svalinn and Vordr.")
    (versioning
      "Semantic versioning (semver). "
      "Lockstep with stapeln container stack releases.")
    (documentation
      "README.adoc for overview. "
      "STATE.scm for current state. "
      "ECOSYSTEM.scm for relationships. "
      "docs/ for architecture and protocol specs.")
    (branching
      "Main branch protected. "
      "Feature branches for new work. "
      "PRs required for merges."))

  (design-rationale
    (why-wasm
      "WASM provides a sandboxed execution environment with deterministic linear memory. "
      "The Zig runtime compiles to wasm32-freestanding, giving us a portable, "
      "platform-independent IPC module with no OS dependencies.")
    (why-ephapax
      "Linear types guarantee at compile time that memory regions are used exactly once, "
      "eliminating use-after-free and double-free without runtime overhead. "
      "This is critical for a zero-copy IPC bridge where memory ownership transfers "
      "between Svalinn and Vordr.")
    (why-multi-language
      "Svalinn is ReScript (compiles to JS), Vordr is Elixir (runs on BEAM). "
      "Rather than forcing one language, selur provides idiomatic bindings for both, "
      "with the Rust/Zig core handling the actual bridge.")))
