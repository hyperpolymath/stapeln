;; SPDX-License-Identifier: PMPL-1.0-or-later
;; STATE.scm - Project state tracking for selur
;; Media-Type: application/vnd.state+scm

(define-state selur
  (metadata
    (version "0.1.0")
    (schema-version "1.0.0")
    (created "2026-01-25")
    (updated "2026-03-10")
    (project "selur")
    (repo "hyperpolymath/stapeln (container-stack/selur)"))

  (project-context
    (name "selur")
    (tagline "Ephapax-linear WASM sealant - Zero-copy IPC bridge between Svalinn and Vordr")
    (tech-stack
      ("Rust" "Core bridge, wasmtime host runtime")
      ("Zig" "WASM runtime module, linear memory management")
      ("Idris2" "ABI formal proofs (bridgeUniqueness)")
      ("ReScript" "Svalinn bindings")
      ("Elixir" "Vordr NIF bindings")))

  (current-position
    (phase "active-development")
    (overall-completion 65)
    (components
      ((name "Core bridge (Rust)")
       (completion 95)
       (notes "wasmtime integration, request/response routing"))
      ((name "WASM runtime (Zig)")
       (completion 50)
       (notes "allocate works, send_request/get_response need real implementation"))
      ((name "Formal proofs (Idris2)")
       (completion 25)
       (notes "bridgeUniqueness proven, memory layout and protocol proofs pending"))
      ((name "Svalinn bindings (ReScript)")
       (completion 90)
       (notes "Full API surface, minor edge cases remain"))
      ((name "Vordr bindings (Elixir NIF)")
       (completion 90)
       (notes "NIF bridge functional, error propagation needs hardening"))
      ((name "Containerfiles")
       (completion 70)
       (notes "Base images defined, launcher scripts missing"))
      ((name "selur-compose")
       (completion 78)
       (notes "Core orchestration works, advanced features pending")))
    (working-features
      "WASM linear memory allocation"
      "Rust-to-WASM bridge via wasmtime"
      "Svalinn ReScript API"
      "Vordr Elixir NIF API"
      "Basic request/response routing"
      "Fuzz testing harness"))

  (route-to-mvp
    (milestones
      ((name "Core Bridge")
       (status "complete")
       (completion 95)
       (items
         ("Rust wasmtime host runtime" . done)
         ("WASM module loading" . done)
         ("Linear memory shared region" . done)
         ("Request/response routing" . done)
         ("Error propagation" . in-progress)))
      ((name "WASM Runtime")
       (status "in-progress")
       (completion 50)
       (items
         ("Memory allocator" . done)
         ("Request parsing and validation" . in-progress)
         ("Response buffer management" . in-progress)
         ("Request queue for host bridging" . in-progress)
         ("Deallocation with free list" . in-progress)))
      ((name "Formal Verification")
       (status "in-progress")
       (completion 25)
       (items
         ("bridgeUniqueness proof" . done)
         ("Memory layout proofs" . todo)
         ("Protocol correctness proofs" . todo)
         ("Linear type consumption proofs" . todo)))
      ((name "Language Bindings")
       (status "in-progress")
       (completion 90)
       (items
         ("Svalinn ReScript bindings" . done)
         ("Vordr Elixir NIF bindings" . done)
         ("Edge case handling" . in-progress)))
      ((name "Containers & Compose")
       (status "in-progress")
       (completion 74)
       (items
         ("Base Containerfiles" . done)
         ("Launcher scripts" . todo)
         ("selur-compose orchestration" . in-progress)
         ("Health checks" . todo)))))

  (blockers-and-issues
    (critical ())
    (high
      ("Ephapax linear type compiler not yet available - using manual annotations"))
    (medium
      ("Zig WASM runtime functions are stubs")
      ("Containerfile launcher scripts missing"))
    (low
      ("LICENSE file was corrupted (404 text)")))

  (critical-next-actions
    (immediate
      "Implement Zig runtime send_request/get_response/deallocate"
      "Fix LICENSE with correct PMPL-1.0-or-later text")
    (this-week
      "Add launcher scripts to Containerfiles"
      "Complete selur-compose advanced features")
    (this-month
      "Idris2 memory layout proofs"
      "Idris2 protocol correctness proofs"
      "Integration tests with real Svalinn/Vordr"))

  (session-history
    ((date "2026-03-10")
     (summary "Fix LICENSE, implement Zig runtime, fix SCM files, add CI"))))

;; Helper functions
(define (get-completion-percentage state)
  (current-position 'overall-completion state))

(define (get-blockers state severity)
  (blockers-and-issues severity state))

(define (get-milestone state name)
  (find (lambda (m) (equal? (car m) name))
        (route-to-mvp 'milestones state)))
