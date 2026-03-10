;; SPDX-License-Identifier: PMPL-1.0-or-later
;; META.scm - Meta-level information for cerro-torre
;; Media-Type: application/meta+scheme

(meta
  (architecture-decisions
    ((id . "ADR-001")
     (title . "Ada/SPARK for supply-chain verification")
     (status . accepted)
     (date . "2025-12-29")
     (context . "Container supply-chain verification demands provable absence of runtime errors in cryptographic and parsing code. Memory-unsafe languages risk CVEs in the very tool meant to prevent them.")
     (decision . "Use Ada/SPARK as the primary implementation language. All security-critical packages (crypto, manifest parsing, provenance chain, verification) carry SPARK_Mode On with discharged proof obligations.")
     (consequences . ("Formal verification of package integrity at compile time"
                      "Smaller developer pool than C/Rust"
                      "Alire ecosystem less mature than Cargo"
                      "GNAT Community Edition sufficient for MVP")))

    ((id . "ADR-002")
     (title . "External Rust utility for Ed25519 signing")
     (status . accepted)
     (date . "2026-01-28")
     (context . "Ada lacks a production-quality Ed25519 binding. libsodium-ada is unmaintained and the proven library has unresolved compilation errors. Blocking on a native Ada binding delays the signing milestone indefinitely.")
     (decision . "Implement Ed25519 key generation and signing as a standalone Rust binary (cerro-sign / ct-sign) invoked by the Ada CLI via process spawn. The Rust binary lives in src-rust/ and builds with Cargo.")
     (consequences . ("Ed25519 operations available immediately via mature Rust ecosystem"
                      "Process boundary isolates signing from verification memory space"
                      "Additional build dependency (Rust toolchain)"
                      "Future: replace with native Ada binding when proven library stabilises")))

    ((id . "ADR-003")
     (title . "Curl-based HTTP client over native Ada HTTP")
     (status . accepted)
     (date . "2025-12-30")
     (context . "Ada Web Server (AWS) provides HTTP client capabilities but lacks TLS 1.3, HTTP/3, and Encrypted Client Hello (ECH) support. Registry interactions require modern TLS for production use.")
     (decision . "Use libcurl via Ada bindings for all HTTP operations. This provides TLS 1.3 + HTTP/3 support through the system curl library, with ECH available when curl is built with appropriate backends.")
     (consequences . ("TLS 1.3 and HTTP/3 available out of the box"
                      "System dependency on libcurl and OpenSSL"
                      "ECH support depends on curl build configuration"
                      "Simpler than maintaining custom TLS stack")))

    ((id . "ADR-004")
     (title . "Rekor as transparency log backend")
     (status . accepted)
     (date . "2025-12-30")
     (context . "Supply-chain verification requires a tamper-evident record of all signing events. Running a custom transparency log is operational overhead; adopting an existing protocol enables interoperability with Sigstore tooling.")
     (decision . "Use Sigstore Rekor as the transparency log backend. Upload signing events via the Rekor REST API. Verify Merkle inclusion proofs against the Rekor log tree.")
     (consequences . ("Interoperable with cosign, rekor-cli, and Sigstore ecosystem"
                      "Depends on public Rekor instance or self-hosted deployment"
                      "Merkle proof verification adds complexity to verification path"
                      "In-toto attestation format for provenance records")))

    ((id . "ADR-005")
     (title . "TOML-based manifest format")
     (status . accepted)
     (date . "2025-12-29")
     (context . "Package manifests need a human-readable, machine-parseable format. JSON is verbose and lacks comments. YAML is ambiguous. TOML is unambiguous, supports comments, and has a simple grammar suitable for formal parsing.")
     (decision . "Use a TOML-based manifest format (.ctp files) for package definitions. Implement a hand-written recursive-descent parser in Ada/SPARK with proven correctness properties.")
     (consequences . ("Human-readable manifests with inline comments"
                      "Parser amenable to SPARK formal verification"
                      "Well-defined grammar, no YAML-style ambiguity"
                      "Custom parser required (no mature Ada TOML library)"))))

  (development-practices
    (code-style "GNAT coding standard, 3-space indent")
    (security
      (principle "Defense in depth")
      (crypto-routing "All crypto through cerro_crypto packages")
      (spark-coverage "SPARK_Mode On for all src/core/ packages"))
    (testing
      (unit "AUnit framework")
      (integration "Live registry tests with localhost:5000")
      (e2e "48 tests, 100% passing"))
    (versioning "SemVer")
    (documentation "AsciiDoc")
    (branching "main for stable"))

  (design-rationale
    (formal-verification "Provable absence of runtime errors in security-critical paths")
    (process-isolation "Signing in separate Rust process prevents memory corruption propagation")
    (ecosystem-interop "Rekor + OCI Distribution v2 for industry-standard compatibility")))
