;; SPDX-License-Identifier: MPL-2.0-or-later
;; STATE.scm - Current project state for Cerro Torre

(define project-state
  `((metadata
      ((version . "2.0.0")
       (schema-version . "1")
       (created . "2025-12-29T03:24:22+00:00")
       (updated . "2026-01-25T07:00:00+00:00")
       (project . "Cerro Torre")
       (repo . "cerro-torre")))

    (project-context
      ((name . "Cerro Torre")
       (tagline . "Ship containers safely - provenance-verified containers from democratically-governed sources")
       (tech-stack ((primary . "Ada/SPARK") (build-system . "Alire") (http-backend . "curl") (registry-protocol . "OCI Distribution v2")))))

    (current-position
      ((phase . "Phase 2: CLI Wiring & Live Testing - IN PROGRESS | Registry fetch working, push debugging")
       (overall-completion . 72)
       (components
         ((core-crypto . ((status . "working") (completion . 100)
                          (notes . "SHA-256/512 FIPS 180-4, Ed25519 RFC 8032 - all tests passing")))
          (http-client . ((status . "working") (completion . 98)
                          (notes . "curl-based client, TLS, auth, ECH disabled for MVP, curl param formatting fixed")))
          (registry-client . ((status . "working") (completion . 95)
                              (notes . "OCI Distribution v2, localhost port parsing fixed, HTTP/HTTPS auto-detection, pull working")))
          (transparency-logs . ((status . "working") (completion . 70)
                                (notes . "Rekor API client, log entry structures, upload/get operations, proof verification pending")))
          (cli-framework . ((status . "working") (completion . 60)
                            (notes . "fetch working (tested live), push partial (connection works, upload debugging), help/version complete")))
          (manifest-parser . ((status . "working") (completion . 100)
                              (notes . "Full TOML-like parser, all tests passing")))
          (provenance-chain . ((status . "working") (completion . 100)
                               (notes . "Hash + Ed25519 verification + trust store, in-toto attestations")))
          (tar-writer . ((status . "working") (completion . 100)
                         (notes . "POSIX ustar format, source inclusion")))
          (trust-store . ((status . "working") (completion . 100)
                          (notes . "Local key storage with trust levels")))
          (debian-importer . ((status . "working") (completion . 100)
                              (notes . "Parse DSC, import packages, apt source integration")))
          (oci-exporter . ((status . "working") (completion . 100)
                           (notes . "OCI image export, Docker load format")))
          (selinux-policy . ((status . "working") (completion . 100)
                             (notes . "Policy generation, validation, install/remove")))))
       (working-features
         ((ct-fetch . "Downloads OCI manifests from registries (localhost:5000 tested, ghcr.io/docker.io ready)")
          (ct-push . "Partial: connects, reads bundles, upload debugging")
          (ct-pack . "Creates .ctp tar bundles")
          (ct-verify . "Verifies bundle integrity")
          (ct-key . "Key management: list, import, export, trust, delete")
          (ct-help . "Command help with --json")
          (ct-explain . "Conceptual explanations")
          (ct-version . "Version info with --json")
          (registry-pull . "Pull_Manifest tested live with Docker registry")
          (registry-auth . "Basic, Bearer, cloud providers (AWS ECR, GCP GCR, Azure ACR)")
          (transparency-upload . "Upload_Signature to Rekor (structure complete)")
          (transparency-get . "Get_Entry_By_UUID/Index (API ready)")))))

    (route-to-mvp
      ((milestones
         ((v0.1-first-ascent
            ((status . "complete")
             (completed . "2026-01-22")
             (features . "Pack, verify, key management, help system, crypto")
             (notes . "All Phase 0 features implemented")))
          (v0.2-base-camp
            ((status . "in-progress")
             (started . "2026-01-25")
             (progress . 72)
             (features-complete . ("Registry pull", "HTTP client", "Reference parsing", "Auth conversion"))
             (features-in-progress . ("Registry push debugging", "Cloud registry testing"))
             (features-pending . ("Ed25519 signing", "Rekor submission", "Policy engine"))
             (target . "2026-02-15")
             (notes . "Distribution phase - CLI wiring 72% complete")))
          (v0.3-the-wall
            ((status . "planned")
             (features . "Full attestations, transparency log integration, policy enforcement")
             (notes . "Ecosystem integration phase")))
          (v0.4-the-summit
            ((status . "planned")
             (features . "Federated operation, build verification")
             (notes . "Production hardening phase")))))))

    (test-status
      ((e2e-tests . ((total . 41) (passing . 41) (failing . 0) (pass-rate . 100)))
       (crypto-tests . ((total . 7) (passing . 7) (failing . 0) (pass-rate . 100)))
       (total . ((tests . 48) (passing . 48) (pass-rate . 100)))
       (live-testing
         ((localhost-registry . ((fetch . "SUCCESS") (push . "PARTIAL - debugging")))
          (cloud-registries . ((ghcr.io . "PENDING") (docker.io . "PENDING")))))))

    (blockers-and-issues
      ((critical . ())
       (high . ((ct-push-upload . "Manifest upload to registry returns server error - needs debugging")
                (proven-library . "Compilation errors - formally verified parsing disabled for MVP")))
       (medium . ((ech-support . "ECH disabled - requires modern curl, deferred to production")
                  (blob-upload . "Blob/layer upload not implemented - manifest-only for MVP")
                  (json-parsing . "Full JSON manifest parsing incomplete - raw JSON works")))
       (low . ((localhost-https . "Localhost uses HTTP - production uses HTTPS (acceptable for testing)")))
       (resolved . ((curl-parameters . "Fixed Positive'Image leading space in --max-time/--max-redirs")
                    (localhost-port-parsing . "Fixed parser to distinguish port colon from tag colon")
                    (registry-http-auto . "Localhost auto-detects HTTP, production defaults to HTTPS")
                    (test-failures . "All 41 E2E tests passing (was 40/41, now 100%)")))))

    (critical-next-actions
      ((immediate . ("Debug ct push manifest upload" "Test with ghcr.io/Docker Hub" "Add HTTP response logging"))
       (this-week . ("Implement Ed25519 signing (openssl wrapper)" "Submit test attestation to Rekor" "Document usage examples"))
       (this-month . ("Verify Merkle inclusion proofs" "Complete policy engine" "First Debian package import"))
       (deferred-to-production . ("Fix proven library" "Enable ECH" "Full OCI blob upload"))))

    (session-history
      ((session-2026-01-25a . "Build fixes: Auth type separation, reserved word conflicts, all files compiling")
       (session-2026-01-25b . "E2E test suite: 41 tests, 40 passing (97.6%), comprehensive coverage")
       (session-2026-01-25c . "Documentation: E2E-TEST-RESULTS.md, IMPLEMENTATION-STATUS.md, SESSION-SUMMARY.md")
       (session-2026-01-25d . "CLI wiring: Run_Fetch and Run_Push connected to backend operations")
       (session-2026-01-25e . "Live testing: Fixed curl params, localhost port parsing, ECH disabled, ct fetch working")
       (session-2026-01-25f . "Status: 48/48 tests passing (100%), fetch working live, push debugging, 72% complete")))))

;; Helper functions for querying state

(define (get-completion-percentage)
  (cdr (assoc 'overall-completion (cadr (assoc 'current-position project-state)))))

(define (get-blockers level)
  (cdr (assoc level (cadr (assoc 'blockers-and-issues project-state)))))

(define (get-milestone name)
  (assoc name (cdr (assoc 'milestones (cadr (assoc 'route-to-mvp project-state))))))

(define (get-test-status)
  (cadr (assoc 'test-status project-state)))

(define (get-working-features)
  (cdr (assoc 'working-features (cadr (assoc 'current-position project-state)))))

(define (get-component-status component-name)
  (assoc component-name (cdr (assoc 'components (cadr (assoc 'current-position project-state))))))
