;; SPDX-License-Identifier: PMPL-1.0-or-later
;; STATE.scm - Current project state for Cerro Torre

(define project-state
  `((metadata
      ((version . "2.1.0")
       (schema-version . "1")
       (created . "2025-12-29T03:24:22+00:00")
       (updated . "2026-03-10T00:00:00+00:00")
       (project . "Cerro Torre")
       (repo . "cerro-torre")))

    (project-context
      ((name . "Cerro Torre")
       (tagline . "Ship containers safely - provenance-verified containers from democratically-governed sources")
       (tech-stack ((primary . "Ada/SPARK") (build-system . "Alire") (http-backend . "curl") (registry-protocol . "OCI Distribution v2")))))

    (current-position
      ((phase . "Phase 2: CLI Wiring & Integration - Debian importer complete, OCI exporter functional, transparency partial")
       (overall-completion . 68)
       (honest-assessment . "Strong core (crypto, HTTP, registry, manifest parsing) with real working code. Importers/exporters mostly implemented for Debian/OCI path. Transparency and verification layers have working API calls but stub proof verification. Alpine/Fedora/OSTree importers are pure stubs. SPARK proofs not discharged.")
       (components
         ((core-crypto . ((status . "working") (completion . 95)
                          (notes . "SHA-256/512, Ed25519 verification working, tests passing. SPARK annotations present but proofs not fully discharged via GNATprove.")))
          (http-client . ((status . "working") (completion . 95)
                          (notes . "curl-based client, TLS, auth, GET/POST/PUT/HEAD/DELETE, file upload/download. ECH disabled for MVP.")))
          (registry-client . ((status . "working") (completion . 85)
                              (notes . "Pull/push manifests and blobs working. Auth (Bearer/Basic/anonymous). Push_Blob_From_File uses placeholder digest. JSON manifest parsing partial (can serialize, partial parse).")))
          (transparency-logs . ((status . "partial") (completion . 55)
                                (notes . "Upload_Signature, Lookup_By_UUID/Index, Search_By_Hash, Get_Log_Info all make real HTTP calls. But: Verify_Inclusion, Verify_SET, Verify_Consistency all return False (stubs). Upload_Attestation, Upload_DSSE, Search_By_Public_Key, Search_By_Email, Verify_Bundle_Offline are Not_Implemented. Bundle serialization minimal.")))
          (cli-framework . ((status . "working") (completion . 65)
                            (notes . "ct fetch and ct push wired to backends. ct pack, ct verify, ct key, ct help, ct explain, ct version working. ct sign, ct keygen, ct doctor, ct diff not yet connected.")))
          (manifest-parser . ((status . "working") (completion . 95)
                              (notes . "Full TOML-like parser, all tests passing")))
          (provenance-chain . ((status . "partial") (completion . 60)
                               (notes . "Structures defined, hash verification working. 2 pragma Unreferenced in implementation. SLSA provenance generation and in-toto attestation format not yet implemented.")))
          (tar-writer . ((status . "working") (completion . 95)
                         (notes . "POSIX ustar format, source inclusion")))
          (trust-store . ((status . "working") (completion . 95)
                          (notes . "Local key storage with trust levels")))
          (debian-importer . ((status . "working") (completion . 90)
                              (notes . "Parse_Dsc (full field extraction + checksums), Import_From_Dsc (Dsc->Manifest conversion), Import_Package (HTTP download from mirror), Import_From_Apt_Source (Sources.gz download, decompress, parse, download .dsc). Not yet tested against live Debian mirrors.")))
          (oci-exporter . ((status . "working") (completion . 80)
                           (notes . "Export_Package and Export_To_Tarball create real OCI image tarballs (layer tar, config.json, manifest.json, SHA256 digests). Push_To_Registry does full blob+config+manifest push pipeline. Attach_Provenance and Attach_SBOM are stubs (pragma Unreferenced).")))
          (selinux-policy . ((status . "partial") (completion . 70)
                             (notes . "Policy generation structures present. 2 pragma Unreferenced in cerro_policy_enforce.adb.")))
          (runtime . ((status . "working") (completion . 75)
                      (notes . "Runtime detection (svalinn/podman/nerdctl/docker), PATH search, version check, rootless detection, Load_Image, Run_Container all implemented. Svalinn HTTP API check is stub.")))
          (builder . ((status . "stub") (completion . 15)
                      (notes . "5 pragma Unreferenced. Build_From_Manifest, sandbox, log capture not implemented.")))
          (alpine-importer . ((status . "stub") (completion . 5)
                              (notes . "4 pragma Unreferenced. Pure stubs returning error values.")))
          (fedora-importer . ((status . "stub") (completion . 5)
                              (notes . "4 pragma Unreferenced. Pure stubs returning error values.")))
          (ostree-exporter . ((status . "stub") (completion . 5)
                              (notes . "7 pragma Unreferenced. Pure stubs.")))
          (pq-crypto . ((status . "stub") (completion . 5)
                        (notes . "liboqs bindings scaffolded. Actual PQ operations not implemented.")))
          (json-module . ((status . "working") (completion . 85)
                          (notes . "CT_JSON builder, string/integer field extraction, array support. Handles Rekor and registry JSON. No nested object parsing yet.")))
          (url-parser . ((status . "working") (completion . 90)
                         (notes . "Image reference parsing with registry/repo/tag/digest extraction. Handles localhost:port, ghcr.io, docker.io formats.")))))
       (working-features
         ((ct-fetch . "Downloads OCI manifests from registries (localhost:5000 tested)")
          (ct-push . "Uploads bundles to registries (layer + config + manifest pipeline)")
          (ct-pack . "Creates .ctp tar bundles")
          (ct-verify . "Verifies bundle integrity")
          (ct-key . "Key management: list, import, export, trust, delete")
          (ct-help . "Command help with --json")
          (ct-explain . "Conceptual explanations")
          (ct-version . "Version info with --json")
          (registry-pull . "Pull_Manifest, Pull_Blob, Manifest_Exists, Blob_Exists, List_Tags")
          (registry-push . "Push_Manifest, Push_Blob, Push_Blob_From_File, Mount_Blob")
          (registry-auth . "Basic, Bearer token exchange, anonymous detection")
          (transparency-upload . "Upload_Signature to Rekor via HTTP POST")
          (transparency-lookup . "Lookup_By_UUID, Lookup_By_Index, Search_By_Hash via HTTP")
          (debian-import . "Parse_Dsc, Import_From_Dsc, Import_Package, Import_From_Apt_Source")
          (oci-export . "Export_Package, Export_To_Tarball, Push_To_Registry")
          (runtime-detect . "Detect svalinn/podman/nerdctl/docker, run containers")))))

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
             (progress . 68)
             (features-complete . ("Registry pull" "Registry push" "HTTP client" "Reference parsing" "Auth conversion" "Debian importer" "OCI export tarball" "Runtime detection"))
             (features-in-progress . ("Transparency verification" "CLI command wiring" "Live mirror testing"))
             (features-pending . ("Ed25519 CLI signing" "Rekor proof verification" "Policy engine" "Builder orchestration"))
             (target . "2026-Q2")
             (notes . "Distribution phase - core path (Debian->OCI->registry) functional, verification layer incomplete")))
          (v0.3-the-wall
            ((status . "planned")
             (features . "Full attestations, transparency log verification, policy enforcement, Alpine/Fedora importers")
             (notes . "Ecosystem integration phase")))
          (v0.4-the-summit
            ((status . "planned")
             (features . "Federated operation, build verification, PQ crypto, OSTree export")
             (notes . "Production hardening phase")))))))

    (test-status
      ((e2e-tests . ((total . 41) (passing . 41) (failing . 0) (pass-rate . 100)))
       (crypto-tests . ((total . 7) (passing . 7) (failing . 0) (pass-rate . 100)))
       (total . ((tests . 48) (passing . 48) (pass-rate . 100)))
       (test-gaps . "No unit tests for: debian importer, OCI exporter, transparency, registry client, runtime. E2E test file exists but coverage limited to core path.")
       (live-testing
         ((localhost-registry . ((fetch . "SUCCESS") (push . "SUCCESS")))
          (cloud-registries . ((ghcr.io . "UNTESTED") (docker.io . "UNTESTED")))
          (debian-mirrors . "UNTESTED - Import_From_Apt_Source not tested against live mirrors")))))

    (blockers-and-issues
      ((critical . ())
       (high . ((proven-library . "Compilation errors - formally verified parsing disabled for MVP")
                (transparency-verification . "Merkle inclusion, SET verification, consistency proofs all stubbed - returns False")
                (spark-proofs . "SPARK annotations present but GNATprove not run - proofs not discharged")))
       (medium . ((ech-support . "ECH disabled - requires modern curl, deferred to production")
                  (blob-from-file-digest . "Push_Blob_From_File uses placeholder all-zeros digest instead of computing SHA256")
                  (json-parsing . "No nested JSON object/array parsing - limits manifest introspection")
                  (attach-provenance . "Attach_Provenance and Attach_SBOM are stubs in OCI exporter")
                  (builder . "Build_From_Manifest not implemented - 5 pragma Unreferenced")))
       (low . ((localhost-https . "Localhost uses HTTP - production uses HTTPS (acceptable for testing)")
               (alpine-fedora-importers . "Pure stubs - not blocking v0.2")
               (ostree-exporter . "Pure stub - not blocking v0.2")))
       (resolved . ((curl-parameters . "Fixed Positive'Image leading space in --max-time/--max-redirs")
                    (localhost-port-parsing . "Fixed parser to distinguish port colon from tag colon")
                    (registry-http-auto . "Localhost auto-detects HTTP, production defaults to HTTPS")
                    (test-failures . "All 41 E2E tests passing (was 40/41, now 100%)")
                    (ct-push . "Registry push pipeline working (was debugging, now functional)")
                    (debian-importer . "Full implementation: Parse_Dsc, Import_Package, Import_From_Apt_Source")
                    (oci-export . "Full tarball export pipeline with SHA256 digests")))))

    (critical-next-actions
      ((immediate . ("Wire ct sign and ct keygen to OpenSSL module" "Test Debian importer against live mirrors" "Fix Push_Blob_From_File digest computation"))
       (this-week . ("Implement Merkle inclusion proof verification" "Submit test attestation to Rekor" "Add unit tests for new components"))
       (this-month . ("Implement Attach_Provenance (SLSA)" "Implement Attach_SBOM" "Complete policy engine" "Discharge SPARK proofs via GNATprove"))
       (deferred . ("Fix proven library" "Enable ECH" "Alpine/Fedora importers" "OSTree exporter" "PQ crypto" "Builder orchestration"))))

    (session-history
      ((session-2026-01-25a . "Build fixes: Auth type separation, reserved word conflicts, all files compiling")
       (session-2026-01-25b . "E2E test suite: 41 tests, 40 passing (97.6%), comprehensive coverage")
       (session-2026-01-25c . "Documentation: E2E-TEST-RESULTS.md, IMPLEMENTATION-STATUS.md, SESSION-SUMMARY.md")
       (session-2026-01-25d . "CLI wiring: Run_Fetch and Run_Push connected to backend operations")
       (session-2026-01-25e . "Live testing: Fixed curl params, localhost port parsing, ECH disabled, ct fetch working")
       (session-2026-01-25f . "Status: 48/48 tests passing (100%), fetch working live, push debugging, 72% complete")
       (session-2026-03-10a . "Honesty audit: Reviewed all source files, counted pragma Unreferenced (37 across 10 files). Adjusted completion from 72% to 68%. Debian importer is genuinely complete (700+ lines, HTTP download, APT source parsing). OCI exporter has real tarball pipeline but Attach_Provenance/SBOM stubbed. Transparency has working HTTP calls but all proof verification returns False. Alpine/Fedora/OSTree are pure stubs at 5%. Builder at 15%. Updated STATE.scm, README.adoc, COMPLETION-PLAN.md with honest numbers.")))))

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
