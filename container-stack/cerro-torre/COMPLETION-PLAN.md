# Cerro Torre - Path to 100% Completion

**Current Status:** ~68% (honest assessment as of 2026-03-10)
**Target:** 100%
**Gap:** ~32% — stubs in importers, exporters, transparency verification, builder, PQ crypto
**Created:** 2026-01-22
**Last Updated:** 2026-03-10

---

## Phase 1: Complete Core Functionality (Priority: HIGH)

### 1.1 Debian Importer (MOSTLY COMPLETE)
**File:** `src/importers/debian/cerro_import_debian.adb`
**Current:** 90% complete (712 lines of real implementation)

**DONE:**
- [x] Parse_Dsc: Full field extraction (Source, Version, Maintainer, Build-Depends, checksums, tarballs)
- [x] Import_From_Dsc: Reads .dsc file, validates fields, converts Dsc_Info to Manifest (metadata, provenance, dependencies, build, outputs, attestations)
- [x] Import_Package: Downloads .dsc from Debian mirror pool via HTTP, falls back to APT source method
- [x] Import_From_Apt_Source: Downloads Sources.gz, decompresses via gunzip, parses to find .dsc URL, downloads and imports
- [x] Parse_Debian_Version: Handles epoch:upstream-revision format
- [x] Parse_Debian_Dependencies: Splits Build-Depends, extracts package names

**Remaining TODO Items:**
- [ ] Test against live Debian mirrors (code complete but untested against real infra)
- [ ] Handle edge cases: missing checksums, unusual .dsc formats
- [ ] Add unit tests

**Dependencies:** HTTP client ✅, GNAT.OS_Lib ✅
**Estimated Effort:** 2-4 hours (testing and edge cases only)

### 1.2 Registry Operations (MOSTLY COMPLETE)
**File:** `src/core/ct_registry.adb` (1107 lines)
**Current:** 85% complete

**DONE:**
- [x] Authenticate: Anonymous detection, WWW-Authenticate parsing, Bearer token exchange
- [x] Pull_Manifest: GET with Accept headers, auth, digest extraction
- [x] Push_Manifest: PUT with Content-Type, auth, debug logging
- [x] Pull_Blob: GET with streaming to file
- [x] Push_Blob: Monolithic upload (POST initiate, PUT with digest)
- [x] Push_Blob_From_File: File upload via CT_HTTP
- [x] Manifest_Exists, Blob_Exists: HEAD requests
- [x] Delete_Manifest: DELETE with auth
- [x] Mount_Blob: Cross-repository mount
- [x] List_Tags: GET with JSON array parsing
- [x] Parse_Reference: Registry/repo/tag/digest extraction (handles localhost:port, ghcr.io, etc.)
- [x] Manifest_To_Json: Full OCI manifest serialization
- [x] Manifest_Digest: SHA256 via Cerro_Crypto
- [x] Verify_Digest: Content hash comparison

**Remaining TODO Items:**
- [ ] Push_Blob_From_File uses placeholder all-zeros digest (needs SHA256 computation)
- [ ] Parse_Manifest: Can parse basic fields but not nested config/layers objects
- [ ] Chunked upload for large blobs (currently monolithic only)
- [ ] Timing-safe digest comparison (currently simple string comparison)
- [ ] Test with cloud registries (ghcr.io, docker.io)

**Dependencies:** HTTP client ✅, Cerro_Crypto ✅, CT_JSON ✅
**Estimated Effort:** 4-6 hours (digest fix, cloud testing)

### 1.3 OCI Exporter Enhancements (MOSTLY COMPLETE)
**File:** `src/exporters/oci/cerro_export_oci.adb` (751 lines)
**Current:** 80% complete

**DONE:**
- [x] Export_Package: Validates manifest, creates tarball via Export_To_Tarball, sets image ref
- [x] Export_To_Tarball: Full pipeline — Create_Rootfs, tar layer, SHA256 digest, config.json, manifest.json (Docker load format), final tarball
- [x] Push_To_Registry: Extracts tarball, pushes layer blob, reads config, pushes config blob, assembles OCI manifest, pushes manifest with tag
- [x] Create_Rootfs: Creates FHS directory structure with marker file
- [x] Create_Config_Json: Generates OCI config with entrypoint, cmd, env, labels, rootfs/diff_ids

**Remaining TODO Items:**
- [ ] Attach_Provenance: SLSA provenance attachment (pragma Unreferenced — stub)
- [ ] Attach_SBOM: CycloneDX/SPDX generation (pragma Unreferenced — stub)
- [ ] Create_Rootfs needs to copy actual package files (currently creates empty FHS)

**Dependencies:** ct_registry ✅, Cerro_Crypto ✅, Cerro_Provenance (partial)
**Estimated Effort:** 4-6 hours (Attach_Provenance, Attach_SBOM, rootfs population)

---

## Phase 2: Runtime & Build System (Priority: MEDIUM)

### 2.1 Runtime Engine (MEDIUM)
**File:** `src/runtime/cerro_runtime.adb`
**Current:** Unknown
**Gap:** Container execution

**TODO Items:**
- [ ] Implement `Run_Container` (delegate to Podman/Docker)
- [ ] Implement `Stop_Container`
- [ ] Implement `Inspect_Container`
- [ ] Pre-flight verification checks

**Dependencies:** None (calls external runtime)
**Estimated Effort:** 4-6 hours

### 2.2 Builder System (MEDIUM)
**File:** `src/build/cerro_builder.adb`
**Current:** Unknown
**Gap:** Build orchestration

**TODO Items:**
- [ ] Implement `Build_From_Manifest`
- [ ] Sandbox build environment
- [ ] Capture build logs
- [ ] Generate build summary

**Dependencies:** manifest parser ✅
**Estimated Effort:** 6-8 hours

### 2.3 Provenance Chain (MEDIUM)
**File:** `src/core/cerro_provenance.adb`
**Current:** Unknown
**Gap:** Attestation generation

**TODO Items:**
- [ ] Implement SLSA provenance v1.0 generation
- [ ] Implement in-toto attestation format
- [ ] Sign provenance with Ed25519
- [ ] Attach to manifests

**Dependencies:** crypto ✅, manifest ✅
**Estimated Effort:** 4-6 hours

---

## Phase 3: Additional Importers (Priority: LOW)

### 3.1 Alpine Importer (LOW)
**File:** `src/importers/alpine/cerro_import_alpine.adb`
**Current:** Stub only
**Gap:** 95%

**TODO Items:**
- [ ] Implement APKBUILD parsing
- [ ] Query Alpine package database
- [ ] Download and verify packages

**Dependencies:** HTTP client ✅
**Estimated Effort:** 6-8 hours
**Priority:** Can defer (lcb-website uses Debian)

### 3.2 Fedora Importer (LOW)
**File:** `src/importers/fedora/cerro_import_fedora.adb`
**Current:** Stub only
**Gap:** 95%

**TODO Items:**
- [ ] Implement .spec file parsing
- [ ] Query Fedora repos (DNF/yum)
- [ ] Download SRPMs

**Dependencies:** HTTP client ✅
**Estimated Effort:** 6-8 hours
**Priority:** Can defer

### 3.3 RPM-OSTree Exporter (LOW)
**File:** `src/exporters/rpm-ostree/cerro_export_ostree.adb`
**Current:** Stub only
**Gap:** 95%

**TODO Items:**
- [ ] Implement OSTree commit creation
- [ ] Generate RPM-OSTree manifest
- [ ] Push to OSTree repo

**Dependencies:** Complex (OSTree libraries)
**Estimated Effort:** 12+ hours
**Priority:** Can defer

---

## Phase 4: Future Features (Priority: DEFERRED)

### 4.1 Post-Quantum Cryptography (DEFERRED)
**File:** `src/core/ct_pqcrypto.adb`
**Current:** Stub only
**Gap:** 100%

**TODO Items:**
- [ ] Integrate CRYSTALS-Dilithium
- [ ] Integrate Falcon
- [ ] Hybrid signatures (Ed25519 + PQ)

**Dependencies:** liboqs bindings ✅
**Estimated Effort:** 16+ hours
**Priority:** Future (post-v1.0)

### 4.2 Certificate Transparency (DEFERRED)
**File:** `src/core/ct_transparency.adb`
**Current:** Stub only
**Gap:** 100%

**TODO Items:**
- [ ] Submit to transparency logs
- [ ] Verify SCTs
- [ ] Monitor log inclusion

**Dependencies:** HTTP client ✅
**Estimated Effort:** 8-12 hours
**Priority:** Future (post-v1.0)

---

## Phase 5: Testing & Quality (Priority: HIGH)

### 5.1 Test Suite (MISSING - CRITICAL)
**Current:** 0 test files
**Gap:** 100%

**TODO Items:**
- [ ] Create `tests/` directory structure
- [ ] Unit tests for crypto module
- [ ] Unit tests for manifest parser
- [ ] Integration test: pack → verify
- [ ] Integration test: import → export
- [ ] Integration test: end-to-end workflow

**Estimated Effort:** 12-16 hours
**Blocking:** v1.0 release

### 5.2 ATS2 Shadow Verifier (NON-CRITICAL)
**Current:** Source exists, patscc not installed
**Gap:** Build not tested

**TODO Items:**
- [ ] Install ATS2 compiler (patscc)
- [ ] Build ct-shadow binary
- [ ] Test against canonical.ctp files
- [ ] Add to CI pipeline (allow_failure: true)

**Estimated Effort:** 2-4 hours
**Priority:** Optional (marked "safe to remove")

---

## Completion Metrics (Updated 2026-03-10)

| Component | Current | Target | Effort Remaining | Priority |
|-----------|---------|--------|------------------|----------|
| Debian importer | 90% | 100% | 2-4h | **HIGH** |
| Registry ops | 85% | 100% | 4-6h | **HIGH** |
| OCI exporter | 80% | 100% | 4-6h | **MEDIUM** |
| Runtime engine | 75% | 100% | 2-4h | **MEDIUM** |
| Transparency | 55% | 100% | 8-12h | **MEDIUM** |
| CLI wiring | 65% | 100% | 4-6h | **MEDIUM** |
| Provenance | 60% | 100% | 4-6h | **MEDIUM** |
| Builder | 15% | 100% | 6-8h | **MEDIUM** |
| Test suite | 40% | 80% | 8-12h | **HIGH** |
| Alpine importer | 5% | 100% | 6-8h | **LOW** |
| Fedora importer | 5% | 100% | 6-8h | **LOW** |
| OSTree exporter | 5% | 100% | 12+h | **LOW** |
| PQ crypto | 5% | 100% | 16+h | **DEFERRED** |
| SPARK proofs | 0% | 50% | 12+h | **DEFERRED** |

**Total Effort Estimate:**
- **Critical Path (HIGH priority):** 20-28 hours
- **Full v1.0 (HIGH + MEDIUM):** 54-74 hours
- **All Features (including LOW):** 90-120 hours

---

## Immediate Action Plan (This Week)

### Priority 1: Unblock lcb-website (6-8 hours)
1. Complete Debian importer Dsc_Info → Manifest conversion
2. Implement Import_Package and Import_From_Apt_Source
3. Test with actual Debian packages (nginx, wordpress, mariadb)
4. Create WordPress manifest for lcb-website

### Priority 2: Core Functionality (12-16 hours)
5. Implement registry push/pull operations
6. Complete OCI exporter (SLSA, SBOM attachment)
7. Implement runtime execution (Run_Container)
8. Basic test suite (crypto, manifest, pack/verify)

### Priority 3: Production Readiness (8-12 hours)
9. Implement builder orchestration
10. Implement provenance generation
11. Integration tests for full workflow
12. Update documentation

**Target:** 95-100% completion in 26-36 hours of focused work

---

## Definition of "100% Complete"

A component is considered 100% complete when:
- ✅ All `pragma Unreferenced` stubs removed
- ✅ All `TODO` comments resolved or documented as future work
- ✅ Code compiles without warnings
- ✅ Unit tests exist and pass
- ✅ Integration tests demonstrate functionality
- ✅ Documentation updated
- ✅ Ready for production use

**Note:** Post-quantum crypto and transparency logging are explicitly marked as v2.0+ features and not required for 100% v1.0 completion.
