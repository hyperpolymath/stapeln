# Cerro Torre - Path to 100% Completion

**Current Status:** 90%
**Target:** 100%
**Gap:** 10% across 13 files with TODOs/stubs
**Created:** 2026-01-22

---

## Phase 1: Complete Core Functionality (Priority: HIGH)

### 1.1 Debian Importer (HIGH - Blocks lcb-website)
**File:** `src/importers/debian/cerro_import_debian.adb`
**Current:** 60% complete
**Gap:** 40%

**TODO Items:**
- [ ] Line 245: Convert `Dsc_Info` → `Cerro_Manifest.Manifest`
  - Map Source, Version, Maintainer fields
  - Add tarball hashes to manifest
  - Set build dependencies
- [ ] Line 275: Implement `Import_Package`
  - Use HTTP client to query apt-cache
  - Download .dsc file
  - Call Import_From_Dsc
- [ ] Line 290: Implement `Import_From_Apt_Source`
  - Query APT Sources.gz via HTTP
  - Download .dsc from mirror
  - Validate checksums

**Dependencies:** HTTP client ✅ (completed this session)
**Estimated Effort:** 4-6 hours
**Blocking:** WordPress manifest creation

### 1.2 Registry Operations (MEDIUM)
**File:** `src/core/ct_registry.adb`
**Current:** Unknown
**Gap:** Registry push/pull functions

**TODO Items:**
- [ ] Implement registry authentication (Bearer tokens)
- [ ] Implement Push_To_Registry using HTTP client
- [ ] Implement Pull_From_Registry
- [ ] Support Docker Hub, GHCR, custom registries

**Dependencies:** HTTP client ✅
**Estimated Effort:** 6-8 hours

### 1.3 OCI Exporter Enhancements (MEDIUM)
**File:** `src/exporters/oci/cerro_export_oci.adb`
**Current:** 75% complete
**Gap:** 25%

**TODO Items:**
- [ ] Line 18: Implement `Export_Package` (full OCI image)
- [ ] Line 41: Implement `Push_To_Registry` (delegate to ct_registry)
- [ ] Line 186: Implement `Attach_Provenance` (SLSA attestation)
- [ ] Line 195: Implement `Attach_SBOM` (CycloneDX/SPDX)

**Dependencies:** ct_registry, provenance module
**Estimated Effort:** 4-6 hours

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

## Completion Metrics

| Component | Current | Target | Effort | Priority |
|-----------|---------|--------|--------|----------|
| Debian importer | 60% | 100% | 4-6h | **HIGH** |
| Registry ops | 20% | 100% | 6-8h | **MEDIUM** |
| OCI exporter | 75% | 100% | 4-6h | **MEDIUM** |
| Runtime engine | 10% | 100% | 4-6h | **MEDIUM** |
| Builder | 10% | 100% | 6-8h | **MEDIUM** |
| Provenance | 25% | 100% | 4-6h | **MEDIUM** |
| Test suite | 0% | 80% | 12-16h | **HIGH** |
| Alpine importer | 5% | 100% | 6-8h | **LOW** |
| Fedora importer | 5% | 100% | 6-8h | **LOW** |
| OSTree exporter | 5% | 100% | 12+h | **LOW** |
| PQ crypto | 0% | 100% | 16+h | **DEFERRED** |
| Transparency | 0% | 100% | 8-12h | **DEFERRED** |

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
