# Outstanding Work Items

**Generated**: 2025-12-28
**Status**: Items requiring more than quick fixes

This document lists work items discovered during the repository audit that require substantive effort beyond simple cleanup.

---

## 1. Core Implementation Stubs (HIGH PRIORITY - MVP Blocking)

These are placeholder implementations that must be completed for MVP.

### 1.1 Cryptographic Operations (`src/core/cerro_crypto.adb`)

| Function | Lines | Status | Effort |
|----------|-------|--------|--------|
| `SHA256_Hash` | 98-110 | Placeholder (returns zeros) | 2-4 hours |
| `SHA384_Hash` | 111-123 | Placeholder | 2 hours |
| `SHA512_Hash` | 123-134 | Placeholder | 2 hours |
| `Verify_Ed25519` | 135-146 | Placeholder | 4-8 hours |

**Decision Required**: libsodium bindings vs pure Ada SPARKNaCl
**Blocked By**: Alire crate availability or manual C binding

### 1.2 Manifest Parser (`src/core/cerro_manifest.adb`)

| Function | Lines | Status | Effort |
|----------|-------|--------|--------|
| `Parse_String` | 180-238 | Placeholder | 8-16 hours |
| `Parse_File` | (calls Parse_String) | Depends on above | 1 hour |
| `Manifest_To_String` | 240-247 | Placeholder | 4 hours |
| Version comparison | 138 | Partial (TODO) | 4 hours |

**Decision Required**: Use `toml_slicer` or hand-roll parser?
**Blocked By**: toml_slicer integration

### 1.3 Provenance Verification (`src/core/cerro_provenance.adb`)

| Function | Lines | Status | Effort |
|----------|-------|--------|--------|
| Other hash algorithms | 31 | TODO | 2 hours |
| Signature verification | 131 | TODO | 4 hours (depends on crypto) |
| Trust store lookup | 179 | TODO | 4 hours |

**Blocked By**: Crypto implementation

### 1.4 Verification Module (`src/core/cerro_verify.adb`)

| Function | Lines | Status | Effort |
|----------|-------|--------|--------|
| `Verify_Manifest_File` | 24 | TODO | 2 hours |
| File hash verification | 91 | TODO | 4 hours |
| Build attestation check | 121 | TODO | 2 hours |
| Maintainer attestation | 126 | TODO | 2 hours |
| Signature age check | 135 | TODO | 1 hour |

---

## 2. CLI Implementation (`src/cli/cerro_cli.adb`)

All command handlers are stubs with TODO comments.

| Command | Lines | TODOs | Effort |
|---------|-------|-------|--------|
| `Run_Import` | 36-55 | 4 TODOs | 8-16 hours |
| `Run_Build` | 57-80 | 5 TODOs | 16-24 hours |
| `Run_Verify` | 82-106 | 5 TODOs | 8 hours |
| `Run_Export` | 108-127 | 4 TODOs | 8 hours |
| `Run_Inspect` | 129-146 | 5 TODOs | 4 hours |

**Blocked By**: Core module implementations

---

## 3. Importers (MVP: Debian only)

### 3.1 Debian Importer (`src/importers/debian/cerro_import_debian.adb`)

| Function | Status | Effort |
|----------|--------|--------|
| `Import_From_Pool` | Stub | 8 hours |
| `Import_From_Dsc` | Stub | 8 hours |
| `Import_From_Sources` | Stub | 4 hours |
| `Parse_Control_File` | Stub | 8 hours |

**External Dependencies**: `dpkg-dev` for parsing

### 3.2 Fedora Importer (v0.2)

All stubs - ~24 hours total effort

### 3.3 Alpine Importer (v0.3)

All stubs - ~16 hours total effort

---

## 4. Exporters

### 4.1 OCI Exporter (`src/exporters/oci/cerro_export_oci.adb`)

| Function | Status | Effort |
|----------|--------|--------|
| `Build_Image` | Stub | 8 hours |
| `Export_Tarball` | Stub | 4 hours |
| `Push_To_Registry` | Stub | 8 hours |
| `Create_Rootfs` | Stub | 4 hours |
| `Generate_Config` | Stub | 4 hours |
| `Attach_Provenance` | Stub | 4 hours |
| `Attach_SBOM` | Stub | 4 hours |

### 4.2 OSTree Exporter (v0.3)

All stubs - ~24 hours total effort

---

## 5. Build System (`src/build/cerro_builder.adb`)

| Function | Status | Effort |
|----------|--------|--------|
| `Execute_Build` | Stub | 16 hours |
| `Fetch_Sources` | Stub | 8 hours |
| `Apply_Patches` | Stub | 4 hours |
| `Run_Build_System` | Stub | 8 hours |
| `Check_Reproducibility` | Stub | 8 hours |

**Decision Required**: Podman vs bubblewrap for isolation

---

## 6. SELinux Policy (`src/policy/cerro_selinux.adb`)

All stubs - v0.3 feature, ~40 hours total effort

---

## 7. Attestation Generation (NEW - MVP)

**Files to create:**
- `src/attestations/cerro_sbom.ads/adb` - SPDX SBOM generation
- `src/attestations/cerro_provenance_emit.ads/adb` - in-toto/SLSA output
- `src/attestations/cerro_signer.ads/adb` - Ed25519 signing

Estimated: 24-32 hours total

---

## 8. Testing Infrastructure

### 8.1 Unit Tests (Not Yet Created)

| Test File | Purpose | Effort |
|-----------|---------|--------|
| `tests/test_manifest.adb` | Manifest parsing | 8 hours |
| `tests/test_crypto.adb` | Crypto operations | 4 hours |
| `tests/test_version.adb` | Version comparison | 4 hours |
| `tests/test_provenance.adb` | Provenance verification | 4 hours |

### 8.2 Integration Tests

| Test | Purpose | Effort |
|------|---------|--------|
| E2E hello build | Full pipeline | 8 hours |
| Signature verification | Trust chain | 4 hours |
| Container execution | OCI validity | 4 hours |

---

## 9. Specification Gaps

### 9.1 Algorithm-Agile Signatures

The signature format in MVP-PLAN.md needs to be formalized in a spec document.

**Create**: `spec/signature-format.md`
**Effort**: 4 hours

### 9.2 Trust Store Specification

`keys/trust.toml.example` defines a format but there's no formal spec.

**Create**: `spec/trust-store.md`
**Effort**: 4 hours

### 9.3 Summary.json Schema

Referenced in ATS2 shadow verifier but not specified.

**Create**: `spec/build-summary.md` or add to existing spec
**Effort**: 2 hours

---

## 10. CI/CD Gaps

### 10.1 CodeQL Workflow

`.github/workflows/codeql.yml` is disabled placeholder. Needs proper Ada analysis configuration.

**Effort**: 2 hours

### 10.2 GitLab CI

MVP-PLAN references `.gitlab-ci.yml` but it doesn't exist yet.

**Create**: `.gitlab-ci.yml`
**Effort**: 4 hours

### 10.3 SPARK Proof CI

Current workflow just echoes - needs actual gnatprove integration.

**Effort**: 4 hours (plus license considerations)

---

## 11. Documentation Gaps

| Document | Status | Effort |
|----------|--------|--------|
| `docs/USAGE.md` | Not created | 4 hours |
| `docs/TRUST.md` | Not created | 4 hours |
| `CHANGELOG.md` | Not created | 1 hour |
| API documentation | Not generated | 4 hours |

---

## Priority Order for MVP

1. **Crypto** (SHA256, Ed25519) - blocks everything
2. **TOML Parser** - blocks manifest handling
3. **Manifest Parser** - blocks build/verify
4. **Debian Fetcher** - blocks import
5. **Build Executor** - blocks package creation
6. **OCI Packer** - blocks container output
7. **Attestation Emitters** - completes provenance story
8. **CLI Wiring** - user interface
9. **Tests** - validation
10. **Documentation** - usability

---

## Effort Estimates Summary

| Category | MVP Effort | Post-MVP Effort |
|----------|------------|-----------------|
| Core crypto | 12-20 hours | - |
| Manifest parser | 12-20 hours | - |
| Verification | 12-16 hours | - |
| CLI | 40-60 hours | - |
| Debian importer | 24-32 hours | - |
| OCI exporter | 32-40 hours | - |
| Attestations | 24-32 hours | - |
| Tests | 32-40 hours | - |
| Documentation | 12-16 hours | - |
| **MVP Total** | **200-276 hours** | - |
| Fedora importer | - | 24 hours |
| Alpine importer | - | 16 hours |
| OSTree exporter | - | 24 hours |
| SELinux policy | - | 40 hours |
| ML-DSA crypto | - | 40 hours |
