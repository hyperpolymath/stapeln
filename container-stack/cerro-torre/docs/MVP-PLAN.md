# Cerro Torre MVP Plan

**Version**: 0.1.0-mvp
**Last Updated**: 2025-12-29
**Status**: Draft

## Executive Summary

Cerro Torre is "**ship containers safely**" — the distribution complement to Svalinn's "run containers nicely".

A user should be able to:
1. **Wrap** an OCI image into a verifiable bundle (`.ctp`)
2. **Move** that bundle around (offline, airgapped, mirrors)
3. **Verify** it deterministically
4. **Install/run** it with minimal ceremony

MVP ≠ a whole distro. MVP = ergonomic CLI for pack/verify/explain with great errors.

### Success Commands

```bash
# Pack an OCI image into a verifiable bundle
ct pack docker.io/library/nginx:1.26 -o nginx.ctp

# Verify the bundle (with clear, actionable errors)
ct verify nginx.ctp

# Explain the verification chain in human terms
ct explain nginx.ctp

# Key management
ct keygen --id my-key
ct key list
```

---

## MVP Success Criteria

| Criterion | Description | Verification |
|-----------|-------------|--------------|
| **One-command pack** | Wrap OCI image into verifiable .ctp | `ct pack nginx:1.26 -o nginx.ctp` succeeds |
| **One-command verify** | Verify bundle with clear pass/fail | `ct verify nginx.ctp` returns 0 or specific error code |
| **Great errors** | Specific, actionable error messages | Hash mismatch vs signature vs key vs policy distinct |
| **Human explanation** | Verification chain readable by humans | `ct explain nginx.ctp` shows trust chain |
| **Key management** | Generate, list, import keys | `ct keygen`, `ct key list` work |
| **Policy support** | Trust policy file for verification | `ct verify --policy policy.json` works |
| **Deterministic output** | Same inputs → byte-identical .ctp | Canonicalization conformance tests pass |

---

## Implementation Stages

### Stage 0: CLI Foundation + Contracts (2 days)

**Goal**: Establish CLI structure and pin down bundle format before implementing logic.

#### Deliverables

1. **CLI Skeleton** (`src/cli/`)
   - Command parser with subcommands: `pack`, `verify`, `explain`, `keygen`, `key`
   - Help text for all commands
   - Exit code constants (see Error Taxonomy)
   - Output formatting (text/JSON modes)

2. **Bundle Format** (`.ctp` file)
   ```
   <name>.ctp                    # Single-file bundle
   ├── manifest.toml             # Embedded TOML manifest
   ├── summary.json              # Hash-stable build summary
   └── signatures/               # Detached signatures
       └── <keyid>.sig
   ```

   Or as a directory (for large images):
   ```
   <name>.ctp/
   ├── manifest.toml
   ├── summary.json
   ├── signatures/
   ├── blobs/                    # Content-addressed blobs
   │   └── sha256/
   └── oci-layout               # Optional OCI image
   ```

3. **Error Taxonomy** (exit codes)

   | Code | Name | Description |
   |------|------|-------------|
   | 0 | SUCCESS | Operation completed |
   | 1 | HASH_MISMATCH | Content tampered or corrupted |
   | 2 | SIGNATURE_INVALID | Signature doesn't verify |
   | 3 | KEY_NOT_TRUSTED | Signer not in policy |
   | 4 | POLICY_REJECTION | Registry/base not allowed |
   | 5 | MISSING_ATTESTATION | Required attestation absent |
   | 10 | MALFORMED_BUNDLE | Invalid structure or format |
   | 11 | IO_ERROR | File not found, permission denied |
   | 12 | NETWORK_ERROR | Registry unreachable |

4. **Policy File Format** (`policy.json`)
   ```json
   {
     "version": "1",
     "signers": { "allowed": ["cerro-*"], "threshold": 1 },
     "registries": { "allowed": ["docker.io/*"], "blocked": [] },
     "suites": { "allowed": ["CT-SIG-01", "CT-SIG-02"] }
   }
   ```

#### Seam Invariants

| Seam | Invariant |
|------|-----------|
| Pack → Verify | `ct verify (ct pack X)` always succeeds |
| Same inputs → Same output | Deterministic canonicalization |
| Error → Message | Every exit code has actionable error text |
| Offline → Works | All operations work without network (given cached content) |

---

### Stage 1: Pack + Verify Core (5-7 days)

**Goal**: Implement `ct pack` and `ct verify` with great errors.

#### 1.1 Cryptographic Operations (Day 1-2)

**File**: `src/core/cerro_crypto.adb`

| Option | Pros | Cons |
|--------|------|------|
| **libsodium bindings** | Battle-tested, fast | External C dependency |
| **Pure Ada (SPARKNaCl)** | No C, fully provable | Less mature, more work |

**MVP Recommendation**: libsodium bindings for SHA256/Ed25519.

- Implement `SHA256_Hash` and `SHA256_File`
- Implement `Ed25519_Sign` and `Ed25519_Verify`
- Constant-time comparison for digests

**Acceptance**: Hash a file, sign/verify round-trip

#### 1.2 OCI Image Reading (Day 2-3)

**File**: `src/oci/cerro_oci_read.ads/adb` (new)

- Read OCI image from local tarball or directory
- Read from registry via `skopeo copy` (shell out for MVP)
- Extract manifest, config, layer digests
- Compute content digests for verification

**Acceptance**: `ct pack oci:./test-image -o test.ctp` extracts image metadata

#### 1.3 Bundle Writer (Day 3-4)

**File**: `src/bundle/cerro_bundle_write.ads/adb` (new)

- Generate canonical `manifest.toml` from OCI image
- Generate `summary.json` with all digests
- Apply canonicalization rules (per `spec/manifest-canonicalization.adoc`)
- Write bundle as single file or directory

**Bundle structure**:
```
nginx.ctp
├── manifest.toml      # Canonical TOML
├── summary.json       # Hash-stable summary
├── blobs/sha256/...   # Content blobs (optional)
└── signatures/        # Signature files
```

**Acceptance**: `ct pack` produces valid bundle structure

#### 1.4 Bundle Verifier (Day 4-5)

**File**: `src/bundle/cerro_bundle_verify.ads/adb` (new)

- Read and parse bundle
- Verify all content hashes match summary
- Verify signatures against policy
- Return specific exit codes for each failure type

**Error output format**:
```
✗ Verification failed: signature invalid

  Bundle:    nginx.ctp
  Signer:    unknown-key-2025
  Algorithm: ed25519

  The signature does not match the manifest content.

  To see expected signers: ct explain nginx.ctp --signers
```

**Acceptance**: `ct verify` returns correct exit code for each error type

#### 1.5 Explain Command (Day 5)

**File**: `src/cli/cerro_explain.ads/adb` (new)

- Parse bundle without verification
- Print human-readable verification chain
- Show package info, provenance, content hashes, signatures, trust chain

**Acceptance**: `ct explain bundle.ctp` prints readable output

---

### Stage 2: Key Management + Policy (2-3 days)

**Goal**: Minimal key UX and trust policy support.

#### 2.1 Key Generation (Day 1)

**File**: `src/keystore/cerro_keygen.ads/adb` (new)

- Generate Ed25519 keypair
- Encrypt private key with Argon2id (per `spec/keystore-policy.json`)
- Store in `~/.config/cerro/keys/`
- Generate human-readable fingerprint

**Command**: `ct keygen --id my-key`

**Output**:
```
Generated keypair: my-key

  Public key:  ~/.config/cerro/keys/my-key.pub
  Private key: ~/.config/cerro/keys/my-key.key (encrypted)

  Fingerprint: SHA256:abc123def456...
  Algorithm:   ed25519 (CT-SIG-01)
```

**Acceptance**: `ct keygen` creates working keypair

#### 2.2 Key Management (Day 1-2)

**File**: `src/keystore/cerro_keystore.ads/adb` (new)

- `ct key list` - Show all keys with fingerprints
- `ct key import <file>` - Import public key
- `ct key export <id> --public` - Export public key
- `ct key default <id>` - Set default signing key

**Key listing format**:
```
Keys in ~/.config/cerro/keys/

  ID              TYPE      SUITE      DEFAULT
  my-key          keypair   CT-SIG-01  ✓
  upstream-nginx  public    CT-SIG-01
```

**Acceptance**: All key subcommands work

#### 2.3 Policy Support (Day 2)

**File**: `src/policy/cerro_policy.ads/adb` (new)

- Read `policy.json` from `--policy` or `~/.config/cerro/policy.json`
- Evaluate signer trust (glob matching)
- Evaluate registry allowlist/blocklist
- Evaluate suite restrictions

**Minimal starter policy**:
```json
{
  "version": "1",
  "signers": { "allowed": ["*"], "threshold": 1 },
  "registries": { "allowed": ["*"] },
  "suites": { "allowed": ["CT-SIG-01"] }
}
```

**Acceptance**: `ct verify --policy strict.json` enforces policy

#### 2.4 Signing Integration (Day 2-3)

**Update**: `src/bundle/cerro_bundle_write.adb`

- Sign summary.json with selected key
- Write signature to `signatures/<keyid>.sig`
- Include suite_id in signature metadata

**Acceptance**: `ct pack -k my-key` produces signed bundle

---

### Stage 3: Testing + Polish (2-3 days)

**Goal**: Conformance tests, error message polish, documentation.

#### 3.1 Canonicalization Conformance (Day 1)

**Directory**: `tests/canon/`

Per `spec/manifest-canonicalization.adoc`:

- **Valid cases**: inputs that must canonicalize identically
- **Invalid cases**: inputs that must be rejected
- **Idempotence**: `canonicalize(canonicalize(x)) == canonicalize(x)`

Test runner compares Ada output against golden files.

**Shadow Verifier** (non-authoritative):
- ATS2 at `tools/ats-shadow/` cross-checks canonicalization
- Checks: LF-only, no TAB, no trailing whitespace, key ordering
- CI runs but `allow_failure: true`

#### 3.2 Error Message Testing (Day 1-2)

**Directory**: `tests/errors/`

For each error code, verify:
- Correct exit code returned
- Message is specific (what failed)
- Message is actionable (what to do)
- Context included (bundle name, key id, etc.)

**Test cases**:
- Hash mismatch (tampered content)
- Signature invalid (wrong key)
- Key not trusted (not in policy)
- Policy rejection (registry blocked)
- Malformed bundle (bad TOML)

#### 3.3 Integration Tests (Day 2)

**Directory**: `tests/integration/`

End-to-end scenarios:
1. `ct pack` → `ct verify` → `ct explain` round-trip
2. Pack unsigned → sign later → verify
3. Verify against permissive vs strict policy
4. Key generation → sign → verify with imported key

#### 3.4 Documentation (Day 2-3)

- Update `README.adoc` with quick start
- `docs/CLI.md` - Command reference (or generate from `--help`)
- `docs/POLICY.md` - Trust policy format and examples
- `CHANGELOG.md` entry for v0.1.0

**CI Pipeline** (`.gitlab-ci.yml`):
```yaml
stages: [build, test, shadow, integration]

build:
  script: alr build

test:
  script: alr exec -- cerro_tests

shadow-verify:
  stage: shadow
  script:
    - patscc -O2 -DATS_MEMALLOC_LIBC -o ct-shadow tools/ats-shadow/main.dats
    - ./ct-shadow --check-all tests/canon/
  allow_failure: true

integration:
  script: ./scripts/test-e2e.sh
```

---

## What to Defer (Post-MVP)

### v0.2: Distribution Commands

| Feature | Description |
|---------|-------------|
| `ct fetch` | Pull .ctp from registry or create from OCI image |
| `ct push` | Publish .ctp to registry/mirror/object store |
| `ct export` / `ct import` | Offline media support (airgap) |
| `ct policy init` | Interactive policy creation |
| ML-DSA-65 (Dilithium) | Post-quantum signatures via liboqs |
| Hybrid signatures | Ed25519 + ML-DSA-87 together |

### v0.3: Attestations + Ecosystem

| Feature | Description |
|---------|-------------|
| SBOM generation | SPDX 2.3 JSON in bundle |
| SLSA provenance | in-toto attestation format |
| Transparency log | Log submission + proof inclusion |
| Threshold signatures | FROST-Ed25519 for governance |
| SELinux policy | CIL policy generation |

### Future

| Feature | Reason to Defer |
|---------|-----------------|
| Debian importer | Build-from-source flow (separate from pack) |
| Fedora/Alpine importers | Multi-distro support |
| SPARK proofs | Correctness first, proofs later |
| Cooperative governance | Organizational, not technical |

---

## Technical Decisions Required

### Decision 1: Signature Algorithm (DECIDED)

**Context**: Cerro Torre attestations are meant to be verifiable for years or decades. Quantum computers could break Ed25519 by 2035, making historical attestations forgeable.

| Factor | Ed25519 (libsodium) | Dilithium/ML-DSA-65 |
|--------|--------------------|--------------------|
| **Quantum resistance** | No (Shor's algorithm) | Yes (NIST FIPS 204) |
| **Signature size** | 64 bytes | ~3,293 bytes |
| **Public key size** | 32 bytes | ~1,952 bytes |
| **Performance** | Faster | ~3-5x slower |
| **Ada bindings** | libsodium-ada exists | Needs liboqs bindings |
| **Standardization** | Mature | NIST standardized 2024 |

**Decision**: Algorithm-agile signatures with phased rollout.

**MVP (v0.1)**: Ed25519 via libsodium, but:
- Design signature format to support multiple algorithms
- Add `algorithm` field to all signature records
- Document that Ed25519 is transitional

**v0.2**: Add ML-DSA-65 (Dilithium) as primary, Ed25519 as fallback (hybrid optional)

**v0.3+**: Evaluate Ed25519 deprecation policy

**Signature Format** (algorithm-agile from day 1):
```json
{
  "signatures": [
    {"algorithm": "ed25519", "keyid": "...", "sig": "..."},
    {"algorithm": "ml-dsa-65", "keyid": "...", "sig": "..."}
  ]
}
```

This ensures attestations signed today can be verified even after quantum computers arrive.

### Decision 2: Crypto Library

**Options**:
1. **libsodium-ada bindings** (Recommended for MVP)
   - Proven implementation
   - Simple API
   - Alire crate available

2. **SPARKNaCl (pure Ada)**
   - No C dependency
   - Fully provable
   - More integration work

3. **liboqs bindings** (Required for v0.2)
   - Post-quantum algorithms (ML-DSA, ML-KEM)
   - C library, needs Ada bindings

**Recommendation**: libsodium for MVP (Ed25519, SHA256). Add liboqs bindings in v0.2 for ML-DSA.

### Decision 3: Build Isolation

**Options**:
1. **Podman container** (Recommended)
   - Strong isolation
   - Reproducible environment
   - Already in toolchain

2. **chroot/bubblewrap**
   - Lighter weight
   - Harder to configure

3. **Native build**
   - Simplest
   - Not hermetic

**Recommendation**: Podman with pinned Debian base image.

### Decision 4: HTTP Client

**Options**:
1. **AWS (Ada Web Server)** (Recommended)
   - Full-featured
   - Already in Alire

2. **curl bindings**
   - Simpler
   - External dependency

**Recommendation**: AWS for consistency with Ada ecosystem.

---

## Implementation Order

```
Week 1:
├── Day 1-2: Stage 0 (CLI skeleton, bundle format, error taxonomy)
├── Day 2-3: Crypto operations (SHA256, Ed25519)
├── Day 3-4: OCI image reading (skopeo integration)
├── Day 4-5: Bundle writer (manifest.toml, summary.json, canonicalization)
└── Day 5:   Bundle verifier (hash checks, signature verification)

Week 2:
├── Day 1:   ct explain command
├── Day 1-2: Key generation (ct keygen)
├── Day 2:   Key management (ct key list/import/export)
├── Day 2-3: Policy support (policy.json parsing, evaluation)
├── Day 3:   Signing integration (ct pack -k)
├── Day 4:   Canonicalization conformance tests
├── Day 4-5: Error message testing
└── Day 5:   Documentation + release
```

---

## Files to Create/Modify

### New Files

| Path | Purpose |
|------|---------|
| `src/cli/ct_pack.ads/adb` | Pack command implementation |
| `src/cli/ct_verify.ads/adb` | Verify command implementation |
| `src/cli/ct_explain.ads/adb` | Explain command implementation |
| `src/cli/ct_keygen.ads/adb` | Keygen command implementation |
| `src/cli/ct_key.ads/adb` | Key management subcommands |
| `src/cli/ct_errors.ads` | Exit codes and error formatting |
| `src/oci/cerro_oci_read.ads/adb` | OCI image reading |
| `src/bundle/cerro_bundle_write.ads/adb` | Bundle writer |
| `src/bundle/cerro_bundle_verify.ads/adb` | Bundle verifier |
| `src/bundle/cerro_canon.ads/adb` | Canonicalization |
| `src/keystore/cerro_keygen.ads/adb` | Key generation |
| `src/keystore/cerro_keystore.ads/adb` | Key storage and management |
| `src/policy/cerro_policy.ads/adb` | Policy parsing and evaluation |
| `tests/canon/` | Canonicalization conformance tests |
| `tests/errors/` | Error message tests |
| `.gitlab-ci.yml` | CI pipeline |
| `docs/CLI.md` | Command reference |
| `docs/POLICY.md` | Policy format documentation |
| `examples/policy.json` | Starter policy file |

### Modified Files

| Path | Changes |
|------|---------|
| `src/core/cerro_crypto.adb` | Implement SHA256, Ed25519 with libsodium |
| `src/cli/cerro_cli.adb` | Command dispatcher for pack/verify/explain/keygen/key |
| `src/cli/cerro_main.adb` | Entry point with argument parsing |
| `alire.toml` | Add libsodium dependency |
| `README.adoc` | Add quick start |

---

## Risk Mitigation

| Risk | Mitigation |
|------|------------|
| libsodium binding issues | Fall back to shelling out to `sha256sum`, `signify` |
| skopeo unavailable | Document as prerequisite, provide install instructions |
| TOML parser edge cases | Use minimal subset, validate strictly |
| Error messages unclear | User testing + iterate on messages |
| Time overrun | Cut key import/export before cutting pack/verify |

---

## Definition of Done

MVP is complete when:

- [ ] `alr build` produces `ct` binary
- [ ] `ct pack nginx:1.26 -o nginx.ctp` creates valid bundle
- [ ] `ct verify nginx.ctp` returns 0 for valid bundle
- [ ] `ct verify tampered.ctp` returns specific error code
- [ ] `ct explain nginx.ctp` prints human-readable chain
- [ ] `ct keygen --id test` creates working keypair
- [ ] `ct key list` shows keys with fingerprints
- [ ] Error messages are specific and actionable
- [ ] Canonicalization conformance tests pass
- [ ] CI pipeline passes all tests
- [ ] README documents quick start
- [ ] CHANGELOG has v0.1.0 entry

---

## Appendix A: Sample ct verify Error Output

```
✗ Verification failed: hash mismatch

  Bundle:     nginx.ctp
  Layer:      blobs/sha256/a1b2c3...
  Expected:   sha256:a1b2c3d4e5f6...
  Actual:     sha256:deadbeef1234...

  The layer content has been modified or corrupted.
  This may indicate tampering during transfer.

  To inspect the bundle: ct explain nginx.ctp --layers
```

```
✗ Verification failed: key not trusted

  Bundle:     nginx.ctp
  Signer:     suspicious-key-2025
  Fingerprint: SHA256:xyz789...

  This key is not in your trust policy.

  To trust this signer:
    ct key import suspicious-key-2025.pub --trust

  To see your policy: cat ~/.config/cerro/policy.json
```

---

## Appendix B: Sample policy.json

```json
{
  "version": "1",
  "default_action": "deny",

  "signers": {
    "allowed": [
      { "key_id": "cerro-official-*", "required": true },
      { "fingerprint": "SHA256:abc123...", "required": false }
    ],
    "threshold": 1
  },

  "registries": {
    "allowed": [
      "docker.io/library/*",
      "ghcr.io/cerro-torre/*"
    ],
    "blocked": []
  },

  "suites": {
    "allowed": ["CT-SIG-01", "CT-SIG-02"],
    "minimum": "CT-SIG-01"
  }
}
```
