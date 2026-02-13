# Cerro Torre Roadmap

## Philosophy

Cerro Torre is "**ship containers safely**" — the distribution complement to Svalinn's "run containers nicely".

A user should be able to:
1. **Wrap** an OCI image into a verifiable bundle (`.ctp`)
2. **Move** that bundle around (offline, airgapped, mirrors)
3. **Verify** it deterministically
4. **Install/run** it with minimal ceremony

---

## Current Status: Pre-Alpha → Alpha (HTTP Layer Complete!)

The project structure and architecture are defined. Core modules exist as stubs with interfaces specified but implementations incomplete.

**Recent Progress (2026-01-24):**
- ✅ **HTTP Client (CT_HTTP)** - Complete with modern security features
- ✅ **Registry Operations (CT_Registry)** - All 9 OCI Distribution Spec operations wired
- ✅ **Formally Verified Primitives** - SafeRegistry, SafeDigest, SafeHTTP in `proven` library
- ✅ **Privacy Stack** - ODNS, Tor, HTTP/3, ECH integrated
- ✅ **IPFS-Ready Architecture** - Content addressing aligns with CIDs

**What Works Now:**
```ada
-- Authenticate with OAuth2 token exchange
CT_Registry.Authenticate (Client, "nginx", "pull");

-- Pull manifest with content negotiation
Pull_Manifest (Client, "nginx", "latest");

-- Push manifest with digest verification
Push_Manifest (Client, "nginx", "v1.0", Manifest);

-- Stream large blobs efficiently
Pull_Blob (Client, "nginx", Digest, Output_Path => "/tmp/layer.tar.gz");
```

**HTTP Security Features:**
- HTTP/1.0–3 + QUIC support (HTTP/3 default recommended)
- Encrypted Client Hello (ECH) for SNI privacy
- DANE/TLSA certificate verification (DNSSEC-based)
- DNS-over-HTTPS (DoH) for encrypted DNS
- Oblivious DNS-over-HTTPS (ODNS/ODoH) for anonymous DNS
- Tor integration via SOCKS5H proxy
- Comprehensive proxy support (HTTP, SOCKS4/5)

**Remaining Work for Alpha:**
- JSON parser integration (uncomment `json` in alire.toml)
- Token extraction from auth responses
- Manifest JSON parsing
- Digest calculation (integrate Cerro_Crypto.SHA256)
- CLI wiring (ct push, ct fetch commands)

---

## MVP v0.1 — "First Ascent"

**Goal:** Ergonomic CLI for pack/verify/explain with great errors

### Core Commands (Must Have)

- [ ] **ct pack** — Create verifiable bundle from OCI image
  ```bash
  ct pack docker.io/library/nginx:1.26 -o nginx.ctp
  ct pack oci:./local-image -o local.ctp
  ```
  - Read OCI image (via skopeo for MVP)
  - Generate canonical manifest.toml
  - Generate summary.json with all digests
  - Sign with specified key (or default)

- [ ] **ct verify** — Verify bundle with specific error codes
  ```bash
  ct verify nginx.ctp
  ct verify nginx.ctp --policy strict.json
  ```
  - Exit 0: valid
  - Exit 1: hash mismatch
  - Exit 2: signature invalid
  - Exit 3: key not trusted
  - Exit 4: policy rejection
  - Exit 10: malformed bundle

- [ ] **ct explain** — Human-readable verification chain
  ```bash
  ct explain nginx.ctp
  ct explain nginx.ctp --signers
  ct explain nginx.ctp --layers
  ```
  - Package info, provenance, content hashes
  - Signatures with fingerprints
  - Trust chain status

### Key Management (Must Have)

- [ ] **ct keygen** — Generate signing keypair
  ```bash
  ct keygen --id my-signing-key
  ct keygen --suite CT-SIG-02  # hybrid post-quantum
  ```
  - Ed25519 for MVP (CT-SIG-01)
  - Argon2id-encrypted private key
  - Human-readable fingerprint

- [ ] **ct key** — Key management subcommands
  ```bash
  ct key list
  ct key import upstream.pub
  ct key export my-key --public
  ct key default my-key
  ```

### Trust Policy (Must Have)

- [ ] **policy.json** — Trust policy file
  - Allowed signers (glob patterns)
  - Allowed registries
  - Allowed crypto suites
  - `ct verify --policy <file>` enforcement

### Quality Attributes (Must Have)

- [ ] **Great error messages**
  - Specific: exactly what failed
  - Actionable: what to do about it
  - Contextual: bundle name, key id, etc.

- [ ] **Deterministic canonicalization**
  - Same inputs → byte-identical output
  - Conformance test suite
  - Per spec/manifest-canonicalization.adoc

### Should Have

- [ ] **SHA-256 + Ed25519 via libsodium**
  - Battle-tested implementation
  - SPARK proofs deferred to v0.3

- [ ] **Configuration file**
  - ~/.config/cerro/config.toml
  - Default policy, default key, output format

### Nice to Have

- [ ] **--json output mode** for all commands
- [ ] **Colored terminal output** with --color=auto

---

## v0.2 — "Base Camp"

**Goal:** Distribution, runtime integration, and adoption ergonomics

### Distribution Commands

- [ ] **ct fetch** — Pull bundle from registry or create from image
  ```bash
  ct fetch cerro-registry.io/nginx:1.26 -o nginx.ctp
  ct fetch docker.io/library/nginx:1.26 -o nginx.ctp --create
  ```

- [ ] **ct push** — Publish bundle to registry/mirror
  ```bash
  ct push nginx.ctp cerro-registry.io/nginx:1.26
  ct push nginx.ctp s3://my-bucket/packages/
  ct push nginx.ctp git://github.com/org/manifests
  ```

- [ ] **ct export / ct import** — Offline media support
  ```bash
  ct export nginx.ctp redis.ctp -o offline-bundle.tar
  ct export --manifest packages.txt -o airgap.tar --include-keys
  ct import airgap.tar --verify --policy strict.json
  ```

### Runtime Integration

- [ ] **ct run** — Delegate to configured runtime (including Svalinn)
  ```bash
  ct run nginx.ctp                     # Uses default runtime
  ct run nginx.ctp --runtime=svalinn   # Explicit Svalinn
  ct run nginx.ctp --runtime=podman    # Explicit podman
  ct run nginx.ctp -- -p 8080:80       # Pass args to runtime
  ```
  - Verify before run (unless --no-verify)
  - Configure default runtime in config.toml
  - Svalinn integration per spec/svalinn-integration.adoc

- [ ] **ct unpack** — Extract OCI layout for external tooling
  ```bash
  ct unpack nginx.ctp -o ./nginx-oci/  # OCI layout on disk
  ct unpack nginx.ctp --format=docker  # Docker save format
  ```
  - Enables use with nerdctl, podman, docker, buildah
  - Preserves all attestations alongside

### Diagnostics

- [ ] **ct doctor** — Check distribution pipeline health
  ```bash
  ct doctor           # Full check
  ct doctor --quick   # Just essentials
  ```
  - Crypto backend availability (libsodium, liboqs)
  - Registry access and authentication
  - Local content store integrity
  - Clock skew / timestamp sanity
  - Key expiration warnings
  - Policy file validity

### Key Rotation & Revocation

- [ ] **ct re-sign** — Re-sign bundle with new key (same content)
  ```bash
  ct re-sign nginx.ctp -k new-key-2026
  ct re-sign nginx.ctp --add-signature  # Keep old, add new
  ```
  - Preserves content hashes
  - Supports key rotation without re-packing
  - Can add signatures (multi-signer)

- [ ] **Policy deny-lists and revocation**
  ```json
  {
    "signers": {
      "denied": [
        { "key_id": "compromised-key", "after": "2025-06-01" }
      ]
    },
    "pin": {
      "nginx.ctp": { "digest": "sha256:abc...", "signers": ["trusted-key"] }
    }
  }
  ```
  - Deny signer from specific date
  - Pin artifact to digest + signer (not just tag)
  - Threshold rules (require N of M signers)

### Bundle Comparison

- [ ] **ct diff** — Human-readable diff between bundles
  ```bash
  ct diff old.ctp new.ctp
  ct diff old.ctp new.ctp --layers     # Just layer changes
  ct diff old.ctp new.ctp --signers    # Just signer changes
  ```
  Output:
  ```
  Comparing: nginx-1.25.ctp → nginx-1.26.ctp

  Layers:
    ~ sha256:abc... → sha256:def...  (base layer changed)
    + sha256:123...                   (new layer added)

  Config:
    ~ ENV["NGINX_VERSION"] = "1.25" → "1.26"

  Signatures:
    ✓ Both signed by: cerro-official-2025
    + New signature: nginx-maintainer-2025

  Attestations:
    ✓ SBOM present in both
    ~ Provenance builder changed: v1.0 → v1.1
  ```
  - Huge QoL for trust debugging
  - Shows what actually changed between versions

### Index & Search

- [ ] **ct index** — Build searchable index of bundles
  ```bash
  ct index ./bundles/              # Index a directory
  ct index --update                # Update existing index
  ```

- [ ] **ct search** — Search bundles by metadata
  ```bash
  ct search nginx                  # By name
  ct search --signer cerro-*       # By signer
  ct search --has-sbom             # Has SBOM attestation
  ct search --digest sha256:abc    # By source image digest
  ct search --after 2025-01-01     # By date
  ```
  Searchable fields:
  - Name, version, description
  - Source image digest
  - Signer key IDs and fingerprints
  - SBOM presence, license info
  - Build provenance (builder, date)
  - Base image lineage

### Post-Quantum Signatures

- [ ] **ML-DSA-65 (Dilithium)** via liboqs bindings
- [ ] **CT-SIG-02** — Hybrid Ed25519 + ML-DSA-87
- [ ] **CT-SIG-03** — Post-quantum only (ML-DSA-87)
- [ ] Signature format already algorithm-agile from v0.1

### Policy Helpers

- [ ] **ct policy init** — Create starter policy interactively
- [ ] **ct policy add-signer** — Trust a signer
- [ ] **ct policy add-registry** — Allow a registry
- [ ] **ct policy deny** — Add to deny-list

---

## v0.3 — "The Wall"

**Goal:** Attestations + ecosystem integration

### Attestations

- [ ] **SBOM generation** — SPDX 2.3 JSON in bundle
- [ ] **SLSA provenance** — in-toto attestation format
- [ ] **Transparency log** — Log submission + proof inclusion
- [ ] **Threshold signatures** — FROST-Ed25519 for governance

### Ecosystem

- [ ] **SELinux policy** — CIL policy generation
- [ ] **OSTree export** — Compatible with rpm-ostree
- [ ] **Svalinn deep integration** — Shared trust store, attestation passing

### Quality

- [ ] **SPARK proofs** for crypto module
- [ ] **Security audit** of core code

---

## v0.4 — "The Summit"

**Goal:** Federated operation, build verification

### Build Flow (Separate from Pack)

- [ ] **Debian importer** — Import from Debian source packages
  ```bash
  ct build debian:nginx/1.26.0-1 -o nginx.ctp
  ```
- [ ] **Fedora importer** — Import from SRPMs
- [ ] **Alpine importer** — Import from APKBUILDs

### Federation

- [ ] **Federated transparency log** — Multiple witnesses
- [ ] **Multi-builder verification** — Consensus on builds
- [ ] **Mirror support** — Delta synchronization

---

## Future / Wishlist

- **Nix Importer** — Import from nixpkgs
- **Bootable Images** — Full bootable system images
- **Secure Boot Integration** — Sign boot components
- **Hardware Attestation** — TPM-based attestation
- **Mobile Verification** — Verify on Android/iOS

---

## Non-Goals

- **GUI Application** — CLI-first, web UI for viewing only
- **Windows Support** — Linux containers and immutable Linux only
- **Binary Package Management** — We verify, distributions distribute
- **Build execution in v0.1** — Pack existing images first, build-from-source later

---

## Success Metrics

### MVP v0.1 Success
- [ ] `ct pack` + `ct verify` + `ct explain` work end-to-end
- [ ] Error messages are specific and actionable
- [ ] Key generation and verification work
- [ ] Canonicalization conformance tests pass
- [ ] Documentation complete enough for others to try

### v0.2 Success
- [ ] `ct run` works with Svalinn and podman
- [ ] `ct doctor` catches common setup issues
- [ ] `ct diff` helps debug trust issues
- [ ] Offline export/import works for airgapped environments
- [ ] Post-quantum signatures work (CT-SIG-02)
- [ ] At least one external contributor

### v0.3 Success
- [ ] SBOM + provenance in every bundle
- [ ] Used in at least one production deployment
- [ ] Security audit complete

### v0.4 Success
- [ ] Multiple independent operators
- [ ] Transparency log with >2 witnesses
- [ ] Debian/Fedora import working
