# Cerro Torre Architecture

## Overview

Cerro Torre is designed as a pipeline that transforms upstream source packages into verified, signed container images with complete provenance.

```
┌─────────────┐    ┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│  Upstream   │───►│   Import    │───►│    Build    │───►│   Export    │
│   Source    │    │             │    │             │    │             │
└─────────────┘    └─────────────┘    └─────────────┘    └─────────────┘
                          │                  │                  │
                          ▼                  ▼                  ▼
                   ┌─────────────────────────────────────────────────┐
                   │              Provenance Chain                   │
                   │  (hashes, signatures, attestations at each step)│
                   └─────────────────────────────────────────────────┘
```

## Core Principles

### 1. Verification Over Trust

Every claim is backed by cryptographic evidence:
- Upstream sources verified against known hashes
- Build outputs verified against expected results
- Signatures from identified keys with recorded trust

### 2. Formal Verification for Security-Critical Code

The `src/core/` modules use SPARK annotations:
- `cerro_crypto` - All cryptographic operations
- `cerro_manifest` - Manifest parsing and validation
- `cerro_provenance` - Provenance chain verification

SPARK proofs guarantee:
- No buffer overflows
- No integer overflows
- No uninitialised memory access
- Functional correctness of crypto operations

### 3. Separation of Concerns

```
┌─────────────────────────────────────────────────────────────┐
│                        CLI Layer                            │
│                    (src/cli/cerro_main)                     │
├─────────────────────────────────────────────────────────────┤
│           Orchestration Layer (Non-SPARK Ada)               │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐    │
│  │ Importers│  │  Builder │  │ Exporters│  │  Policy  │    │
│  └──────────┘  └──────────┘  └──────────┘  └──────────┘    │
├─────────────────────────────────────────────────────────────┤
│               Core Layer (SPARK Verified)                   │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐    │
│  │  Crypto  │  │ Manifest │  │Provenance│  │  Verify  │    │
│  └──────────┘  └──────────┘  └──────────┘  └──────────┘    │
└─────────────────────────────────────────────────────────────┘
```

## Module Descriptions

### Core Layer (`src/core/`)

**cerro_crypto.ads/adb**
- Hash functions (SHA-256, SHA-384, SHA-512, BLAKE3)
- Ed25519 signature verification
- Constant-time comparison (timing attack prevention)
- Hex encoding/decoding

**cerro_manifest.ads/adb**
- Package manifest data types
- TOML parsing into Manifest records
- Structural validation
- Serialisation back to TOML

**cerro_provenance.ads/adb**
- Provenance chain data structures
- Upstream hash verification
- Patch hash verification
- Chain integrity checking

**cerro_verify.ads/adb**
- High-level verification API
- Policy-based verification
- Combines manifest, provenance, and signature checks

### Orchestration Layer

**src/build/cerro_builder.ads/adb**
- Source fetching with hash verification
- Patch application
- Build command execution
- Reproducibility checking

**src/policy/cerro_selinux.ads/adb**
- SELinux policy generation (CIL format)
- Permission checking
- Policy installation

### Importers (`src/importers/`)

Each importer translates from a distribution's package format to Cerro manifests:

**debian/** - Debian .dsc files
- Parse control file format
- Extract source and patches
- Map dependencies

**fedora/** - Fedora SRPMs
- Parse RPM .spec files
- Koji integration
- Handle macros

**alpine/** - Alpine APKBUILD
- Parse shell script format
- aports repository support

### Exporters (`src/exporters/`)

Each exporter produces output in a target format:

**oci/** - OCI Container Images
- Rootfs creation
- OCI config.json generation
- Registry push
- SLSA provenance attachment

**rpm-ostree/** - OSTree Repositories
- OSTree commit creation
- Static delta generation
- rpm-ostree treefile generation

## Data Flow

### Import Flow

```
1. User: cerro import debian:nginx/1.26.0-1
2. Importer downloads .dsc from Debian mirror
3. Importer parses .dsc, extracts metadata
4. Importer downloads orig.tar.gz and debian.tar.gz
5. Core verifies hashes against Debian's recorded values
6. Importer generates .ctp manifest with full provenance
7. Manifest saved to manifests/nginx.ctp
```

### Build Flow

```
1. User: cerro build manifests/nginx.ctp
2. Builder reads manifest
3. Core verifies manifest signatures
4. Builder fetches source (re-verifying hashes)
5. Builder applies patches in order (verifying each)
6. Builder runs build commands in isolated environment
7. Builder collects output files and hashes them
8. Builder updates manifest with file hashes
9. Builder signs manifest with builder key
10. Artifacts saved to output directory
```

### Export Flow

```
1. User: cerro export --format=oci nginx:1.26.0-1
2. Exporter reads manifest
3. Core verifies manifest and signatures
4. Exporter creates rootfs from file list
5. Exporter generates OCI config.json
6. Exporter creates OCI image tarball
7. Exporter attaches provenance attestation
8. Image ready for podman load or registry push
```

## Security Model

### Trust Boundaries

```
┌─────────────────────────────────────────────────────────────┐
│                    Untrusted Zone                           │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐         │
│  │  Network    │  │  Upstream   │  │  User       │         │
│  │  Traffic    │  │  Sources    │  │  Input      │         │
│  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘         │
└─────────┼────────────────┼────────────────┼─────────────────┘
          │                │                │
          ▼                ▼                ▼
┌─────────────────────────────────────────────────────────────┐
│                   Verification Boundary                     │
│                   (SPARK-verified code)                     │
│         All data validated before further processing        │
└─────────────────────────────────────────────────────────────┘
          │
          ▼
┌─────────────────────────────────────────────────────────────┐
│                    Trusted Zone                             │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐         │
│  │  Verified   │  │  Signed     │  │  Trusted    │         │
│  │  Manifests  │  │  Artifacts  │  │  Keys       │         │
│  └─────────────┘  └─────────────┘  └─────────────┘         │
└─────────────────────────────────────────────────────────────┘
```

### Key Management

- **Upstream keys**: Verify source authenticity (GPG keys from GNU, etc.)
- **Builder keys**: Attest to reproducible builds
- **Maintainer keys**: Approve packages for distribution
- **Witness keys**: Sign transparency log entries

Keys are stored in `keys/` with trust levels in `keys/trust.toml`.

## Future Architecture

### Federated Transparency Log

```
┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│  Operator A │    │  Operator B │    │  Operator C │
│    Log      │◄──►│    Log      │◄──►│    Log      │
└──────┬──────┘    └──────┬──────┘    └──────┬──────┘
       │                  │                  │
       └──────────────────┼──────────────────┘
                          │
                    ┌─────▼─────┐
                    │  Witness  │
                    │  Network  │
                    └───────────┘
```

Each operator maintains their own log. Witnesses cross-verify consistency. Clients can query any operator and verify against witnesses.
