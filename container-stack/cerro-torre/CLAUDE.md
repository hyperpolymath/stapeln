# CLAUDE.md — Cerro Torre Development Context

This file provides context for AI-assisted development of the Cerro Torre project.

## Project Overview

Cerro Torre is a supply-chain-verified Linux distribution for containers and immutable systems. It combines:

- **Format-agnostic package imports** (Debian primary, Fedora/Alpine/Nix secondary)
- **Formally verified tooling** in Ada/SPARK
- **Democratic cooperative governance**
- **Complete cryptographic provenance** for all packages

## Technical Stack

### Primary Implementation Language: Ada/SPARK

The core tooling is written in Ada with SPARK annotations for formally verified critical sections.

**Use SPARK for:**
- All cryptographic operations (hashing, signature verification)
- Manifest parsing and validation
- Provenance chain verification  
- SELinux policy generation/validation
- Any code that touches security-critical data

**Use regular Ada for:**
- Build orchestration
- Network operations (HTTP clients, repository sync)
- Command-line interface
- File system operations
- Import/export drivers

**SPARK Proof Obligations:**
- Absence of runtime errors (no overflow, no index out of bounds)
- Functional correctness for crypto operations
- Information flow (no secret data leakage)
- Termination (all loops must provably terminate)

### Build System

Use Alire (Ada package manager) for dependency management. The project should build with:

```bash
alr build
```

### Directory Structure

```
cerro-torre/
├── docs/                    # Project documentation
│   ├── ARCHITECTURE.md      # System architecture
│   └── ROADMAP.md           # Development roadmap
├── governance/              # Cooperative governance
│   ├── articles.md          # Articles of association
│   ├── covenant.md          # Palimpsest Covenant
│   └── decisions/           # Architectural decision records
├── keys/                    # Public keys for verification
├── manifests/               # Package manifests (.ctp files)
├── spec/                    # Specifications
│   └── manifest-format.md   # CTP manifest specification
└── src/
    ├── core/                # SPARK-verified core
    │   ├── cerro_crypto.*   # Cryptographic operations
    │   ├── cerro_manifest.* # Manifest parsing
    │   ├── cerro_provenance.* # Provenance chain
    │   └── cerro_verify.*   # Verification logic
    ├── build/               # Build orchestration
    │   └── cerro_builder.*
    ├── policy/              # SELinux policy generation
    │   └── cerro_selinux.*
    ├── importers/           # Package source importers
    │   ├── debian/          # Debian .dsc importer
    │   ├── fedora/          # Fedora SRPM importer
    │   └── alpine/          # Alpine APKBUILD importer
    ├── exporters/           # Output format exporters
    │   ├── oci/             # OCI container images
    │   └── rpm-ostree/      # OSTree/rpm-ostree integration
    └── cli/                 # Command-line interface
        └── cerro_main.adb
```

### Key Data Types

The manifest format specification is in `spec/manifest-format.md`. Key types to implement:

```ada
-- Package identifier
type Package_Name is new String with
   Dynamic_Predicate => Is_Valid_Package_Name(Package_Name);

-- Version with epoch
type Version is record
   Epoch    : Natural := 0;
   Upstream : Unbounded_String;
   Revision : Positive;
end record;

-- Cryptographic hash
type Hash_Algorithm is (SHA256, SHA384, SHA512, Blake3);
type Hash_Value is record
   Algorithm : Hash_Algorithm;
   Digest    : Unbounded_String; -- Hex-encoded
end record;

-- Provenance record
type Provenance is record
   Upstream_URL       : Unbounded_String;
   Upstream_Hash      : Hash_Value;
   Upstream_Signature : Unbounded_String; -- Optional
   Imported_From      : Unbounded_String; -- e.g., "debian:nginx/1.26.0-1"
   Import_Date        : Ada.Calendar.Time;
   Patches            : String_Vectors.Vector;
end record;
```

### SPARK Annotations Example

```ada
package Cerro_Crypto
   with SPARK_Mode => On
is
   type Digest_256 is array (1 .. 32) of Unsigned_8;
   
   function SHA256_Hash (Data : String) return Digest_256
      with Global => null,
           Pre    => Data'Length <= Natural'Last - 64,
           Post   => SHA256_Hash'Result'Length = 32;
   
   function Verify_Signature
      (Data      : String;
       Signature : String;
       Public_Key: String) return Boolean
      with Global => null,
           Pre    => Data'Length > 0 
                     and Signature'Length = 64
                     and Public_Key'Length = 32;
                     
end Cerro_Crypto;
```

## Development Guidelines

### Do NOT Use

- **Python** — Project preference is to avoid Python entirely
- **Docker** — Use Podman for container operations
- **GitHub** — Primary repository is GitLab

### Preferences

- **Podman** over Docker for any container operations
- **GitLab CI** for CI/CD pipelines
- Memory-safe languages throughout (Ada, Rust if Ada insufficient for something)
- Explicit error handling, no exceptions for control flow

### Testing

- Unit tests using AUnit
- SPARK proof discharge using GNATprove
- Integration tests for importers using real Debian/Fedora packages
- Property-based testing where applicable

### Code Style

- Follow GNAT coding standard
- All public APIs documented with Ada doc comments
- SPARK contracts on all security-critical code
- No use of `pragma Suppress` for proof obligations

## Current Phase: Proof of Concept

The immediate goals are:

1. **Manifest parser** — Parse .ctp files, validate against spec
2. **Basic crypto** — SHA256 hashing, Ed25519 signature verification
3. **Debian importer prototype** — Parse a simple Debian .dsc, emit .ctp
4. **End-to-end demo** — Take one package through the full flow

### First Package Target

Start with `hello` (GNU Hello) — it's simple, well-maintained, and has minimal dependencies. Success looks like:

```bash
cerro import debian:hello/2.10-3
cerro build manifests/hello.ctp
cerro export --format=oci hello:2.10-3
podman run cerro-torre/hello:2.10-3
```

## External Dependencies

### Ada Libraries (via Alire)

- `gnatcoll` — General utilities
- `aws` — HTTP client (Ada Web Server)
- `xmlada` — XML parsing (for some upstream formats)
- `libsodium-ada` — Cryptographic operations (or bindings to libsodium)
- `json-ada` — JSON handling

### System Dependencies

- `dpkg-dev` — For parsing Debian source packages
- `rpm` — For parsing Fedora SRPMs (secondary)
- `podman` — Container operations
- `ostree` — OSTree operations

## Context Links

- Manifest format specification: `spec/manifest-format.md`
- Architecture: `docs/ARCHITECTURE.md`
- Roadmap: `docs/ROADMAP.md`
- Governance: `governance/articles.md`
- Palimpsest Covenant: `governance/covenant.md`
- Decision records: `governance/decisions/`

## Questions to Resolve

These are open design questions to address during implementation:

1. **Crypto library choice**: Pure Ada implementation vs. binding to libsodium?
2. **Manifest parser**: Hand-written recursive descent vs. parser generator?
3. **Parallelism model**: Ada tasks vs. external process orchestration?
4. **Transparency log protocol**: Define our own vs. adopt Sigstore's format?

## Contact

This is a solo project in early stages. The goal is to create a foundation that a community can build on.
