# Cerro Torre Provenance Chain Specification

**Version**: 0.1.0-draft  
**Status**: Draft Specification  
**SPDX-License-Identifier**: PMPL-1.0-or-later

## Overview

This document specifies how Cerro Torre tracks and verifies the complete provenance of packages from original source to final container image. The goal is **no gaps** — every transformation from source code to deployed binary must be attested, signed, and verifiable.

## Principles

1. **Complete Chain**: Every package must have an unbroken attestation chain from upstream source to output artefact
2. **Multiple Attesters**: No single party can forge provenance; multiple independent attestations required
3. **Cryptographic Binding**: Each link in the chain is cryptographically bound to the previous
4. **Reproducible Verification**: Anyone can verify the chain without special access
5. **Transparency**: All attestations are published to public transparency logs

## Attestation Model

Cerro Torre uses an extended [in-toto](https://in-toto.io/) attestation model with [SLSA](https://slsa.dev/) provenance predicates.

### Attestation Statement Structure

```json
{
  "_type": "https://in-toto.io/Statement/v1",
  "subject": [
    {
      "name": "cerro-torre/nginx",
      "digest": {
        "sha256": "abc123..."
      }
    }
  ],
  "predicateType": "https://cerro-torre.org/provenance/v1",
  "predicate": {
    // Predicate-specific content
  }
}
```

### Attestation Types

#### 1. Source Attestation

Attests to the origin and integrity of upstream source code.

```json
{
  "predicateType": "https://cerro-torre.org/provenance/v1/source",
  "predicate": {
    "upstream": {
      "uri": "https://nginx.org/download/nginx-1.26.0.tar.gz",
      "digest": {
        "sha256": "abc123..."
      }
    },
    "upstreamSignature": {
      "uri": "https://nginx.org/download/nginx-1.26.0.tar.gz.asc",
      "keyring": "cerro-torre:nginx-keys",
      "verified": true,
      "verifiedAt": "2024-12-07T10:30:00Z"
    },
    "importedFrom": {
      "distribution": "debian",
      "package": "nginx",
      "version": "1.26.0-1",
      "uri": "https://deb.debian.org/debian/pool/main/n/nginx/nginx_1.26.0-1.dsc"
    },
    "patches": [
      {
        "name": "01-fix-systemd-socket.patch",
        "digest": {
          "sha256": "def456..."
        },
        "origin": "debian",
        "description": "Fix systemd socket activation"
      }
    ]
  }
}
```

**Required Fields**:
- `upstream.uri`: Original source location
- `upstream.digest`: Hash of upstream source

**Optional but Recommended**:
- `upstreamSignature`: Cryptographic signature from upstream maintainer
- `importedFrom`: Source distribution reference
- `patches`: Applied modifications with rationale

#### 2. Build Attestation

Attests to the build process that transformed source into binary.

```json
{
  "predicateType": "https://slsa.dev/provenance/v1",
  "predicate": {
    "buildDefinition": {
      "buildType": "https://cerro-torre.org/build/v1",
      "externalParameters": {
        "manifest": {
          "uri": "https://gitlab.com/cerro-torre/manifests/-/blob/main/nginx.ctp",
          "digest": {
            "sha256": "ghi789..."
          }
        },
        "variants": ["ssl", "http2"]
      },
      "internalParameters": {
        "builderImage": {
          "uri": "cerro-torre/builder:2024.12",
          "digest": {
            "sha256": "jkl012..."
          }
        }
      },
      "resolvedDependencies": [
        {
          "name": "libc",
          "version": "2.38-1",
          "digest": {
            "sha256": "mno345..."
          },
          "attestationBundle": "https://logs.cerro-torre.org/bundle/mno345"
        }
      ]
    },
    "runDetails": {
      "builder": {
        "id": "https://cerro-torre.org/builders/official-1",
        "version": "1.0.0"
      },
      "metadata": {
        "invocationId": "build-12345",
        "startedOn": "2024-12-07T10:30:00Z",
        "finishedOn": "2024-12-07T10:45:00Z"
      }
    }
  }
}
```

**SLSA Level Requirements**:

| Level | Requirements |
|-------|--------------|
| SLSA 1 | Build attestation exists |
| SLSA 2 | Hosted build service, signed attestation |
| SLSA 3 | Hardened build environment, non-falsifiable provenance |
| SLSA 4 | Two-party review, hermetic builds |

Cerro Torre targets **SLSA 3** for official builds, with **SLSA 4** for critical packages.

#### 3. SBOM Attestation

Attests to the complete software bill of materials.

```json
{
  "predicateType": "https://spdx.dev/Document/v2.3",
  "predicate": {
    "spdxVersion": "SPDX-2.3",
    "dataLicense": "CC0-1.0",
    "SPDXID": "SPDXRef-DOCUMENT",
    "name": "nginx-1.26.0-1",
    "packages": [
      {
        "name": "nginx",
        "versionInfo": "1.26.0-1",
        "supplier": "Organization: Cerro Torre Cooperative",
        "downloadLocation": "https://repo.cerro-torre.org/packages/nginx-1.26.0-1.ctp",
        "checksums": [
          {
            "algorithm": "SHA256",
            "checksumValue": "abc123..."
          }
        ],
        "externalRefs": [
          {
            "referenceCategory": "SECURITY",
            "referenceType": "cpe23Type",
            "referenceLocator": "cpe:2.3:a:nginx:nginx:1.26.0:*:*:*:*:*:*:*"
          }
        ]
      }
    ],
    "relationships": [
      {
        "spdxElementId": "SPDXRef-nginx",
        "relationshipType": "DEPENDS_ON",
        "relatedSpdxElement": "SPDXRef-openssl"
      }
    ]
  }
}
```

#### 4. Signature Attestation

Attests to release signing by authorised keyholders.

```json
{
  "predicateType": "https://cerro-torre.org/provenance/v1/signature",
  "predicate": {
    "signatureScheme": "frost-ed25519",
    "threshold": {
      "k": 3,
      "n": 5
    },
    "signers": [
      {
        "id": "cerro-torre:keyholder-alice",
        "publicKey": "ed25519:abc123...",
        "signedAt": "2024-12-07T11:00:00Z"
      },
      {
        "id": "cerro-torre:keyholder-bob",
        "publicKey": "ed25519:def456...",
        "signedAt": "2024-12-07T11:05:00Z"
      },
      {
        "id": "cerro-torre:keyholder-carol",
        "publicKey": "ed25519:ghi789...",
        "signedAt": "2024-12-07T11:10:00Z"
      }
    ],
    "aggregateSignature": "frost:xyz..."
  }
}
```

#### 5. Review Attestation

Attests to human review of package changes.

```json
{
  "predicateType": "https://cerro-torre.org/provenance/v1/review",
  "predicate": {
    "reviewType": "security",
    "reviewer": {
      "id": "cerro-torre:maintainer-david",
      "role": "security-team"
    },
    "reviewedAt": "2024-12-07T09:00:00Z",
    "scope": ["source-diff", "dependency-changes", "build-flags"],
    "outcome": "approved",
    "notes": "Reviewed upstream changes for CVE-2024-1234 fix"
  }
}
```

## Chain Structure

A complete provenance chain links attestations through content hashes:

```
┌─────────────────────────────────────────────────────────────────┐
│                     PROVENANCE CHAIN                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌──────────────┐                                               │
│  │   UPSTREAM   │  nginx-1.26.0.tar.gz                         │
│  │   SOURCE     │  sha256:aaa...                               │
│  └──────┬───────┘                                               │
│         │                                                        │
│         ▼                                                        │
│  ┌──────────────┐                                               │
│  │   SOURCE     │  Attests: upstream hash, signature verified   │
│  │ ATTESTATION  │  Subject: sha256:aaa...                       │
│  │              │  Signed by: importer-bot                      │
│  └──────┬───────┘                                               │
│         │                                                        │
│         ▼                                                        │
│  ┌──────────────┐                                               │
│  │   REVIEW     │  Attests: human reviewed source changes       │
│  │ ATTESTATION  │  Subject: sha256:aaa...                       │
│  │              │  Signed by: maintainer-david                  │
│  └──────┬───────┘                                               │
│         │                                                        │
│         ▼                                                        │
│  ┌──────────────┐                                               │
│  │    BUILD     │  Attests: build process, environment          │
│  │ ATTESTATION  │  Materials: sha256:aaa... (source)            │
│  │              │  Subject: sha256:bbb... (binary)              │
│  │              │  Signed by: official-builder-1                │
│  └──────┬───────┘                                               │
│         │                                                        │
│         ▼                                                        │
│  ┌──────────────┐                                               │
│  │    SBOM      │  Attests: complete dependency list            │
│  │ ATTESTATION  │  Subject: sha256:bbb...                       │
│  │              │  Signed by: official-builder-1                │
│  └──────┬───────┘                                               │
│         │                                                        │
│         ▼                                                        │
│  ┌──────────────┐                                               │
│  │  SIGNATURE   │  Attests: release approval (3-of-5)           │
│  │ ATTESTATION  │  Subject: sha256:bbb...                       │
│  │              │  Signed by: alice, bob, carol (threshold)     │
│  └──────┬───────┘                                               │
│         │                                                        │
│         ▼                                                        │
│  ┌──────────────┐                                               │
│  │   RELEASED   │  nginx:1.26.0-1                               │
│  │   PACKAGE    │  sha256:bbb...                                │
│  └──────────────┘                                               │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

## Chain Validation Rules

### Rule 1: Unbroken Chain

Every package must have attestations covering:
1. Source origin (Source Attestation)
2. Build process (Build Attestation)
3. Dependency composition (SBOM Attestation)
4. Release approval (Signature Attestation)

Missing links cause verification failure.

### Rule 2: Hash Continuity

Each attestation's `subject.digest` must match the `materials[].digest` of the next attestation in the chain. This cryptographically binds the chain.

### Rule 3: Temporal Ordering

Attestation timestamps must be in logical order:
- Source attestation before build attestation
- Build attestation before signature attestation
- Review attestation (if present) before signature attestation

### Rule 4: Signer Trust

Each attestation must be signed by a key in the appropriate trust set:
- Source attestations: Importer keys or maintainer keys
- Build attestations: Official builder keys
- SBOM attestations: Builder keys
- Signature attestations: Release keyholder keys (threshold)
- Review attestations: Maintainer keys

### Rule 5: Transparency Log Inclusion

All attestations must be submitted to the federated transparency log before a package is considered released. Clients verify log inclusion as part of installation.

## Attestation Bundles

For efficiency, attestations are distributed as bundles:

```json
{
  "mediaType": "application/vnd.cerro-torre.bundle+json",
  "attestations": [
    { /* source attestation */ },
    { /* build attestation */ },
    { /* sbom attestation */ },
    { /* signature attestation */ }
  ],
  "logEntries": [
    {
      "logId": "cerro-torre-log-1",
      "logIndex": 12345,
      "inclusionProof": "..."
    },
    {
      "logId": "cerro-torre-log-2", 
      "logIndex": 67890,
      "inclusionProof": "..."
    }
  ]
}
```

## Verification Algorithm

```
VERIFY_PACKAGE(package, bundle):
  1. Extract attestations from bundle
  2. For each attestation:
     a. Verify signature against trust store
     b. Verify log inclusion (at least 2-of-3 logs)
  3. Verify chain continuity:
     a. Source attestation subject == Build attestation material
     b. Build attestation subject == package hash
  4. Verify threshold signature (k-of-n signers)
  5. If all pass: VERIFIED
     Else: REJECT with specific failure reason
```

## Failure Modes

| Failure | Meaning | Action |
|---------|---------|--------|
| `MISSING_SOURCE_ATTESTATION` | No source provenance | Cannot install |
| `MISSING_BUILD_ATTESTATION` | No build provenance | Cannot install |
| `INVALID_SIGNATURE` | Attestation signature invalid | Cannot install |
| `UNTRUSTED_SIGNER` | Signer not in trust store | Cannot install |
| `CHAIN_BREAK` | Hash mismatch between links | Cannot install |
| `LOG_INCLUSION_FAILED` | Not in transparency logs | Cannot install |
| `THRESHOLD_NOT_MET` | Fewer than k signers | Cannot install |
| `EXPIRED_ATTESTATION` | Attestation too old | Warning (configurable) |

## Offline Verification

For air-gapped environments, attestation bundles can be verified without network access if:
1. Trust store (public keys) is pre-loaded
2. Attestation bundle is included with package
3. Log inclusion proofs are included in bundle

The log inclusion proofs allow verification that attestations were logged, without contacting the logs.

## Appendix: Trust Store Format

```json
{
  "version": 1,
  "updated": "2024-12-07T00:00:00Z",
  "keys": {
    "importers": [
      {
        "id": "cerro-torre:importer-bot",
        "algorithm": "ed25519",
        "publicKey": "...",
        "validFrom": "2024-01-01T00:00:00Z",
        "validUntil": "2025-01-01T00:00:00Z"
      }
    ],
    "builders": [
      {
        "id": "cerro-torre:official-builder-1",
        "algorithm": "ed25519",
        "publicKey": "...",
        "validFrom": "2024-01-01T00:00:00Z"
      }
    ],
    "maintainers": [
      {
        "id": "cerro-torre:maintainer-david",
        "algorithm": "ed25519",
        "publicKey": "...",
        "teams": ["security-team", "web-team"]
      }
    ],
    "releasers": [
      {
        "id": "cerro-torre:keyholder-alice",
        "algorithm": "ed25519",
        "publicKey": "...",
        "thresholdGroup": "release-signers"
      }
    ]
  },
  "thresholds": {
    "release-signers": {
      "k": 3,
      "n": 5,
      "members": ["keyholder-alice", "keyholder-bob", "keyholder-carol", "keyholder-david", "keyholder-eve"]
    }
  }
}
```

## Changelog

### 0.1.0-draft (2024-12-07)
- Initial draft specification
