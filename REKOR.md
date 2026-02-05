# Rekor Integration in stapeln

## What is Rekor?

**Rekor is NOT something we are developing.** It's an existing transparency log from the Sigstore project.

### Official Info
- **Project**: Sigstore Rekor
- **Purpose**: Transparency log for software supply chain
- **Website**: https://sigstore.dev
- **Public Instance**: https://rekor.sigstore.dev
- **GitHub**: https://github.com/sigstore/rekor
- **License**: Apache-2.0

### What Does It Do?

Rekor provides an **immutable, append-only ledger** for software artifacts:

1. **Record signatures** - When you sign a container with Cerro Torre
2. **Timestamp proofs** - Cryptographic proof of when signing occurred
3. **Merkle tree** - Verifiable inclusion proofs (like Certificate Transparency)
4. **Public auditability** - Anyone can verify the log
5. **Non-repudiation** - Signer can't deny they signed it

### Why We Use It in stapeln

**Cerro Torre** (the verified container builder) uses Rekor to:

```
┌─────────────────────────────────────────────────┐
│ 1. Developer builds container with Cerro Torre  │
└─────────────┬───────────────────────────────────┘
              │
              ↓
┌─────────────────────────────────────────────────┐
│ 2. Cerro Torre signs with Ed25519 key          │
└─────────────┬───────────────────────────────────┘
              │
              ↓
┌─────────────────────────────────────────────────┐
│ 3. Signature + metadata uploaded to Rekor       │
│    - Container hash (SHA-256)                   │
│    - Signature (Ed25519)                        │
│    - Public key                                 │
│    - Timestamp                                  │
└─────────────┬───────────────────────────────────┘
              │
              ↓
┌─────────────────────────────────────────────────┐
│ 4. Rekor returns inclusion proof                │
│    - Log entry UUID                             │
│    - Merkle tree proof                          │
│    - Timestamp                                  │
└─────────────┬───────────────────────────────────┘
              │
              ↓
┌─────────────────────────────────────────────────┐
│ 5. stapeln displays in supply chain view        │
│    ✅ Signed by: Jonathan D.A. Jewell           │
│    ✅ Logged to: rekor.sigstore.dev             │
│    ✅ Entry UUID: abc123...                     │
│    ✅ Verified at: 2026-02-05 08:45:23 UTC      │
└─────────────────────────────────────────────────┘
```

## How stapeln Uses Rekor

### 1. Cerro Torre Integration

When you build a container in stapeln:

```bash
# stapeln calls Cerro Torre
ct pack myapp.toml --sign --transparency-log

# Cerro Torre:
# 1. Builds .ctp bundle
# 2. Signs with Ed25519 key
# 3. Uploads to Rekor
# 4. Stores Rekor entry in .ctp metadata
```

### 2. Verification in stapeln

When you deploy a stack, stapeln:

```bash
# 1. Reads .ctp bundle
ct verify myapp.ctp

# 2. Checks Rekor entry
curl https://rekor.sigstore.dev/api/v1/log/entries/{uuid}

# 3. Verifies Merkle inclusion proof
# 4. Displays green checkmark ✅ in UI
```

### 3. Supply Chain Visualization (Page 1)

stapeln shows Rekor data at the bottom of Paragon View:

```
┌───────────────────────────────────────────────────────┐
│ Supply Chain Provenance                               │
├───────────────────────────────────────────────────────┤
│                                                       │
│  Source Code → Build → Sign → Attest → Verify → Run │
│     ✅           ✅      ✅      ✅        ✅      ✅  │
│                                                       │
│  Transparency Log: rekor.sigstore.dev                │
│  Entry UUID: 24296fb24b8ad77a1234567890abcdef        │
│  Last verified: 2026-02-05 08:45:23 UTC              │
│  Merkle tree index: 12345678                         │
│  Attestation: in-toto SLSA Level 3                   │
│  SBOM: 347 packages, 0 CVEs                          │
│                                                       │
│  [View Full Provenance] [Verify Signatures]          │
└───────────────────────────────────────────────────────┘
```

## Rekor API Integration

### REST API Endpoints

stapeln talks to Rekor via REST:

```javascript
// Get log entry by UUID
GET https://rekor.sigstore.dev/api/v1/log/entries/{uuid}

// Search by public key
POST https://rekor.sigstore.dev/api/v1/log/entries/retrieve
{
  "publicKey": "MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQg..."
}

// Get inclusion proof
GET https://rekor.sigstore.dev/api/v1/log/proof?entryUUID={uuid}
```

### GraphQL Integration in stapeln

```graphql
type RekorEntry {
  uuid: ID!
  logIndex: Int!
  body: String!
  integratedTime: DateTime!
  logID: String!
  verification: RekorVerification!
}

type RekorVerification {
  signedEntryTimestamp: String!
  inclusionProof: InclusionProof!
}

type InclusionProof {
  treeSize: Int!
  rootHash: String!
  logIndex: Int!
  hashes: [String!]!
}

query GetRekorEntry($uuid: ID!) {
  rekorEntry(uuid: $uuid) {
    uuid
    logIndex
    integratedTime
    verification {
      inclusionProof {
        rootHash
        hashes
      }
    }
  }
}
```

## Alternative Transparency Logs

While Rekor is the default, stapeln can also use:

1. **Self-hosted Rekor** - Run your own instance
2. **Google Trillian** - Lower-level transparency log
3. **Certificate Transparency logs** - For TLS certs
4. **Custom transparency logs** - Implement Merkle tree API

### Configuration in Settings (Page 3)

```rescript
// Settings.res
type transparencyLog =
  | RekorPublic  // rekor.sigstore.dev
  | RekorSelfHosted(string)  // Custom URL
  | Trillian(string)
  | Custom(string)
```

## Benefits of Rekor Integration

| Benefit | Description |
|---------|-------------|
| **Immutability** | Can't delete or modify log entries |
| **Auditability** | Anyone can verify the log |
| **Non-repudiation** | Signer can't deny they signed |
| **Timestamp proof** | Cryptographic proof of when signing occurred |
| **Supply chain transparency** | Full visibility into artifact provenance |
| **SLSA compliance** | Required for SLSA Level 3+ |

## Comparison: Rekor vs. Other Solutions

| Feature | Rekor | Docker Content Trust | Notary v2 |
|---------|-------|---------------------|-----------|
| **Transparency log** | ✅ | ❌ | ❌ |
| **Open source** | ✅ Apache-2.0 | ✅ Apache-2.0 | ✅ Apache-2.0 |
| **Merkle tree proofs** | ✅ | ❌ | ❌ |
| **Public auditability** | ✅ | ❌ | ❌ |
| **SLSA support** | ✅ | ❌ | ⚠️ Partial |
| **In-toto attestations** | ✅ | ❌ | ⚠️ Partial |
| **Sigstore ecosystem** | ✅ | ❌ | ⚠️ Migrating |

## Cerro Torre + Rekor Workflow

```bash
# 1. Pack container (calls Rekor internally)
ct pack myapp.toml \
  --sign \
  --key ~/.ct/keys/default.key \
  --transparency-log https://rekor.sigstore.dev

# Output:
# ✅ Built: myapp.ctp (12.3 MB)
# ✅ Signed with: Ed25519 key (fingerprint: abc123...)
# ✅ Logged to: rekor.sigstore.dev
# ✅ Entry UUID: 24296fb24b8ad77a1234567890abcdef
# ✅ Log index: 12345678
# ✅ Merkle root: 9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08

# 2. Verify (checks Rekor)
ct verify myapp.ctp

# Output:
# ✅ Signature valid
# ✅ Rekor entry verified
# ✅ Merkle inclusion proof valid
# ✅ Timestamp: 2026-02-05 08:45:23 UTC
# ✅ SBOM present (347 packages)
# ✅ No CVEs detected

# 3. Deploy in stapeln
# stapeln reads Rekor metadata from .ctp and displays in UI
```

## Resources

- **Sigstore Homepage**: https://sigstore.dev
- **Rekor GitHub**: https://github.com/sigstore/rekor
- **Rekor API Docs**: https://www.sigstore.dev/rekor
- **SLSA Framework**: https://slsa.dev
- **In-toto Attestations**: https://in-toto.io

## Summary

- ❌ **Rekor is NOT something we're developing**
- ✅ **Rekor is an existing Sigstore project**
- ✅ **stapeln integrates WITH Rekor** via Cerro Torre
- ✅ **Public instance at rekor.sigstore.dev**
- ✅ **Provides transparency log for supply chain**
- ✅ **Required for SLSA Level 3+ compliance**
