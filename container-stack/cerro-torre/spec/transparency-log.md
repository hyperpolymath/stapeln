# Cerro Torre Federated Transparency Log Protocol

**Version**: 0.1.0-draft  
**Status**: Draft Specification  
**SPDX-License-Identifier**: PMPL-1.0-or-later

## Overview

Cerro Torre operates a **federated transparency log** for package attestations. Unlike centralised systems (Sigstore's Rekor, Certificate Transparency), no single operator controls the log infrastructure. This prevents:

- Single points of compromise
- Coerced log manipulation by state actors
- Corporate capture of trust infrastructure
- Service discontinuation risk

## Design Principles

1. **No Single Point of Trust**: Clients require attestations in multiple independent logs
2. **Geographic Distribution**: Log operators span jurisdictions
3. **Organisational Diversity**: Operators include cooperatives, universities, NGOs — not just corporations
4. **Append-Only**: Logs are cryptographically append-only (Merkle trees)
5. **Publicly Auditable**: Anyone can verify log consistency
6. **Permissionless Mirroring**: Anyone can mirror the complete log

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                    FEDERATED LOG NETWORK                             │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│   ┌─────────────┐    ┌─────────────┐    ┌─────────────┐             │
│   │   LOG 1     │    │   LOG 2     │    │   LOG 3     │    ...      │
│   │ (Europe)    │    │ (Americas)  │    │ (Asia-Pac)  │             │
│   │ Operator: A │    │ Operator: B │    │ Operator: C │             │
│   └──────┬──────┘    └──────┬──────┘    └──────┬──────┘             │
│          │                  │                  │                     │
│          └────────────┬─────┴──────────────────┘                     │
│                       │                                              │
│                       ▼                                              │
│          ┌────────────────────────┐                                  │
│          │    WITNESS NETWORK     │                                  │
│          │  (Consistency Checks)  │                                  │
│          └────────────────────────┘                                  │
│                       │                                              │
│                       ▼                                              │
│          ┌────────────────────────┐                                  │
│          │       CLIENTS          │                                  │
│          │  (Verify 2-of-3 logs)  │                                  │
│          └────────────────────────┘                                  │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

## Log Structure

Each log is a Merkle tree of attestation entries.

### Entry Format

```json
{
  "logId": "cerro-torre-log-eu-1",
  "logIndex": 12345,
  "integratedTime": 1733567400,
  "body": {
    "apiVersion": "0.1",
    "kind": "attestation",
    "spec": {
      "content": "<base64-encoded attestation>",
      "contentType": "application/vnd.in-toto+json"
    }
  },
  "inclusionProof": {
    "logIndex": 12345,
    "rootHash": "abc123...",
    "treeSize": 100000,
    "hashes": ["def456...", "ghi789...", "..."]
  }
}
```

### Merkle Tree Construction

```
                    Root Hash
                   /         \
                  /           \
             Hash(0-3)      Hash(4-7)
             /      \       /      \
        Hash(0-1) Hash(2-3) Hash(4-5) Hash(6-7)
         /    \    /    \    /    \    /    \
        E0   E1   E2   E3   E4   E5   E6   E7
```

Each entry `E[i]` contains an attestation. The tree structure allows:
- O(log n) inclusion proofs
- O(log n) consistency proofs between tree states
- Efficient append operations

## Federation Protocol

### Log Registration

New logs join the federation by:

1. Operating infrastructure meeting minimum requirements
2. Publishing a signed log identity document
3. Demonstrating 30 days of stable operation
4. Receiving approval from existing log operators (majority vote)

### Log Identity Document

```json
{
  "logId": "cerro-torre-log-eu-1",
  "operator": {
    "name": "Example University",
    "jurisdiction": "DE",
    "type": "academic",
    "contact": "security@example.edu"
  },
  "publicKey": {
    "algorithm": "ed25519",
    "key": "..."
  },
  "endpoints": {
    "submit": "https://log.example.edu/cerro-torre/v1/submit",
    "query": "https://log.example.edu/cerro-torre/v1/query",
    "proof": "https://log.example.edu/cerro-torre/v1/proof"
  },
  "mirrors": [
    "https://mirror1.example.org/cerro-torre-log-eu-1/",
    "https://mirror2.example.net/cerro-torre-log-eu-1/"
  ],
  "signature": "..."
}
```

### Operator Requirements

| Requirement | Minimum |
|-------------|---------|
| Uptime SLA | 99.5% |
| Response time | < 2 seconds |
| Storage commitment | 5 years minimum |
| Geographic redundancy | 2+ locations |
| Incident response | 24-hour acknowledgment |
| Insurance/bonding | Optional but encouraged |

### Operator Diversity Requirements

The federation must maintain:
- At least 3 independent operators
- No single operator controlling > 40% of logs
- Operators in at least 2 distinct legal jurisdictions
- At least 1 non-corporate operator (academic, cooperative, NGO)

## Submission Protocol

### Submitting an Attestation

```
POST /v1/submit
Content-Type: application/vnd.in-toto+json

{
  "_type": "https://in-toto.io/Statement/v1",
  "subject": [...],
  "predicateType": "...",
  "predicate": {...}
}
```

### Response

```json
{
  "logId": "cerro-torre-log-eu-1",
  "logIndex": 12345,
  "integratedTime": 1733567400,
  "inclusionPromise": {
    "signedEntryTimestamp": "..."
  }
}
```

### Multi-Log Submission

Clients should submit to **all** federation logs. The `cerro` tool does this automatically:

```bash
cerro submit-attestation attestation.json
# Submits to all 5 logs, waits for 3 confirmations
```

## Query Protocol

### Get Entry by Index

```
GET /v1/entry/{logIndex}
```

### Get Entry by Hash

```
GET /v1/entry?hash={sha256:...}
```

### Get Inclusion Proof

```
GET /v1/proof/inclusion?logIndex={index}&treeSize={size}
```

Response:
```json
{
  "logIndex": 12345,
  "treeSize": 100000,
  "rootHash": "abc123...",
  "hashes": ["def456...", "ghi789...", "..."],
  "logSignature": "..."
}
```

### Get Consistency Proof

```
GET /v1/proof/consistency?from={oldSize}&to={newSize}
```

Response:
```json
{
  "fromSize": 90000,
  "toSize": 100000,
  "fromRoot": "old123...",
  "toRoot": "new456...",
  "hashes": ["..."],
  "logSignature": "..."
}
```

## Verification

### Client Verification Requirements

Cerro Torre clients enforce:

| Policy | Requirement |
|--------|-------------|
| Minimum logs | 2 independent logs must contain attestation |
| Freshness | Inclusion proof < 24 hours old |
| Consistency | No conflicting entries across logs |

### Verification Algorithm

```
VERIFY_LOG_INCLUSION(attestation, bundle):
  confirmed_logs = []
  
  for each log_entry in bundle.logEntries:
    log = get_log_identity(log_entry.logId)
    
    # Verify inclusion proof
    if not verify_merkle_proof(
      log_entry.inclusionProof,
      hash(attestation),
      log.currentRoot
    ):
      continue
    
    # Verify log signature on root
    if not verify_signature(
      log_entry.inclusionProof.rootHash,
      log_entry.logSignature,
      log.publicKey
    ):
      continue
    
    confirmed_logs.append(log_entry.logId)
  
  # Require threshold
  if len(confirmed_logs) < 2:
    return FAIL("Insufficient log coverage")
  
  # Check for conflicts
  if has_conflicting_entries(attestation, confirmed_logs):
    return FAIL("Conflicting log entries detected")
  
  return SUCCESS(confirmed_logs)
```

## Witness Network

Independent witnesses monitor log consistency:

### Witness Responsibilities

1. **Consistency Monitoring**: Regularly fetch signed tree heads from all logs
2. **Split Detection**: Alert if logs present different views to different clients
3. **Public Reporting**: Publish witnessed tree heads to public archive

### Witness Protocol

```
Every 10 minutes:
  for each log in federation:
    head = fetch_signed_tree_head(log)
    
    if head.treeSize > last_seen[log].treeSize:
      proof = fetch_consistency_proof(
        log,
        last_seen[log].treeSize,
        head.treeSize
      )
      
      if not verify_consistency(proof, last_seen[log].rootHash, head.rootHash):
        ALERT("Log consistency violation", log)
      
      last_seen[log] = head
    
    publish_witnessed_head(log, head, witness_signature)
```

### Witness Diversity

Like logs, witnesses should be diverse:
- Operated by different organisations than log operators
- Geographically distributed
- Include community-run witnesses

## Gossip Protocol

Clients and witnesses share observed tree heads:

```json
{
  "type": "tree_head_gossip",
  "logId": "cerro-torre-log-eu-1",
  "treeSize": 100000,
  "rootHash": "abc123...",
  "timestamp": 1733567400,
  "observedBy": "client-xyz",
  "observedAt": 1733567450
}
```

If a client observes a tree head that conflicts with gossip from other clients, this indicates a potential split-view attack and should be reported.

## Offline Operation

For air-gapped environments:

### Offline Bundle Format

```json
{
  "attestations": [...],
  "logEntries": [...],
  "witnessedHeads": [
    {
      "logId": "cerro-torre-log-eu-1",
      "head": {...},
      "witnesses": [
        {"witnessId": "witness-1", "signature": "..."},
        {"witnessId": "witness-2", "signature": "..."}
      ]
    }
  ],
  "trustStore": {
    "logs": [...],
    "witnesses": [...]
  },
  "generatedAt": "2024-12-07T00:00:00Z"
}
```

Offline verification trusts witnessed heads rather than live queries.

## Incident Response

### Log Compromise

If a log operator is compromised:

1. Other operators issue signed alert
2. Clients automatically increase threshold (e.g., 3-of-4 instead of 2-of-3)
3. Compromised log is removed from federation
4. Attestations only in compromised log are flagged for re-attestation

### Split-View Attack Detection

If witnesses detect split views:

1. All witnesses publish conflicting heads
2. Clients refuse to verify until resolved
3. Federation investigates and potentially removes malicious operator

### Key Rotation

Log operators should rotate keys annually:

1. New key published 30 days before activation
2. Transition period: both keys valid
3. Old key revoked after transition

## API Reference

### Endpoints

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/v1/submit` | POST | Submit attestation |
| `/v1/entry/{index}` | GET | Get entry by index |
| `/v1/entry?hash={hash}` | GET | Get entry by hash |
| `/v1/proof/inclusion` | GET | Get inclusion proof |
| `/v1/proof/consistency` | GET | Get consistency proof |
| `/v1/head` | GET | Get signed tree head |
| `/v1/federation` | GET | Get federation membership |

### Error Codes

| Code | Meaning |
|------|---------|
| 400 | Invalid request |
| 404 | Entry not found |
| 409 | Duplicate entry |
| 429 | Rate limited |
| 500 | Server error |
| 503 | Log temporarily unavailable |

## Initial Federation

Cerro Torre will launch with:

1. **Log 1**: Operated by Cerro Torre Cooperative (bootstrapping)
2. **Log 2**: Partner organisation (TBD)
3. **Log 3**: Academic institution (TBD)

Until 3 independent logs exist, the federation operates in **bootstrap mode** with reduced threshold requirements (1-of-N).

## Changelog

### 0.1.0-draft (2024-12-07)
- Initial draft specification
