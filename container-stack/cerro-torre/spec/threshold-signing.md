# Cerro Torre Threshold Signing with FROST

**Version**: 0.1.0-draft  
**Status**: Draft Specification  
**SPDX-License-Identifier**: PMPL-1.0-or-later

## Overview

Cerro Torre uses **FROST (Flexible Round-Optimised Schnorr Threshold signatures)** for release signing. No single keyholder can sign a release — a threshold of k-of-n keyholders must cooperate.

This prevents:
- Single point of compromise (stolen key)
- Coercion of individual keyholders
- Rogue maintainer releases
- Key loss causing project death

## Why FROST?

### Comparison with Alternatives

| Approach | Single Point of Failure | Key Recovery | Signature Size | Verification Cost |
|----------|------------------------|--------------|----------------|-------------------|
| Single key | Yes | No | Small | Fast |
| Multi-sig (n signatures) | No | Partial | Large (n×) | Slow (n×) |
| Shamir Secret Sharing | Yes (at signing time) | Yes | Small | Fast |
| **FROST** | No | Yes | Small | Fast |

FROST produces a single standard Ed25519/Schnorr signature that verifiers cannot distinguish from a single-signer signature. The threshold mechanism is invisible to downstream — they just verify one signature against one public key.

### FROST Properties

- **Threshold**: Any k of n keyholders can sign (e.g., 3-of-5)
- **No Trusted Dealer**: Key generation is distributed; no single party sees the full private key
- **Robust**: Misbehaving signers are detected and signing can proceed without them
- **Standard Output**: Produces standard Schnorr signatures (Ed25519-compatible)
- **Proactive Security**: Keys can be refreshed without changing the public key

## Key Ceremony

### Distributed Key Generation (DKG)

Initial key setup requires all n keyholders to participate in a distributed key generation ceremony.

```
┌─────────────────────────────────────────────────────────────────┐
│                DISTRIBUTED KEY GENERATION                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Round 1: Each participant generates and broadcasts commitment  │
│                                                                  │
│   ┌───────┐  ┌───────┐  ┌───────┐  ┌───────┐  ┌───────┐        │
│   │ Alice │  │  Bob  │  │ Carol │  │ David │  │  Eve  │        │
│   └───┬───┘  └───┬───┘  └───┬───┘  └───┬───┘  └───┬───┘        │
│       │          │          │          │          │              │
│       ▼          ▼          ▼          ▼          ▼              │
│   Commit_A   Commit_B   Commit_C   Commit_D   Commit_E          │
│       │          │          │          │          │              │
│       └──────────┴──────────┴──────────┴──────────┘              │
│                          │                                       │
│                          ▼                                       │
│                    Broadcast All                                 │
│                                                                  │
│   Round 2: Each participant reveals and verifies                 │
│                                                                  │
│       ┌──────────────────────────────────────────┐              │
│       │         Verify all commitments           │              │
│       │         Compute group public key         │              │
│       │         Compute individual shares        │              │
│       └──────────────────────────────────────────┘              │
│                          │                                       │
│                          ▼                                       │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  Group Public Key: PK (published)                        │   │
│   │  Alice's Share: sk_A (secret to Alice)                   │   │
│   │  Bob's Share: sk_B (secret to Bob)                       │   │
│   │  ...                                                     │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Ceremony Requirements

| Requirement | Description |
|-------------|-------------|
| Participants | All n keyholders must participate |
| Communication | Authenticated, encrypted channels between all pairs |
| Verification | Each participant verifies all others' commitments |
| Output | Group public key (public), individual shares (private to each) |
| Backup | Each keyholder backs up their share securely |

### Ceremony Protocol

```
DKG_CEREMONY(participants, threshold_k, total_n):
  
  # Round 1: Commitment
  for each participant P:
    P generates random polynomial f_P of degree k-1
    P computes commitment C_P = Commit(f_P)
    P broadcasts C_P to all participants
  
  # All participants collect all commitments
  commitments = collect_all(C_1, C_2, ..., C_n)
  
  # Round 2: Share distribution
  for each participant P:
    for each other participant Q:
      P computes share s_PQ = f_P(Q.index)
      P sends s_PQ privately to Q
  
  # Verification
  for each participant P:
    shares_received = [s_1P, s_2P, ..., s_nP]
    for each share s_QP:
      if not verify_share(s_QP, commitments[Q]):
        ABORT("Invalid share from Q")
    
    # Compute own secret share
    P.secret_share = sum(shares_received)
    
    # Compute group public key
    P.group_public_key = sum(commitments[i].public_point for i in 1..n)
  
  # All participants should have same group_public_key
  verify_consensus(all participants agree on group_public_key)
  
  return group_public_key
```

## Signing Protocol

When k keyholders agree to sign a release:

```
┌─────────────────────────────────────────────────────────────────┐
│                    FROST SIGNING PROTOCOL                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Message to sign: M = hash(release_attestation)                 │
│   Signers: Alice, Bob, Carol (3-of-5 threshold)                 │
│                                                                  │
│   Round 1: Nonce Commitment                                      │
│                                                                  │
│   Alice              Bob                Carol                    │
│     │                 │                   │                      │
│     ▼                 ▼                   ▼                      │
│   (D_A, E_A)       (D_B, E_B)         (D_C, E_C)                │
│     │                 │                   │                      │
│     └─────────────────┼───────────────────┘                      │
│                       │                                          │
│                       ▼                                          │
│               Coordinator collects                               │
│               Broadcasts all (D, E) pairs                        │
│                                                                  │
│   Round 2: Signature Share                                       │
│                                                                  │
│   Alice              Bob                Carol                    │
│     │                 │                   │                      │
│     ▼                 ▼                   ▼                      │
│   z_A = sign(M)    z_B = sign(M)      z_C = sign(M)             │
│     │                 │                   │                      │
│     └─────────────────┼───────────────────┘                      │
│                       │                                          │
│                       ▼                                          │
│              Coordinator aggregates                              │
│              σ = aggregate(z_A, z_B, z_C)                       │
│                                                                  │
│   Output: Standard Ed25519 signature σ                           │
│           Verifiable with group public key PK                    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Signing Protocol Detail

```
FROST_SIGN(message, signers, threshold_k):
  
  # Coordinator role (can be any signer or separate service)
  coordinator = select_coordinator()
  
  # Round 1: Commitment
  for each signer S in signers:
    (d_S, e_S) = S.generate_nonce_pair()
    (D_S, E_S) = (d_S * G, e_S * G)  # Commitments
    S sends (D_S, E_S) to coordinator
  
  coordinator broadcasts all (D_i, E_i) to all signers
  
  # Binding factor computation (prevents certain attacks)
  rho = hash("FROST-binding", group_public_key, message, 
             encode_all(D_1, E_1, ..., D_k, E_k))
  
  # Group commitment
  R = sum(D_i + rho_i * E_i for all signers)
  
  # Challenge
  c = hash("FROST-challenge", R, group_public_key, message)
  
  # Round 2: Signature shares
  for each signer S in signers:
    lambda_S = lagrange_coefficient(S.index, signer_indices)
    z_S = d_S + (e_S * rho_S) + (lambda_S * S.secret_share * c)
    S sends z_S to coordinator
  
  # Aggregation
  z = sum(z_i for all signers)
  signature = (R, z)
  
  # Verification (sanity check)
  if not verify_schnorr(signature, message, group_public_key):
    # Someone misbehaved - identify and retry without them
    identify_misbehaving_signer()
    ABORT or RETRY
  
  return signature
```

## Keyholder Management

### Keyholder Selection Criteria

Keyholders should be:
- Geographically distributed (different countries/regions)
- Organisationally independent (not all from same employer)
- Reachable within reasonable time (for signing ceremonies)
- Technically competent (understand security practices)
- Committed to project values (Palimpsest Covenant adopters)

### Initial Cerro Torre Configuration

```
Threshold: 3-of-5

Keyholders:
  1. Project Founder (UK)
  2. [TBD - Americas region]
  3. [TBD - Europe region]
  4. [TBD - Asia-Pacific region]
  5. [TBD - Academic/NGO]

Rationale:
  - 3-of-5 means 2 keyholders can be unavailable/compromised
  - Geographic distribution prevents jurisdiction attacks
  - Organisational diversity prevents employer pressure
```

### Adding a Keyholder

Requires existing k-of-n keyholders to participate in a key refresh ceremony:

```
ADD_KEYHOLDER(new_participant):
  # Existing threshold must approve
  approval = collect_signatures(k existing keyholders, 
                                "approve adding {new_participant}")
  
  # Run share redistribution
  # This creates share for new participant without changing group key
  new_shares = redistribute_shares(
    existing_shares,
    new_n = n + 1,
    new_participant
  )
  
  # Update keyholder registry
  registry.add(new_participant, new_participant.public_verification_key)
  
  # Publish updated configuration
  publish_keyholder_change(old_config, new_config, approval)
```

### Removing a Keyholder

```
REMOVE_KEYHOLDER(departed_participant):
  # Existing threshold must approve
  approval = collect_signatures(k existing keyholders excluding departed,
                                "approve removing {departed_participant}")
  
  # Run key refresh (invalidates departed participant's share)
  refresh_shares(
    remaining_participants,
    invalidate = departed_participant.share
  )
  
  # Update keyholder registry
  registry.remove(departed_participant)
  
  # Publish updated configuration
  publish_keyholder_change(old_config, new_config, approval)
```

### Key Refresh (Proactive Security)

Even without keyholder changes, shares should be refreshed periodically:

```
REFRESH_SHARES():
  # Scheduled: every 6 months
  # Or triggered by: suspected compromise, keyholder departure
  
  # All current keyholders participate
  for each keyholder K:
    K generates refresh polynomial r_K of degree k-1 with r_K(0) = 0
    K distributes r_K(j) to each keyholder j
  
  # Each keyholder updates their share
  for each keyholder K:
    K.new_share = K.old_share + sum(r_j(K.index) for all j)
  
  # Group public key unchanged!
  # Old shares now useless (attacker would need old AND new)
```

## Operational Procedures

### Release Signing Workflow

```
1. Release coordinator prepares release attestation bundle
   - All package attestations
   - SBOM
   - Release notes
   - Computed hash: release_hash

2. Coordinator posts signing request to keyholders
   - Message: release_hash
   - Deadline: 48 hours
   - Required: k approvals

3. Keyholders review release
   - Verify attestation chain
   - Check for anomalies
   - Signal approval or concerns

4. If k keyholders approve:
   - Schedule signing ceremony
   - All approving keyholders join (video call + signing client)
   
5. Execute FROST signing protocol
   - Coordinator role rotates
   - Signature produced
   
6. Coordinator publishes:
   - Release with signature
   - Signing ceremony record (who participated, when)
```

### Emergency Procedures

**Keyholder Compromise**:
1. Alert all keyholders immediately
2. Do NOT sign anything until resolved
3. Execute key refresh excluding compromised keyholder
4. Investigate scope of compromise
5. If releases were signed during compromise window, consider revocation

**Keyholder Unavailability**:
1. If < k keyholders available, signing is blocked (by design)
2. Contact unavailable keyholders through backup channels
3. If extended unavailability, initiate keyholder replacement process

**Key Loss**:
1. Keyholder reports lost key material
2. Key refresh (lost share becomes useless)
3. Keyholder re-enrols with new share from refresh

## Implementation

### FROST Libraries

- **Rust**: `frost-ed25519` (ZCash Foundation)
- **Go**: `github.com/bytemare/frost`
- **Reference**: IETF draft-irtf-cfrg-frost

### Cerro Torre Integration

The `cerro` tool includes signing subcommands:

```bash
# Keyholder: participate in DKG
cerro keys init --ceremony-id abc123 --participant-id alice

# Keyholder: participate in signing
cerro sign participate --request-id xyz789

# Coordinator: request signatures
cerro sign request --message release.bundle --keyholders alice,bob,carol

# Anyone: verify signature
cerro verify --signature release.sig --message release.bundle --pubkey cerro-torre.pub
```

### Keyholder Client Security

Keyholders should:
- Store shares on hardware security modules (HSM) or air-gapped machines
- Use dedicated signing devices (not daily-use laptops)
- Maintain encrypted backups of shares in separate physical locations
- Use hardware 2FA for signing ceremony authentication

## Transparency

All signing events are logged:

```json
{
  "type": "signing_ceremony",
  "timestamp": "2024-12-07T12:00:00Z",
  "request_id": "release-2024-12-07",
  "message_hash": "sha256:abc123...",
  "threshold": "3-of-5",
  "participants": [
    {"id": "alice", "approved": true, "signed": true},
    {"id": "bob", "approved": true, "signed": true},
    {"id": "carol", "approved": true, "signed": true},
    {"id": "david", "approved": false, "reason": "unavailable"},
    {"id": "eve", "approved": true, "signed": false, "reason": "technical issue"}
  ],
  "coordinator": "alice",
  "outcome": "success",
  "signature": "ed25519:xyz..."
}
```

This record is published to the transparency log alongside the signed release.

## Changelog

### 0.1.0-draft (2024-12-07)
- Initial draft specification
