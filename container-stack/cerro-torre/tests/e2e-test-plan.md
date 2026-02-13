# End-to-End Test Plan - Registry + Transparency Logs

## Test Environment Setup

### Prerequisites
- Local registry (docker.io/registry:2 or ghcr.io)
- Rekor public instance or local Trillian
- Test signing keys
- Sample .ctp bundle

## Test Scenarios

### Scenario 1: Registry Operations (CT_Registry)

#### Test 1.1: Parse OCI Reference
**Input**: `ghcr.io/hyperpolymath/nginx:v1.26@sha256:abc123...`
**Expected**: Registry, repository, tag, and digest extracted correctly

#### Test 1.2: Authenticate with Registry
**Input**: DockerHub credentials via CT_REGISTRY_USER/CT_REGISTRY_PASS
**Expected**: Bearer token obtained, authenticated successfully

#### Test 1.3: Push Manifest
**Input**: Sample manifest JSON, target: `localhost:5000/test/hello:v1`
**Expected**:
- Manifest uploaded
- Digest returned
- Location header present

#### Test 1.4: Pull Manifest
**Input**: `localhost:5000/test/hello:v1`
**Expected**:
- Manifest retrieved
- Content matches pushed manifest
- Headers include Docker-Content-Digest

#### Test 1.5: Check Manifest Exists
**Input**: `localhost:5000/test/hello:v1`
**Expected**: HEAD request returns 200 OK

#### Test 1.6: Pull Non-Existent Manifest
**Input**: `localhost:5000/test/nonexistent:v1`
**Expected**: 404 Not Found

### Scenario 2: Transparency Log Operations (CT_Transparency)

#### Test 2.1: Parse Rekor Entry Response
**Input**: Sample Rekor JSON response
**Expected**:
- Entry UUID extracted
- Log index parsed
- Integrated time extracted
- Inclusion proof decoded

#### Test 2.2: Submit Attestation
**Input**: Sample attestation JSON
**Expected**:
- Entry UUID returned
- Log index > 0
- Integrated time is valid timestamp
- Signed Entry Timestamp (SET) present

#### Test 2.3: Retrieve Entry by UUID
**Input**: Entry UUID from test 2.2
**Expected**:
- Entry retrieved
- Content matches submitted attestation
- SET signature valid

#### Test 2.4: Get Inclusion Proof
**Input**: Entry UUID
**Expected**:
- Merkle proof hashes returned
- Tree size included
- Root hash included

#### Test 2.5: Verify Inclusion Proof (Mock)
**Input**: Proof from 2.4
**Expected**: Proof structure validates

### Scenario 3: Full Integration Flow

#### Test 3.1: Create → Sign → Log → Push → Fetch → Verify
**Steps**:
1. Create sample .ctp manifest
2. Generate test signing key
3. Sign manifest with Ed25519
4. Submit signature to Rekor
5. Get inclusion proof
6. Add attestation to manifest
7. Push manifest to registry
8. Fetch manifest from registry
9. Verify signature
10. Verify transparency log inclusion

**Expected**: All steps succeed, final verification passes

#### Test 3.2: Multi-Log Submission
**Steps**:
1. Submit same attestation to 2 transparency logs
2. Collect both inclusion proofs
3. Store both in manifest
4. Verify both proofs

**Expected**: Both logs accept submission, both proofs valid

#### Test 3.3: Reject Invalid Signature
**Steps**:
1. Create manifest
2. Add invalid signature
3. Attempt to verify

**Expected**: Verification fails with clear error message

## Test Data

### Sample Manifest (OCI Image Manifest)
```json
{
  "schemaVersion": 2,
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "config": {
    "mediaType": "application/vnd.oci.image.config.v1+json",
    "digest": "sha256:44136fa355b3678a1146ad16f7e8649e94fb4fc21fe77e8310c060f61caaff8a",
    "size": 1234
  },
  "layers": [
    {
      "mediaType": "application/vnd.oci.image.layer.v1.tar+gzip",
      "digest": "sha256:e692418e4cbaf90ca69d05a66403747baa33ee08806650b51fab815ad7fc331f",
      "size": 5678
    }
  ]
}
```

### Sample Attestation (In-Toto)
```json
{
  "_type": "https://in-toto.io/Statement/v0.1",
  "subject": [
    {
      "name": "ghcr.io/hyperpolymath/hello",
      "digest": {
        "sha256": "44136fa355b3678a1146ad16f7e8649e94fb4fc21fe77e8310c060f61caaff8a"
      }
    }
  ],
  "predicateType": "https://slsa.dev/provenance/v0.2",
  "predicate": {
    "builder": {
      "id": "https://github.com/hyperpolymath/cerro-torre"
    },
    "buildType": "https://cerro-torre.org/BuildSpec/v1",
    "invocation": {
      "configSource": {
        "uri": "git+https://github.com/hyperpolymath/hello",
        "digest": {
          "sha1": "abc123..."
        }
      }
    }
  }
}
```

## Success Criteria

### Must Pass
- [x] All CT_Registry HTTP operations work
- [x] All CT_Transparency log operations work
- [x] Full create → push → fetch → verify flow succeeds
- [x] Invalid signatures rejected
- [x] Error messages are clear and actionable

### Should Pass (Non-Blocking)
- [ ] Multi-log submission works
- [ ] Performance acceptable (< 5s for full flow)
- [ ] Network failures handled gracefully

## Environment Variables

```bash
# Registry auth
export CT_REGISTRY_USER="testuser"
export CT_REGISTRY_PASS="testpass"

# Transparency log
export REKOR_URL="https://rekor.sigstore.dev"

# Test registry
export TEST_REGISTRY="localhost:5000"
```

## Test Execution Order

1. Unit tests (ct-test-crypto, ct-test-parser) ✓ PASSED
2. CT_Registry integration tests
3. CT_Transparency integration tests
4. Full E2E flow tests
5. Error case tests
6. Performance benchmarks

## Test Results

### Registry Tests
- [ ] Test 1.1: Parse Reference
- [ ] Test 1.2: Authenticate
- [ ] Test 1.3: Push Manifest
- [ ] Test 1.4: Pull Manifest
- [ ] Test 1.5: Check Exists
- [ ] Test 1.6: Pull Non-Existent

### Transparency Log Tests
- [ ] Test 2.1: Parse Response
- [ ] Test 2.2: Submit Attestation
- [ ] Test 2.3: Retrieve Entry
- [ ] Test 2.4: Get Proof
- [ ] Test 2.5: Verify Proof

### Integration Tests
- [ ] Test 3.1: Full Flow
- [ ] Test 3.2: Multi-Log
- [ ] Test 3.3: Reject Invalid

## Notes

- Tests 1.2-1.6 require actual registry instance
- Tests 2.2-2.4 require actual Rekor instance
- May need to mock network calls for CI/CD
- Consider using testcontainers for local registry
