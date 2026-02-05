# stapeln Formal Verification Specification

## Overview

stapeln uses Idris2 for formal verification of container stack properties. This ensures correctness guarantees that cannot be achieved with runtime validation alone.

## Verification Layers

### Layer 1: Topology Verification
**File:** `src/abi/Topology.idr`

Proves:
- All nodes have unique IDs
- All connections reference valid nodes
- No circular dependencies
- Port numbers are in valid range (1-65535)
- No port conflicts

```idris2
||| Proof that all node IDs are unique
uniqueNodeIds : (nodes : List Node) -> Dec (AllUnique (map (.id) nodes))

||| Proof that all connections are valid
validConnections : (nodes : List Node) -> (conns : List Connection) ->
                   Dec (All (\c => c.from `elem` nodeIds && c.to `elem` nodeIds) conns)

||| Proof that topology is acyclic
acyclicTopology : (conns : List Connection) -> Dec (IsAcyclic conns)
```

### Layer 2: Resource Verification
**File:** `src/abi/Resources.idr`

Proves:
- CPU limits are within bounds (0.1 - 4.0)
- Memory limits are valid (minimum 128M)
- Total resources don't exceed system capacity
- No resource starvation

```idris2
||| Proof that CPU allocation is valid
validCpuAllocation : (cpu : Double) -> So (cpu >= 0.1 && cpu <= 4.0)

||| Proof that memory allocation is valid
validMemoryAllocation : (mem : Nat) -> So (mem >= 134217728)  -- 128M in bytes

||| Proof that total resource usage is feasible
feasibleResourceAllocation : (nodes : List Node) -> (systemCpu : Double) -> (systemMem : Nat) ->
                              Dec (sum (map (.resources.cpu) nodes) <= systemCpu)
```

### Layer 3: Security Verification
**File:** `src/abi/Security.idr`

Proves:
- All public-facing containers have firewalls enabled
- Encrypted connections use valid protocols
- No containers run as privileged
- Image signatures are verifiable

```idris2
||| Proof that all gateway nodes have firewalls
allGatewaysProtected : (nodes : List Node) ->
                       Dec (All (\n => n.type == Gateway `implies` n.firewall == True) nodes)

||| Proof that sensitive connections are encrypted
sensitiveConnectionsEncrypted : (conns : List Connection) ->
                                 Dec (All (\c => isSensitive c `implies` c.encrypted) conns)

||| Proof that no containers have dangerous privileges
noPrivilegedContainers : (nodes : List Node) ->
                         Dec (All (\n => n.privileged == False) nodes)
```

### Layer 4: Image Verification
**File:** `src/abi/Images.idr`

Proves:
- All images have valid names
- Image sizes match expected values
- Lago Grey images meet size constraints
- All images are from approved registries

```idris2
||| Proof that image name is valid
validImageName : (img : String) -> Dec (ValidImageNameFormat img)

||| Proof that Lago Grey image is under size limit
lagoGreyImageSize : (formations : List Formation) -> (baseSize : Nat) ->
                    So (baseSize + sum (map (.size) formations) <= 17_500_000)

||| Proof that all images are from approved registries
approvedImageSources : (nodes : List Node) -> (approved : List String) ->
                       Dec (All (\n => any (\reg => reg `isPrefixOf` n.baseImage) approved) nodes)
```

## Integration with Mustfile

The Idris2 verification layer generates evidence that can be checked by the Mustfile:

```yaml
# Mustfile
verification:
  - name: topology-verified
    description: "Topology structure verified by Idris2"
    critical: true
    proof: topology.proof

  - name: resources-verified
    description: "Resource allocations verified by Idris2"
    critical: true
    proof: resources.proof

  - name: security-verified
    description: "Security properties verified by Idris2"
    critical: true
    proof: security.proof
```

## Build Process

1. **Design Phase (UI)**: User creates topology in stapeln-ui.html
2. **Export Phase**: Topology exported to JSON
3. **Verification Phase (Idris2)**:
   ```bash
   idris2 --client ':l Verification.idr'
   idris2 --client ':exec verifyStack "stack.json"'
   ```
4. **Proof Generation**: Idris2 generates proof certificates
5. **Deployment Phase**: Proof certificates checked before deployment

## Proof Certificates

Format: JSON with signature

```json
{
  "version": "1.0",
  "timestamp": "2026-02-05T10:00:00Z",
  "stack_id": "abc123",
  "proofs": {
    "topology": {
      "status": "verified",
      "properties": ["unique_ids", "valid_connections", "acyclic"],
      "signature": "ed448+dilithium5:AAABBBCCC..."
    },
    "resources": {
      "status": "verified",
      "properties": ["valid_cpu", "valid_memory", "feasible"],
      "signature": "ed448+dilithium5:DDDEEEFFF..."
    },
    "security": {
      "status": "verified",
      "properties": ["firewalls", "encryption", "no_privileged"],
      "signature": "ed448+dilithium5:GGGHHHIII..."
    }
  },
  "signature": "ed448+dilithium5:JJJKKKLLL...",
  "rekor_uuid": "xyz789"
}
```

## Current Status

- ✅ ABI foundation (Types.idr, Layout.idr, Foreign.idr)
- ✅ Verification specification (this document)
- ⏳ Topology verification implementation
- ⏳ Resource verification implementation
- ⏳ Security verification implementation
- ⏳ Image verification implementation
- ⏳ Integration with stapeln-ui
- ⏳ Build process automation

## Next Steps

1. Implement `src/abi/Topology.idr` with formal proofs
2. Implement `src/abi/Resources.idr` with formal proofs
3. Implement `src/abi/Security.idr` with formal proofs
4. Implement `src/abi/Images.idr` with formal proofs
5. Create verification CLI tool: `stapeln-verify`
6. Integrate with deployment workflow
7. Add proof certificate generation
8. Document verification guarantees

## Benefits

✅ **Compile-time guarantees**: Properties verified before deployment
✅ **Formal proofs**: Mathematical certainty, not just testing
✅ **Security assurance**: Cryptographic proofs of security properties
✅ **Audit trail**: Proof certificates logged to Rekor
✅ **Zero runtime overhead**: All verification done at build time
