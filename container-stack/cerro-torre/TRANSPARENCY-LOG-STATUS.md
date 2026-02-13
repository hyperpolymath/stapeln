# Transparency Log Integration - Implementation Status

## Completed Features ✅

### Rekor API Client (CT_Transparency module)

**Upload Operations:**
```ada
function Upload_Signature
  (Client     : Log_Client;
   Signature  : String;       -- Base64-encoded signature
   Artifact   : String;       -- Artifact content or path
   Hash       : String := ""; -- Pre-computed SHA256 hash
   Public_Key : String)       -- PEM-encoded public key
   return Upload_Result;
```
- Builds JSON request per Rekor hashedrekord spec
- POST to `/api/v1/log/entries`
- Parses response to extract UUID, log index, integrated time
- Returns viewer URL for entry (search.sigstore.dev)

**Lookup Operations:**
```ada
function Lookup_By_UUID (Client : Log_Client; UUID : Entry_UUID) return Lookup_Result;
function Lookup_By_Index (Client : Log_Client; Index : Unsigned_64) return Lookup_Result;
function Search_By_Hash (Client : Log_Client; Hash : String; Algo : Hash_Algorithm := SHA256) return Lookup_Result;
```
- GET `/api/v1/log/entries/{uuid}` - Retrieve by UUID
- GET `/api/v1/log/entries?logIndex=N` - Retrieve by index
- POST `/api/v1/index/retrieve` - Search by artifact hash

**Log Info:**
```ada
function Get_Log_Info (Client : Log_Client) return Tree_Info;
```
- GET `/api/v1/log` - Current tree state (root hash, tree size)

### HTTP Client Integration

- Uses CT_HTTP module for all network operations
- Uses CT_JSON module for request/response handling
- Proper error handling:
  - `Network_Error` - Connection failures
  - `Rate_Limited` - HTTP 429 responses
  - `Server_Error` - HTTP 5xx responses
  - `Entry_Not_Found` - HTTP 404 responses
  - `Invalid_Entry` - Malformed data

### Supported Log Providers

```ada
type Log_Provider is
  (Sigstore_Rekor,    -- https://rekor.sigstore.dev (production)
   Sigstore_Staging,  -- https://rekor.staging.sigstore.dev
   CT_TLOG,           -- https://tlog.cerro-torre.dev (future)
   Custom);           -- Self-hosted Rekor-compatible
```

## Example Usage

### Upload a Signature to Rekor

```ada
with CT_Transparency; use CT_Transparency;

procedure Upload_Example is
   Client : Log_Client;
   Result : Upload_Result;
begin
   -- Create client (defaults to Sigstore production)
   Client := Create_Client;

   -- Upload signature
   Result := Upload_Signature (
      Client     => Client,
      Signature  => "base64-encoded-signature",
      Artifact   => "artifact-content",
      Hash       => "abc123...",  -- SHA256 hex
      Public_Key => "-----BEGIN PUBLIC KEY-----..."
   );

   if Result.Error = Success then
      Put_Line ("Entry uploaded:");
      Put_Line ("  UUID: " & String (Result.The_Entry.UUID));
      Put_Line ("  Index: " & Result.The_Entry.Log_Index'Image);
      Put_Line ("  View: " & To_String (Result.URL));
   else
      Put_Line ("Upload failed: " & Error_Message (Result.Error));
   end if;
end Upload_Example;
```

### Lookup an Entry

```ada
procedure Lookup_Example is
   Client : Log_Client;
   Result : Lookup_Result;
   UUID   : Entry_UUID := "24296fb24b8ad77a..." & (others => '0');
begin
   Client := Create_Client;
   Result := Lookup_By_UUID (Client, UUID);

   if Result.Error = Success and not Result.Entries.Is_Empty then
      Put_Line ("Entry found:");
      Put_Line ("  Log Index: " & Result.Entries.First_Element.Log_Index'Image);
   elsif Result.Error = Entry_Not_Found then
      Put_Line ("Entry not found in log");
   end if;
end Lookup_Example;
```

### Search by Hash

```ada
procedure Search_Example is
   Client : Log_Client;
   Result : Lookup_Result;
begin
   Client := Create_Client;
   Result := Search_By_Hash (
      Client => Client,
      Hash   => "abc123def456...",  -- Hex-encoded SHA256
      Algo   => SHA256
   );

   if Result.Error = Success then
      Put_Line ("Found " & Result.Entries.Length'Image & " entries");
   end if;
end Search_Example;
```

## Integration with Cerro Torre Workflow

### Planned CLI Commands

```bash
# Sign and upload to transparency log
ct sign bundle.ctp --key signing-key.pem
ct log submit bundle.ctp

# Verify bundle and check logs
ct verify bundle.ctp
ct log verify bundle.ctp

# Search for entries
ct log search --hash sha256:abc123...
ct log search --key pubkey.pem

# Get log info
ct log info
```

### Planned Workflow

1. **Package Creation** (`ct pack`)
   - Create `.ctp` bundle from OCI image
   - Generate manifest with metadata

2. **Signing** (`ct sign`)
   - Sign bundle with Ed25519 key
   - Create cryptographic signature over bundle hash

3. **Log Submission** (`ct log submit`)
   - Upload signature to Rekor (production + staging)
   - Store entry UUIDs and inclusion proofs in bundle
   - Require 2+ log submissions for quorum

4. **Registry Push** (`ct push`)
   - Push bundle to OCI registry (ghcr.io, docker.io, etc.)
   - Include log proofs as annotations

5. **Verification** (`ct verify`)
   - Download bundle from registry
   - Verify signature
   - Check transparency log inclusion
   - Validate Merkle proofs
   - Ensure quorum (2+ logs)

## Remaining Work ⏳

### High Priority

1. **Fix Build Issues**
   - Resolve Auth_Credentials type mismatch between CT_Registry and CT_HTTP
   - Fix proven library compilation errors (external dependency)
   - Complete successful build

2. **Base64 Support**
   - Add base64 encoder/decoder for entry bodies
   - Parse base64-encoded JSON from Rekor responses
   - Current limitation: Can't decode full entry bodies

3. **Merkle Proof Verification**
   - Implement RFC 6962 Section 2.1 verification
   - Verify inclusion proofs against tree root
   - Critical for security guarantees

4. **SET Signature Verification**
   - Verify Signed Entry Timestamps
   - Requires crypto bindings (Ed25519, ECDSA)
   - Validates log operator's signature

5. **CLI Integration**
   - Wire transparency operations to `ct log` commands
   - Add `ct log submit`, `ct log verify`, `ct log search`
   - Integrate with sign/verify workflow

### Medium Priority

6. **Attestation Uploads**
   - Implement `Upload_Attestation` (in-toto attestations)
   - Implement `Upload_DSSE` (DSSE envelopes)
   - Support more Rekor entry types

7. **Offline Bundles**
   - Create Sigstore bundle format (JSON)
   - Bundle includes: signature + log proof + timestamps
   - Verify bundles without network access

8. **Consistency Proofs**
   - Implement RFC 6962 consistency proof verification
   - Verify log hasn't been tampered with
   - Monitor log consistency over time

### Low Priority

9. **Advanced Search**
   - Search by public key
   - Search by email/identity
   - Filter by entry kind

10. **Multiple Log Support**
    - Submit to multiple logs concurrently
    - Quorum logic (require N of M logs)
    - Federated transparency

## API Endpoints Implemented

| Endpoint | Method | Status | Purpose |
|----------|--------|--------|---------|
| `/api/v1/log/entries` | POST | ✅ | Upload signatures |
| `/api/v1/log/entries/{uuid}` | GET | ✅ | Lookup by UUID |
| `/api/v1/log/entries?logIndex=N` | GET | ✅ | Lookup by index |
| `/api/v1/index/retrieve` | POST | ✅ | Search by hash |
| `/api/v1/log` | GET | ✅ | Get tree info |
| `/api/v1/log/proof` | GET | ⏳ | Consistency proofs |
| `/api/v1/log/publicKey` | GET | ⏳ | Get log public key |

## Security Considerations

### Current Implementation

✅ **TLS Verification**: All HTTPS connections verify certificates
✅ **Error Handling**: Proper error codes for all failure modes
✅ **Input Validation**: UUID format validation, hash format checks

### Pending Security Features

⏳ **Merkle Proof Verification**: Not yet implemented - **DO NOT USE IN PRODUCTION**
⏳ **SET Verification**: Not yet implemented - **DO NOT USE IN PRODUCTION**
⏳ **Constant-Time Comparison**: Digest verification not timing-safe
⏳ **Log Public Key Pinning**: Trust-on-first-use, no key pinning yet

**WARNING**: Current implementation is **NOT suitable for production use** until:
1. Merkle proof verification is implemented
2. SET signature verification is implemented
3. Build completes successfully
4. Full end-to-end testing passes

## Testing Status

- ✅ Code compiles individually (ct_transparency.adb)
- ⏳ Full project build (blocked by type mismatches)
- ⏳ Unit tests (need successful build)
- ⏳ Integration tests with Sigstore Rekor (need successful build)
- ⏳ End-to-end workflow (need CLI integration)

## Next Steps

1. **Fix Auth_Credentials type mismatch** - Unify CT_Registry and CT_HTTP types
2. **Complete build** - Resolve all compilation errors
3. **Add base64 support** - Decode Rekor entry bodies
4. **Implement Merkle proofs** - Core security feature
5. **Add CLI commands** - `ct log submit/verify/search`
6. **Test with Rekor** - Upload real signatures to Sigstore

---

**Last Updated**: 2026-01-25
**Implementation**: ct_transparency.adb (commit ab3dc0a)
**Status**: Core client complete, blocked by build issues
