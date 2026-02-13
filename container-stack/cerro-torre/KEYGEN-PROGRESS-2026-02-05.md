# Cerro Torre: Ed25519 Signing Implementation - Session 2026-02-05

**Status:** 🟡 In Progress (82% → 85%)
**Time:** ~1 hour
**Commit:** `569684b`

## ✅ Completed

### 1. Ed25519 Keygen Working

Implemented `ct keygen` command with shell-based MVP:

```bash
$ ct keygen --id my-key
Generating Ed25519 keypair...
✓ Private key saved: /home/user/.config/cerro-torre/keys/my-key.priv
  ⚠️  KEEP THIS FILE SECURE - it can sign bundles as you!
✓ Public key imported to trust store
✓ Trust level set to 'ultimate'

Key details:
  ID:          my-key
  Suite:       CT-SIG-01
  Fingerprint: a90f4f9a5f0d99f380cdb2154e24046f7eba5af9340f25fff724b18f90ca590a
  Public key:  1ccd4c2152fda74f4c243712dc59a89de0b114a6979fc48c8e4006856b5cbfb8

To sign a bundle:
  ct pack manifest.ctp -o output.ctp --sign --key my-key
```

**Implementation Details:**
- Shell script wrapper generates Ed25519 keypair via OpenSSL
- Extracts 32-byte seed + 32-byte public key = 64-byte private key (128 hex chars)
- Saves to `~/.config/cerro-torre/keys/<id>.priv`
- Imports public key to trust store with "ultimate" trust level
- Computes SHA-256 fingerprint for verification

**Why Shell Script?**
The direct Ada OpenSSL bindings via `GNAT.OS_Lib.Spawn` have a runtime issue (Spawn doesn't properly report command success/failure). The shell-based approach works immediately and can be replaced with direct bindings once the Spawn issue is debugged.

### 2. Trust Store Integration

Public keys automatically imported to `~/.config/cerro-torre/trust/`:
- `.pub` files with hex-encoded Ed25519 public keys
- `.meta` files with key metadata (ID, fingerprint, trust level)
- Trust levels: untrusted, marginal, full, ultimate

### 3. Key Management Commands

Existing `ct key` commands work with generated keys:
```bash
ct key list                    # List all keys
ct key import <file>           # Import public key
ct key export <id> -o <file>   # Export public key
ct key trust <id> <level>      # Set trust level
ct key default <id>            # Set default signing key
```

## ❌ Remaining Work

### 1. Signing Workflow (High Priority)

Modify `Cerro_Pack.Create_Bundle` to sign bundles when `Opts.Sign = True`:

**Changes needed:**
- Read private key from `~/.config/cerro-torre/keys/<key-id>.priv`
- Compute SHA-256 hash of bundle contents
- Sign hash with `Cerro_Crypto_OpenSSL.Sign_Ed25519`
- Add signature to `summary.json` attestations array

**Summary.json format:**
```json
{
  "ctp_version": "1.0",
  "package": { "name": "nginx", "version": "1.25.0" },
  "content": { "manifest_sha256": "abc123..." },
  "attestations": [
    {
      "type": "signature",
      "suite": "CT-SIG-01",
      "key_id": "my-key",
      "fingerprint": "a90f4f9a...",
      "signature": "ed25519_signature_hex...",
      "timestamp": "2026-02-05T05:10:00Z"
    }
  ]
}
```

### 2. CLI Integration

Add signing flags to `ct pack`:
```bash
ct pack manifest.ctp -o bundle.ctp --sign --key <key-id>
```

**Implementation:**
- Parse `--sign` and `--key <id>` arguments in `Run_Pack`
- Set `Opts.Sign := True` and `Opts.Key_Id := <id>`
- Pass to `Cerro_Pack.Create_Bundle`

### 3. Verification Workflow

Modify `Cerro_Verify` to check signatures:
- Extract signature from `summary.json`
- Load public key from trust store
- Verify signature matches bundle hash
- Check key trust level meets policy requirements

### 4. Debug GNAT.OS_Lib.Spawn (Lower Priority)

**Issue:** `Spawn` doesn't correctly report OpenSSL command success/failure

**Root cause hypothesis:**
- `GNAT.OS_Lib.Spawn` returns `True` if process spawned, not if it exited with code 0
- May need to use `GNAT.OS_Lib.Spawn_Pid` + `GNAT.OS_Lib.Wait_Process` for exit code
- Or use `GNAT.Expect` for more control

**Once fixed:**
- Replace shell script with direct OpenSSL bindings
- Cleaner error handling
- No temp shell script files

## Testing Done

1. ✅ Keygen creates 128-char hex private key (64 bytes)
2. ✅ Public key extracted correctly (64 hex chars = 32 bytes)
3. ✅ Trust store import works
4. ✅ Fingerprint computation matches
5. ✅ Multiple keys can be generated with different IDs
6. ❌ End-to-end signing not tested yet (no pack integration)

## Files Changed

- `src/cli/cerro_cli.adb` - Added `Run_Keygen` implementation (+256 lines)

## Next Session Goals

**Option A: Complete Signing (2-3 hours)**
1. Implement signing in `Cerro_Pack`
2. Add CLI flags to `Run_Pack`
3. Test: keygen → pack --sign → verify
4. Document signing workflow

**Option B: Move to Next Toolchain Component**
- Svalinn (90% → 100%): MCP communication, .ctp verification
- Vörðr (mixed): Container runtime integration
- Selur (50% → 100%): WASM IPC bridge

## References

- Ed25519: RFC 8032 (Edwards-Curve Digital Signature Algorithm)
- OpenSSL Ed25519: `openssl genpkey -algorithm ED25519`
- Trust store: `~/.config/cerro-torre/trust/`
- Key format: 64-byte (32 seed + 32 public) = 128 hex chars

---

**Status Summary:**
- Cerro Torre: **85%** (was 82%, keygen working but signing not integrated)
- Next critical path: Pack signing workflow
- Estimated to 100%: 2-3 hours (signing) + 4-6 hours (testing, docs)
