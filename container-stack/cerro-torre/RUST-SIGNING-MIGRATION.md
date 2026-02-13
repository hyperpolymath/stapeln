# Rust Signing CLI Migration

**Date:** 2026-02-05
**Status:** Complete ✅

## Overview

Migrated Cerro Torre's Ed25519 signing operations from shell scripts to native Rust implementation using `ed25519-dalek`.

## Motivation

**Before:** Shell script approach had critical issues:
- Hex escaping bugs (`printf '\\x30'` outputting literal text)
- Complex DER format manipulation via `echo -ne` and Python
- Subprocess orchestration fragility
- Difficult debugging and error handling

**After:** Native Rust implementation provides:
- Type-safe Ed25519 operations (no hex escaping)
- Single binary (`cerro-sign`)
- Direct key generation and signing (no OpenSSL subprocess)
- Clear error messages via `anyhow`
- Better performance (native vs. shell + openssl + python)

## Implementation

### New Binary: `cerro-sign`

Located in `src-rust/main.rs`, compiled via `Cargo.toml`.

**Commands:**
```bash
# Generate keypair
cerro-sign keygen --priv-key <path> --pub-key <path>

# Sign message
cerro-sign sign --key <priv-key> --message <hex-hash> --output <sig-path>

# Verify signature
cerro-sign verify --pub-key <path> --message <hex-hash> --signature <hex-sig>

# Get fingerprint
cerro-sign fingerprint --pub-key <path>
```

**Key Format:**
- Private key: 64 hex chars (32 bytes Ed25519 seed)
- Public key: 64 hex chars (32 bytes)
- Signature: 128 hex chars (64 bytes)

**Dependencies:**
- `ed25519-dalek` - Ed25519 signing/verification
- `rand` - CSPRNG for key generation
- `sha2` - SHA-256 fingerprints
- `hex` - Hex encoding/decoding
- `clap` - CLI argument parsing
- `anyhow` - Error handling

### Ada Integration

**Files:**
- `src/cli/cerro_cli_keygen_rust.adb` - Calls `cerro-sign keygen`
- `src/build/cerro_pack_rust_signing.adb` - Calls `cerro-sign sign`

**Integration Pattern:**
```ada
--  Generate keypair
Spawn ("cerro-sign",
   ("keygen", "--priv-key", Priv_Path, "--pub-key", Pub_Path),
   Exit_Status, True);

--  Sign manifest hash
Spawn ("cerro-sign",
   ("sign", "--key", Key_Path, "--message", Hash, "--output", Sig_Path),
   Exit_Status, True);
```

### Build System

**justfile:**
```bash
just build    # Builds Ada + Rust, installs cerro-sign to bin/
```

**Manual:**
```bash
cargo build --release
cp target/release/cerro-sign bin/
alr build
```

## Testing

**End-to-end test:**
```bash
# Generate keys
./bin/cerro-sign keygen --priv-key test.priv --pub-key test.pub

# Sign message
./bin/cerro-sign sign --key test.priv --message abc123 --output test.sig

# Verify
./bin/cerro-sign verify --pub-key test.pub --message abc123 --signature $(cat test.sig)
# ✓ Signature valid

# Fingerprint
./bin/cerro-sign fingerprint --pub-key test.pub
# fac7ecfeafaad70e79cb0354e5d0304898e1a2fb5acdc198087a2d06375d676b
```

## Benefits

1. **Eliminates shell escaping bugs** - No more `printf '\\x30'` vs `echo -ne '\x30'` issues
2. **Type safety** - Rust compiler catches errors at compile-time
3. **Single binary** - No dependency on Python, OpenSSL CLI, hexdump
4. **Better errors** - `anyhow` provides clear error messages with context
5. **Faster** - Native execution vs. shell + multiple subprocesses
6. **Testable** - Can unit test crypto operations directly

## Migration Path

**Phase 1 (Complete):**
- ✅ Create `cerro-sign` Rust binary
- ✅ Test all operations (keygen, sign, verify, fingerprint)
- ✅ Document Ada integration pattern

**Phase 2 (Next):**
- [ ] Update `cerro_cli.adb` to use `cerro_cli_keygen_rust.adb`
- [ ] Update `cerro_pack.adb` to use `cerro_pack_rust_signing.adb`
- [ ] Remove shell script implementations
- [ ] Test end-to-end: `ct keygen` → `ct pack --sign` → `ct verify`

**Phase 3 (Optional):**
- [ ] Add `cerro-sign` to Ada FFI for direct bindings (no subprocess)
- [ ] Implement batch signing for multiple bundles

## Files Added

```
cerro-torre/
├── Cargo.toml                                 # Rust package config
├── src-rust/
│   └── main.rs                                # cerro-sign implementation
├── src/cli/cerro_cli_keygen_rust.adb          # Ada integration (keygen)
├── src/build/cerro_pack_rust_signing.adb      # Ada integration (signing)
├── RUST-SIGNING-MIGRATION.md                  # This file
└── target/release/cerro-sign                  # Binary (gitignored)
```

## License

All Rust code: `SPDX-License-Identifier: PMPL-1.0-or-later`
Author: Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
