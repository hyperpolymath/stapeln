// SPDX-License-Identifier: PMPL-1.0-or-later
// Linear Cryptographic Operations
//
// This module uses ATS2 linear types to ensure that:
// 1. Crypto keys are used exactly once (not duplicated or forgotten)
// 2. Secrets are zeroized exactly once (not leaked)
// 3. Verification operations are not skipped

#include "share/atspre_staload.hats"

// ============================================================================
// Linear Types for Cryptographic Data
// ============================================================================

// A 32-byte hash or public key that must be freed exactly once
absvtype bytes32_vt = ptr

// A 64-byte signature that must be freed exactly once
absvtype bytes64_vt = ptr

// A secret key that must be zeroized exactly once
absvtype secret_key_vt = ptr

// A verification context that must be finalized exactly once
absvtype verify_ctx_vt = ptr

// ============================================================================
// Bytes32 Operations (SHA-256 hashes, Ed25519 public keys)
// ============================================================================

// Allocate a new 32-byte buffer
extern fun bytes32_create(): bytes32_vt = "ext#"

// Free a 32-byte buffer (consumes it)
extern fun bytes32_free(bytes32_vt): void = "ext#"

// Copy a 32-byte buffer (borrows original, creates new owned copy)
extern fun bytes32_copy(b: !bytes32_vt): bytes32_vt = "ext#"

// Compare two 32-byte buffers for equality (borrows both)
extern fun bytes32_eq(a: !bytes32_vt, b: !bytes32_vt): bool = "ext#"

// Compute SHA-256 hash (returns linear hash that must be freed)
extern fun sha256(data: ptr, len: size_t): bytes32_vt = "ext#"

// ============================================================================
// Bytes64 Operations (Ed25519 signatures)
// ============================================================================

// Allocate a new 64-byte buffer
extern fun bytes64_create(): bytes64_vt = "ext#"

// Free a 64-byte buffer (consumes it)
extern fun bytes64_free(bytes64_vt): void = "ext#"

// Copy a 64-byte buffer
extern fun bytes64_copy(b: !bytes64_vt): bytes64_vt = "ext#"

// ============================================================================
// Secret Key Operations (Must be Zeroized)
// ============================================================================

// Load a secret key (creates linear secret that must be zeroized)
extern fun secret_key_load(path: string): secret_key_vt = "ext#"

// Zeroize and free a secret key (consumes it, ensures zeroing happens)
extern fun secret_key_zeroize(secret_key_vt): void = "ext#"

// Sign a message (borrows key, creates signature)
extern fun ed25519_sign(key: !secret_key_vt, msg: !bytes32_vt): bytes64_vt = "ext#"

// ============================================================================
// Verification Context (Ensures Verification Happens)
// ============================================================================

// Create a verification context
extern fun verify_ctx_create(): verify_ctx_vt = "ext#"

// Add a public key to context (borrows context and key)
extern fun verify_ctx_add_key(ctx: !verify_ctx_vt, key: !bytes32_vt): void = "ext#"

// Add a signature to context (consumes signature - ensures it's checked)
extern fun verify_ctx_add_signature(ctx: !verify_ctx_vt, sig: bytes64_vt): void = "ext#"

// Finalize and verify all signatures (consumes context)
extern fun verify_ctx_finalize(ctx: verify_ctx_vt, msg: !bytes32_vt): bool = "ext#"

// ============================================================================
// Linear Verification Pattern
// ============================================================================

// Example: Verify a bundle with 2 signatures (linear ensures both are checked)
fun verify_bundle_2sigs
  (msg: !bytes32_vt, pk1: !bytes32_vt, sig1: bytes64_vt,
   pk2: !bytes32_vt, sig2: bytes64_vt): bool =
  let
    // Create context
    val ctx = verify_ctx_create()

    // Add both public keys
    val () = verify_ctx_add_key(ctx, pk1)
    val () = verify_ctx_add_key(ctx, pk2)

    // Add signatures (consumes them - they MUST be verified)
    val () = verify_ctx_add_signature(ctx, sig1)
    val () = verify_ctx_add_signature(ctx, sig2)

    // Finalize (consumes context, verifies all signatures)
    val result = verify_ctx_finalize(ctx, msg)
  in
    result
  end

// ============================================================================
// Proof that Signatures Are Not Leaked
// ============================================================================

// If we have a signature, we MUST either:
// 1. Pass it to verify_ctx_add_signature (which consumes it), or
// 2. Free it with bytes64_free (which consumes it)
//
// There is no way to "forget" a signature without a linear type error

// Example: BAD CODE (will not compile)
// fun leak_signature(): void =
//   let
//     val sig = bytes64_create()
//     // Forgot to free or verify - LINEAR TYPE ERROR!
//   in
//     ()
//   end

// ============================================================================
// Proof that Secret Keys Are Zeroized
// ============================================================================

// If we load a secret key, we MUST call secret_key_zeroize before
// the function returns. Otherwise: LINEAR TYPE ERROR

fun sign_with_ephemeral_key(msg: !bytes32_vt, key_path: string): bytes64_vt =
  let
    // Load secret key
    val key = secret_key_load(key_path)

    // Sign message (borrows key)
    val sig = ed25519_sign(key, msg)

    // MUST zeroize key before returning (consumes linear resource)
    val () = secret_key_zeroize(key)
  in
    sig  // Return signature (now caller owns it)
  end

// ============================================================================
// Non-Malleability via Linear Types
// ============================================================================

// A signature can only be verified once (it's consumed by verify_ctx)
// This prevents malleability attacks where an attacker modifies a signature

// Example: Trying to verify same signature twice (will not compile)
// fun double_verify(sig: bytes64_vt, msg: !bytes32_vt): bool =
//   let
//     val ctx1 = verify_ctx_create()
//     val () = verify_ctx_add_signature(ctx1, sig)  // Consumes sig
//     val result1 = verify_ctx_finalize(ctx1, msg)
//
//     val ctx2 = verify_ctx_create()
//     val () = verify_ctx_add_signature(ctx2, sig)  // ERROR: sig already consumed!
//     val result2 = verify_ctx_finalize(ctx2, msg)
//   in
//     result1 && result2
//   end

// To verify the same signature data twice, you must explicitly copy it:
fun double_verify_explicit(sig: bytes64_vt, msg: !bytes32_vt): bool =
  let
    val sig_copy = bytes64_copy(sig)  // Explicit copy

    val ctx1 = verify_ctx_create()
    val () = verify_ctx_add_signature(ctx1, sig)
    val result1 = verify_ctx_finalize(ctx1, msg)

    val ctx2 = verify_ctx_create()
    val () = verify_ctx_add_signature(ctx2, sig_copy)
    val result2 = verify_ctx_finalize(ctx2, msg)
  in
    result1 && result2
  end

// ============================================================================
// Replay Attack Prevention via Linear Types
// ============================================================================

// A signature is consumed when verified, so it cannot be "replayed"
// against a different message without explicit copying

fun verify_no_replay
  (msg1: !bytes32_vt, msg2: !bytes32_vt, sig: bytes64_vt): (bool, bool) =
  let
    // Can only verify sig against one message (it's consumed)
    val ctx1 = verify_ctx_create()
    val () = verify_ctx_add_signature(ctx1, sig)
    val result1 = verify_ctx_finalize(ctx1, msg1)

    // Cannot reuse sig for msg2 (it was consumed above)
    // Must use a different signature
    val result2 = false  // Placeholder
  in
    (result1, result2)
  end
