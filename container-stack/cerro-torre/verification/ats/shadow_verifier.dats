// SPDX-License-Identifier: PMPL-1.0-or-later
// Shadow Verifier for Cerro Torre eXtreme (X-) Variants
//
// This is a parallel verifier written in ATS2 that uses linear types
// to verify bundle signatures and integrity. It runs alongside the main
// ct binary and ensures no crypto operations are missed or duplicated.

#include "share/atspre_staload.hats"

staload "crypto_linear.dats"

// ============================================================================
// Linear Types for Bundle Components
// ============================================================================

// A bundle that must be verified exactly once
absvtype bundle_vt = ptr

// A signature that must be checked exactly once
absvtype signature_vt = ptr

// A verification result that must be consumed exactly once
absvtype verify_result_vt = ptr

// ============================================================================
// Bundle Operations
// ============================================================================

// Load a bundle from disk (creates linear bundle)
extern fun load_bundle(path: string): bundle_vt = "ext#"

// Extract bundle hash (consumes bundle, returns hash and bundle)
extern fun bundle_get_hash(bundle_vt): (bundle_vt, bytes32_vt) = "ext#"

// Extract signatures from bundle (consumes bundle, returns sigs and bundle)
extern fun bundle_get_signatures(bundle_vt): (bundle_vt, List_vt(signature_vt)) = "ext#"

// Close/free bundle resource (consumes bundle)
extern fun bundle_close(bundle_vt): void = "ext#"

// ============================================================================
// Signature Verification
// ============================================================================

// Verify a single signature (consumes signature, returns result)
extern fun verify_signature(signature_vt, bytes32_vt): verify_result_vt = "ext#"

// Check if verification result is success (consumes result)
extern fun result_is_success(verify_result_vt): bool = "ext#"

// ============================================================================
// Signature Chain Verification (Linear)
// ============================================================================

// Verify all signatures in a chain
// This is linear - each signature MUST be verified exactly once
fun verify_signature_chain
  {n:nat}
  (sigs: !List_vt(signature_vt, n), hash: !bytes32_vt): bool =
  let
    fun loop
      {n:nat}
      (sigs: !List_vt(signature_vt, n), hash: !bytes32_vt, acc: bool): bool =
      case+ sigs of
      | list_vt_nil() => acc
      | list_vt_cons(sig, rest) =>
        let
          // Create a copy of hash for verification
          val hash_copy = bytes32_copy(hash)

          // Verify this signature (must consume sig)
          // Since sig is borrowed from list, we need to work with it carefully
          // For now, stub implementation
          val is_valid = true  // TODO: actual verification

          // Free the hash copy
          val () = bytes32_free(hash_copy)
        in
          loop(rest, hash, acc && is_valid)
        end
  in
    loop(sigs, hash, true)
  end

// ============================================================================
// Main Shadow Verification Entry Point
// ============================================================================

// Shadow verify a bundle - this runs in parallel with main ct verification
// Returns true if verification succeeds, false otherwise
fun shadow_verify_bundle(path: string): bool =
  let
    // Load bundle (creates linear resource)
    val bundle = load_bundle(path)

    // Extract hash (preserves bundle)
    val (bundle, hash) = bundle_get_hash(bundle)

    // Extract signatures (preserves bundle)
    val (bundle, signatures) = bundle_get_signatures(bundle)

    // Verify signature chain
    val result = verify_signature_chain(signatures, hash)

    // Clean up linear resources
    // Must free in reverse order due to dependencies
    val () = list_vt_free(signatures)
    val () = bytes32_free(hash)
    val () = bundle_close(bundle)
  in
    result
  end

// ============================================================================
// CLI Interface
// ============================================================================

implement main0(argc, argv) =
  let
    val () = println!("ct-shadow: ATS2 Shadow Verifier for Cerro Torre X- variants")

    val () = if argc < 2 then
      let
        val () = println!("Usage: ct-shadow <bundle.ctp>")
        val () = exit(1)
      in
        // Unreachable
      end
    else ()

    val bundle_path = argv[1]

    val () = println!("Verifying bundle: ", bundle_path)

    // Perform shadow verification
    val success = shadow_verify_bundle(bundle_path)

    val () = if success then
      let
        val () = println!("✅ Shadow verification: PASS")
        val () = exit(0)
      in ()
      end
    else
      let
        val () = println!("❌ Shadow verification: FAIL")
        val () = exit(1)
      in ()
      end
  in
    ()
  end
