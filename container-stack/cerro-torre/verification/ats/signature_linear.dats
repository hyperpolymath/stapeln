// SPDX-License-Identifier: PMPL-1.0-or-later
// Linear Signature Chain Verification
//
// Uses linear types to ensure all signatures in a chain are verified

#include "share/atspre_staload.hats"
staload "crypto_linear.dats"

// ============================================================================
// Signature Chain Types
// ============================================================================

// A signature chain entry (public key + signature)
// Both must be freed exactly once
typedef chain_entry = @{
  publicKey = bytes32_vt,
  signature = bytes64_vt
}

// Linear list of chain entries
// All entries must be consumed (verified or freed)
vtypedef chain_vt(n:int) = list_vt(chain_entry, n)

// ============================================================================
// Chain Construction
// ============================================================================

// Create an empty chain
fun chain_empty(): chain_vt(0) =
  list_vt_nil()

// Add an entry to the chain (transfers ownership of pk and sig)
fun chain_cons
  {n:nat}
  (pk: bytes32_vt, sig: bytes64_vt, rest: chain_vt(n)): chain_vt(n+1) =
  let
    val entry = @{publicKey = pk, signature = sig}
  in
    list_vt_cons(entry, rest)
  end

// ============================================================================
// Chain Verification (Linear)
// ============================================================================

// Verify all signatures in a chain against a message
// This is linear - every signature MUST be verified, none can be skipped
fun chain_verify_all
  {n:nat}
  (chain: chain_vt(n), msg: !bytes32_vt): bool =
  let
    // Helper function to verify and consume each entry
    fun loop
      {n:nat}
      (chain: chain_vt(n), msg: !bytes32_vt, acc: bool): bool =
      case+ chain of
      | ~list_vt_nil() => acc
      | ~list_vt_cons(entry, rest) =>
        let
          // Extract public key and signature (consumes entry)
          val+ @{publicKey = pk, signature = sig} = entry

          // Create verification context
          val ctx = verify_ctx_create()

          // Add public key
          val () = verify_ctx_add_key(ctx, pk)

          // Add signature (consumes it - ensures verification)
          val () = verify_ctx_add_signature(ctx, sig)

          // Verify (consumes context)
          val result = verify_ctx_finalize(ctx, msg)

          // Free the public key (it was borrowed, now must be freed)
          val () = bytes32_free(pk)

          // Continue with rest of chain
          val final_result = loop(rest, msg, acc && result)
        in
          final_result
        end
  in
    loop(chain, msg, true)
  end

// ============================================================================
// Threshold Verification
// ============================================================================

// Verify at least N out of M signatures in the chain
// Linear types ensure all signatures are accounted for
fun chain_verify_threshold
  {n:nat}{threshold:nat | threshold <= n}
  (chain: chain_vt(n), msg: !bytes32_vt, threshold: int(threshold)): bool =
  let
    // Count valid signatures
    fun count_valid
      {n:nat}
      (chain: chain_vt(n), msg: !bytes32_vt, count: int): int =
      case+ chain of
      | ~list_vt_nil() => count
      | ~list_vt_cons(entry, rest) =>
        let
          val+ @{publicKey = pk, signature = sig} = entry

          val ctx = verify_ctx_create()
          val () = verify_ctx_add_key(ctx, pk)
          val () = verify_ctx_add_signature(ctx, sig)
          val result = verify_ctx_finalize(ctx, msg)

          val () = bytes32_free(pk)

          val new_count = if result then count + 1 else count
        in
          count_valid(rest, msg, new_count)
        end

    val valid_count = count_valid(chain, msg, 0)
  in
    valid_count >= threshold
  end

// ============================================================================
// Chain Ordering (Signatures Can Be Verified in Any Order)
// ============================================================================

// Helper to verify two signatures independently
fun verify_two_sigs_unordered
  (pk1: bytes32_vt, sig1: bytes64_vt, pk2: bytes32_vt, sig2: bytes64_vt,
   msg: !bytes32_vt): bool =
  let
    // Verify first signature
    val ctx1 = verify_ctx_create()
    val () = verify_ctx_add_key(ctx1, pk1)
    val () = verify_ctx_add_signature(ctx1, sig1)
    val result1 = verify_ctx_finalize(ctx1, msg)
    val () = bytes32_free(pk1)

    // Verify second signature
    val ctx2 = verify_ctx_create()
    val () = verify_ctx_add_key(ctx2, pk2)
    val () = verify_ctx_add_signature(ctx2, sig2)
    val result2 = verify_ctx_finalize(ctx2, msg)
    val () = bytes32_free(pk2)
  in
    result1 && result2
  end

// ============================================================================
// Proof: All Signatures Are Verified
// ============================================================================

// By using linear types, we prove at compile-time that:
// 1. Every signature in the chain is either verified OR freed
// 2. No signature can be skipped
// 3. No signature can be verified twice (without explicit copy)
//
// If we forget to verify a signature, we get: LINEAR TYPE ERROR

// Example: BAD CODE (will not compile)
// fun skip_signature(chain: chain_vt(2), msg: !bytes32_vt): bool =
//   case+ chain of
//   | ~list_vt_cons(entry1, rest) =>
//     let
//       val+ @{publicKey = pk1, signature = sig1} = entry1
//       // Verify sig1...
//       val () = bytes32_free(pk1)
//       val () = bytes64_free(sig1)
//
//       // Oops, forgot to process rest!
//       // LINEAR TYPE ERROR: rest not consumed
//     in
//       true
//     end

// ============================================================================
// Replay Attack Prevention (Signature Reuse)
// ============================================================================

// A signature cannot be verified against two different messages
// without explicit copying (which is expensive and obvious in code)

// Example: Trying to reuse signature (will not compile)
// fun replay_attack(sig: bytes64_vt, msg1: !bytes32_vt, msg2: !bytes32_vt): bool =
//   let
//     val pk = bytes32_create()
//
//     val ctx1 = verify_ctx_create()
//     val () = verify_ctx_add_key(ctx1, pk)
//     val () = verify_ctx_add_signature(ctx1, sig)  // Consumes sig
//     val result1 = verify_ctx_finalize(ctx1, msg1)
//
//     val ctx2 = verify_ctx_create()
//     val () = verify_ctx_add_key(ctx2, pk)
//     val () = verify_ctx_add_signature(ctx2, sig)  // ERROR: sig already consumed!
//     val result2 = verify_ctx_finalize(ctx2, msg2)
//
//     val () = bytes32_free(pk)
//   in
//     result1 && result2
//   end

// ============================================================================
// Multi-Step Verification (Build -> Sign -> Verify Pipeline)
// ============================================================================

// Ensure signatures are added at each pipeline stage
datatype pipeline_stage =
  | Built of () // Bundle built, no signatures yet
  | Signed of (chain_vt(1)) // One signature added
  | Countersigned of (chain_vt(2)) // Two signatures added
  | Verified of (bool) // All signatures verified

// Transition from Built to Signed (adds first signature)
fun pipeline_sign
  (stage: pipeline_stage, pk: bytes32_vt, sig: bytes64_vt): pipeline_stage =
  case+ stage of
  | Built() =>
    let
      val chain = chain_cons(pk, sig, chain_empty())
    in
      Signed(chain)
    end
  | _ => stage // Should not happen

// Transition from Signed to Countersigned (adds second signature)
fun pipeline_countersign
  {n:pos}
  (stage: pipeline_stage, pk: bytes32_vt, sig: bytes64_vt): pipeline_stage =
  case+ stage of
  | Signed(chain) =>
    let
      val chain2 = chain_cons(pk, sig, chain)
    in
      Countersigned(chain2)
    end
  | _ => stage // Should not happen

// Transition from Countersigned to Verified (verifies all signatures)
fun pipeline_verify
  (stage: pipeline_stage, msg: !bytes32_vt): pipeline_stage =
  case+ stage of
  | Countersigned(chain) =>
    let
      val result = chain_verify_all(chain, msg)
    in
      Verified(result)
    end
  | _ => stage // Should not happen

// Example: Complete pipeline ensures both signatures are verified
fun complete_pipeline
  (msg: !bytes32_vt, pk1: bytes32_vt, sig1: bytes64_vt,
   pk2: bytes32_vt, sig2: bytes64_vt): bool =
  let
    val stage0 = Built()
    val stage1 = pipeline_sign(stage0, pk1, sig1)
    val stage2 = pipeline_countersign(stage1, pk2, sig2)
    val stage3 = pipeline_verify(stage2, msg)
  in
    case+ stage3 of
    | Verified(result) => result
    | _ => false
  end
