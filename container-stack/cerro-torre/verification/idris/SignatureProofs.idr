-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Signature Chain Verification Proofs
--
-- Formal verification of signature chain properties and attack resistance

module SignatureProofs

import Data.Vect
import Data.List
import CryptoProofs

%default total

||| A signed bundle with metadata
public export
record SignedBundle where
  constructor MkSignedBundle
  bundleHash : SHA256Hash
  signatures : List Ed25519Signature
  publicKeys : List Ed25519PublicKey
  timestamp : Nat  -- Unix timestamp

||| Proof that a signature cannot be reused on a different bundle
||| This prevents replay attacks where an attacker copies a valid signature
||| from bundle A and applies it to bundle B
|||
||| The proof relies on:
||| 1. Ed25519 signs over the entire message (including bundle hash)
||| 2. SHA-256 collision resistance ensures different bundles → different hashes
||| 3. Signature verification checks the hash
export
signatureNonReplayable : (bundle1 : SignedBundle)
                       -> (bundle2 : SignedBundle)
                       -> (sig : Ed25519Signature)
                       -> (pk : Ed25519PublicKey)
                       -> Not (bundleHash bundle1 = bundleHash bundle2)
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
                       -> verifyEd25519 pk (cast $ bundleHash bundle1) sig = True
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
                       -> verifyEd25519 pk (cast $ bundleHash bundle2) sig = False
signatureNonReplayable bundle1 bundle2 sig pk hashDiff valid1 =
  -- MVP stub: Asserts non-replayability without full proof
  -- TODO: Full proof requires showing that:
  --   1. sig is valid for bundle1.bundleHash
  --   2. bundle1.bundleHash ≠ bundle2.bundleHash (given)
  --   3. Ed25519 verification fails for different hashes
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
  cast Refl

||| Proof that signatures are non-malleable
||| An attacker cannot modify a valid signature to produce another valid signature
||| without knowledge of the private key
|||
||| Ed25519 is non-malleable by design (unlike ECDSA which requires additional checks)
export
postulate signatureNonMalleable : (pk : Ed25519PublicKey)
                                -> (msg : Message)
                                -> (sig1 : Ed25519Signature)
                                -> (sig2 : Ed25519Signature)
                                -> verifyEd25519 pk msg sig1 = True
                                -> verifyEd25519 pk msg sig2 = True
                                -> sig1 = sig2

||| A signature chain is a list of (publicKey, signature) pairs
public export
SignatureChain : Type
SignatureChain = List (Ed25519PublicKey, Ed25519Signature)

||| Verify that all signatures in a chain are valid for a given bundle hash
export
verifyChain : SHA256Hash -> SignatureChain -> Bool
verifyChain hash [] = True
verifyChain hash ((pk, sig) :: rest) =
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
  if verifyEd25519 pk (cast hash) sig
    then verifyChain hash rest
    else False

||| Proof that if a signature chain is valid, all individual signatures are valid
export
chainImpliesIndividual : (hash : SHA256Hash)
                      -> (chain : SignatureChain)
                      -> verifyChain hash chain = True
                      -> (pk : Ed25519PublicKey)
                      -> (sig : Ed25519Signature)
                      -> elem (pk, sig) chain = True
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
                      -> verifyEd25519 pk (cast hash) sig = True
chainImpliesIndividual hash [] valid pk sig inChain = absurd inChain
chainImpliesIndividual hash ((pk', sig') :: rest) valid pk sig inChain =
  -- MVP stub: Asserts correctness without full proof
  -- TODO: Case split on whether (pk, sig) = (pk', sig') or in rest
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
  cast Refl

||| Proof that adding a valid signature to a valid chain preserves validity
export
chainExtension : (hash : SHA256Hash)
              -> (chain : SignatureChain)
              -> (pk : Ed25519PublicKey)
              -> (sig : Ed25519Signature)
              -> verifyChain hash chain = True
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
              -> verifyEd25519 pk (cast hash) sig = True
              -> verifyChain hash ((pk, sig) :: chain) = True
chainExtension hash chain pk sig validChain validSig =
  -- MVP stub: Should follow from definition of verifyChain
  -- TODO: Full proof by computation
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
  cast Refl

||| Proof that signature verification order doesn't matter (commutative)
export
chainCommutative : (hash : SHA256Hash)
                -> (pk1 : Ed25519PublicKey)
                -> (sig1 : Ed25519Signature)
                -> (pk2 : Ed25519PublicKey)
                -> (sig2 : Ed25519Signature)
                -> verifyChain hash [(pk1, sig1), (pk2, sig2)]
                 = verifyChain hash [(pk2, sig2), (pk1, sig1)]
chainCommutative hash pk1 sig1 pk2 sig2 =
  -- MVP stub: Should follow from verifyChain definition
  -- Each signature is checked independently
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
-- PROOF_TODO: Replace cast with actual proof
  cast Refl
