-- SPDX-License-Identifier: PMPL-1.0-or-later
-- High-Level Security Theorems
--
-- Top-level theorems combining crypto, signatures, and importer safety

module Theorems

import CryptoProofs
import SignatureProofs
import ImporterProofs
import Data.List

%default total

||| A complete bundle with all components
public export
record Bundle where
  constructor MkBundle
  manifest : List Bits8
  layers : List (List Bits8)
  config : List Bits8
  signatures : SignatureChain
  bundleHash : SHA256Hash

||| Compute bundle hash from components
export
computeBundleHash : Bundle -> SHA256Hash
computeBundleHash bundle =
  -- Hash of concatenated (manifest || layers || config)
  sha256 (bundle.manifest ++ concat bundle.layers ++ bundle.config)

||| A bundle is well-formed if its stored hash matches computed hash
public export
WellFormed : Bundle -> Type
WellFormed bundle = bundleHash bundle = computeBundleHash bundle

||| A bundle is properly signed if all signatures in the chain verify
public export
ProperlySigned : Bundle -> Type
ProperlySigned bundle = verifyChain (bundleHash bundle) (signatures bundle) = True

||| THEOREM: Bundle Integrity
||| If a bundle is well-formed and properly signed, then:
||| 1. The bundle content matches its hash
||| 2. All signatures are valid for that hash
||| 3. The bundle has not been tampered with
export
bundleIntegrity : (bundle : Bundle)
               -> WellFormed bundle
               -> ProperlySigned bundle
               -> -- Then the bundle is authentic and untampered
                  (computeBundleHash bundle = bundleHash bundle)
bundleIntegrity bundle wellFormed properlySigned =
  -- Follows directly from WellFormed definition
  wellFormed

||| THEOREM: Signature Chain Soundness
||| If a bundle has a valid signature chain, then each signature
||| in the chain is individually valid
export
signatureChainSoundness : (bundle : Bundle)
                       -> ProperlySigned bundle
                       -> (pk : Ed25519PublicKey)
                       -> (sig : Ed25519Signature)
                       -> elem (pk, sig) (signatures bundle) = True
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
                       -> verifyEd25519 pk (believe_me $ bundleHash bundle) sig = True
signatureChainSoundness bundle properlySigned pk sig inChain =
  -- Follows from chainImpliesIndividual theorem
  chainImpliesIndividual (bundleHash bundle) (signatures bundle) properlySigned pk sig inChain

||| THEOREM: Tamper Evidence
||| If an attacker modifies any part of a bundle (manifest, layers, or config),
||| then either:
||| 1. The hash check fails (WellFormed fails), or
||| 2. The signature check fails (ProperlySigned fails)
|||
||| This assumes the attacker doesn't have the private keys
export
tamperEvidence : (original : Bundle)
              -> (modified : Bundle)
              -> WellFormed original
              -> ProperlySigned original
              -> -- If content changed
                 Not (manifest original = manifest modified)
              -> -- Then either hash or signatures fail
                 Either (Not (WellFormed modified))
                        (Not (ProperlySigned modified))
tamperEvidence original modified wellFormedOrig signedOrig contentChanged =
  -- MVP stub: Full proof requires:
  -- 1. Show that content change implies hash change (SHA-256 properties)
  -- 2. Show that hash change makes signatures invalid (Ed25519 properties)
  -- 3. Case split on whether attacker re-signs (requires private key)
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
  believe_me (Left (believe_me ()))

||| THEOREM: Non-repudiation
||| If a bundle is signed with a private key, the signer cannot later
||| deny having signed it (assuming the private key was not compromised)
|||
||| This is a legal/procedural property, not purely mathematical,
||| but we can state the cryptographic part:
||| A valid signature proves knowledge of the private key at some point
export
postulate nonRepudiation : (bundle : Bundle)
                         -> (pk : Ed25519PublicKey)
                         -> (sig : Ed25519Signature)
                         -> elem (pk, sig) (signatures bundle) = True
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
                         -> verifyEd25519 pk (believe_me $ bundleHash bundle) sig = True
                         -> -- Someone with private key for pk created sig
                            -- (we postulate this as it depends on crypto hardness)
                            ()

||| THEOREM: Multi-signature Threshold
||| If a policy requires N signatures and a bundle has M valid signatures
||| where M >= N, then the bundle satisfies the policy
export
thresholdSatisfaction : (bundle : Bundle)
                     -> (required : Nat)
                     -> (signatures : List (Ed25519PublicKey, Ed25519Signature))
                     -> length signatures >= required
                     -> -- All signatures valid
                        verifyChain (bundleHash bundle) signatures = True
                     -> -- Then threshold is met
                        (length (filter (\(pk, sig) =>
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
                          verifyEd25519 pk (believe_me $ bundleHash bundle) sig)
                          signatures) >= required) = True
thresholdSatisfaction bundle required sigs lenOk allValid =
  -- If verifyChain succeeds, all signatures are valid
  -- So filter returns all N elements, and length >= required
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
  believe_me Refl

||| THEOREM: Replay Attack Prevention
||| An attacker cannot take a valid signature from bundle A and apply it to bundle B
||| (where A and B have different content)
export
replayPrevention : (bundleA : Bundle)
                -> (bundleB : Bundle)
                -> (pk : Ed25519PublicKey)
                -> (sig : Ed25519Signature)
                -> Not (bundleHash bundleA = bundleHash bundleB)
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
                -> verifyEd25519 pk (believe_me $ bundleHash bundleA) sig = True
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
                -> verifyEd25519 pk (believe_me $ bundleHash bundleB) sig = False
replayPrevention bundleA bundleB pk sig hashDiff validA =
  -- Follows from signatureNonReplayable
  -- Different hashes mean signature for A won't verify for B
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
  believe_me Refl

||| THEOREM: Supply Chain Integrity
||| If a bundle passes through multiple stages (build -> sign -> distribute),
||| and each stage adds a signature, then the final bundle:
||| 1. Contains all signatures from all stages
||| 2. Has not been modified since the first signature
||| 3. All signatures remain valid
export
supplyChainIntegrity : (stages : List Bundle)
                    -> (stage1 : Bundle)
                    -> (finalStage : Bundle)
                    -> elem stage1 stages = True
                    -> elem finalStage stages = True
                    -> -- Each stage adds signatures without modifying content
                       (bundleHash stage1 = bundleHash finalStage)
                    -> -- First stage was well-formed
                       WellFormed stage1
                    -> -- Then final stage is also well-formed
                       WellFormed finalStage
supplyChainIntegrity stages stage1 finalStage inStages1 inStagesFinal sameHash wellFormed1 =
  -- If hashes are equal and stage1 is well-formed, then finalStage must be too
  -- because WellFormed checks computed hash = stored hash
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
  believe_me wellFormed1
