-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Verification.idr — Container verification predicates and proofs
--
-- This module provides verification predicates that can be checked
-- at compile time (proofs) or runtime (assertions).

module Verification

import Container
import Data.So
import Data.List

%default total

--------------------------------------------------------------------------------
-- Verification Results
--------------------------------------------------------------------------------

||| Result of a verification check
public export
data VerifyResult : Type where
  ||| Verification passed
  Verified : VerifyResult
  ||| Verification failed with reason
  Failed : (reason : String) -> VerifyResult
  ||| Verification skipped (not applicable)
  Skipped : (reason : String) -> VerifyResult

public export
Eq VerifyResult where
  Verified == Verified = True
  (Failed r1) == (Failed r2) = r1 == r2
  (Skipped r1) == (Skipped r2) = r1 == r2
  _ == _ = False

public export
Show VerifyResult where
  show Verified = "Verified"
  show (Failed r) = "Failed: " ++ r
  show (Skipped r) = "Skipped: " ++ r

||| Check if verification passed
public export
isVerified : VerifyResult -> Bool
isVerified Verified = True
isVerified _ = False

--------------------------------------------------------------------------------
-- Attestation Types
--------------------------------------------------------------------------------

||| Types of attestations that can be attached to containers
public export
data AttestationType : Type where
  ||| Cryptographic signature from a trusted signer
  SignatureAttestation : AttestationType
  ||| Hash of container image
  ImageHashAttestation : AttestationType
  ||| Software Bill of Materials
  SBOMAttestation : AttestationType
  ||| Vulnerability scan results
  VulnScanAttestation : AttestationType
  ||| Build provenance (in-toto)
  ProvenanceAttestation : AttestationType
  ||| Policy compliance
  PolicyAttestation : AttestationType

public export
Eq AttestationType where
  SignatureAttestation == SignatureAttestation = True
  ImageHashAttestation == ImageHashAttestation = True
  SBOMAttestation == SBOMAttestation = True
  VulnScanAttestation == VulnScanAttestation = True
  ProvenanceAttestation == ProvenanceAttestation = True
  PolicyAttestation == PolicyAttestation = True
  _ == _ = False

public export
Show AttestationType where
  show SignatureAttestation = "signature"
  show ImageHashAttestation = "image-hash"
  show SBOMAttestation = "sbom"
  show VulnScanAttestation = "vuln-scan"
  show ProvenanceAttestation = "provenance"
  show PolicyAttestation = "policy"

||| An attestation record
public export
record Attestation where
  constructor MkAttestation
  attestType : AttestationType
  signer     : String
  timestamp  : Nat  -- Unix timestamp
  valid      : Bool

--------------------------------------------------------------------------------
-- Verification Predicates
--------------------------------------------------------------------------------

||| A container is image-verified if it has a valid image hash attestation
public export
data ImageVerified : Container state -> Type where
  MkImageVerified : (c : Container state) ->
                    (att : Attestation) ->
                    (att.attestType = ImageHashAttestation) ->
                    (att.valid = True) ->
                    ImageVerified c

||| A container is signed if it has a valid signature attestation
public export
data Signed : Container state -> Type where
  MkSigned : (c : Container state) ->
             (att : Attestation) ->
             (att.attestType = SignatureAttestation) ->
             (att.valid = True) ->
             Signed c

||| A container has SBOM if it has a valid SBOM attestation
public export
data HasSBOM : Container state -> Type where
  MkHasSBOM : (c : Container state) ->
              (att : Attestation) ->
              (att.attestType = SBOMAttestation) ->
              (att.valid = True) ->
              HasSBOM c

||| A container is vulnerability-scanned
public export
data VulnScanned : Container state -> Type where
  MkVulnScanned : (c : Container state) ->
                  (att : Attestation) ->
                  (att.attestType = VulnScanAttestation) ->
                  (att.valid = True) ->
                  VulnScanned c

--------------------------------------------------------------------------------
-- Verification Policy
--------------------------------------------------------------------------------

||| A verification policy specifies required attestations
public export
record VerificationPolicy where
  constructor MkPolicy
  requireSignature   : Bool
  requireImageHash   : Bool
  requireSBOM        : Bool
  requireVulnScan    : Bool
  requireProvenance  : Bool
  trustedSigners     : List String
  maxVulnSeverity    : Nat  -- 0=critical, 1=high, 2=medium, 3=low, 4=none

||| Default strict policy
public export
strictPolicy : VerificationPolicy
strictPolicy = MkPolicy True True True True True [] 2

||| Minimal policy (just signature)
public export
minimalPolicy : VerificationPolicy
minimalPolicy = MkPolicy True False False False False [] 4

--------------------------------------------------------------------------------
-- Verification Functions
--------------------------------------------------------------------------------

||| Check if an attestation satisfies a requirement
public export
checkAttestation : AttestationType -> List Attestation -> VerifyResult
checkAttestation atype [] = Failed ("Missing attestation: " ++ show atype)
checkAttestation atype (a :: as) =
  if a.attestType == atype && a.valid
    then Verified
    else checkAttestation atype as

||| Check if signer is trusted
public export
checkSigner : List String -> Attestation -> VerifyResult
checkSigner [] _ = Failed "No trusted signers configured"
checkSigner trusted att =
  if elem att.signer trusted
    then Verified
    else Failed ("Untrusted signer: " ++ att.signer)

||| Verify a container against a policy
public export
verifyAgainstPolicy : VerificationPolicy ->
                      List Attestation ->
                      List VerifyResult
verifyAgainstPolicy policy atts =
  let sigCheck = if policy.requireSignature
                   then checkAttestation SignatureAttestation atts
                   else Skipped "Signature not required"
      hashCheck = if policy.requireImageHash
                    then checkAttestation ImageHashAttestation atts
                    else Skipped "Image hash not required"
      sbomCheck = if policy.requireSBOM
                    then checkAttestation SBOMAttestation atts
                    else Skipped "SBOM not required"
      vulnCheck = if policy.requireVulnScan
                    then checkAttestation VulnScanAttestation atts
                    else Skipped "Vuln scan not required"
      provCheck = if policy.requireProvenance
                    then checkAttestation ProvenanceAttestation atts
                    else Skipped "Provenance not required"
  in [sigCheck, hashCheck, sbomCheck, vulnCheck, provCheck]

||| Check if all verifications passed
public export
allVerified : List VerifyResult -> Bool
allVerified [] = True
allVerified (Verified :: xs) = allVerified xs
allVerified (Skipped _ :: xs) = allVerified xs
allVerified (Failed _ :: _) = False

--------------------------------------------------------------------------------
-- Proofs about Verification
--------------------------------------------------------------------------------

||| If we have proofs of all required attestations, verification succeeds
export
verifiedImpliesPass : (ImageVerified c, Signed c, HasSBOM c) ->
                      allVerified (verifyAgainstPolicy strictPolicy atts) = True
verifiedImpliesPass prf = ?verifiedImpliesPass_proof -- TODO: complete proof

||| A container with valid signature attestation is signed
export
validSigImpliesSigned : (c : Container s) ->
                        (att : Attestation) ->
                        (att.attestType = SignatureAttestation) ->
                        (att.valid = True) ->
                        Signed c
validSigImpliesSigned c att typePrf validPrf = MkSigned c att typePrf validPrf
