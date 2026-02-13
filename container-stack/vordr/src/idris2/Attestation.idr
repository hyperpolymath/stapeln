-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Attestation.idr — Attestation parsing and validation
--
-- This module handles parsing and validating attestation envelopes
-- in formats like in-toto, DSSE, and Sigstore.

module Attestation

import Container
import Verification
import Data.List -- Ensures unlines is available

%default total

--------------------------------------------------------------------------------
-- Attestation Envelope Formats
--------------------------------------------------------------------------------

||| Supported envelope formats
public export
data EnvelopeFormat : Type where
  ||| Dead Simple Signing Envelope (Sigstore)
  DSSE : EnvelopeFormat
  ||| in-toto attestation format
  InToto : EnvelopeFormat
  ||| Simple JSON envelope
  SimpleJSON : EnvelopeFormat
  ||| Cerro Torre .ctp bundle
  CerroTorre : EnvelopeFormat

public export
Show EnvelopeFormat where
  show DSSE = "dsse"
  show InToto = "in-toto"
  show SimpleJSON = "json"
  show CerroTorre = "ctp"

--------------------------------------------------------------------------------
-- Attestation Envelope
--------------------------------------------------------------------------------

||| A signature on an envelope
public export
record Signature where
  constructor MkSignature
  keyId     : String
  algorithm : String  -- e.g., "ed25519", "ecdsa-p256"
  sig       : String  -- Base64 encoded

||| An attestation envelope wrapping a payload
public export
record Envelope where
  constructor MkEnvelope
  format      : EnvelopeFormat
  payloadType : String
  payload     : String  -- Base64 or raw
  signatures  : List Signature

--------------------------------------------------------------------------------
-- Attestation Predicates
--------------------------------------------------------------------------------

||| A DSSE envelope has the correct structure
public export
data ValidDSSE : Envelope -> Type where
  MkValidDSSE : (env : Envelope) ->
                (format env == DSSE) -> -- Using function application for field access
                (length env.signatures > 0) ->
                ValidDSSE env

||| An in-toto attestation has required fields
public export
data ValidInToto : Envelope -> Type where
  MkValidInToto : (env : Envelope) ->
                  (format env == InToto) -> -- Using function application for field access
                  (env.payloadType == "application/vnd.in-toto+json") ->
                  ValidInToto env

--------------------------------------------------------------------------------
-- Attestation Bundle
--------------------------------------------------------------------------------

||| A bundle of attestations for a container
public export
record AttestationBundle where
  constructor MkBundle
  containerRef : String  -- Image reference
  attestations : List Attestation
  envelopes    : List Envelope

||| Create an empty bundle for a container
public export
emptyBundle : String -> AttestationBundle
emptyBundle ref = MkBundle ref [] []

||| Add an attestation to a bundle
public export
addAttestation : Attestation -> AttestationBundle -> AttestationBundle
addAttestation att bundle =
  { attestations := att :: bundle.attestations } bundle

||| Add an envelope to a bundle
public export
addEnvelope : Envelope -> AttestationBundle -> AttestationBundle
addEnvelope env bundle =
  { envelopes := env :: bundle.envelopes } bundle

--------------------------------------------------------------------------------
-- Verification Chain
--------------------------------------------------------------------------------

||| A verification chain records the sequence of checks
public export
record VerificationChain where
  constructor MkChain
  steps   : List (String, VerifyResult)
  overall : VerifyResult

||| Create an empty chain
public export
emptyChain : VerificationChain
emptyChain = MkChain [] (Skipped "No checks performed")

||| Add a step to the chain
public export
addStep : String -> VerifyResult -> VerificationChain -> VerificationChain
addStep name result chain =
  let newSteps = (name, result) :: chain.steps
      newOverall = case (chain.overall, result) of
                     (Failed _, _) => chain.overall  -- Keep first failure
                     (_, Failed r) => Failed r
                     (Verified, _) => Verified
                     (_, Verified) => Verified
                     (Skipped _, Skipped _) => chain.overall
  in MkChain newSteps newOverall

||| Check if chain passed
public export
chainPassed (MkChain _ overall) = isVerified overall -- Simplified definition

--------------------------------------------------------------------------------
-- Standard Verification Workflow
--------------------------------------------------------------------------------

||| Run standard verification on a bundle
public export
verifyBundle : VerificationPolicy -> AttestationBundle -> VerificationChain
verifyBundle policy bundle =
  let results = verifyAgainstPolicy policy bundle.attestations
      names = ["signature", "image-hash", "sbom", "vuln-scan", "provenance"]
      steps = zip names results
      overall = if allVerified results then Verified else Failed "Policy not satisfied"
  in MkChain steps overall

--------------------------------------------------------------------------------
-- Show Instances
--------------------------------------------------------------------------------

public export
Show Signature where
  show sig = "Signature(keyId=" ++ sig.keyId ++ ", algo=" ++ sig.algorithm ++ ")"

public export
Show Envelope where
  show env = "Envelope(format=" ++ show env.format ++
             ", payloadType=" ++ env.payloadType ++
             ", sigs=" ++ show (length env.signatures) ++ ")"

public export
Show VerificationChain where
  show chain =
    let stepStr = Data.List.unlines (map (\(n, r) => "  " ++ n ++ ": " ++ show r) chain.steps)
    in "VerificationChain:\n" ++ stepStr ++ "Overall: " ++ show chain.overall

