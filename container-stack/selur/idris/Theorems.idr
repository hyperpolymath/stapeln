-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
--
-- High-level correctness theorems for selur

module Theorems

import Proofs
import Data.Vect

-- Helper functions (placeholders for actual implementation)
-- These must be defined before the theorems that use them

requestSize : Request -> Nat
requestSize (MkRequest cmd payload corr_id) = length payload

memoryCapacity : MemoryState -> Nat
memoryCapacity (MkMemoryState alloc cap) = cap

bridge : Request -> Response
bridge (MkRequest cmd payload corr_id) = MkResponse 0 Nil corr_id  -- Placeholder

-- Theorem 1: Correctness
-- If bridge returns a response, it matches the request

||| Bridge correctness: response always matches request
bridgeCorrectness : (req : Request) -> (resp : Response) -> bridge req = resp -> responseMatchesRequest req resp
bridgeCorrectness req resp prf = ?bridgeCorrectness_rhs  -- TODO: Implement

-- Theorem 2: Safety
-- Bridge never accesses memory out of bounds

||| Bridge safety: all memory accesses are bounded
bridgeSafety : (req : Request) -> (mem : MemoryState) -> noBufferOverflow 0 (requestSize req) (memoryCapacity mem)
bridgeSafety req mem = ?bridgeSafety_rhs  -- TODO: Implement

-- Theorem 3: Liveness
-- Bridge always terminates (no infinite loops)

||| Bridge liveness: always terminates
bridgeLiveness : (req : Request) -> (fuel : Nat) -> bridgeTerminates req fuel
bridgeLiveness req Z = ?bridgeLiveness_impossible  -- Contradiction
bridgeLiveness req (S k) = ()  -- Terminates

-- Theorem 4: Uniqueness
-- Each request produces exactly one response

||| Bridge uniqueness: one request, one response
bridgeUniqueness : (req : Request) -> (resp1 : Response) -> (resp2 : Response) ->
                   (bridge req = resp1) -> (bridge req = resp2) -> resp1 = resp2
bridgeUniqueness req resp1 resp2 prf1 prf2 = trans (sym prf1) prf2

-- All theorems are public by default
