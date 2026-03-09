-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Stapeln ABI type definitions (Idris2) with dependent types and proofs

module Stapeln.ABI.Types

import Data.List
import Data.Nat
import Decidable.Equality

%default total

-- ============================================================================
-- Result Codes
-- ============================================================================

public export
data ResultCode = Ok | Error | InvalidParam | NotFound | OutOfMemory

public export
resultToInt : ResultCode -> Int
resultToInt Ok = 0
resultToInt Error = 1
resultToInt InvalidParam = 2
resultToInt NotFound = 3
resultToInt OutOfMemory = 4

-- DecEq for ResultCode enables compile-time equality proofs
public export
DecEq ResultCode where
  decEq Ok Ok = Yes Refl
  decEq Ok Error = No (\case Refl impossible)
  decEq Ok InvalidParam = No (\case Refl impossible)
  decEq Ok NotFound = No (\case Refl impossible)
  decEq Ok OutOfMemory = No (\case Refl impossible)
  decEq Error Ok = No (\case Refl impossible)
  decEq Error Error = Yes Refl
  decEq Error InvalidParam = No (\case Refl impossible)
  decEq Error NotFound = No (\case Refl impossible)
  decEq Error OutOfMemory = No (\case Refl impossible)
  decEq InvalidParam Ok = No (\case Refl impossible)
  decEq InvalidParam Error = No (\case Refl impossible)
  decEq InvalidParam InvalidParam = Yes Refl
  decEq InvalidParam NotFound = No (\case Refl impossible)
  decEq InvalidParam OutOfMemory = No (\case Refl impossible)
  decEq NotFound Ok = No (\case Refl impossible)
  decEq NotFound Error = No (\case Refl impossible)
  decEq NotFound InvalidParam = No (\case Refl impossible)
  decEq NotFound NotFound = Yes Refl
  decEq NotFound OutOfMemory = No (\case Refl impossible)
  decEq OutOfMemory Ok = No (\case Refl impossible)
  decEq OutOfMemory Error = No (\case Refl impossible)
  decEq OutOfMemory InvalidParam = No (\case Refl impossible)
  decEq OutOfMemory NotFound = No (\case Refl impossible)
  decEq OutOfMemory OutOfMemory = Yes Refl

-- ============================================================================
-- Core Records
-- ============================================================================

public export
record ServiceSpec where
  constructor MkServiceSpec
  name : String
  kind : String
  port : Int

public export
record StackSpec where
  constructor MkStackSpec
  stackId : Int
  name : String
  description : String
  services : List ServiceSpec

public export
record ValidationFinding where
  constructor MkValidationFinding
  findingId : String
  severity : String
  message : String
  hint : String

public export
record ValidationReport where
  constructor MkValidationReport
  score : Int
  findings : List ValidationFinding

-- ============================================================================
-- Dependent Types for Validated Inputs
-- ============================================================================

||| A port number proven to be in the valid range 1..65535.
||| We use Nat internally for proof ergonomics; the caller converts from Int.
public export
data ValidPort : (p : Nat) -> Type where
  MkValidPort : (p : Nat)
             -> {auto gt : LTE 1 p}
             -> {auto lt : LTE p 65535}
             -> ValidPort p

||| Proof witness that a string is non-empty.
public export
data NonEmpty : String -> Type where
  IsNonEmpty : (s : String) -> {auto prf : s = s} -> (0 _ : length s = 0 -> Void) -> NonEmpty s

||| A service name proven to be non-empty.
public export
data ValidServiceName : String -> Type where
  MkValidServiceName : (s : String) -> NonEmpty s -> ValidServiceName s

||| A stack specification proven to contain at least one service,
||| where every service has a non-empty name.
public export
data ValidStack : StackSpec -> Type where
  MkValidStack : (spec : StackSpec)
              -> {auto nonempty : NonEmptyProof (services spec)}
              -> (allNamed : All (\svc => NonEmpty (name svc)) (services spec))
              -> ValidStack spec
  where
    ||| Proof that a list is non-empty (has at least one element).
    public export
    data NonEmptyProof : List a -> Type where
      IsNonEmpty : NonEmptyProof (x :: xs)

-- ============================================================================
-- Decision Procedures (runtime validation with proof construction)
-- ============================================================================

||| Decide whether a Nat is a valid port number.
public export
isValidPort : (p : Nat) -> Dec (LTE 1 p, LTE p 65535)
isValidPort p =
  case isLTE 1 p of
    Yes gt => case isLTE p 65535 of
      Yes lt => Yes (gt, lt)
      No contra => No (\(_, lt) => contra lt)
    No contra => No (\(gt, _) => contra gt)

||| Decide whether a string is non-empty.
||| We compare length against 0; if positive, the string is non-empty.
public export
isNonEmptyString : (s : String) -> Dec (length s = 0 -> Void)
isNonEmptyString s =
  case decEq (length s) 0 of
    Yes eq => No (\f => f eq)
    No neq => Yes neq
