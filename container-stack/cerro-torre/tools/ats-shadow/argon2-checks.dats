(*
  argon2-checks.dats: ATS2 shadow model for Argon2id parameter validation

  Non-authoritative: intended for CI shadow verification only.
  This checks that keystore policy parameters satisfy constraints
  without implementing actual Argon2id (that's the SPARK core's job).

  Checks:
    - salt_bytes in [16, 32]
    - hash_bytes in [32, 64]
    - parallelism in [1, 8]
    - memory_kib in [65536, 1048576]
    - time_cost in [2, 10]
    - profile name is valid
    - upgrade only increases parameters
*)

#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"

(* --- Constraint bounds from spec/keystore-policy.json --- *)

val SALT_BYTES_MIN: int = 16
val SALT_BYTES_MAX: int = 32
val HASH_BYTES_MIN: int = 32
val HASH_BYTES_MAX: int = 64
val PARALLELISM_MIN: int = 1
val PARALLELISM_MAX: int = 8
val MEMORY_KIB_MIN: int = 65536
val MEMORY_KIB_MAX: int = 1048576
val TIME_COST_MIN: int = 2
val TIME_COST_MAX: int = 10

(* --- Types --- *)

typedef argon2_params = @{
  salt_bytes = int,
  hash_bytes = int,
  parallelism = int,
  memory_kib = int,
  time_cost = int
}

typedef validation_result = @{
  valid = bool,
  error_msg = string
}

(* --- Validation functions --- *)

fun
validate_salt_bytes(p: int): validation_result =
  if p < SALT_BYTES_MIN then
    @{ valid = false, error_msg = "salt_bytes below minimum (16)" }
  else if p > SALT_BYTES_MAX then
    @{ valid = false, error_msg = "salt_bytes above maximum (32)" }
  else
    @{ valid = true, error_msg = "" }

fun
validate_hash_bytes(p: int): validation_result =
  if p < HASH_BYTES_MIN then
    @{ valid = false, error_msg = "hash_bytes below minimum (32)" }
  else if p > HASH_BYTES_MAX then
    @{ valid = false, error_msg = "hash_bytes above maximum (64)" }
  else
    @{ valid = true, error_msg = "" }

fun
validate_parallelism(p: int): validation_result =
  if p < PARALLELISM_MIN then
    @{ valid = false, error_msg = "parallelism below minimum (1)" }
  else if p > PARALLELISM_MAX then
    @{ valid = false, error_msg = "parallelism above maximum (8)" }
  else
    @{ valid = true, error_msg = "" }

fun
validate_memory_kib(m: int): validation_result =
  if m < MEMORY_KIB_MIN then
    @{ valid = false, error_msg = "memory_kib below minimum (65536)" }
  else if m > MEMORY_KIB_MAX then
    @{ valid = false, error_msg = "memory_kib above maximum (1048576)" }
  else
    @{ valid = true, error_msg = "" }

fun
validate_time_cost(t: int): validation_result =
  if t < TIME_COST_MIN then
    @{ valid = false, error_msg = "time_cost below minimum (2)" }
  else if t > TIME_COST_MAX then
    @{ valid = false, error_msg = "time_cost above maximum (10)" }
  else
    @{ valid = true, error_msg = "" }

(* --- Full parameter validation --- *)

fun
validate_params(params: argon2_params): validation_result = let
  val r1 = validate_salt_bytes(params.salt_bytes)
in
  if ~r1.valid then r1
  else let
    val r2 = validate_hash_bytes(params.hash_bytes)
  in
    if ~r2.valid then r2
    else let
      val r3 = validate_parallelism(params.parallelism)
    in
      if ~r3.valid then r3
      else let
        val r4 = validate_memory_kib(params.memory_kib)
      in
        if ~r4.valid then r4
        else let
          val r5 = validate_time_cost(params.time_cost)
        in
          if ~r5.valid then r5
          else @{ valid = true, error_msg = "" }
        end
      end
    end
  end
end

(* --- Upgrade validation (allow_increase_only) --- *)

fun
validate_upgrade(old_p: argon2_params, new_p: argon2_params): validation_result =
  if new_p.salt_bytes < old_p.salt_bytes then
    @{ valid = false, error_msg = "upgrade cannot decrease salt_bytes" }
  else if new_p.hash_bytes < old_p.hash_bytes then
    @{ valid = false, error_msg = "upgrade cannot decrease hash_bytes" }
  else if new_p.parallelism < old_p.parallelism then
    @{ valid = false, error_msg = "upgrade cannot decrease parallelism" }
  else if new_p.memory_kib < old_p.memory_kib then
    @{ valid = false, error_msg = "upgrade cannot decrease memory_kib" }
  else if new_p.time_cost < old_p.time_cost then
    @{ valid = false, error_msg = "upgrade cannot decrease time_cost" }
  else
    @{ valid = true, error_msg = "" }

(* --- Profile validation --- *)

datatype profile_name =
  | Interactive
  | Moderate
  | High
  | Unknown of string

fun
parse_profile(s: string): profile_name =
  if s = "interactive" then Interactive()
  else if s = "moderate" then Moderate()
  else if s = "high" then High()
  else Unknown(s)

fun
validate_profile_name(s: string): validation_result = let
  val p = parse_profile(s)
in
  case+ p of
  | Unknown(name) => @{ valid = false, error_msg = "unknown profile: " + name }
  | _ => @{ valid = true, error_msg = "" }
end

(* --- Profile defaults (for shadow checking) --- *)

fun
get_profile_defaults(p: profile_name): argon2_params =
  case+ p of
  | Interactive() => @{
      salt_bytes = 16,
      hash_bytes = 32,
      parallelism = 1,
      memory_kib = 131072,
      time_cost = 3
    }
  | Moderate() => @{
      salt_bytes = 16,
      hash_bytes = 32,
      parallelism = 2,
      memory_kib = 262144,
      time_cost = 4
    }
  | High() => @{
      salt_bytes = 16,
      hash_bytes = 32,
      parallelism = 4,
      memory_kib = 524288,
      time_cost = 5
    }
  | Unknown(_) => @{
      salt_bytes = 0,
      hash_bytes = 0,
      parallelism = 0,
      memory_kib = 0,
      time_cost = 0
    }

(* --- Invariant: argon2id MUST NOT be used for artifact hashing --- *)

(* This is a compile-time marker. The actual enforcement is in code review
   and CI grep checks. This type exists to document the invariant. *)

abstype artifact_digest  (* opaque: cannot be constructed from argon2 output *)
abstype password_verifier  (* opaque: argon2id output, cannot be used as artifact_digest *)

(* The type system prevents confusion at compile time if used properly.
   This is the ATS-style way of encoding the non-goal invariants. *)

(*
  Usage in CI:

  1. Build this file to verify it compiles (syntax/type check)
  2. Extract the constraint constants and compare to spec/keystore-policy.json
  3. Run grep checks to ensure argon2 is not imported in artifact modules

  patscc -tcats argon2-checks.dats  # Type-check only
*)
