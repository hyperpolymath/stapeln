// SPDX-License-Identifier: PMPL-1.0-or-later
// Policy Engine for Svalinn - Gatekeeper policy evaluation

// Policy mode
type policyMode =
  | Strict // Reject failures
  | Permissive // Warn and continue

// Gatekeeper policy structure
type policy = {
  version: int,
  requiredPredicates: array<string>,
  allowedSigners: array<string>,
  logQuorum: int,
  mode: option<policyMode>,
  notes: option<string>,
}

// Policy evaluation result
type evaluationResult = {
  allowed: bool,
  mode: policyMode,
  predicatesFound: array<string>,
  missingPredicates: array<string>,
  signersVerified: array<string>,
  invalidSigners: array<string>,
  logCount: int,
  logQuorumMet: bool,
  violations: array<string>,
  warnings: array<string>,
}

// Convert string to policy mode
let policyModeFromString = (str: string): option<policyMode> => {
  switch str {
  | "strict" => Some(Strict)
  | "permissive" => Some(Permissive)
  | _ => None
  }
}

// Convert policy mode to string
let policyModeToString = (mode: policyMode): string => {
  switch mode {
  | Strict => "strict"
  | Permissive => "permissive"
  }
}

// Parse policy from JSON
let parsePolicy = (json: Js.Json.t): option<policy> => {
  try {
    let obj = json->Js.Json.decodeObject->Belt.Option.getExn

    let version = obj
      ->Js.Dict.get("version")
      ->Belt.Option.flatMap(Js.Json.decodeNumber)
      ->Belt.Option.map(Belt.Float.toInt)
      ->Belt.Option.getExn

    let requiredPredicates = obj
      ->Js.Dict.get("requiredPredicates")
      ->Belt.Option.flatMap(Js.Json.decodeArray)
      ->Belt.Option.map(arr => arr->Belt.Array.keepMap(Js.Json.decodeString))
      ->Belt.Option.getExn

    let allowedSigners = obj
      ->Js.Dict.get("allowedSigners")
      ->Belt.Option.flatMap(Js.Json.decodeArray)
      ->Belt.Option.map(arr => arr->Belt.Array.keepMap(Js.Json.decodeString))
      ->Belt.Option.getExn

    let logQuorum = obj
      ->Js.Dict.get("logQuorum")
      ->Belt.Option.flatMap(Js.Json.decodeNumber)
      ->Belt.Option.map(Belt.Float.toInt)
      ->Belt.Option.getExn

    let mode = obj
      ->Js.Dict.get("mode")
      ->Belt.Option.flatMap(Js.Json.decodeString)
      ->Belt.Option.flatMap(policyModeFromString)

    let notes = obj->Js.Dict.get("notes")->Belt.Option.flatMap(Js.Json.decodeString)

    Some({
      version,
      requiredPredicates,
      allowedSigners,
      logQuorum,
      mode,
      notes,
    })
  } catch {
  | _ => None
  }
}

// Bundle attestation structure (simplified)
type attestation = {
  predicateType: string,
  subject: array<string>,
  signer: string,
  logEntry: option<string>,
}

// Parse attestation from JSON
let parseAttestation = (json: Js.Json.t): option<attestation> => {
  try {
    let obj = json->Js.Json.decodeObject->Belt.Option.getExn

    let predicateType = obj
      ->Js.Dict.get("predicateType")
      ->Belt.Option.flatMap(Js.Json.decodeString)
      ->Belt.Option.getExn

    let subject = obj
      ->Js.Dict.get("subject")
      ->Belt.Option.flatMap(Js.Json.decodeArray)
      ->Belt.Option.map(arr => arr->Belt.Array.keepMap(Js.Json.decodeString))
      ->Belt.Option.getWithDefault([])

    let signer = obj
      ->Js.Dict.get("signer")
      ->Belt.Option.flatMap(Js.Json.decodeString)
      ->Belt.Option.getExn

    let logEntry = obj->Js.Dict.get("logEntry")->Belt.Option.flatMap(Js.Json.decodeString)

    Some({
      predicateType,
      subject,
      signer,
      logEntry,
    })
  } catch {
  | _ => None
  }
}

// Evaluate policy against attestations
let evaluate = (policy: policy, attestations: array<attestation>): evaluationResult => {
  let mode = policy.mode->Belt.Option.getWithDefault(Strict)

  // Check required predicates
  let predicatesFound = Belt.Array.keepMap(attestations, att =>
    if Belt.Array.some(policy.requiredPredicates, pred => pred == att.predicateType) {
      Some(att.predicateType)
    } else {
      None
    }
  )->Belt.Array.reduce([], (acc, pred) =>
    if Belt.Array.some(acc, p => p == pred) {
      acc
    } else {
      Belt.Array.concat(acc, [pred])
    }
  )

  let missingPredicates = Belt.Array.keep(policy.requiredPredicates, pred =>
    !Belt.Array.some(predicatesFound, p => p == pred)
  )

  // Check allowed signers
  let signersVerified = Belt.Array.keepMap(attestations, att =>
    if Belt.Array.some(policy.allowedSigners, signer => signer == att.signer) {
      Some(att.signer)
    } else {
      None
    }
  )->Belt.Array.reduce([], (acc, signer) =>
    if Belt.Array.some(acc, s => s == signer) {
      acc
    } else {
      Belt.Array.concat(acc, [signer])
    }
  )

  let invalidSigners = Belt.Array.keepMap(attestations, att =>
    if !Belt.Array.some(policy.allowedSigners, signer => signer == att.signer) {
      Some(att.signer)
    } else {
      None
    }
  )->Belt.Array.reduce([], (acc, signer) =>
    if Belt.Array.some(acc, s => s == signer) {
      acc
    } else {
      Belt.Array.concat(acc, [signer])
    }
  )

  // Check log quorum
  let logCount = Belt.Array.keep(attestations, att => att.logEntry->Belt.Option.isSome)
    ->Belt.Array.length

  let logQuorumMet = logCount >= policy.logQuorum

  // Collect violations
  let violations = []

  let violations = if Belt.Array.length(missingPredicates) > 0 {
    Belt.Array.concat(
      violations,
      Belt.Array.map(missingPredicates, pred => "Missing required predicate: " ++ pred),
    )
  } else {
    violations
  }

  let violations = if Belt.Array.length(invalidSigners) > 0 {
    Belt.Array.concat(
      violations,
      Belt.Array.map(invalidSigners, signer => "Invalid signer: " ++ signer),
    )
  } else {
    violations
  }

  let violations = if !logQuorumMet {
    let msg = "Log quorum not met: " ++ Belt.Int.toString(logCount) ++ " < " ++ Belt.Int.toString(policy.logQuorum)
    Belt.Array.concat(violations, [msg])
  } else {
    violations
  }

  // Determine if allowed
  let allowed = switch mode {
  | Strict => Belt.Array.length(violations) == 0
  | Permissive => true // Always allow, violations become warnings
  }

  // Warnings (only in permissive mode)
  let warnings = if mode == Permissive && Belt.Array.length(violations) > 0 {
    violations
  } else {
    []
  }

  {
    allowed,
    mode,
    predicatesFound,
    missingPredicates,
    signersVerified,
    invalidSigners,
    logCount,
    logQuorumMet,
    violations,
    warnings,
  }
}

// Load policy from file
@scope("Deno") @val external readTextFile: string => promise<string> = "readTextFile"

let loadPolicy = async (path: string): option<policy> => {
  try {
    let content = await readTextFile(path)
    let json = Js.Json.parseExn(content)
    parsePolicy(json)
  } catch {
  | Js.Exn.Error(e) => {
      let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Unknown error")
      let errorMsg = "Failed to load policy from " ++ path ++ ": " ++ message
      Js.Console.error(errorMsg)
      None
    }
  }
}

// Default policy (strict, single signer, single log)
let defaultPolicy: policy = {
  version: 1,
  requiredPredicates: [
    "https://slsa.dev/provenance/v1",
    "https://spdx.dev/Document",
  ],
  allowedSigners: [],
  logQuorum: 1,
  mode: Some(Strict),
  notes: Some("Default strict policy"),
}

// Create permissive policy (for testing/development)
let permissivePolicy: policy = {
  version: 1,
  requiredPredicates: [],
  allowedSigners: [],
  logQuorum: 0,
  mode: Some(Permissive),
  notes: Some("Permissive policy - accepts all bundles with warnings"),
}

// Format evaluation result for logging
let formatResult = (result: evaluationResult): Js.Json.t => {
  Js.Json.object_(
    Js.Dict.fromArray([
      ("allowed", Js.Json.boolean(result.allowed)),
      ("mode", Js.Json.string(policyModeToString(result.mode))),
      (
        "predicatesFound",
        Js.Json.array(Belt.Array.map(result.predicatesFound, Js.Json.string)),
      ),
      (
        "missingPredicates",
        Js.Json.array(Belt.Array.map(result.missingPredicates, Js.Json.string)),
      ),
      (
        "signersVerified",
        Js.Json.array(Belt.Array.map(result.signersVerified, Js.Json.string)),
      ),
      (
        "invalidSigners",
        Js.Json.array(Belt.Array.map(result.invalidSigners, Js.Json.string)),
      ),
      ("logCount", Js.Json.number(Belt.Int.toFloat(result.logCount))),
      ("logQuorumMet", Js.Json.boolean(result.logQuorumMet)),
      ("violations", Js.Json.array(Belt.Array.map(result.violations, Js.Json.string))),
      ("warnings", Js.Json.array(Belt.Array.map(result.warnings, Js.Json.string))),
    ])
  )
}

// Validate policy against schema
let validatePolicy = (validator: Validation.t, policyJson: Js.Json.t): Validation.validationResult => {
  Validation.validatePolicy(validator, policyJson)
}
