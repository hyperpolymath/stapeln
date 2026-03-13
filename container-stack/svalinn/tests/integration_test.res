// SPDX-License-Identifier: PMPL-1.0-or-later
// Integration tests for Svalinn Gateway
//
// Tests cover: policy engine, validation, auth types, RBAC scope checking,
// security headers configuration, MCP client config, gateway types,
// metrics collection, pattern-matched parsing, and error handling.
// MCP calls to Vordr are not exercised (no live backend); instead we test
// the pure logic that surrounds those calls.

// ---------------------------------------------------------------------------
// Test framework
// ---------------------------------------------------------------------------
module Test = {
  type testResult = {
    name: string,
    passed: bool,
    error: option<string>,
  }

  let results: ref<array<testResult>> = ref([])

  let test = (name: string, fn: unit => promise<unit>): unit => {
    let _ = async () => {
      try {
        await fn()
        results := Belt.Array.concat(results.contents, [{name, passed: true, error: None}])
        Js.Console.log("  PASS  " ++ name)
      } catch {
      | Js.Exn.Error(e) => {
          let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Unknown error")
          results := Belt.Array.concat(results.contents, [{name, passed: false, error: Some(message)}])
          Js.Console.error("  FAIL  " ++ name ++ ": " ++ message)
        }
      }
    }
    ()
  }

  let assertEquals = (actual: 'a, expected: 'a, message: string): unit => {
    if actual != expected {
      raise(
        Js.Exn.raiseError(
          message ++ " (expected: " ++ Js.String.make(expected) ++ ", got: " ++ Js.String.make(actual) ++ ")",
        ),
      )
    }
  }

  let assertTrue = (condition: bool, message: string): unit => {
    if !condition {
      raise(Js.Exn.raiseError(message))
    }
  }

  let assertFalse = (condition: bool, message: string): unit => {
    if condition {
      raise(Js.Exn.raiseError(message))
    }
  }

  let report = (): unit => {
    let total = Belt.Array.length(results.contents)
    let passed = Belt.Array.keep(results.contents, r => r.passed)->Belt.Array.length
    let failed = total - passed

    Js.Console.log("\n" ++ "=".repeat(50))
    Js.Console.log("Test Results:")
    Js.Console.log("  Total:  " ++ Belt.Int.toString(total))
    Js.Console.log("  Passed: " ++ Belt.Int.toString(passed))
    Js.Console.log("  Failed: " ++ Belt.Int.toString(failed))
    Js.Console.log("=".repeat(50))

    if failed > 0 {
      %raw(`Deno.exit(1)`)
    }
  }
}

// ===========================================================================
// 1. MCP Client Tests
// ===========================================================================
module McpClientTests = {
  Test.test("MCP config from environment uses correct defaults", async () => {
    let config = McpClient.fromEnv()
    Test.assertEquals(config.endpoint, "http://localhost:8080", "Default endpoint should be localhost:8080")
    Test.assertEquals(config.timeout, 30000, "Default timeout should be 30s")
    Test.assertEquals(config.retries, 3, "Default retries should be 3")
  })

  Test.test("MCP defaultConfig matches documented values", async () => {
    let config = McpClient.defaultConfig
    Test.assertEquals(config.endpoint, "http://localhost:8080", "Default endpoint")
    Test.assertEquals(config.timeout, 30000, "Default timeout 30s")
    Test.assertEquals(config.retries, 3, "Default retries 3")
  })
}

// ===========================================================================
// 2. Validation Tests
// ===========================================================================
module ValidationTests = {
  Test.test("Validator initializes without error", async () => {
    let _validator = Validation.make()
    Test.assertTrue(true, "Validator should initialize")
  })

  Test.test("hasRequiredFields returns true when all fields present", async () => {
    let data = Js.Json.object_(
      Js.Dict.fromArray([
        ("imageDigest", Js.Json.string("sha256:abc123")),
        ("imageName", Js.Json.string("alpine:latest")),
      ]),
    )
    let result = Validation.hasRequiredFields(data, ["imageDigest", "imageName"])
    Test.assertTrue(result, "Should detect all required fields present")
  })

  Test.test("hasRequiredFields returns false when field missing", async () => {
    let data = Js.Json.object_(
      Js.Dict.fromArray([("imageName", Js.Json.string("alpine:latest"))]),
    )
    let result = Validation.hasRequiredFields(data, ["imageDigest", "imageName"])
    Test.assertFalse(result, "Should detect missing imageDigest")
  })

  Test.test("hasRequiredFields returns false for non-object JSON", async () => {
    let data = Js.Json.string("not an object")
    let result = Validation.hasRequiredFields(data, ["field"])
    Test.assertFalse(result, "Non-object should fail required fields check")
  })

  Test.test("getString extracts string field correctly", async () => {
    let data = Js.Json.object_(
      Js.Dict.fromArray([("name", Js.Json.string("test-container"))]),
    )
    let value = Validation.getString(data, "name")
    Test.assertTrue(Belt.Option.isSome(value), "Should find name field")
    Test.assertEquals(
      Belt.Option.getWithDefault(value, ""),
      "test-container",
      "Should extract correct string value",
    )
  })

  Test.test("getString returns None for missing field", async () => {
    let data = Js.Json.object_(Js.Dict.fromArray([]))
    let value = Validation.getString(data, "missing")
    Test.assertTrue(Belt.Option.isNone(value), "Should return None for missing field")
  })

  Test.test("getBool extracts boolean field correctly", async () => {
    let data = Js.Json.object_(
      Js.Dict.fromArray([("allowed", Js.Json.boolean(true))]),
    )
    let value = Validation.getBool(data, "allowed")
    Test.assertTrue(Belt.Option.isSome(value), "Should find allowed field")
    Test.assertEquals(Belt.Option.getWithDefault(value, false), true, "Should extract true")
  })

  Test.test("getNumber extracts numeric field correctly", async () => {
    let data = Js.Json.object_(
      Js.Dict.fromArray([("port", Js.Json.number(8080.0))]),
    )
    let value = Validation.getNumber(data, "port")
    Test.assertTrue(Belt.Option.isSome(value), "Should find port field")
    Test.assertEquals(Belt.Option.getWithDefault(value, 0.0), 8080.0, "Should extract 8080")
  })

  Test.test("getArray extracts array field correctly", async () => {
    let data = Js.Json.object_(
      Js.Dict.fromArray([
        ("items", Js.Json.array([Js.Json.string("a"), Js.Json.string("b")])),
      ]),
    )
    let value = Validation.getArray(data, "items")
    Test.assertTrue(Belt.Option.isSome(value), "Should find items field")
    Test.assertEquals(
      Belt.Array.length(Belt.Option.getWithDefault(value, [])),
      2,
      "Should have 2 items",
    )
  })

  Test.test("validate returns schema-not-found for unknown schema id", async () => {
    let validator = Validation.make()
    let data = Js.Json.object_(Js.Dict.fromArray([]))
    let result = Validation.validate(validator, "nonexistent-schema", data)
    Test.assertFalse(result.valid, "Should fail for unknown schema")
    Test.assertTrue(Belt.Option.isSome(result.errors), "Should contain error details")
  })

  Test.test("isAllowedRegistry permits known registries", async () => {
    let policy = Validation.defaultPolicy
    Test.assertTrue(
      Validation.isAllowedRegistry("docker.io/library/alpine:latest", policy),
      "docker.io should be allowed",
    )
    Test.assertTrue(
      Validation.isAllowedRegistry("ghcr.io/hyperpolymath/svalinn:v1", policy),
      "ghcr.io should be allowed",
    )
    Test.assertTrue(
      Validation.isAllowedRegistry("quay.io/wolfi-base:latest", policy),
      "quay.io should be allowed",
    )
  })

  Test.test("isAllowedRegistry rejects unknown registries", async () => {
    let policy = Validation.defaultPolicy
    Test.assertFalse(
      Validation.isAllowedRegistry("evil-registry.example.com/malware:latest", policy),
      "Unknown registry should be rejected",
    )
  })

  Test.test("isDeniedImage blocks denied images", async () => {
    let policy: Validation.policy = {
      allowedRegistries: [],
      deniedImages: ["bad-image:latest"],
    }
    Test.assertTrue(
      Validation.isDeniedImage("bad-image:latest", policy),
      "Denied image should be flagged",
    )
    Test.assertFalse(
      Validation.isDeniedImage("good-image:latest", policy),
      "Non-denied image should pass",
    )
  })
}

// ===========================================================================
// 3. Policy Engine Tests
// ===========================================================================
module PolicyEngineTests = {
  // -- Parsing --

  Test.test("parsePolicy parses valid strict policy JSON", async () => {
    let policyJson = Js.Json.object_(
      Js.Dict.fromArray([
        ("version", Js.Json.number(1.0)),
        (
          "requiredPredicates",
          Js.Json.array([Js.Json.string("https://slsa.dev/provenance/v1")]),
        ),
        ("allowedSigners", Js.Json.array([Js.Json.string("sha256:abc123")])),
        ("logQuorum", Js.Json.number(1.0)),
        ("mode", Js.Json.string("strict")),
      ]),
    )

    let policy = PolicyEngine.parsePolicy(policyJson)
    Test.assertTrue(Belt.Option.isSome(policy), "Should parse valid policy")

    let p = switch policy {
    | Some(v) => v
    | None => raise(Js.Exn.raiseError("Expected Some for policy"))
    }
    Test.assertEquals(p.version, 1, "Version should be 1")
    Test.assertEquals(
      Belt.Array.length(p.requiredPredicates),
      1,
      "Should have 1 required predicate",
    )
    Test.assertEquals(
      Belt.Array.length(p.allowedSigners),
      1,
      "Should have 1 allowed signer",
    )
    Test.assertEquals(p.logQuorum, 1, "Log quorum should be 1")
    Test.assertEquals(p.mode, Some(PolicyEngine.Strict), "Mode should be Strict")
  })

  Test.test("parsePolicy parses permissive mode", async () => {
    let policyJson = Js.Json.object_(
      Js.Dict.fromArray([
        ("version", Js.Json.number(1.0)),
        ("requiredPredicates", Js.Json.array([])),
        ("allowedSigners", Js.Json.array([])),
        ("logQuorum", Js.Json.number(0.0)),
        ("mode", Js.Json.string("permissive")),
      ]),
    )
    let policy = PolicyEngine.parsePolicy(policyJson)
    Test.assertTrue(Belt.Option.isSome(policy), "Should parse permissive policy")
    let p = switch policy {
    | Some(v) => v
    | None => raise(Js.Exn.raiseError("Expected Some for permissive policy"))
    }
    Test.assertEquals(p.mode, Some(PolicyEngine.Permissive), "Mode should be Permissive")
  })

  Test.test("parsePolicy returns None for malformed JSON", async () => {
    let bad = Js.Json.string("not an object")
    let result = PolicyEngine.parsePolicy(bad)
    Test.assertTrue(Belt.Option.isNone(result), "Should return None for non-object")
  })

  Test.test("parsePolicy returns None when required fields missing", async () => {
    let partial = Js.Json.object_(
      Js.Dict.fromArray([("version", Js.Json.number(1.0))]),
    )
    let result = PolicyEngine.parsePolicy(partial)
    Test.assertTrue(Belt.Option.isNone(result), "Should return None when requiredPredicates missing")
  })

  // -- Evaluation: strict mode --

  Test.test("Strict: allow when all requirements met", async () => {
    let policy: PolicyEngine.policy = {
      version: 1,
      requiredPredicates: ["https://slsa.dev/provenance/v1"],
      allowedSigners: ["sha256:abc123"],
      logQuorum: 1,
      mode: Some(PolicyEngine.Strict),
      notes: None,
    }

    let attestations: array<PolicyEngine.attestation> = [
      {
        predicateType: "https://slsa.dev/provenance/v1",
        subject: ["sha256:image123"],
        signer: "sha256:abc123",
        logEntry: Some("rekor-entry-123"),
      },
    ]

    let result = PolicyEngine.evaluate(policy, attestations)
    Test.assertTrue(result.allowed, "Should allow when all requirements met")
    Test.assertEquals(Belt.Array.length(result.violations), 0, "Should have no violations")
    Test.assertEquals(Belt.Array.length(result.predicatesFound), 1, "Should find 1 predicate")
    Test.assertEquals(Belt.Array.length(result.signersVerified), 1, "Should verify 1 signer")
    Test.assertTrue(result.logQuorumMet, "Log quorum should be met")
  })

  Test.test("Strict: reject when required predicate missing", async () => {
    let policy: PolicyEngine.policy = {
      version: 1,
      requiredPredicates: ["https://slsa.dev/provenance/v1", "https://spdx.dev/Document"],
      allowedSigners: ["sha256:abc123"],
      logQuorum: 1,
      mode: Some(PolicyEngine.Strict),
      notes: None,
    }

    let attestations: array<PolicyEngine.attestation> = [
      {
        predicateType: "https://slsa.dev/provenance/v1",
        subject: ["sha256:image123"],
        signer: "sha256:abc123",
        logEntry: Some("rekor-entry-123"),
      },
    ]

    let result = PolicyEngine.evaluate(policy, attestations)
    Test.assertFalse(result.allowed, "Should reject when predicate missing in strict mode")
    Test.assertTrue(Belt.Array.length(result.violations) > 0, "Should have violations")
    Test.assertEquals(
      Belt.Array.length(result.missingPredicates),
      1,
      "Should report 1 missing predicate",
    )
    Test.assertEquals(
      result.missingPredicates->Belt.Array.get(0)->Belt.Option.getWithDefault(""),
      "https://spdx.dev/Document",
      "Missing predicate should be spdx.dev/Document",
    )
  })

  Test.test("Strict: reject when signer not in allowed list", async () => {
    let policy: PolicyEngine.policy = {
      version: 1,
      requiredPredicates: ["https://slsa.dev/provenance/v1"],
      allowedSigners: ["sha256:trusted-key"],
      logQuorum: 1,
      mode: Some(PolicyEngine.Strict),
      notes: None,
    }

    let attestations: array<PolicyEngine.attestation> = [
      {
        predicateType: "https://slsa.dev/provenance/v1",
        subject: ["sha256:image123"],
        signer: "sha256:untrusted-key",
        logEntry: Some("rekor-entry-456"),
      },
    ]

    let result = PolicyEngine.evaluate(policy, attestations)
    Test.assertFalse(result.allowed, "Should reject untrusted signer in strict mode")
    Test.assertEquals(
      Belt.Array.length(result.invalidSigners),
      1,
      "Should report 1 invalid signer",
    )
  })

  Test.test("Strict: reject when log quorum not met", async () => {
    let policy: PolicyEngine.policy = {
      version: 1,
      requiredPredicates: ["https://slsa.dev/provenance/v1"],
      allowedSigners: ["sha256:abc123"],
      logQuorum: 2,
      mode: Some(PolicyEngine.Strict),
      notes: None,
    }

    let attestations: array<PolicyEngine.attestation> = [
      {
        predicateType: "https://slsa.dev/provenance/v1",
        subject: ["sha256:image123"],
        signer: "sha256:abc123",
        logEntry: Some("single-entry"),
      },
    ]

    let result = PolicyEngine.evaluate(policy, attestations)
    Test.assertFalse(result.allowed, "Should reject when log quorum not met")
    Test.assertFalse(result.logQuorumMet, "logQuorumMet should be false")
    Test.assertEquals(result.logCount, 1, "Should count 1 log entry")
  })

  Test.test("Strict: reject with empty attestations", async () => {
    let policy: PolicyEngine.policy = {
      version: 1,
      requiredPredicates: ["https://slsa.dev/provenance/v1"],
      allowedSigners: ["sha256:abc123"],
      logQuorum: 1,
      mode: Some(PolicyEngine.Strict),
      notes: None,
    }

    let attestations: array<PolicyEngine.attestation> = []

    let result = PolicyEngine.evaluate(policy, attestations)
    Test.assertFalse(result.allowed, "Empty attestations should be rejected in strict mode")
    Test.assertTrue(Belt.Array.length(result.violations) > 0, "Should have violations")
  })

  // -- Evaluation: permissive mode --

  Test.test("Permissive: allow even with violations, emit warnings", async () => {
    let policy: PolicyEngine.policy = {
      version: 1,
      requiredPredicates: ["https://slsa.dev/provenance/v1"],
      allowedSigners: ["sha256:abc123"],
      logQuorum: 1,
      mode: Some(PolicyEngine.Permissive),
      notes: None,
    }

    let attestations: array<PolicyEngine.attestation> = []

    let result = PolicyEngine.evaluate(policy, attestations)
    Test.assertTrue(result.allowed, "Permissive mode should allow even with violations")
    Test.assertTrue(Belt.Array.length(result.warnings) > 0, "Should have warnings")
  })

  Test.test("Permissive: violations become warnings, not blockers", async () => {
    let policy: PolicyEngine.policy = {
      version: 1,
      requiredPredicates: ["https://slsa.dev/provenance/v1", "https://spdx.dev/Document"],
      allowedSigners: ["sha256:trusted"],
      logQuorum: 3,
      mode: Some(PolicyEngine.Permissive),
      notes: None,
    }

    let attestations: array<PolicyEngine.attestation> = [
      {
        predicateType: "https://slsa.dev/provenance/v1",
        subject: ["sha256:img"],
        signer: "sha256:unknown",
        logEntry: None,
      },
    ]

    let result = PolicyEngine.evaluate(policy, attestations)
    Test.assertTrue(result.allowed, "Should be allowed in permissive mode")
    // Missing predicate (spdx), invalid signer, log quorum not met
    Test.assertTrue(Belt.Array.length(result.warnings) >= 2, "Should have multiple warnings")
  })

  // -- Mode conversion --

  Test.test("policyModeFromString round-trips correctly", async () => {
    Test.assertEquals(
      PolicyEngine.policyModeFromString("strict"),
      Some(PolicyEngine.Strict),
      "strict string",
    )
    Test.assertEquals(
      PolicyEngine.policyModeFromString("permissive"),
      Some(PolicyEngine.Permissive),
      "permissive string",
    )
    Test.assertTrue(
      Belt.Option.isNone(PolicyEngine.policyModeFromString("invalid")),
      "invalid should return None",
    )
  })

  Test.test("policyModeToString produces expected strings", async () => {
    Test.assertEquals(PolicyEngine.policyModeToString(Strict), "strict", "Strict to string")
    Test.assertEquals(
      PolicyEngine.policyModeToString(Permissive),
      "permissive",
      "Permissive to string",
    )
  })

  // -- Default policies --

  Test.test("defaultPolicy is strict with SLSA and SPDX predicates", async () => {
    let p = PolicyEngine.defaultPolicy
    Test.assertEquals(p.mode, Some(PolicyEngine.Strict), "Default should be strict")
    Test.assertEquals(
      Belt.Array.length(p.requiredPredicates),
      2,
      "Default should require 2 predicates",
    )
    Test.assertTrue(
      Belt.Array.some(p.requiredPredicates, pred => pred == "https://slsa.dev/provenance/v1"),
      "Should require SLSA provenance",
    )
    Test.assertTrue(
      Belt.Array.some(p.requiredPredicates, pred => pred == "https://spdx.dev/Document"),
      "Should require SPDX document",
    )
  })

  Test.test("permissivePolicy allows everything", async () => {
    let p = PolicyEngine.permissivePolicy
    Test.assertEquals(p.mode, Some(PolicyEngine.Permissive), "Should be permissive")
    Test.assertEquals(p.logQuorum, 0, "Should have zero log quorum")
    Test.assertEquals(
      Belt.Array.length(p.requiredPredicates),
      0,
      "Should have no required predicates",
    )
  })

  // -- formatResult --

  Test.test("formatResult produces well-formed JSON", async () => {
    let evalResult: PolicyEngine.evaluationResult = {
      allowed: true,
      mode: PolicyEngine.Strict,
      predicatesFound: ["https://slsa.dev/provenance/v1"],
      missingPredicates: [],
      signersVerified: ["sha256:abc"],
      invalidSigners: [],
      logCount: 1,
      logQuorumMet: true,
      violations: [],
      warnings: [],
    }

    let json = PolicyEngine.formatResult(evalResult)
    let obj = json->Js.Json.decodeObject
    Test.assertTrue(Belt.Option.isSome(obj), "Should be a JSON object")

    let dict = switch obj {
    | Some(v) => v
    | None => raise(Js.Exn.raiseError("Expected formatResult to return a JSON object"))
    }
    let allowed = dict->Js.Dict.get("allowed")->Belt.Option.flatMap(Js.Json.decodeBoolean)
    Test.assertEquals(allowed, Some(true), "allowed field should be true")

    let mode = dict->Js.Dict.get("mode")->Belt.Option.flatMap(Js.Json.decodeString)
    Test.assertEquals(mode, Some("strict"), "mode field should be 'strict'")
  })

  // -- Attestation parsing --

  Test.test("parseAttestation parses valid attestation JSON", async () => {
    let json = Js.Json.object_(
      Js.Dict.fromArray([
        ("predicateType", Js.Json.string("https://slsa.dev/provenance/v1")),
        ("subject", Js.Json.array([Js.Json.string("sha256:img123")])),
        ("signer", Js.Json.string("sha256:signer-key")),
        ("logEntry", Js.Json.string("rekor-entry-789")),
      ]),
    )

    let att = PolicyEngine.parseAttestation(json)
    Test.assertTrue(Belt.Option.isSome(att), "Should parse valid attestation")
    let a = switch att {
    | Some(v) => v
    | None => raise(Js.Exn.raiseError("Expected Some for attestation"))
    }
    Test.assertEquals(a.predicateType, "https://slsa.dev/provenance/v1", "predicateType")
    Test.assertEquals(a.signer, "sha256:signer-key", "signer")
    Test.assertEquals(a.logEntry, Some("rekor-entry-789"), "logEntry")
  })

  Test.test("parseAttestation returns None for invalid JSON", async () => {
    let bad = Js.Json.string("bad data")
    Test.assertTrue(
      Belt.Option.isNone(PolicyEngine.parseAttestation(bad)),
      "Should return None for non-object",
    )
  })
}

// ===========================================================================
// 4. Auth Types Tests
// ===========================================================================
module AuthTypesTests = {
  // -- authMethod conversion --

  Test.test("authMethodFromString parses all valid methods", async () => {
    Test.assertEquals(AuthTypes.authMethodFromString("oauth2"), Some(AuthTypes.OAuth2), "oauth2")
    Test.assertEquals(AuthTypes.authMethodFromString("oidc"), Some(AuthTypes.OIDC), "oidc")
    Test.assertEquals(AuthTypes.authMethodFromString("api-key"), Some(AuthTypes.ApiKey), "api-key")
    Test.assertEquals(AuthTypes.authMethodFromString("mtls"), Some(AuthTypes.MTLS), "mtls")
    Test.assertEquals(AuthTypes.authMethodFromString("none"), Some(AuthTypes.None), "none")
  })

  Test.test("authMethodFromString rejects unknown methods", async () => {
    Test.assertTrue(
      Belt.Option.isNone(AuthTypes.authMethodFromString("invalid")),
      "Should reject 'invalid'",
    )
    Test.assertTrue(
      Belt.Option.isNone(AuthTypes.authMethodFromString("basic")),
      "Should reject 'basic'",
    )
    Test.assertTrue(Belt.Option.isNone(AuthTypes.authMethodFromString("")), "Should reject empty")
  })

  Test.test("authMethodToString round-trips with authMethodFromString", async () => {
    let methods: array<AuthTypes.authMethod> = [
      AuthTypes.OAuth2,
      AuthTypes.OIDC,
      AuthTypes.ApiKey,
      AuthTypes.MTLS,
      AuthTypes.None,
    ]
    Belt.Array.forEach(methods, method => {
      let str = AuthTypes.authMethodToString(method)
      let parsed = AuthTypes.authMethodFromString(str)
      Test.assertTrue(
        Belt.Option.isSome(parsed),
        "Round-trip should succeed for " ++ str,
      )
    })
  })

  // -- permissionAction conversion --

  Test.test("permissionActionFromString parses CRUD + Execute", async () => {
    Test.assertEquals(
      AuthTypes.permissionActionFromString("create"),
      Some(AuthTypes.Create),
      "create",
    )
    Test.assertEquals(
      AuthTypes.permissionActionFromString("read"),
      Some(AuthTypes.Read),
      "read",
    )
    Test.assertEquals(
      AuthTypes.permissionActionFromString("update"),
      Some(AuthTypes.Update),
      "update",
    )
    Test.assertEquals(
      AuthTypes.permissionActionFromString("delete"),
      Some(AuthTypes.Delete),
      "delete",
    )
    Test.assertEquals(
      AuthTypes.permissionActionFromString("execute"),
      Some(AuthTypes.Execute),
      "execute",
    )
  })

  Test.test("permissionActionFromString rejects invalid actions", async () => {
    Test.assertTrue(
      Belt.Option.isNone(AuthTypes.permissionActionFromString("admin")),
      "Should reject 'admin'",
    )
  })

  Test.test("permissionActionToString round-trips correctly", async () => {
    let actions: array<AuthTypes.permissionAction> = [
      AuthTypes.Create,
      AuthTypes.Read,
      AuthTypes.Update,
      AuthTypes.Delete,
      AuthTypes.Execute,
    ]
    Belt.Array.forEach(actions, action => {
      let str = AuthTypes.permissionActionToString(action)
      let parsed = AuthTypes.permissionActionFromString(str)
      Test.assertTrue(Belt.Option.isSome(parsed), "Round-trip should succeed for " ++ str)
    })
  })

  // -- RBAC default roles --

  Test.test("defaultRoles has 4 roles: admin, operator, viewer, auditor", async () => {
    let roles = AuthTypes.defaultRoles
    Test.assertEquals(Belt.Array.length(roles), 4, "Should have 4 default roles")

    let roleNames = Belt.Array.map(roles, r => r.name)
    Test.assertTrue(Belt.Array.some(roleNames, n => n == "admin"), "Should have admin role")
    Test.assertTrue(Belt.Array.some(roleNames, n => n == "operator"), "Should have operator role")
    Test.assertTrue(Belt.Array.some(roleNames, n => n == "viewer"), "Should have viewer role")
    Test.assertTrue(Belt.Array.some(roleNames, n => n == "auditor"), "Should have auditor role")
  })

  Test.test("admin role has wildcard resource with all actions", async () => {
    let adminRole = switch Belt.Array.keep(AuthTypes.defaultRoles, r => r.name == "admin")->Belt.Array.get(0) {
    | Some(v) => v
    | None => raise(Js.Exn.raiseError("Expected admin role to exist"))
    }

    Test.assertEquals(Belt.Array.length(adminRole.permissions), 1, "Admin should have 1 permission")
    let perm = switch Belt.Array.get(adminRole.permissions, 0) {
    | Some(v) => v
    | None => raise(Js.Exn.raiseError("Expected admin permissions[0] to exist"))
    }
    Test.assertEquals(perm.resource, "*", "Admin resource should be wildcard")
    Test.assertEquals(Belt.Array.length(perm.actions), 5, "Admin should have all 5 actions")
  })

  Test.test("operator role can manage containers but only read policies", async () => {
    let operatorRole = switch Belt.Array.keep(AuthTypes.defaultRoles, r => r.name == "operator")->Belt.Array.get(0) {
    | Some(v) => v
    | None => raise(Js.Exn.raiseError("Expected operator role to exist"))
    }

    // Container permission should have all CRUD + Execute
    let containerPerm = switch Belt.Array.keep(operatorRole.permissions, p => p.resource == "containers")->Belt.Array.get(0) {
    | Some(v) => v
    | None => raise(Js.Exn.raiseError("Expected operator containers permission to exist"))
    }
    Test.assertEquals(
      Belt.Array.length(containerPerm.actions),
      5,
      "Operator should have full container access",
    )

    // Policy permission should be read-only
    let policyPerm = switch Belt.Array.keep(operatorRole.permissions, p => p.resource == "policies")->Belt.Array.get(0) {
    | Some(v) => v
    | None => raise(Js.Exn.raiseError("Expected operator policies permission to exist"))
    }
    Test.assertEquals(
      Belt.Array.length(policyPerm.actions),
      1,
      "Operator should have read-only policy access",
    )
    Test.assertEquals(
      switch Belt.Array.get(policyPerm.actions, 0) {
      | Some(v) => v
      | None => raise(Js.Exn.raiseError("Expected policy actions[0] to exist"))
      },
      AuthTypes.Read,
      "Policy action should be Read",
    )
  })

  Test.test("viewer role is read-only across all resources", async () => {
    let viewerRole = switch Belt.Array.keep(AuthTypes.defaultRoles, r => r.name == "viewer")->Belt.Array.get(0) {
    | Some(v) => v
    | None => raise(Js.Exn.raiseError("Expected viewer role to exist"))
    }

    Belt.Array.forEach(viewerRole.permissions, perm => {
      Test.assertEquals(
        Belt.Array.length(perm.actions),
        1,
        "Viewer should have exactly 1 action per resource",
      )
      Test.assertEquals(
        switch Belt.Array.get(perm.actions, 0) {
        | Some(v) => v
        | None => raise(Js.Exn.raiseError("Expected viewer actions[0] to exist for " ++ perm.resource))
        },
        AuthTypes.Read,
        "Viewer action should always be Read for resource " ++ perm.resource,
      )
    })
  })

  // -- Default scopes --

  Test.test("defaultScopes contains expected scope keys", async () => {
    let scopes = AuthTypes.defaultScopes
    Test.assertTrue(
      Belt.Map.String.has(scopes, "svalinn:read"),
      "Should have svalinn:read scope",
    )
    Test.assertTrue(
      Belt.Map.String.has(scopes, "svalinn:write"),
      "Should have svalinn:write scope",
    )
    Test.assertTrue(
      Belt.Map.String.has(scopes, "svalinn:admin"),
      "Should have svalinn:admin scope",
    )
    Test.assertTrue(
      Belt.Map.String.has(scopes, "containers:create"),
      "Should have containers:create scope",
    )
    Test.assertTrue(
      Belt.Map.String.has(scopes, "containers:read"),
      "Should have containers:read scope",
    )
    Test.assertTrue(
      Belt.Map.String.has(scopes, "containers:delete"),
      "Should have containers:delete scope",
    )
    Test.assertTrue(
      Belt.Map.String.has(scopes, "images:verify"),
      "Should have images:verify scope",
    )
    Test.assertTrue(
      Belt.Map.String.has(scopes, "policies:manage"),
      "Should have policies:manage scope",
    )
  })
}

// ===========================================================================
// 5. Auth Middleware Tests
// ===========================================================================
module AuthMiddlewareTests = {
  Test.test("createAuthConfig produces disabled auth by default", async () => {
    let config = Middleware.createAuthConfig(())
    Test.assertFalse(config.enabled, "Auth should be disabled by default")
    Test.assertEquals(
      Belt.Array.length(config.excludePaths),
      5,
      "Should have 5 default excluded paths",
    )
  })

  Test.test("Default excluded paths include health and metrics", async () => {
    let config = Middleware.createAuthConfig(())
    let paths = config.excludePaths
    Test.assertTrue(Belt.Array.some(paths, p => p == "/healthz"), "Should exclude /healthz")
    Test.assertTrue(Belt.Array.some(paths, p => p == "/health"), "Should exclude /health")
    Test.assertTrue(Belt.Array.some(paths, p => p == "/ready"), "Should exclude /ready")
    Test.assertTrue(Belt.Array.some(paths, p => p == "/metrics"), "Should exclude /metrics")
    Test.assertTrue(
      Belt.Array.some(paths, p => p == "/.well-known/"),
      "Should exclude /.well-known/",
    )
  })

  Test.test("Default auth methods are OIDC and ApiKey", async () => {
    let config = Middleware.createAuthConfig(())
    Test.assertEquals(Belt.Array.length(config.methods), 2, "Should have 2 auth methods")
    Test.assertEquals(
      switch Belt.Array.get(config.methods, 0) {
      | Some(v) => v
      | None => raise(Js.Exn.raiseError("Expected methods[0] to exist"))
      },
      AuthTypes.OIDC,
      "First method should be OIDC",
    )
    Test.assertEquals(
      switch Belt.Array.get(config.methods, 1) {
      | Some(v) => v
      | None => raise(Js.Exn.raiseError("Expected methods[1] to exist"))
      },
      AuthTypes.ApiKey,
      "Second method should be ApiKey",
    )
  })

  Test.test("Default apiKey config uses X-API-Key header", async () => {
    let config = Middleware.createAuthConfig(())
    Test.assertTrue(Belt.Option.isSome(config.apiKey), "Should have apiKey config")
    let apiKeyConfig = switch config.apiKey {
    | Some(v) => v
    | None => raise(Js.Exn.raiseError("Expected apiKey config to exist"))
    }
    Test.assertEquals(apiKeyConfig.header, "X-API-Key", "Header should be X-API-Key")
    Test.assertTrue(Belt.Option.isNone(apiKeyConfig.prefix), "No prefix by default")
  })
}

// ===========================================================================
// 6. Security Headers Tests (configuration-level validation)
// ===========================================================================
module SecurityHeadersTests = {
  // SecurityHeaders operates on Hono context objects which require a running
  // HTTP server. Here we verify the module structure and constants are
  // accessible. Full header-presence tests would run against a live server.

  Test.test("SecurityHeaders module exports applySecurityHeaders function", async () => {
    // Confirm the function exists at the module level. If it were missing
    // this would be a compile error, but we keep it as a runtime sanity check.
    let _fn = SecurityHeaders.applySecurityHeaders
    Test.assertTrue(true, "applySecurityHeaders is exported")
  })

  Test.test("SecurityHeaders module exports middleware function", async () => {
    let _fn = SecurityHeaders.middleware
    Test.assertTrue(true, "middleware is exported")
  })

  Test.test("SecurityHeaders module exports applyCorsHeaders function", async () => {
    let _fn = SecurityHeaders.applyCorsHeaders
    Test.assertTrue(true, "applyCorsHeaders is exported")
  })

  Test.test("SecurityHeaders module exports applyRateLimitHeaders function", async () => {
    let _fn = SecurityHeaders.applyRateLimitHeaders
    Test.assertTrue(true, "applyRateLimitHeaders is exported")
  })

  Test.test("SecurityHeaders module exports applyErrorHeaders function", async () => {
    let _fn = SecurityHeaders.applyErrorHeaders
    Test.assertTrue(true, "applyErrorHeaders is exported")
  })
}

// ===========================================================================
// 7. Gateway Types Tests
// ===========================================================================
module GatewayTypesTests = {
  Test.test("containerState variants are correctly defined", async () => {
    // Verify variant construction compiles and the set is complete
    let states: array<GatewayTypes.containerState> = [
      GatewayTypes.Created,
      GatewayTypes.Running,
      GatewayTypes.Paused,
      GatewayTypes.Stopped,
      GatewayTypes.Removed,
    ]
    Test.assertEquals(Belt.Array.length(states), 5, "Should have 5 container states")
  })

  Test.test("runRequest type constructs with all fields", async () => {
    let req: GatewayTypes.runRequest = {
      imageName: "alpine",
      imageDigest: "sha256:abc123",
      name: Some("test-container"),
      command: Some(["sh", "-c", "echo hello"]),
      env: Some(Js.Dict.fromArray([("FOO", "bar")])),
      detach: Some(true),
      removeOnExit: Some(false),
      profile: Some("default"),
    }
    Test.assertEquals(req.imageName, "alpine", "imageName")
    Test.assertEquals(req.imageDigest, "sha256:abc123", "imageDigest")
    Test.assertTrue(Belt.Option.isSome(req.name), "name should be Some")
    Test.assertTrue(Belt.Option.isSome(req.command), "command should be Some")
  })

  Test.test("apiResponse Ok and Error variants construct correctly", async () => {
    let ok: GatewayTypes.apiResponse<string> = GatewayTypes.Ok("success")
    let err: GatewayTypes.apiResponse<string> = GatewayTypes.Error({
      code: "NOT_FOUND",
      message: "Container not found",
      details: None,
    })

    switch ok {
    | GatewayTypes.Ok(v) => Test.assertEquals(v, "success", "Ok value")
    | GatewayTypes.Error(_) => Test.assertTrue(false, "Should be Ok")
    }

    switch err {
    | GatewayTypes.Ok(_) => Test.assertTrue(false, "Should be Error")
    | GatewayTypes.Error(e) =>
      Test.assertEquals(e.code, "NOT_FOUND", "Error code should be NOT_FOUND")
    }
  })
}

// ===========================================================================
// 8. Metrics Tests
// ===========================================================================
module MetricsTests = {
  Test.test("Counter starts at zero and increments", async () => {
    let counter = Metrics.makeCounter(~name="test_counter", ~help="Test")
    Test.assertEquals(counter.value, 0.0, "Counter starts at 0")
    Metrics.increment(counter)
    Test.assertEquals(counter.value, 1.0, "Counter is 1 after increment")
    Metrics.incrementBy(counter, 5.0)
    Test.assertEquals(counter.value, 6.0, "Counter is 6 after incrementBy(5)")
  })

  Test.test("Gauge can be set to arbitrary values", async () => {
    let gauge = Metrics.makeGauge(~name="test_gauge", ~help="Test")
    Test.assertEquals(gauge.value, 0.0, "Gauge starts at 0")
    Metrics.setGauge(gauge, 42.0)
    Test.assertEquals(gauge.value, 42.0, "Gauge is 42 after set")
    Metrics.setGauge(gauge, 0.0)
    Test.assertEquals(gauge.value, 0.0, "Gauge back to 0")
  })

  Test.test("Histogram observe updates sum, count, and buckets", async () => {
    let h = Metrics.makeHistogram(
      ~name="test_hist",
      ~help="Test",
      ~buckets=[0.1, 0.5, 1.0],
    )
    Test.assertEquals(h.sum, 0.0, "Sum starts at 0")
    Test.assertEquals(h.count, 0.0, "Count starts at 0")

    Metrics.observe(h, 0.05)
    Test.assertEquals(h.count, 1.0, "Count is 1 after one observation")
    Test.assertEquals(h.sum, 0.05, "Sum is 0.05")

    Metrics.observe(h, 0.3)
    Test.assertEquals(h.count, 2.0, "Count is 2 after two observations")

    Metrics.observe(h, 2.0)
    Test.assertEquals(h.count, 3.0, "Count is 3")
  })

  Test.test("formatCounter produces Prometheus text format", async () => {
    let counter = Metrics.makeCounter(~name="http_total", ~help="Total requests")
    Metrics.increment(counter)
    let output = Metrics.formatCounter(counter)
    Test.assertTrue(
      Js.String2.includes(output, "# HELP http_total Total requests"),
      "Should contain HELP line",
    )
    Test.assertTrue(
      Js.String2.includes(output, "# TYPE http_total counter"),
      "Should contain TYPE line",
    )
    Test.assertTrue(
      Js.String2.includes(output, "http_total 1"),
      "Should contain value",
    )
  })

  Test.test("formatGauge produces Prometheus text format", async () => {
    let gauge = Metrics.makeGauge(~name="active_conns", ~help="Active connections")
    Metrics.setGauge(gauge, 5.0)
    let output = Metrics.formatGauge(gauge)
    Test.assertTrue(
      Js.String2.includes(output, "# TYPE active_conns gauge"),
      "Should contain gauge TYPE",
    )
    Test.assertTrue(
      Js.String2.includes(output, "active_conns 5"),
      "Should contain value 5",
    )
  })

  Test.test("formatHistogram produces cumulative bucket lines", async () => {
    let h = Metrics.makeHistogram(
      ~name="duration",
      ~help="Duration",
      ~buckets=[0.1, 1.0],
    )
    Metrics.observe(h, 0.05)
    Metrics.observe(h, 0.5)
    let output = Metrics.formatHistogram(h)
    Test.assertTrue(
      Js.String2.includes(output, "# TYPE duration histogram"),
      "Should contain histogram TYPE",
    )
    Test.assertTrue(
      Js.String2.includes(output, "duration_sum"),
      "Should contain sum line",
    )
    Test.assertTrue(
      Js.String2.includes(output, "duration_count 2"),
      "Should contain count",
    )
    Test.assertTrue(
      Js.String2.includes(output, `le="+Inf"`),
      "Should contain +Inf bucket",
    )
  })

  // Sentinel for module init
  let test = ()
}

// ===========================================================================
// 9. PolicyEngine Pattern-Match Tests
// ===========================================================================
module PolicyEngineParsingTests = {
  Test.test("parsePolicy returns None for non-object JSON", async () => {
    let result = PolicyEngine.parsePolicy(Js.Json.string("not an object"))
    Test.assertTrue(Belt.Option.isNone(result), "String input should return None")
  })

  Test.test("parsePolicy returns None for missing required fields", async () => {
    let json = Js.Json.object_(Js.Dict.fromArray([
      ("version", Js.Json.number(1.0)),
      // missing requiredPredicates, allowedSigners, logQuorum
    ]))
    let result = PolicyEngine.parsePolicy(json)
    Test.assertTrue(Belt.Option.isNone(result), "Missing fields should return None")
  })

  Test.test("parsePolicy succeeds with all required fields", async () => {
    let json = Js.Json.object_(Js.Dict.fromArray([
      ("version", Js.Json.number(1.0)),
      ("requiredPredicates", Js.Json.array([Js.Json.string("https://slsa.dev/provenance/v1")])),
      ("allowedSigners", Js.Json.array([Js.Json.string("signer-1")])),
      ("logQuorum", Js.Json.number(1.0)),
    ]))
    let result = PolicyEngine.parsePolicy(json)
    Test.assertTrue(Belt.Option.isSome(result), "Valid policy should parse")
  })

  Test.test("parsePolicy preserves optional mode field", async () => {
    let json = Js.Json.object_(Js.Dict.fromArray([
      ("version", Js.Json.number(1.0)),
      ("requiredPredicates", Js.Json.array([])),
      ("allowedSigners", Js.Json.array([])),
      ("logQuorum", Js.Json.number(0.0)),
      ("mode", Js.Json.string("permissive")),
    ]))
    let result = PolicyEngine.parsePolicy(json)
    Test.assertTrue(Belt.Option.isSome(result), "Should parse with mode")
    switch result {
    | Some(p) =>
      Test.assertTrue(Belt.Option.isSome(p.mode), "Mode should be Some")
    | None => Test.assertTrue(false, "Should have parsed")
    }
  })

  Test.test("parseAttestation returns None for non-object JSON", async () => {
    let result = PolicyEngine.parseAttestation(Js.Json.number(42.0))
    Test.assertTrue(Belt.Option.isNone(result), "Number input should return None")
  })

  Test.test("parseAttestation returns None when predicateType missing", async () => {
    let json = Js.Json.object_(Js.Dict.fromArray([
      ("signer", Js.Json.string("signer-1")),
    ]))
    let result = PolicyEngine.parseAttestation(json)
    Test.assertTrue(Belt.Option.isNone(result), "Missing predicateType should return None")
  })

  Test.test("parseAttestation returns None when signer missing", async () => {
    let json = Js.Json.object_(Js.Dict.fromArray([
      ("predicateType", Js.Json.string("https://slsa.dev/provenance/v1")),
    ]))
    let result = PolicyEngine.parseAttestation(json)
    Test.assertTrue(Belt.Option.isNone(result), "Missing signer should return None")
  })

  Test.test("parseAttestation succeeds with required fields", async () => {
    let json = Js.Json.object_(Js.Dict.fromArray([
      ("predicateType", Js.Json.string("https://slsa.dev/provenance/v1")),
      ("signer", Js.Json.string("signer-1")),
    ]))
    let result = PolicyEngine.parseAttestation(json)
    Test.assertTrue(Belt.Option.isSome(result), "Valid attestation should parse")
  })

  let test = ()
}

// ===========================================================================
// 10. McpClient Error Handling Tests
// ===========================================================================
module McpClientErrorTests = {
  Test.test("MCP health returns false when endpoint unreachable", async () => {
    // Use a port that's almost certainly not listening
    let config: McpClient.config = {
      endpoint: "http://127.0.0.1:19999",
      timeout: 500,
      retries: 0,
    }
    let result = await McpClient.health(config)
    Test.assertFalse(result, "Health should be false for unreachable endpoint")
  })

  let test = ()
}

// ===========================================================================
// Run all tests
// ===========================================================================
let runTests = async () => {
  Js.Console.log("Running Svalinn Integration Tests\n")

  // Trigger all test module side effects (tests are registered at module init)
  McpClientTests.test
  ValidationTests.test
  PolicyEngineTests.test
  AuthTypesTests.test
  AuthMiddlewareTests.test
  SecurityHeadersTests.test
  GatewayTypesTests.test
  MetricsTests.test
  PolicyEngineParsingTests.test
  McpClientErrorTests.test

  // Wait for async tests to settle
  await %raw(`new Promise(resolve => setTimeout(resolve, 200))`)

  Test.report()
}

// Execute tests
let _ = runTests()
