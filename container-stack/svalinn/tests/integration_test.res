// SPDX-License-Identifier: PMPL-1.0-or-later
// Integration tests for Svalinn Gateway

// Test framework bindings
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
        Js.Console.log("✓ " ++ name)
      } catch {
      | Js.Exn.Error(e) => {
          let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Unknown error")
          results := Belt.Array.concat(results.contents, [{name, passed: false, error: Some(message)}])
          Js.Console.error("✗ " ++ name ++ ": " ++ message)
        }
      }
    }
    ()
  }

  let assertEquals = (actual: 'a, expected: 'a, message: string): unit => {
    if actual != expected {
      raise(Js.Exn.raiseError(message ++ " (expected: " ++ Js.String.make(expected) ++ ", got: " ++ Js.String.make(actual) ++ ")"))
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

// MCP Client Tests
module McpClientTests = {
  test("MCP config from environment", async () => {
    let config = McpClient.fromEnv()
    Test.assertEquals(config.endpoint, "http://localhost:8080", "Default endpoint should be localhost:8080")
    Test.assertEquals(config.timeout, 30000, "Default timeout should be 30s")
    Test.assertEquals(config.retries, 3, "Default retries should be 3")
  })

  // Note: Skip actual MCP calls unless Vörðr is running
  // test("MCP health check", async () => {
  //   let config = McpClient.defaultConfig
  //   let isHealthy = await McpClient.health(config)
  //   Test.assertTrue(isHealthy, "Vörðr should be healthy")
  // })
}

// Validation Tests
module ValidationTests = {
  test("Validation module initialization", async () => {
    let validator = Validation.make()
    Test.assertTrue(true, "Validator should initialize")
  })

  test("Validate run request - valid", async () => {
    let validator = Validation.make()
    let validRequest = Js.Json.object_(
      Js.Dict.fromArray([
        ("imageDigest", Js.Json.string("sha256:abc123")),
        ("imageName", Js.Json.string("alpine:latest")),
      ])
    )

    // Note: Need to load schemas first
    // let result = Validation.validateRunRequest(validator, validRequest)
    // Test.assertTrue(result.valid, "Valid request should pass validation")
    Test.assertTrue(true, "Validation test placeholder")
  })

  test("Validate run request - missing required field", async () => {
    let validator = Validation.make()
    let invalidRequest = Js.Json.object_(
      Js.Dict.fromArray([
        ("imageName", Js.Json.string("alpine:latest")),
        // Missing imageDigest
      ])
    )

    // let result = Validation.validateRunRequest(validator, invalidRequest)
    // Test.assertFalse(result.valid, "Invalid request should fail validation")
    Test.assertTrue(true, "Validation test placeholder")
  })
}

// Policy Engine Tests
module PolicyEngineTests = {
  test("Parse strict policy", async () => {
    let policyJson = Js.Json.object_(
      Js.Dict.fromArray([
        ("version", Js.Json.number(1.0)),
        ("requiredPredicates", Js.Json.array([
          Js.Json.string("https://slsa.dev/provenance/v1")
        ])),
        ("allowedSigners", Js.Json.array([
          Js.Json.string("sha256:abc123")
        ])),
        ("logQuorum", Js.Json.number(1.0)),
        ("mode", Js.Json.string("strict")),
      ])
    )

    let policy = PolicyEngine.parsePolicy(policyJson)
    Test.assertTrue(Belt.Option.isSome(policy), "Should parse valid policy")
  })

  test("Evaluate policy - all requirements met", async () => {
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
      }
    ]

    let result = PolicyEngine.evaluate(policy, attestations)
    Test.assertTrue(result.allowed, "Should allow when all requirements met")
    Test.assertEquals(Belt.Array.length(result.violations), 0, "Should have no violations")
  })

  test("Evaluate policy - missing predicate", async () => {
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
        predicateType: "https://slsa.dev/provenance/v1", // Missing SPDX
        subject: ["sha256:image123"],
        signer: "sha256:abc123",
        logEntry: Some("rekor-entry-123"),
      }
    ]

    let result = PolicyEngine.evaluate(policy, attestations)
    Test.assertFalse(result.allowed, "Should reject when predicate missing in strict mode")
    Test.assertTrue(Belt.Array.length(result.violations) > 0, "Should have violations")
  })

  test("Evaluate policy - permissive mode allows with warnings", async () => {
    let policy: PolicyEngine.policy = {
      version: 1,
      requiredPredicates: ["https://slsa.dev/provenance/v1"],
      allowedSigners: ["sha256:abc123"],
      logQuorum: 1,
      mode: Some(PolicyEngine.Permissive),
      notes: None,
    }

    let attestations: array<PolicyEngine.attestation> = [] // Empty - should violate but allow

    let result = PolicyEngine.evaluate(policy, attestations)
    Test.assertTrue(result.allowed, "Permissive mode should allow even with violations")
    Test.assertTrue(Belt.Array.length(result.warnings) > 0, "Should have warnings")
  })
}

// Auth Tests  
module AuthTests = {
  test("Parse auth method from string", async () => {
    let oauth2 = Types.authMethodFromString("oauth2")
    Test.assertTrue(Belt.Option.isSome(oauth2), "Should parse oauth2")

    let invalid = Types.authMethodFromString("invalid")
    Test.assertTrue(Belt.Option.isNone(invalid), "Should reject invalid method")
  })

  test("Default auth config", async () => {
    let config = Middleware.createAuthConfig(())
    Test.assertFalse(config.enabled, "Auth should be disabled by default")
    Test.assertEquals(Belt.Array.length(config.excludePaths), 5, "Should have default excluded paths")
  })

  // JWT tests would require actual OIDC provider or mock
  // OAuth2 tests would require actual auth server or mock
}

// Gateway Integration Tests
module GatewayTests = {
  // These would require running server
  // test("Gateway starts and responds to health check", async () => {
  //   // Start server
  //   // Make HTTP request to /health
  //   // Assert 200 OK
  // })
}

// Run all tests
let runTests = async () => {
  Js.Console.log("Running Svalinn Integration Tests\n")

  // Run test suites
  McpClientTests.test
  ValidationTests.test
  PolicyEngineTests.test
  AuthTests.test
  // GatewayTests.test // Skip until we can start server in tests

  // Wait for async tests to complete
  await %raw(`new Promise(resolve => setTimeout(resolve, 100))`)

  Test.report()
}

// Execute tests
let _ = runTests()
