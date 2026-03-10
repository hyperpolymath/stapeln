// SPDX-License-Identifier: PMPL-1.0-or-later
// Unit tests for Rokur policy engine.

import { assertEquals, assertThrows } from "@std/assert";
import { afterEach, beforeEach, describe, it } from "@std/testing/bdd";
import { createPolicyEvaluator } from "../policy/engine.js";

function secretEnvName(secretName) {
  const normalized = secretName.toUpperCase().replace(/[^A-Z0-9]/g, "_");
  return `ROKUR_SECRET_${normalized}`;
}

function setEnv(key, value) {
  Deno.env.set(key, value);
}

function deleteEnv(key) {
  try {
    Deno.env.delete(key);
  } catch { /* noop */ }
}

// Clean up any policy-related env vars before/after each test.
function cleanPolicyEnv() {
  for (
    const key of [
      "ROKUR_POLICY_BACKEND",
      "ROKUR_POLICY_COMMAND",
      "ROKUR_POLICY_COMMAND_ARGS",
      "ROKUR_POLICY_TIMEOUT_MS",
      "ROKUR_SECRET_DB_PASSWORD",
      "ROKUR_SECRET_API_KEY",
      "ROKUR_SECRET_TOKEN",
    ]
  ) {
    deleteEnv(key);
  }
}

describe("createPolicyEvaluator", () => {
  beforeEach(cleanPolicyEnv);
  afterEach(cleanPolicyEnv);

  it("creates evaluator with default builtin backend", () => {
    const evaluator = createPolicyEvaluator({
      requiredSecrets: ["db_password"],
      secretEnvName,
    });

    assertEquals(evaluator.config.resolvedBackend, "builtin");
    assertEquals(evaluator.config.externalCommandConfigured, false);
  });

  it("throws when external backend requested without command", () => {
    setEnv("ROKUR_POLICY_BACKEND", "external");

    assertThrows(
      () => createPolicyEvaluator({ requiredSecrets: ["x"], secretEnvName }),
      Error,
      "ROKUR_POLICY_BACKEND=external requires ROKUR_POLICY_COMMAND",
    );
  });

  it("throws for invalid backend value", () => {
    setEnv("ROKUR_POLICY_BACKEND", "magic");

    assertThrows(
      () => createPolicyEvaluator({ requiredSecrets: ["x"], secretEnvName }),
      Error,
      'Invalid ROKUR_POLICY_BACKEND: "magic"',
    );
  });

  it("auto backend resolves to builtin when no command configured", () => {
    setEnv("ROKUR_POLICY_BACKEND", "auto");

    const evaluator = createPolicyEvaluator({
      requiredSecrets: ["x"],
      secretEnvName,
    });

    assertEquals(evaluator.config.resolvedBackend, "builtin");
  });

  it("auto backend resolves to external when command configured", () => {
    setEnv("ROKUR_POLICY_BACKEND", "auto");
    setEnv("ROKUR_POLICY_COMMAND", "/usr/bin/echo");

    const evaluator = createPolicyEvaluator({
      requiredSecrets: ["x"],
      secretEnvName,
    });

    assertEquals(evaluator.config.resolvedBackend, "external");
    assertEquals(evaluator.config.externalCommandConfigured, true);
  });
});

describe("builtin policy evaluation", () => {
  beforeEach(cleanPolicyEnv);
  afterEach(cleanPolicyEnv);

  it("allows when all required secrets are present", async () => {
    setEnv("ROKUR_SECRET_DB_PASSWORD", "hunter2");
    setEnv("ROKUR_SECRET_API_KEY", "key123");

    const evaluator = createPolicyEvaluator({
      requiredSecrets: ["db_password", "api_key"],
      secretEnvName,
    });

    const decision = await evaluator.evaluate({});

    assertEquals(decision.allowed, true);
    assertEquals(decision.code, "AUTHORIZED");
    assertEquals(decision.engine, "builtin");
    assertEquals(decision.requiredSecretCount, 2);
    assertEquals(decision.missingSecretCount, 0);
  });

  it("denies when a required secret is missing", async () => {
    setEnv("ROKUR_SECRET_DB_PASSWORD", "hunter2");
    // api_key NOT set

    const evaluator = createPolicyEvaluator({
      requiredSecrets: ["db_password", "api_key"],
      secretEnvName,
    });

    const decision = await evaluator.evaluate({});

    assertEquals(decision.allowed, false);
    assertEquals(decision.code, "REQUIRED_SECRETS_MISSING");
    assertEquals(decision.engine, "builtin");
    assertEquals(decision.requiredSecretCount, 2);
    assertEquals(decision.missingSecretCount, 1);
  });

  it("denies when a required secret is blank", async () => {
    setEnv("ROKUR_SECRET_DB_PASSWORD", "  ");

    const evaluator = createPolicyEvaluator({
      requiredSecrets: ["db_password"],
      secretEnvName,
    });

    const decision = await evaluator.evaluate({});

    assertEquals(decision.allowed, false);
    assertEquals(decision.missingSecretCount, 1);
  });

  it("denies when all secrets are missing", async () => {
    const evaluator = createPolicyEvaluator({
      requiredSecrets: ["db_password", "api_key", "token"],
      secretEnvName,
    });

    const decision = await evaluator.evaluate({});

    assertEquals(decision.allowed, false);
    assertEquals(decision.missingSecretCount, 3);
    assertEquals(decision.requiredSecretCount, 3);
  });

  it("allows with zero required secrets (vacuously true)", async () => {
    const evaluator = createPolicyEvaluator({
      requiredSecrets: [],
      secretEnvName,
    });

    const decision = await evaluator.evaluate({});

    assertEquals(decision.allowed, true);
    assertEquals(decision.requiredSecretCount, 0);
    assertEquals(decision.missingSecretCount, 0);
  });

  it("normalizes secret names to uppercase env vars", async () => {
    setEnv("ROKUR_SECRET_MY_DB_HOST", "localhost");

    const evaluator = createPolicyEvaluator({
      requiredSecrets: ["my-db-host"],
      secretEnvName,
    });

    const decision = await evaluator.evaluate({});

    assertEquals(decision.allowed, true);
  });
});

describe("external policy evaluation", () => {
  beforeEach(cleanPolicyEnv);
  afterEach(cleanPolicyEnv);

  it("calls external command and parses allow response", async () => {
    setEnv("ROKUR_POLICY_BACKEND", "external");
    setEnv("ROKUR_POLICY_COMMAND", "deno");
    setEnv(
      "ROKUR_POLICY_COMMAND_ARGS",
      '["run","--quiet","policy/ephapax_adapter_example.js"]',
    );

    const evaluator = createPolicyEvaluator({
      requiredSecrets: ["test_secret"],
      secretEnvName,
    });

    const decision = await evaluator.evaluate({ image: "alpine:3.19" });

    // The example adapter always allows when missingSecretCount === 0
    assertEquals(decision.allowed, true);
    assertEquals(decision.engine, "external");
  });

  it("fails closed when external command does not exist", async () => {
    setEnv("ROKUR_POLICY_BACKEND", "external");
    setEnv("ROKUR_POLICY_COMMAND", "/nonexistent/command/rokur_test");

    const evaluator = createPolicyEvaluator({
      requiredSecrets: ["test_secret"],
      secretEnvName,
    });

    const decision = await evaluator.evaluate({});

    assertEquals(decision.allowed, false);
    assertEquals(decision.engine, "external");
  });

  it("fails closed on timeout", async () => {
    setEnv("ROKUR_POLICY_BACKEND", "external");
    setEnv("ROKUR_POLICY_COMMAND", "sleep");
    setEnv("ROKUR_POLICY_COMMAND_ARGS", "30");
    setEnv("ROKUR_POLICY_TIMEOUT_MS", "200");

    const evaluator = createPolicyEvaluator({
      requiredSecrets: ["test_secret"],
      secretEnvName,
    });

    const decision = await evaluator.evaluate({});

    assertEquals(decision.allowed, false);
    assertEquals(decision.engine, "external");
  });

  it("parses command args from JSON array", () => {
    setEnv("ROKUR_POLICY_BACKEND", "external");
    setEnv("ROKUR_POLICY_COMMAND", "echo");
    setEnv("ROKUR_POLICY_COMMAND_ARGS", '["--flag","value"]');

    const evaluator = createPolicyEvaluator({
      requiredSecrets: [],
      secretEnvName,
    });

    assertEquals(evaluator.config.resolvedBackend, "external");
  });

  it("parses command args from CSV", () => {
    setEnv("ROKUR_POLICY_BACKEND", "external");
    setEnv("ROKUR_POLICY_COMMAND", "echo");
    setEnv("ROKUR_POLICY_COMMAND_ARGS", "--flag,value");

    const evaluator = createPolicyEvaluator({
      requiredSecrets: [],
      secretEnvName,
    });

    assertEquals(evaluator.config.resolvedBackend, "external");
  });
});
