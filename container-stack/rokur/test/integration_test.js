// SPDX-License-Identifier: PMPL-1.0-or-later
// Integration tests for Rokur HTTP server.
// Starts the actual server and tests all endpoints.

import { assertEquals, assertExists } from "@std/assert";
import { afterAll, beforeAll, describe, it } from "@std/testing/bdd";

// Integration tests manage server subprocesses across beforeAll/afterAll boundaries,
// which triggers Deno's resource leak sanitizer. Disable sanitizers for these suites.
const suiteOpts = { sanitizeResources: false, sanitizeOps: false };

const TEST_PORT = 19090;
const BASE_URL = `http://127.0.0.1:${TEST_PORT}`;
const API_TOKEN = "test-token-rokur-integration";

let serverProcess;

async function waitForServer(url, maxAttempts = 30) {
  for (let i = 0; i < maxAttempts; i++) {
    try {
      const response = await fetch(`${url}/health`);
      if (response.ok) return;
    } catch {
      // Server not ready yet.
    }
    await new Promise((resolve) => setTimeout(resolve, 100));
  }
  throw new Error(`Server at ${url} did not become ready`);
}

describe("Rokur HTTP integration", suiteOpts, () => {
  beforeAll(async () => {
    serverProcess = new Deno.Command("deno", {
      args: ["run", "--allow-net", "--allow-env", "main.js"],
      cwd: new URL("..", import.meta.url).pathname,
      env: {
        ...Deno.env.toObject(),
        ROKUR_HOST: "127.0.0.1",
        ROKUR_PORT: String(TEST_PORT),
        ROKUR_ENV: "development",
        ROKUR_API_TOKEN: API_TOKEN,
        ROKUR_REQUIRED_SECRETS: "db_password,api_key",
        ROKUR_SECRET_DB_PASSWORD: "hunter2",
        ROKUR_SECRET_API_KEY: "key-abc-123",
        ROKUR_AUDIT_LOG: "false",
        ROKUR_REQUEST_LOG: "false",
        ROKUR_RATE_LIMIT_MAX: "1000",
      },
      stdout: "null",
      stderr: "null",
    }).spawn();

    await waitForServer(BASE_URL);
  });

  afterAll(() => {
    try {
      serverProcess.kill("SIGTERM");
    } catch {
      // Already exited.
    }
  });

  // -----------------------------------------------------------------------
  // GET /health
  // -----------------------------------------------------------------------

  it("GET /health returns 200 with service info", async () => {
    const response = await fetch(`${BASE_URL}/health`);
    assertEquals(response.status, 200);

    const body = await response.json();
    assertEquals(body.status, "ok");
    assertEquals(body.service, "rokur");
    assertEquals(body.policyConfigured, true);
    assertEquals(body.tokenAuthEnabled, true);
    assertEquals(body.requiredSecretCount, 2);
    assertExists(body.timestamp);
    assertExists(body.version);
  });

  it("GET /health does not require authentication", async () => {
    const response = await fetch(`${BASE_URL}/health`);
    assertEquals(response.status, 200);
  });

  it("GET /health returns x-request-id header", async () => {
    const response = await fetch(`${BASE_URL}/health`);
    const requestId = response.headers.get("x-request-id");
    assertExists(requestId);
    assertEquals(requestId.length > 0, true);
  });

  it("GET /health forwards provided x-request-id", async () => {
    const response = await fetch(`${BASE_URL}/health`, {
      headers: { "x-request-id": "custom-id-12345" },
    });
    const requestId = response.headers.get("x-request-id");
    assertEquals(requestId, "custom-id-12345");
  });

  // -----------------------------------------------------------------------
  // GET /v1/secrets/status
  // -----------------------------------------------------------------------

  it("GET /v1/secrets/status returns 200 when all secrets present", async () => {
    const response = await fetch(`${BASE_URL}/v1/secrets/status`, {
      headers: { "x-rokur-token": API_TOKEN },
    });
    assertEquals(response.status, 200);

    const body = await response.json();
    assertEquals(body.allowed, true);
    assertEquals(body.policy, "allow");
    assertEquals(body.code, "AUTHORIZED");
    assertEquals(body.requiredSecretCount, 2);
    assertEquals(body.missingSecretCount, 0);
    assertExists(body.requestId);
  });

  it("GET /v1/secrets/status returns 401 without token", async () => {
    const response = await fetch(`${BASE_URL}/v1/secrets/status`);
    assertEquals(response.status, 401);

    const body = await response.json();
    assertEquals(body.error, "Unauthorized");
  });

  it("GET /v1/secrets/status returns 401 with wrong token", async () => {
    const response = await fetch(`${BASE_URL}/v1/secrets/status`, {
      headers: { "x-rokur-token": "wrong-token" },
    });
    assertEquals(response.status, 401);
  });

  // -----------------------------------------------------------------------
  // POST /v1/authorize-start
  // -----------------------------------------------------------------------

  it("POST /v1/authorize-start returns 200 with valid request", async () => {
    const response = await fetch(`${BASE_URL}/v1/authorize-start`, {
      method: "POST",
      headers: {
        "content-type": "application/json",
        "x-rokur-token": API_TOKEN,
      },
      body: JSON.stringify({ image: "alpine:3.19", name: "my-container" }),
    });
    assertEquals(response.status, 200);

    const body = await response.json();
    assertEquals(body.allowed, true);
    assertEquals(body.image, "alpine:3.19");
    assertEquals(body.name, "my-container");
    assertEquals(body.policyEngine, "builtin");
    assertExists(body.decisionTimestamp);
    assertExists(body.requestId);
  });

  it("POST /v1/authorize-start returns 401 without token", async () => {
    const response = await fetch(`${BASE_URL}/v1/authorize-start`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ image: "alpine:3.19" }),
    });
    assertEquals(response.status, 401);
  });

  it("POST /v1/authorize-start returns 400 with invalid JSON", async () => {
    const response = await fetch(`${BASE_URL}/v1/authorize-start`, {
      method: "POST",
      headers: {
        "content-type": "application/json",
        "x-rokur-token": API_TOKEN,
      },
      body: "not json",
    });
    assertEquals(response.status, 400);

    const body = await response.json();
    assertEquals(body.error, "Invalid JSON body");
  });

  it("POST /v1/authorize-start handles missing image/name gracefully", async () => {
    const response = await fetch(`${BASE_URL}/v1/authorize-start`, {
      method: "POST",
      headers: {
        "content-type": "application/json",
        "x-rokur-token": API_TOKEN,
      },
      body: JSON.stringify({}),
    });
    assertEquals(response.status, 200);

    const body = await response.json();
    assertEquals(body.allowed, true);
    assertEquals(body.image, null);
    assertEquals(body.name, null);
  });

  // -----------------------------------------------------------------------
  // GET /metrics
  // -----------------------------------------------------------------------

  it("GET /metrics returns counters and config", async () => {
    const response = await fetch(`${BASE_URL}/metrics`);
    assertEquals(response.status, 200);

    const body = await response.json();
    assertEquals(body.service, "rokur");
    assertExists(body.uptime_ms);
    assertExists(body.started_at);
    assertEquals(typeof body.requests_total, "number");
    assertEquals(typeof body.authorizations_allowed, "number");
    assertEquals(typeof body.authorizations_denied, "number");
    assertEquals(typeof body.auth_failures, "number");
    assertEquals(body.required_secret_count, 2);
    assertExists(body.rate_limiter);
  });

  it("GET /metrics does not require authentication", async () => {
    const response = await fetch(`${BASE_URL}/metrics`);
    assertEquals(response.status, 200);
  });

  // -----------------------------------------------------------------------
  // POST /v1/secrets/reload
  // -----------------------------------------------------------------------

  it("POST /v1/secrets/reload requires authentication", async () => {
    const response = await fetch(`${BASE_URL}/v1/secrets/reload`, {
      method: "POST",
    });
    assertEquals(response.status, 401);
  });

  it("POST /v1/secrets/reload succeeds with valid token", async () => {
    const response = await fetch(`${BASE_URL}/v1/secrets/reload`, {
      method: "POST",
      headers: { "x-rokur-token": API_TOKEN },
    });
    assertEquals(response.status, 200);

    const body = await response.json();
    assertEquals(body.status, "reloaded");
    assertEquals(typeof body.previousRequiredSecretCount, "number");
    assertEquals(typeof body.currentRequiredSecretCount, "number");
    assertExists(body.requestId);
  });

  // -----------------------------------------------------------------------
  // 404 handling
  // -----------------------------------------------------------------------

  it("returns 404 for unknown paths", async () => {
    const response = await fetch(`${BASE_URL}/unknown/path`);
    assertEquals(response.status, 404);

    const body = await response.json();
    assertEquals(body.error, "Not Found");
    assertEquals(body.path, "/unknown/path");
  });

  it("returns 404 for wrong HTTP method", async () => {
    const response = await fetch(`${BASE_URL}/v1/authorize-start`);
    assertEquals(response.status, 404);
  });

  // -----------------------------------------------------------------------
  // Request ID propagation
  // -----------------------------------------------------------------------

  it("generates request ID when none provided", async () => {
    const response = await fetch(`${BASE_URL}/v1/secrets/status`, {
      headers: { "x-rokur-token": API_TOKEN },
    });
    const requestId = response.headers.get("x-request-id");
    assertExists(requestId);

    const body = await response.json();
    assertEquals(body.requestId, requestId);
  });

  it("uses provided request ID", async () => {
    const response = await fetch(`${BASE_URL}/v1/secrets/status`, {
      headers: {
        "x-rokur-token": API_TOKEN,
        "x-request-id": "trace-abc-999",
      },
    });
    assertEquals(response.headers.get("x-request-id"), "trace-abc-999");

    const body = await response.json();
    assertEquals(body.requestId, "trace-abc-999");
  });
});

describe("Rokur with missing secrets", suiteOpts, () => {
  let serverProcess;
  const port = 19091;
  const url = `http://127.0.0.1:${port}`;

  beforeAll(async () => {
    serverProcess = new Deno.Command("deno", {
      args: ["run", "--allow-net", "--allow-env", "main.js"],
      cwd: new URL("..", import.meta.url).pathname,
      env: {
        ...Deno.env.toObject(),
        ROKUR_HOST: "127.0.0.1",
        ROKUR_PORT: String(port),
        ROKUR_ENV: "development",
        ROKUR_ALLOW_UNAUTHENTICATED: "true",
        ROKUR_REQUIRED_SECRETS: "db_password,api_key",
        ROKUR_SECRET_DB_PASSWORD: "present",
        // api_key deliberately NOT set
        ROKUR_AUDIT_LOG: "false",
        ROKUR_REQUEST_LOG: "false",
      },
      stdout: "null",
      stderr: "null",
    }).spawn();

    // Wait for server readiness.
    for (let i = 0; i < 30; i++) {
      try {
        const res = await fetch(`${url}/health`);
        if (res.ok) break;
      } catch { /* not ready */ }
      await new Promise((r) => setTimeout(r, 100));
    }
  });

  afterAll(() => {
    try {
      serverProcess.kill("SIGTERM");
    } catch { /* noop */ }
  });

  it("returns 409 when secrets are missing", async () => {
    const response = await fetch(`${url}/v1/secrets/status`);
    assertEquals(response.status, 409);

    const body = await response.json();
    assertEquals(body.allowed, false);
    assertEquals(body.policy, "deny");
    assertEquals(body.missingSecretCount, 1);
  });

  it("authorize-start also denies with missing secrets", async () => {
    const response = await fetch(`${url}/v1/authorize-start`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ image: "nginx:latest" }),
    });
    assertEquals(response.status, 409);

    const body = await response.json();
    assertEquals(body.allowed, false);
    assertEquals(body.image, "nginx:latest");
  });
});
