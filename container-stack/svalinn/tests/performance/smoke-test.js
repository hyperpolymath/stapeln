#!/usr/bin/env -S deno run --allow-net
// SPDX-License-Identifier: PMPL-1.0-or-later
// Svalinn smoke test — verifies all endpoints respond correctly
//
// Usage:
//   deno run --allow-net tests/performance/smoke-test.js [--url http://localhost:8000]

import { parseArgs } from "jsr:@std/cli/parse-args";

// ---------------------------------------------------------------------------
// CLI argument parsing
// ---------------------------------------------------------------------------

const args = parseArgs(Deno.args, {
  string: ["url"],
  default: { url: "http://localhost:8000" },
});

const BASE_URL = String(args.url);

// ---------------------------------------------------------------------------
// Endpoint definitions with expected status codes
// ---------------------------------------------------------------------------

const ENDPOINTS = [
  { method: "GET", path: "/healthz", expectedStatus: 200, label: "Health check" },
  { method: "GET", path: "/metrics", expectedStatus: 200, label: "Metrics" },
  {
    method: "GET",
    path: "/api/v1/containers",
    expectedStatus: 401,
    label: "Containers (no auth, expect 401)",
  },
  {
    method: "GET",
    path: "/api/v1/policies",
    expectedStatus: 401,
    label: "Policies (no auth, expect 401)",
  },
  {
    method: "GET",
    path: "/api/v1/audit",
    expectedStatus: 401,
    label: "Audit log (no auth, expect 401)",
  },
  { method: "GET", path: "/", expectedStatus: [200, 301, 302, 404], label: "Root" },
];

// ---------------------------------------------------------------------------
// Result types
// ---------------------------------------------------------------------------

/**
 * @typedef {Object} TestResult
 * @property {string} label
 * @property {string} method
 * @property {string} path
 * @property {boolean} passed
 * @property {number} status
 * @property {number} latencyMs
 * @property {string} [error]
 */

// ---------------------------------------------------------------------------
// Single endpoint test
// ---------------------------------------------------------------------------

/**
 * Test a single endpoint and return the result.
 * @param {{ method: string, path: string, expectedStatus: number | number[], label: string }} endpoint
 * @returns {Promise<TestResult>}
 */
async function testEndpoint(endpoint) {
  const url = `${BASE_URL}${endpoint.path}`;
  const start = performance.now();

  try {
    const response = await fetch(url, {
      method: endpoint.method,
      signal: AbortSignal.timeout(5000),
      redirect: "manual",
    });

    const latencyMs = performance.now() - start;

    // Drain body
    await response.body?.cancel();

    const expectedStatuses = Array.isArray(endpoint.expectedStatus)
      ? endpoint.expectedStatus
      : [endpoint.expectedStatus];

    const passed = expectedStatuses.includes(response.status);

    return {
      label: endpoint.label,
      method: endpoint.method,
      path: endpoint.path,
      passed,
      status: response.status,
      latencyMs,
    };
  } catch (err) {
    const latencyMs = performance.now() - start;
    return {
      label: endpoint.label,
      method: endpoint.method,
      path: endpoint.path,
      passed: false,
      status: 0,
      latencyMs,
      error: err instanceof Error ? err.message : String(err),
    };
  }
}

// ---------------------------------------------------------------------------
// Output formatting
// ---------------------------------------------------------------------------

/**
 * Print results as a formatted table.
 * @param {TestResult[]} results
 */
function printResults(results) {
  const passedCount = results.filter((r) => r.passed).length;
  const totalCount = results.length;

  console.log("\n" + "=".repeat(72));
  console.log("  Svalinn Smoke Test Results");
  console.log("=".repeat(72));
  console.log("");

  // Column widths
  const statusCol = 6;
  const methodCol = 6;
  const pathCol = 24;
  const httpCol = 6;
  const latencyCol = 10;
  const labelCol = 30;

  // Header
  const header =
    "  " +
    "PASS".padEnd(statusCol) +
    "METHOD".padEnd(methodCol) +
    "PATH".padEnd(pathCol) +
    "HTTP".padEnd(httpCol) +
    "LATENCY".padEnd(latencyCol) +
    "LABEL";
  console.log(header);
  console.log("  " + "-".repeat(70));

  for (const r of results) {
    const passStr = r.passed ? "OK" : "FAIL";
    const statusStr = r.status > 0 ? String(r.status) : "ERR";
    const latencyStr = `${r.latencyMs.toFixed(1)}ms`;
    const errorSuffix = r.error ? ` [${r.error}]` : "";

    const line =
      "  " +
      passStr.padEnd(statusCol) +
      r.method.padEnd(methodCol) +
      r.path.padEnd(pathCol) +
      statusStr.padEnd(httpCol) +
      latencyStr.padEnd(latencyCol) +
      r.label +
      errorSuffix;

    console.log(line);
  }

  console.log("");
  console.log(`  Result: ${passedCount}/${totalCount} passed`);
  console.log("=".repeat(72));

  return passedCount === totalCount;
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

async function main() {
  console.log(`Svalinn smoke test`);
  console.log(`  Target: ${BASE_URL}`);
  console.log(`  Endpoints: ${ENDPOINTS.length}`);

  const results = [];

  for (const ep of ENDPOINTS) {
    const result = await testEndpoint(ep);
    results.push(result);
  }

  const allPassed = printResults(results);

  if (!allPassed) {
    console.log("\nSome endpoints failed. Check the server is running and accessible.");
    Deno.exit(1);
  }

  console.log("\nAll smoke tests passed.");
}

main();
