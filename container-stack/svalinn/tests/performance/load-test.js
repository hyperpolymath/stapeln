#!/usr/bin/env -S deno run --allow-net
// SPDX-License-Identifier: PMPL-1.0-or-later
// Svalinn load test — measures throughput and latency under concurrent load
//
// Usage:
//   deno run --allow-net tests/performance/load-test.js [options]
//
// Options:
//   --url          Base URL (default: http://localhost:8000)
//   --concurrency  Number of concurrent workers (default: 10)
//   --duration     Test duration in seconds (default: 10)
//   --requests     Maximum total requests (default: 1000)

import { parseArgs } from "jsr:@std/cli/parse-args";

// ---------------------------------------------------------------------------
// CLI argument parsing
// ---------------------------------------------------------------------------

const args = parseArgs(Deno.args, {
  string: ["url"],
  default: {
    url: "http://localhost:8000",
    concurrency: 10,
    duration: 10,
    requests: 1000,
  },
});

const BASE_URL = String(args.url);
const CONCURRENCY = Number(args.concurrency);
const DURATION_S = Number(args.duration);
const MAX_REQUESTS = Number(args.requests);

// ---------------------------------------------------------------------------
// Endpoints under test (round-robin)
// ---------------------------------------------------------------------------

const ENDPOINTS = [
  { method: "GET", path: "/healthz", label: "GET /healthz" },
  { method: "GET", path: "/metrics", label: "GET /metrics" },
  { method: "GET", path: "/api/v1/containers", label: "GET /api/v1/containers" },
];

// ---------------------------------------------------------------------------
// Result tracking
// ---------------------------------------------------------------------------

/** @type {number[]} */
const latencies = [];
let totalRequests = 0;
let successCount = 0;
let failureCount = 0;
let endpointIndex = 0;
let stopped = false;

/**
 * Pick the next endpoint in round-robin order.
 * @returns {{ method: string, path: string, label: string }}
 */
function nextEndpoint() {
  const ep = ENDPOINTS[endpointIndex % ENDPOINTS.length];
  endpointIndex++;
  return ep;
}

// ---------------------------------------------------------------------------
// Single request execution
// ---------------------------------------------------------------------------

/**
 * Fire a single HTTP request and record its latency.
 */
async function fireRequest() {
  const ep = nextEndpoint();
  const url = `${BASE_URL}${ep.path}`;
  const start = performance.now();

  try {
    const response = await fetch(url, {
      method: ep.method,
      signal: AbortSignal.timeout(5000),
    });

    const elapsed = performance.now() - start;
    latencies.push(elapsed);
    totalRequests++;

    if (response.status >= 200 && response.status < 300) {
      successCount++;
    } else {
      failureCount++;
    }

    // Drain body to free resources
    await response.body?.cancel();
  } catch (_err) {
    const elapsed = performance.now() - start;
    latencies.push(elapsed);
    totalRequests++;
    failureCount++;
  }
}

// ---------------------------------------------------------------------------
// Concurrency pool
// ---------------------------------------------------------------------------

/**
 * Run a single worker that keeps firing requests until the test ends.
 */
async function worker() {
  while (!stopped && totalRequests < MAX_REQUESTS) {
    await fireRequest();
  }
}

// ---------------------------------------------------------------------------
// Percentile calculation
// ---------------------------------------------------------------------------

/**
 * Compute a percentile from a sorted array of numbers.
 * @param {number[]} sortedValues - Pre-sorted array of latencies.
 * @param {number} p - Percentile (0-100).
 * @returns {number}
 */
function percentile(sortedValues, p) {
  if (sortedValues.length === 0) return 0;
  const index = Math.ceil((p / 100) * sortedValues.length) - 1;
  return sortedValues[Math.max(0, index)];
}

// ---------------------------------------------------------------------------
// Result formatting
// ---------------------------------------------------------------------------

/**
 * Print a formatted results table to stdout.
 * @param {number} elapsedMs - Total wall-clock time in milliseconds.
 */
function printResults(elapsedMs) {
  const sorted = [...latencies].sort((a, b) => a - b);
  const elapsedS = elapsedMs / 1000;
  const rps = totalRequests / elapsedS;
  const p50 = percentile(sorted, 50);
  const p95 = percentile(sorted, 95);
  const p99 = percentile(sorted, 99);
  const minLatency = sorted.length > 0 ? sorted[0] : 0;
  const maxLatency = sorted.length > 0 ? sorted[sorted.length - 1] : 0;
  const avgLatency =
    sorted.length > 0 ? sorted.reduce((a, b) => a + b, 0) / sorted.length : 0;

  console.log("\n" + "=".repeat(60));
  console.log("  Svalinn Load Test Results");
  console.log("=".repeat(60));
  console.log("");
  console.log("  Configuration");
  console.log("  -------------");
  console.log(`  Base URL       : ${BASE_URL}`);
  console.log(`  Concurrency    : ${CONCURRENCY}`);
  console.log(`  Duration limit : ${DURATION_S}s`);
  console.log(`  Request limit  : ${MAX_REQUESTS}`);
  console.log("");
  console.log("  Throughput");
  console.log("  ----------");
  console.log(`  Total requests : ${totalRequests}`);
  console.log(`  Successful 2xx : ${successCount}`);
  console.log(`  Failed         : ${failureCount}`);
  console.log(`  Elapsed time   : ${elapsedS.toFixed(2)}s`);
  console.log(`  Requests/sec   : ${rps.toFixed(2)}`);
  console.log("");
  console.log("  Latency (ms)");
  console.log("  ------------");
  console.log(`  Min            : ${minLatency.toFixed(2)}`);
  console.log(`  Avg            : ${avgLatency.toFixed(2)}`);
  console.log(`  p50            : ${p50.toFixed(2)}`);
  console.log(`  p95            : ${p95.toFixed(2)}`);
  console.log(`  p99            : ${p99.toFixed(2)}`);
  console.log(`  Max            : ${maxLatency.toFixed(2)}`);
  console.log("");
  console.log("=".repeat(60));
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

async function main() {
  console.log(`Svalinn load test starting...`);
  console.log(
    `  Target: ${BASE_URL}  Concurrency: ${CONCURRENCY}  Duration: ${DURATION_S}s  Max requests: ${MAX_REQUESTS}`
  );
  console.log("");

  const startTime = performance.now();

  // Set a hard duration timer
  const timer = setTimeout(() => {
    stopped = true;
  }, DURATION_S * 1000);

  // Launch concurrent workers
  const workers = [];
  for (let i = 0; i < CONCURRENCY; i++) {
    workers.push(worker());
  }

  await Promise.all(workers);
  clearTimeout(timer);

  const elapsedMs = performance.now() - startTime;
  printResults(elapsedMs);

  // Exit with non-zero if more than 50% of requests failed
  if (totalRequests > 0 && failureCount / totalRequests > 0.5) {
    console.log("\nWARNING: More than 50% of requests failed.");
    Deno.exit(1);
  }
}

main();
