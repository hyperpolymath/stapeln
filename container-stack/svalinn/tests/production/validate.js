#!/usr/bin/env -S deno run --allow-net --allow-env
// SPDX-License-Identifier: PMPL-1.0-or-later
// Svalinn production validation — checks all production requirements
//
// Usage:
//   deno run --allow-net --allow-env validate.js [--url URL] [--strict]
//
// Flags:
//   --url URL    Base URL of the Svalinn instance (default: http://localhost:8000)
//   --strict     Fail on any SKIP result (for CI pipelines)
//
// Exit codes:
//   0  All checks passed (SKIPs allowed unless --strict)
//   1  One or more checks failed, or SKIPs present in strict mode

// ---------------------------------------------------------------------------
// Argument parsing
// ---------------------------------------------------------------------------

const args = parseArgs(Deno.args);

/**
 * parseArgs — extract --url and --strict from CLI arguments.
 * @param {string[]} argv - Raw CLI arguments
 * @returns {{ url: string, strict: boolean }}
 */
function parseArgs(argv) {
  let url = Deno.env.get("SVALINN_URL") || "http://localhost:8000";
  let strict = false;

  for (let i = 0; i < argv.length; i++) {
    if (argv[i] === "--url" && argv[i + 1]) {
      url = argv[++i];
    } else if (argv[i] === "--strict") {
      strict = true;
    }
  }

  // Strip trailing slash for consistency
  url = url.replace(/\/+$/, "");
  return { url, strict };
}

// ---------------------------------------------------------------------------
// Result tracking
// ---------------------------------------------------------------------------

/** @type {Array<{ category: string, check: string, status: "PASS"|"FAIL"|"SKIP", detail: string }>} */
const results = [];

/**
 * record — store a single check result.
 * @param {string} category - Group name (e.g. "Health", "Security Headers")
 * @param {string} check    - Individual check description
 * @param {"PASS"|"FAIL"|"SKIP"} status
 * @param {string} detail   - Human-readable explanation
 */
function record(category, check, status, detail) {
  results.push({ category, check, status, detail });
}

// ---------------------------------------------------------------------------
// HTTP helpers
// ---------------------------------------------------------------------------

/**
 * safeFetch — fetch with a timeout, returning null on network errors.
 * @param {string} url
 * @param {RequestInit & { timeoutMs?: number }} [opts]
 * @returns {Promise<Response|null>}
 */
async function safeFetch(url, opts = {}) {
  const { timeoutMs = 5000, ...fetchOpts } = opts;
  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), timeoutMs);

  try {
    const resp = await fetch(url, { ...fetchOpts, signal: controller.signal });
    return resp;
  } catch (_err) {
    return null;
  } finally {
    clearTimeout(timer);
  }
}

// ---------------------------------------------------------------------------
// 1. Endpoint health checks
// ---------------------------------------------------------------------------

/**
 * checkHealth — verify /healthz, /readyz, and /metrics endpoints.
 */
async function checkHealth() {
  // /healthz — must return 200 with {"status": "healthy"}
  const healthResp = await safeFetch(`${args.url}/healthz`);
  if (!healthResp) {
    record("Health", "GET /healthz responds", "FAIL", "No response (connection refused or timeout)");
  } else if (healthResp.status !== 200) {
    record("Health", "GET /healthz responds", "FAIL", `Expected 200, got ${healthResp.status}`);
  } else {
    let bodyOk = false;
    try {
      const body = await healthResp.json();
      bodyOk = body.status === "healthy";
    } catch { /* ignore parse error */ }

    record(
      "Health",
      "GET /healthz responds",
      bodyOk ? "PASS" : "FAIL",
      bodyOk ? '200 with {"status":"healthy"}' : "200 but body missing or wrong",
    );
  }

  // /readyz — must respond (200 or 503), not timeout
  const readyResp = await safeFetch(`${args.url}/readyz`);
  if (!readyResp) {
    record("Health", "GET /readyz responds", "FAIL", "No response (connection refused or timeout)");
  } else if (readyResp.status === 200 || readyResp.status === 503) {
    record("Health", "GET /readyz responds", "PASS", `Returned ${readyResp.status}`);
  } else {
    record("Health", "GET /readyz responds", "FAIL", `Unexpected status ${readyResp.status}`);
  }

  // /metrics — must return 200 with Prometheus text format
  const metricsResp = await safeFetch(`${args.url}/metrics`);
  if (!metricsResp) {
    record("Health", "GET /metrics responds", "FAIL", "No response");
  } else if (metricsResp.status !== 200) {
    record("Health", "GET /metrics responds", "FAIL", `Expected 200, got ${metricsResp.status}`);
  } else {
    const text = await metricsResp.text();
    // Prometheus format lines start with # HELP, # TYPE, or metric_name
    const looksPrometheus = /^#\s+(HELP|TYPE)\s+/m.test(text) || /^\w+\s+[\d.]+/m.test(text);
    record(
      "Health",
      "GET /metrics responds",
      looksPrometheus ? "PASS" : "FAIL",
      looksPrometheus ? "200 with Prometheus-format text" : "200 but body does not look like Prometheus format",
    );
  }
}

// ---------------------------------------------------------------------------
// 2. Security headers
// ---------------------------------------------------------------------------

/** Required security headers and their expected values (null = any value is fine). */
const REQUIRED_HEADERS = [
  { name: "Strict-Transport-Security", expected: null },
  { name: "X-Frame-Options", expected: "DENY" },
  { name: "X-Content-Type-Options", expected: "nosniff" },
  { name: "Content-Security-Policy", expected: null },
  { name: "Referrer-Policy", expected: null },
  { name: "Permissions-Policy", expected: null },
];

/**
 * checkSecurityHeaders — verify all required security headers on a response.
 */
async function checkSecurityHeaders() {
  const resp = await safeFetch(`${args.url}/healthz`);
  if (!resp) {
    for (const h of REQUIRED_HEADERS) {
      record("Security Headers", `${h.name} present`, "SKIP", "Cannot connect to server");
    }
    return;
  }

  // Consume the body so the connection is released
  await resp.text();

  for (const h of REQUIRED_HEADERS) {
    const val = resp.headers.get(h.name);
    if (!val) {
      record("Security Headers", `${h.name} present`, "FAIL", "Header missing");
    } else if (h.expected && val.toUpperCase() !== h.expected.toUpperCase()) {
      record("Security Headers", `${h.name} = ${h.expected}`, "FAIL", `Got "${val}"`);
    } else {
      record("Security Headers", `${h.name} present`, "PASS", `"${val}"`);
    }
  }
}

// ---------------------------------------------------------------------------
// 3. Rate limiting
// ---------------------------------------------------------------------------

/**
 * checkRateLimiting — verify rate-limit headers and 429 enforcement.
 */
async function checkRateLimiting() {
  const resp = await safeFetch(`${args.url}/healthz`);
  if (!resp) {
    record("Rate Limiting", "X-RateLimit-Limit present", "SKIP", "Cannot connect");
    record("Rate Limiting", "X-RateLimit-Remaining present", "SKIP", "Cannot connect");
    record("Rate Limiting", "429 on burst", "SKIP", "Cannot connect");
    return;
  }
  await resp.text();

  const limitHeader = resp.headers.get("X-RateLimit-Limit");
  record(
    "Rate Limiting",
    "X-RateLimit-Limit present",
    limitHeader ? "PASS" : "FAIL",
    limitHeader ? `"${limitHeader}"` : "Header missing",
  );

  const remainingHeader = resp.headers.get("X-RateLimit-Remaining");
  record(
    "Rate Limiting",
    "X-RateLimit-Remaining present",
    remainingHeader ? "PASS" : "FAIL",
    remainingHeader ? `"${remainingHeader}"` : "Header missing",
  );

  // Fire rapid requests to trigger 429
  let got429 = false;
  const BURST_COUNT = 200;
  const promises = [];
  for (let i = 0; i < BURST_COUNT; i++) {
    promises.push(safeFetch(`${args.url}/healthz`, { timeoutMs: 3000 }));
  }
  const responses = await Promise.allSettled(promises);
  for (const r of responses) {
    if (r.status === "fulfilled" && r.value && r.value.status === 429) {
      got429 = true;
      break;
    }
  }
  // Drain bodies
  for (const r of responses) {
    if (r.status === "fulfilled" && r.value) {
      try { await r.value.text(); } catch { /* ignore */ }
    }
  }
  record(
    "Rate Limiting",
    "429 on burst",
    got429 ? "PASS" : "FAIL",
    got429 ? `Got 429 within ${BURST_COUNT} rapid requests` : `No 429 after ${BURST_COUNT} requests`,
  );
}

// ---------------------------------------------------------------------------
// 4. Auth enforcement
// ---------------------------------------------------------------------------

/**
 * checkAuthEnforcement — verify protected endpoints return 401 without a token.
 * Only runs when AUTH_ENABLED=true.
 */
async function checkAuthEnforcement() {
  const authEnabled = Deno.env.get("AUTH_ENABLED");
  if (authEnabled !== "true") {
    record("Auth", "GET /api/v1/containers returns 401", "SKIP", "AUTH_ENABLED != true");
    record("Auth", "POST /api/v1/run returns 401", "SKIP", "AUTH_ENABLED != true");
    return;
  }

  // GET /api/v1/containers without token
  const containersResp = await safeFetch(`${args.url}/api/v1/containers`);
  if (!containersResp) {
    record("Auth", "GET /api/v1/containers returns 401", "FAIL", "No response");
  } else {
    await containersResp.text();
    record(
      "Auth",
      "GET /api/v1/containers returns 401",
      containersResp.status === 401 ? "PASS" : "FAIL",
      `Got ${containersResp.status}`,
    );
  }

  // POST /api/v1/run without token
  const runResp = await safeFetch(`${args.url}/api/v1/run`, { method: "POST" });
  if (!runResp) {
    record("Auth", "POST /api/v1/run returns 401", "FAIL", "No response");
  } else {
    await runResp.text();
    record(
      "Auth",
      "POST /api/v1/run returns 401",
      runResp.status === 401 ? "PASS" : "FAIL",
      `Got ${runResp.status}`,
    );
  }
}

// ---------------------------------------------------------------------------
// 5. CORS validation
// ---------------------------------------------------------------------------

/**
 * checkCors — verify wildcard origins are not allowed.
 */
async function checkCors() {
  // Request with a suspicious origin
  const resp = await safeFetch(`${args.url}/healthz`, {
    headers: { Origin: "https://evil.example.com" },
  });
  if (!resp) {
    record("CORS", "No wildcard Access-Control-Allow-Origin", "SKIP", "Cannot connect");
    record("CORS", "Unknown origin rejected", "SKIP", "Cannot connect");
    return;
  }
  await resp.text();

  const acao = resp.headers.get("Access-Control-Allow-Origin");
  record(
    "CORS",
    "No wildcard Access-Control-Allow-Origin",
    acao === "*" ? "FAIL" : "PASS",
    acao ? `"${acao}"` : "Header absent (good — no blanket CORS)",
  );

  // Check that an unknown origin does not get reflected
  const reflected = acao === "https://evil.example.com";
  record(
    "CORS",
    "Unknown origin rejected",
    reflected ? "FAIL" : "PASS",
    reflected ? "Origin was reflected — open redirect risk" : "Unknown origin not reflected",
  );
}

// ---------------------------------------------------------------------------
// 6. TLS validation
// ---------------------------------------------------------------------------

/**
 * checkTls — verify TLS is active and the certificate is valid.
 * Only runs when the target URL uses https://.
 */
async function checkTls() {
  if (!args.url.startsWith("https://")) {
    record("TLS", "TLS active", "SKIP", "URL is not HTTPS");
    record("TLS", "Certificate valid", "SKIP", "URL is not HTTPS");
    return;
  }

  // Deno's fetch validates TLS by default — a successful connection means
  // the certificate is trusted and not expired.
  const resp = await safeFetch(`${args.url}/healthz`);
  if (!resp) {
    record("TLS", "TLS active", "FAIL", "Could not establish TLS connection");
    record("TLS", "Certificate valid", "FAIL", "Connection failed");
    return;
  }
  await resp.text();

  record("TLS", "TLS active", "PASS", "HTTPS connection established");
  record("TLS", "Certificate valid", "PASS", "Deno accepted the certificate chain");
}

// ---------------------------------------------------------------------------
// Runner and reporting
// ---------------------------------------------------------------------------

/**
 * printResults — render the results table and summary to stdout.
 */
function printResults() {
  const COL_CAT = 20;
  const COL_CHK = 42;
  const COL_STS = 6;

  const divider = "─".repeat(COL_CAT + COL_CHK + COL_STS + 40 + 5);

  console.log("");
  console.log(`Svalinn Production Validation — ${args.url}`);
  console.log(divider);
  console.log(
    `${"Category".padEnd(COL_CAT)} ${"Check".padEnd(COL_CHK)} ${"Status".padEnd(COL_STS)}  Detail`,
  );
  console.log(divider);

  for (const r of results) {
    const statusTag = r.status === "PASS"
      ? "\x1b[32mPASS\x1b[0m"
      : r.status === "FAIL"
      ? "\x1b[31mFAIL\x1b[0m"
      : "\x1b[33mSKIP\x1b[0m";

    console.log(
      `${r.category.padEnd(COL_CAT)} ${r.check.padEnd(COL_CHK)} ${statusTag.padEnd(COL_STS + 9)}  ${r.detail}`,
    );
  }
  console.log(divider);

  const passed = results.filter((r) => r.status === "PASS").length;
  const failed = results.filter((r) => r.status === "FAIL").length;
  const skipped = results.filter((r) => r.status === "SKIP").length;

  console.log(`\nTotal: ${results.length}  |  PASS: ${passed}  |  FAIL: ${failed}  |  SKIP: ${skipped}`);

  if (failed > 0) {
    console.log("\x1b[31m\nRESULT: FAIL — production requirements not met\x1b[0m");
  } else if (args.strict && skipped > 0) {
    console.log("\x1b[33m\nRESULT: FAIL — strict mode, SKIPs not permitted\x1b[0m");
  } else {
    console.log("\x1b[32m\nRESULT: PASS — all production checks satisfied\x1b[0m");
  }
}

/**
 * main — run all validation suites and report.
 */
async function main() {
  console.log(`\nValidating Svalinn at ${args.url} ...`);
  if (args.strict) {
    console.log("(strict mode — SKIPs will cause failure)");
  }

  await checkHealth();
  await checkSecurityHeaders();
  await checkRateLimiting();
  await checkAuthEnforcement();
  await checkCors();
  await checkTls();

  printResults();

  const failed = results.filter((r) => r.status === "FAIL").length;
  const skipped = results.filter((r) => r.status === "SKIP").length;

  if (failed > 0 || (args.strict && skipped > 0)) {
    Deno.exit(1);
  }
  Deno.exit(0);
}

await main();
