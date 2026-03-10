// SPDX-License-Identifier: PMPL-1.0-or-later
// Rokur - minimal secrets gate for container-start authorization.

import { loadConfig } from "./config.js";
import { createPolicyEvaluator } from "./policy/engine.js";
import {
  closeAuditLog,
  openAuditLog,
  recordAuthFailure,
  recordDecision,
} from "./audit.js";
import { createRateLimiter } from "./rate_limit.js";

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

let config = loadConfig();

function secretEnvName(secretName) {
  const normalized = secretName.toUpperCase().replace(/[^A-Z0-9]/g, "_");
  return `ROKUR_SECRET_${normalized}`;
}

// ---------------------------------------------------------------------------
// Metrics counters
// ---------------------------------------------------------------------------

const metrics = {
  requestsTotal: 0,
  requestsByStatus: {},
  authorizationsAllowed: 0,
  authorizationsDenied: 0,
  authFailures: 0,
  rateLimitDenials: 0,
  startedAt: new Date().toISOString(),
};

function incrementStatusCounter(status) {
  metrics.requestsByStatus[status] = (metrics.requestsByStatus[status] ?? 0) +
    1;
}

// ---------------------------------------------------------------------------
// Startup validation
// ---------------------------------------------------------------------------

function fatalConfigurationError(message) {
  console.error(JSON.stringify({
    level: "ERROR",
    message,
    service: "rokur",
    host: config.host,
    port: config.port,
    environment: config.env,
  }));
  Deno.exit(1);
}

function validateStartupConfiguration() {
  if (
    !Number.isInteger(config.port) || config.port < 1 || config.port > 65535
  ) {
    fatalConfigurationError(
      `Invalid ROKUR_PORT: "${config.port}". Expected integer between 1 and 65535.`,
    );
  }

  if (!config.apiToken) {
    if (config.isProduction || !config.allowUnauthenticated) {
      fatalConfigurationError(
        "ROKUR_API_TOKEN is required unless ROKUR_ALLOW_UNAUTHENTICATED=true is explicitly set in non-production mode.",
      );
    }
  }

  if (config.requiredSecrets.length === 0) {
    if (config.isProduction || !config.allowEmptyRequiredSecrets) {
      fatalConfigurationError(
        "ROKUR_REQUIRED_SECRETS must define at least one secret unless ROKUR_ALLOW_EMPTY_REQUIRED_SECRETS=true is explicitly set in non-production mode.",
      );
    }
  }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function jsonResponse(payload, status = 200, extraHeaders = {}) {
  return new Response(JSON.stringify(payload), {
    status,
    headers: { "content-type": "application/json", ...extraHeaders },
  });
}

function isAuthorizedRequest(request) {
  if (!config.apiToken) {
    return true;
  }

  const providedToken = (request.headers.get("x-rokur-token") ?? "").trim();
  return providedToken.length > 0 && providedToken === config.apiToken;
}

async function parseJsonBody(request) {
  if (!request.body) {
    return null;
  }

  try {
    const body = await request.json();
    if (body && typeof body === "object") {
      return body;
    }
  } catch {
    // No-op: invalid JSON handled by caller.
  }

  return null;
}

/**
 * Generates a unique request ID (v4 UUID format).
 */
function generateRequestId() {
  return crypto.randomUUID();
}

/**
 * Extracts client IP from request, respecting X-Forwarded-For when behind
 * a trusted proxy (ROKUR_TRUST_PROXY=true).
 */
function getClientIp(request, connInfo) {
  const directIp = connInfo?.remoteAddr?.hostname ?? "unknown";

  const trustProxy = (Deno.env.get("ROKUR_TRUST_PROXY") ?? "").trim()
    .toLowerCase();
  if (
    trustProxy === "1" || trustProxy === "true" || trustProxy === "yes" ||
    trustProxy === "on"
  ) {
    const forwarded = request.headers.get("x-forwarded-for");
    if (forwarded) {
      const firstIp = forwarded.split(",")[0].trim();
      if (firstIp.length > 0) {
        return firstIp;
      }
    }
  }

  return directIp;
}

// ---------------------------------------------------------------------------
// Policy evaluator
// ---------------------------------------------------------------------------

let policyEvaluator;

function initPolicyEvaluator() {
  return createPolicyEvaluator({
    requiredSecrets: config.requiredSecrets,
    secretEnvName,
  });
}

try {
  policyEvaluator = initPolicyEvaluator();
} catch (error) {
  const message = error instanceof Error ? error.message : String(error);
  fatalConfigurationError(message);
}

// ---------------------------------------------------------------------------
// Rate limiter
// ---------------------------------------------------------------------------

const rateLimiter = createRateLimiter();

// ---------------------------------------------------------------------------
// Route handlers
// ---------------------------------------------------------------------------

async function decisionPayload(input, requiredSecrets) {
  const decision = await policyEvaluator.evaluate(input);

  return {
    allowed: decision.allowed,
    policy: decision.allowed ? "allow" : "deny",
    code: decision.code,
    image: typeof input.image === "string" ? input.image : null,
    name: typeof input.name === "string" ? input.name : null,
    requiredSecretCount: typeof decision.requiredSecretCount === "number"
      ? decision.requiredSecretCount
      : requiredSecrets.length,
    missingSecretCount: typeof decision.missingSecretCount === "number"
      ? decision.missingSecretCount
      : (decision.allowed ? 0 : requiredSecrets.length),
    policyEngine: decision.engine,
    decisionTimestamp: new Date().toISOString(),
  };
}

function handleHealth(requestId) {
  return jsonResponse(
    {
      status: "ok",
      service: "rokur",
      version: "1.0.0",
      environment: config.env,
      policyConfigured: config.requiredSecrets.length > 0,
      policyBackend: policyEvaluator.config.resolvedBackend,
      tokenAuthEnabled: config.apiToken.length > 0,
      requiredSecretCount: config.requiredSecrets.length,
      timestamp: new Date().toISOString(),
    },
    200,
    { "x-request-id": requestId },
  );
}

async function handleSecretsStatus(request, requestId, clientIp) {
  if (!isAuthorizedRequest(request)) {
    metrics.authFailures += 1;
    rateLimiter.recordAuthFailure(clientIp);
    await recordAuthFailure({
      requestId,
      path: "/v1/secrets/status",
      clientIp,
    });
    return jsonResponse({ error: "Unauthorized", requestId }, 401, {
      "x-request-id": requestId,
    });
  }

  const payload = await decisionPayload({}, config.requiredSecrets);

  if (payload.allowed) {
    metrics.authorizationsAllowed += 1;
  } else {
    metrics.authorizationsDenied += 1;
  }

  await recordDecision({
    requestId,
    action: "secrets-status",
    allowed: payload.allowed,
    code: payload.code,
    engine: payload.policyEngine,
    image: null,
    name: null,
    requiredSecretCount: payload.requiredSecretCount,
    missingSecretCount: payload.missingSecretCount,
    clientIp,
    authenticated: true,
  });

  return jsonResponse({ ...payload, requestId }, payload.allowed ? 200 : 409, {
    "x-request-id": requestId,
  });
}

async function handleAuthorizeStart(request, requestId, clientIp) {
  if (!isAuthorizedRequest(request)) {
    metrics.authFailures += 1;
    rateLimiter.recordAuthFailure(clientIp);
    await recordAuthFailure({
      requestId,
      path: "/v1/authorize-start",
      clientIp,
    });
    return jsonResponse({ error: "Unauthorized", requestId }, 401, {
      "x-request-id": requestId,
    });
  }

  const body = await parseJsonBody(request);
  if (!body) {
    return jsonResponse({ error: "Invalid JSON body", requestId }, 400, {
      "x-request-id": requestId,
    });
  }

  const payload = await decisionPayload(body, config.requiredSecrets);

  if (payload.allowed) {
    metrics.authorizationsAllowed += 1;
  } else {
    metrics.authorizationsDenied += 1;
  }

  await recordDecision({
    requestId,
    action: "authorize-start",
    allowed: payload.allowed,
    code: payload.code,
    engine: payload.policyEngine,
    image: payload.image,
    name: payload.name,
    requiredSecretCount: payload.requiredSecretCount,
    missingSecretCount: payload.missingSecretCount,
    clientIp,
    authenticated: true,
  });

  return jsonResponse({ ...payload, requestId }, payload.allowed ? 200 : 409, {
    "x-request-id": requestId,
  });
}

function handleMetrics(requestId) {
  const uptimeMs = Date.now() - new Date(metrics.startedAt).getTime();

  return jsonResponse(
    {
      service: "rokur",
      uptime_ms: uptimeMs,
      started_at: metrics.startedAt,
      requests_total: metrics.requestsTotal,
      requests_by_status: metrics.requestsByStatus,
      authorizations_allowed: metrics.authorizationsAllowed,
      authorizations_denied: metrics.authorizationsDenied,
      auth_failures: metrics.authFailures,
      rate_limit_denials: metrics.rateLimitDenials,
      rate_limiter: rateLimiter.stats(),
      required_secret_count: config.requiredSecrets.length,
      policy_backend: policyEvaluator.config.resolvedBackend,
      timestamp: new Date().toISOString(),
    },
    200,
    { "x-request-id": requestId },
  );
}

function handleReloadSecrets(request, requestId, clientIp) {
  if (!isAuthorizedRequest(request)) {
    metrics.authFailures += 1;
    rateLimiter.recordAuthFailure(clientIp);
    return jsonResponse({ error: "Unauthorized", requestId }, 401, {
      "x-request-id": requestId,
    });
  }

  const previousCount = config.requiredSecrets.length;
  config = loadConfig();

  try {
    policyEvaluator = initPolicyEvaluator();
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    return jsonResponse(
      {
        error: "Reload failed",
        detail: message,
        requestId,
      },
      500,
      { "x-request-id": requestId },
    );
  }

  console.log(JSON.stringify({
    level: "INFO",
    message: "Secrets reloaded",
    previousCount,
    newCount: config.requiredSecrets.length,
    requestId,
    service: "rokur",
  }));

  return jsonResponse(
    {
      status: "reloaded",
      previousRequiredSecretCount: previousCount,
      currentRequiredSecretCount: config.requiredSecrets.length,
      policyBackend: policyEvaluator.config.resolvedBackend,
      requestId,
    },
    200,
    { "x-request-id": requestId },
  );
}

// ---------------------------------------------------------------------------
// Server handler
// ---------------------------------------------------------------------------

const serverHandler = async (request, connInfo) => {
  const requestId = request.headers.get("x-request-id") || generateRequestId();
  const clientIp = getClientIp(request, connInfo);
  const url = new URL(request.url);
  const { pathname } = url;
  const method = request.method.toUpperCase();
  const startTime = performance.now();

  metrics.requestsTotal += 1;

  // Rate limit check (skip health and metrics — they are public/operational).
  if (pathname !== "/health" && pathname !== "/metrics") {
    const rateCheck = rateLimiter.checkRequest(clientIp);
    if (!rateCheck.allowed) {
      metrics.rateLimitDenials += 1;
      incrementStatusCounter(429);

      if (config.requestLogEnabled) {
        console.log(JSON.stringify({
          level: "WARN",
          message: "Rate limited",
          requestId,
          method,
          path: pathname,
          clientIp,
          reason: rateCheck.reason,
          retryAfterMs: rateCheck.retryAfterMs,
          service: "rokur",
        }));
      }

      return jsonResponse(
        {
          error: "Too Many Requests",
          retryAfterMs: rateCheck.retryAfterMs,
          requestId,
        },
        429,
        {
          "x-request-id": requestId,
          "retry-after": String(Math.ceil(rateCheck.retryAfterMs / 1000)),
        },
      );
    }
  }

  // Route dispatch.
  let response;
  if (method === "GET" && pathname === "/health") {
    response = handleHealth(requestId);
  } else if (method === "GET" && pathname === "/v1/secrets/status") {
    response = await handleSecretsStatus(request, requestId, clientIp);
  } else if (method === "POST" && pathname === "/v1/authorize-start") {
    response = await handleAuthorizeStart(request, requestId, clientIp);
  } else if (method === "GET" && pathname === "/metrics") {
    response = handleMetrics(requestId);
  } else if (method === "POST" && pathname === "/v1/secrets/reload") {
    response = handleReloadSecrets(request, requestId, clientIp);
  } else {
    response = jsonResponse(
      { error: "Not Found", path: pathname, requestId },
      404,
      { "x-request-id": requestId },
    );
  }

  const durationMs = (performance.now() - startTime).toFixed(2);
  incrementStatusCounter(response.status);

  // Request logging.
  if (config.requestLogEnabled) {
    console.log(JSON.stringify({
      level: "INFO",
      message: "request",
      requestId,
      method,
      path: pathname,
      status: response.status,
      durationMs: Number(durationMs),
      clientIp,
      service: "rokur",
    }));
  }

  return response;
};

// ---------------------------------------------------------------------------
// Graceful shutdown
// ---------------------------------------------------------------------------

const abortController = new AbortController();

function initiateShutdown(signal) {
  console.log(JSON.stringify({
    level: "INFO",
    message: `Received ${signal}, shutting down gracefully`,
    service: "rokur",
  }));
  abortController.abort();
  closeAuditLog();
}

Deno.addSignalListener("SIGTERM", () => initiateShutdown("SIGTERM"));
Deno.addSignalListener("SIGINT", () => initiateShutdown("SIGINT"));

// SIGHUP triggers a full config reload without restart.
Deno.addSignalListener("SIGHUP", () => {
  const previousCount = config.requiredSecrets.length;
  config = loadConfig();
  try {
    policyEvaluator = initPolicyEvaluator();
    console.log(JSON.stringify({
      level: "INFO",
      message: "SIGHUP: configuration reloaded",
      previousCount,
      newCount: config.requiredSecrets.length,
      service: "rokur",
    }));
  } catch (error) {
    console.error(JSON.stringify({
      level: "ERROR",
      message: "SIGHUP: reload failed, keeping previous configuration",
      error: error instanceof Error ? error.message : String(error),
      service: "rokur",
    }));
  }
});

// ---------------------------------------------------------------------------
// Startup
// ---------------------------------------------------------------------------

validateStartupConfiguration();
await openAuditLog({
  enabled: config.auditLogEnabled,
  path: config.auditLogPath,
});

console.log(JSON.stringify({
  level: "INFO",
  message: "Starting Rokur",
  version: "1.0.0",
  host: config.host,
  port: config.port,
  environment: config.env,
  requiredSecretCount: config.requiredSecrets.length,
  policyBackend: policyEvaluator.config.resolvedBackend,
  policyExternalCommandConfigured:
    policyEvaluator.config.externalCommandConfigured,
  allowUnauthenticated: config.allowUnauthenticated,
  allowEmptyRequiredSecrets: config.allowEmptyRequiredSecrets,
  tokenAuthEnabled: config.apiToken.length > 0,
  requestLogEnabled: config.requestLogEnabled,
  auditLogEnabled: config.auditLogEnabled,
  service: "rokur",
}));

Deno.serve({
  hostname: config.host,
  port: config.port,
  signal: abortController.signal,
  onListen({ hostname, port }) {
    console.log(JSON.stringify({
      level: "INFO",
      message: `Rokur listening on ${hostname}:${port}`,
      service: "rokur",
    }));
  },
}, serverHandler);
