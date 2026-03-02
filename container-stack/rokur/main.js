// SPDX-License-Identifier: PMPL-1.0-or-later
// Rokur - minimal secrets gate for container-start authorization.

import { createPolicyEvaluator } from "./policy/engine.js";

const HOST = Deno.env.get("ROKUR_HOST") ?? "127.0.0.1";
const PORT = Number(Deno.env.get("ROKUR_PORT") ?? "9090");
const ENVIRONMENT = (Deno.env.get("ROKUR_ENV") ?? "development").trim().toLowerCase();
const IS_PRODUCTION = ENVIRONMENT === "production";
const API_TOKEN = (Deno.env.get("ROKUR_API_TOKEN") ?? "").trim();

const REQUIRED_SECRETS = Array.from(new Set((Deno.env.get("ROKUR_REQUIRED_SECRETS") ?? "")
  .split(",")
  .map((value) => value.trim())
  .filter((value) => value.length > 0)));

function parseBooleanEnv(name, defaultValue = false) {
  const rawValue = Deno.env.get(name);
  if (rawValue === undefined || rawValue === null || rawValue.trim().length === 0) {
    return defaultValue;
  }

  const normalized = rawValue.trim().toLowerCase();
  if (normalized === "1" || normalized === "true" || normalized === "yes" || normalized === "on") {
    return true;
  }
  if (
    normalized === "0" || normalized === "false" || normalized === "no" ||
    normalized === "off"
  ) {
    return false;
  }

  throw new Error(
    `Invalid boolean value for ${name}: "${rawValue}". Use true/false, 1/0, yes/no, or on/off.`,
  );
}

const ALLOW_UNAUTHENTICATED = parseBooleanEnv("ROKUR_ALLOW_UNAUTHENTICATED", false);
const ALLOW_EMPTY_REQUIRED_SECRETS = parseBooleanEnv(
  "ROKUR_ALLOW_EMPTY_REQUIRED_SECRETS",
  false,
);

function fatalConfigurationError(message) {
  console.error(
    JSON.stringify({
      level: "ERROR",
      message,
      service: "rokur",
      host: HOST,
      port: PORT,
      environment: ENVIRONMENT,
    }),
  );
  Deno.exit(1);
}

function validateStartupConfiguration() {
  if (!Number.isInteger(PORT) || PORT < 1 || PORT > 65535) {
    fatalConfigurationError(`Invalid ROKUR_PORT: "${PORT}". Expected integer between 1 and 65535.`);
  }

  if (!API_TOKEN) {
    if (IS_PRODUCTION || !ALLOW_UNAUTHENTICATED) {
      fatalConfigurationError(
        "ROKUR_API_TOKEN is required unless ROKUR_ALLOW_UNAUTHENTICATED=true is explicitly set in non-production mode.",
      );
    }
  }

  if (REQUIRED_SECRETS.length === 0) {
    if (IS_PRODUCTION || !ALLOW_EMPTY_REQUIRED_SECRETS) {
      fatalConfigurationError(
        "ROKUR_REQUIRED_SECRETS must define at least one secret unless ROKUR_ALLOW_EMPTY_REQUIRED_SECRETS=true is explicitly set in non-production mode.",
      );
    }
  }
}

function jsonResponse(payload, status = 200) {
  return new Response(JSON.stringify(payload), {
    status,
    headers: { "content-type": "application/json" },
  });
}

function secretEnvName(secretName) {
  const normalized = secretName.toUpperCase().replace(/[^A-Z0-9]/g, "_");
  return `ROKUR_SECRET_${normalized}`;
}

function isAuthorizedRequest(request) {
  if (!API_TOKEN) {
    return true;
  }

  const providedToken = (request.headers.get("x-rokur-token") ?? "").trim();
  return providedToken.length > 0 && providedToken === API_TOKEN;
}

async function parseJsonBody(request) {
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

let policyEvaluator;
try {
  policyEvaluator = createPolicyEvaluator({
    requiredSecrets: REQUIRED_SECRETS,
    secretEnvName,
  });
} catch (error) {
  const message = error instanceof Error ? error.message : String(error);
  fatalConfigurationError(message);
}

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

const serverHandler = async (request) => {
  const url = new URL(request.url);
  const { pathname } = url;
  const method = request.method.toUpperCase();

  if (method === "GET" && pathname === "/health") {
    return jsonResponse({
      status: "ok",
      service: "rokur",
      environment: ENVIRONMENT,
      policyConfigured: REQUIRED_SECRETS.length > 0,
      policyBackend: policyEvaluator.config.resolvedBackend,
      tokenAuthEnabled: API_TOKEN.length > 0,
      timestamp: new Date().toISOString(),
    });
  }

  if (method === "GET" && pathname === "/v1/secrets/status") {
    if (!isAuthorizedRequest(request)) {
      return jsonResponse({ error: "Unauthorized" }, 401);
    }

    const payload = await decisionPayload({}, REQUIRED_SECRETS);
    return jsonResponse(payload, payload.allowed ? 200 : 409);
  }

  if (method === "POST" && pathname === "/v1/authorize-start") {
    if (!isAuthorizedRequest(request)) {
      return jsonResponse({ error: "Unauthorized" }, 401);
    }

    const body = await parseJsonBody(request);
    if (!body) {
      return jsonResponse({ error: "Invalid JSON body" }, 400);
    }

    const payload = await decisionPayload(body, REQUIRED_SECRETS);
    return jsonResponse(payload, payload.allowed ? 200 : 409);
  }

  return jsonResponse({ error: "Not Found", path: pathname }, 404);
};

console.log(
  JSON.stringify({
    level: "INFO",
    message: "Starting Rokur",
    host: HOST,
    port: PORT,
    environment: ENVIRONMENT,
    requiredSecretCount: REQUIRED_SECRETS.length,
    policyBackend: policyEvaluator.config.resolvedBackend,
    policyExternalCommandConfigured: policyEvaluator.config.externalCommandConfigured,
    allowUnauthenticated: ALLOW_UNAUTHENTICATED,
    allowEmptyRequiredSecrets: ALLOW_EMPTY_REQUIRED_SECRETS,
    tokenAuthEnabled: API_TOKEN.length > 0,
  }),
);

validateStartupConfiguration();

Deno.serve({ hostname: HOST, port: PORT }, serverHandler);
