// SPDX-License-Identifier: PMPL-1.0-or-later
// Rokur configuration — centralised environment variable parsing.
//
// All Deno.env.get() calls for Rokur configuration are consolidated here.
// Call loadConfig() at startup and again on SIGHUP to pick up changes.

/**
 * Parses a boolean from an environment variable.
 *
 * Accepts: true/false, 1/0, yes/no, on/off (case-insensitive).
 * Returns the defaultValue when the variable is unset or empty.
 * Throws on unrecognised values.
 *
 * @param {string} name - Environment variable name.
 * @param {boolean} defaultValue - Value when the variable is absent.
 * @returns {boolean}
 */
function parseBooleanEnv(name, defaultValue = false) {
  const rawValue = Deno.env.get(name);
  if (
    rawValue === undefined || rawValue === null || rawValue.trim().length === 0
  ) {
    return defaultValue;
  }

  const normalized = rawValue.trim().toLowerCase();
  if (
    normalized === "1" || normalized === "true" || normalized === "yes" ||
    normalized === "on"
  ) {
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

/**
 * Parses ROKUR_REQUIRED_SECRETS into a deduplicated array.
 *
 * @returns {string[]}
 */
function parseRequiredSecrets() {
  return Array.from(
    new Set(
      (Deno.env.get("ROKUR_REQUIRED_SECRETS") ?? "")
        .split(",")
        .map((value) => value.trim())
        .filter((value) => value.length > 0),
    ),
  );
}

/**
 * Parses a positive integer from an environment variable.
 *
 * @param {string} name - Environment variable name.
 * @param {number} defaultValue - Fallback when absent or invalid.
 * @returns {number}
 */
function parsePositiveInt(name, defaultValue) {
  const raw = Deno.env.get(name);
  if (!raw || raw.trim().length === 0) {
    return defaultValue;
  }

  const parsed = Number(raw);
  if (!Number.isInteger(parsed) || parsed < 1) {
    return defaultValue;
  }

  return parsed;
}

/**
 * Loads every Rokur configuration value from the environment.
 *
 * Safe to call multiple times — each invocation re-reads the environment so
 * that SIGHUP-triggered reloads pick up changes.
 *
 * @returns {object} Configuration object.
 */
export function loadConfig() {
  const host = Deno.env.get("ROKUR_HOST") ?? "127.0.0.1";
  const port = Number(Deno.env.get("ROKUR_PORT") ?? "9090");
  const apiToken = (Deno.env.get("ROKUR_API_TOKEN") ?? "").trim();
  const requiredSecrets = parseRequiredSecrets();
  const env = (Deno.env.get("ROKUR_ENV") ?? "development").trim().toLowerCase();

  const policyBackend = (Deno.env.get("ROKUR_POLICY_BACKEND") ?? "builtin")
    .trim().toLowerCase();
  const policyCommand = (Deno.env.get("ROKUR_POLICY_COMMAND") ?? "").trim();
  const policyCommandArgs = (Deno.env.get("ROKUR_POLICY_COMMAND_ARGS") ?? "")
    .trim();
  const policyTimeoutMs = parsePositiveInt("ROKUR_POLICY_TIMEOUT_MS", 1500);

  const rateLimitWindowMs = parsePositiveInt(
    "ROKUR_RATE_LIMIT_WINDOW_MS",
    60_000,
  );
  const rateLimitMax = parsePositiveInt("ROKUR_RATE_LIMIT_MAX", 60);
  const rateLimitAuthFailMax = parsePositiveInt(
    "ROKUR_RATE_LIMIT_AUTH_FAIL_MAX",
    5,
  );

  const auditLogEnabled = parseBooleanEnv("ROKUR_AUDIT_LOG", true);
  const auditLogPath = (Deno.env.get("ROKUR_AUDIT_LOG_PATH") ?? "").trim();
  const requestLogEnabled = parseBooleanEnv("ROKUR_REQUEST_LOG", true);

  const allowUnauthenticated = parseBooleanEnv(
    "ROKUR_ALLOW_UNAUTHENTICATED",
    false,
  );
  const allowEmptyRequiredSecrets = parseBooleanEnv(
    "ROKUR_ALLOW_EMPTY_REQUIRED_SECRETS",
    false,
  );

  return {
    host,
    port,
    apiToken,
    requiredSecrets,
    env,
    isProduction: env === "production",

    policyBackend,
    policyCommand,
    policyCommandArgs,
    policyTimeoutMs,

    rateLimitWindowMs,
    rateLimitMax,
    rateLimitAuthFailMax,

    auditLogEnabled,
    auditLogPath,
    requestLogEnabled,

    allowUnauthenticated,
    allowEmptyRequiredSecrets,
  };
}
