// SPDX-License-Identifier: PMPL-1.0-or-later
// Rokur audit log — structured, append-only record of every authorization decision.

let auditFileHandle = null;
let auditEnabled = true;

/**
 * Opens the audit log file handle (if file-based logging is configured).
 * Reads configuration from environment at call time (not import time).
 * Call once at startup. Safe to call multiple times (idempotent after close).
 *
 * @param {Object} [options] - Optional overrides for testing.
 * @param {boolean} [options.enabled] - Override ROKUR_AUDIT_LOG env.
 * @param {string} [options.path] - Override ROKUR_AUDIT_LOG_PATH env.
 */
export async function openAuditLog(options = {}) {
  auditEnabled = options.enabled ??
    (Deno.env.get("ROKUR_AUDIT_LOG") ?? "true").trim().toLowerCase() !==
      "false";
  const logPath = options.path ??
    (Deno.env.get("ROKUR_AUDIT_LOG_PATH") ?? "").trim();

  if (!auditEnabled || !logPath) {
    return;
  }

  try {
    auditFileHandle = await Deno.open(logPath, {
      write: true,
      create: true,
      append: true,
    });
  } catch (error) {
    console.error(JSON.stringify({
      level: "WARN",
      message: "Failed to open audit log file, falling back to stderr",
      path: logPath,
      error: error instanceof Error ? error.message : String(error),
      service: "rokur",
    }));
    auditFileHandle = null;
  }
}

/**
 * Closes the audit log file handle. Call on shutdown.
 */
export function closeAuditLog() {
  if (auditFileHandle) {
    try {
      auditFileHandle.close();
    } catch {
      // Ignore close errors during shutdown.
    }
    auditFileHandle = null;
  }
}

/**
 * Records an authorization decision to the audit log.
 *
 * @param {Object} entry - Audit entry fields.
 * @param {string} entry.requestId - Unique request identifier.
 * @param {string} entry.action - The action being authorized.
 * @param {boolean} entry.allowed - Whether the request was allowed.
 * @param {string} entry.code - Decision code.
 * @param {string} entry.engine - Policy engine used.
 * @param {string|null} entry.image - Container image (if applicable).
 * @param {string|null} entry.name - Container name (if applicable).
 * @param {number} entry.requiredSecretCount - Number of required secrets.
 * @param {number} entry.missingSecretCount - Number of missing secrets.
 * @param {string} entry.clientIp - Client IP address.
 * @param {boolean} entry.authenticated - Whether the request carried a valid token.
 */
export async function recordDecision(entry) {
  if (!auditEnabled) {
    return;
  }

  const record = {
    type: "authorization_decision",
    timestamp: new Date().toISOString(),
    requestId: entry.requestId,
    action: entry.action,
    allowed: entry.allowed,
    code: entry.code,
    engine: entry.engine,
    image: entry.image ?? null,
    name: entry.name ?? null,
    requiredSecretCount: entry.requiredSecretCount,
    missingSecretCount: entry.missingSecretCount,
    clientIp: entry.clientIp,
    authenticated: entry.authenticated,
  };

  await writeRecord(record);
}

/**
 * Records an authentication failure to the audit log.
 *
 * @param {Object} entry - Auth failure entry fields.
 * @param {string} entry.requestId - Unique request identifier.
 * @param {string} entry.path - Request path.
 * @param {string} entry.clientIp - Client IP address.
 */
export async function recordAuthFailure(entry) {
  if (!auditEnabled) {
    return;
  }

  const record = {
    type: "authentication_failure",
    timestamp: new Date().toISOString(),
    requestId: entry.requestId,
    path: entry.path,
    clientIp: entry.clientIp,
  };

  await writeRecord(record);
}

/**
 * Writes a record to the audit file or stderr.
 *
 * @param {Object} record
 */
async function writeRecord(record) {
  const line = JSON.stringify(record) + "\n";

  if (auditFileHandle) {
    try {
      const encoder = new TextEncoder();
      await auditFileHandle.write(encoder.encode(line));
    } catch {
      // Fall through to stderr.
      console.error(line.trimEnd());
    }
  } else {
    console.error(line.trimEnd());
  }
}
