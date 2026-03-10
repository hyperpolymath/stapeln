// SPDX-License-Identifier: PMPL-1.0-or-later
// Rokur rate limiter — sliding window per-IP throttle for token brute-force protection.

const DEFAULT_WINDOW_MS = 60_000;
const DEFAULT_MAX_REQUESTS = 60;
const DEFAULT_AUTH_FAIL_MAX = 5;

/**
 * Creates a rate limiter with configurable windows.
 *
 * Two separate limits:
 * - General request rate: ROKUR_RATE_LIMIT_MAX per ROKUR_RATE_LIMIT_WINDOW_MS (default 60/min)
 * - Auth failure rate: ROKUR_RATE_LIMIT_AUTH_FAIL_MAX per window (default 5/min)
 *
 * Auth failure limit is stricter to protect against token brute-force.
 *
 * @returns {{ checkRequest: (ip: string) => { allowed: boolean, retryAfterMs: number }, recordAuthFailure: (ip: string) => void, reset: () => void, stats: () => object }}
 */
export function createRateLimiter() {
  const windowMs = parsePositiveInt(
    "ROKUR_RATE_LIMIT_WINDOW_MS",
    DEFAULT_WINDOW_MS,
  );
  const maxRequests = parsePositiveInt(
    "ROKUR_RATE_LIMIT_MAX",
    DEFAULT_MAX_REQUESTS,
  );
  const authFailMax = parsePositiveInt(
    "ROKUR_RATE_LIMIT_AUTH_FAIL_MAX",
    DEFAULT_AUTH_FAIL_MAX,
  );

  /** @type {Map<string, number[]>} */
  const requestWindows = new Map();

  /** @type {Map<string, number[]>} */
  const authFailWindows = new Map();

  // Periodic cleanup to prevent memory growth from stale entries.
  const cleanupInterval = setInterval(() => {
    const cutoff = Date.now() - windowMs;
    pruneMap(requestWindows, cutoff);
    pruneMap(authFailWindows, cutoff);
  }, windowMs * 2);

  // Prevent cleanup timer from keeping the process alive.
  if (typeof cleanupInterval === "number") {
    Deno.unrefTimer(cleanupInterval);
  }

  return {
    /**
     * Checks whether a request from the given IP is within rate limits.
     * Also checks auth failure rate — if the IP has too many auth failures,
     * it is denied even if general rate is fine.
     *
     * @param {string} ip - Client IP address.
     * @returns {{ allowed: boolean, retryAfterMs: number, reason: string|null }}
     */
    checkRequest(ip) {
      const now = Date.now();
      const cutoff = now - windowMs;

      // Check auth failure rate first (stricter).
      const authFails = getWindow(authFailWindows, ip, cutoff);
      if (authFails.length >= authFailMax) {
        const oldestFail = authFails[0];
        const retryAfterMs = oldestFail + windowMs - now;
        return {
          allowed: false,
          retryAfterMs,
          reason: "AUTH_FAIL_RATE_EXCEEDED",
        };
      }

      // Check general request rate.
      const requests = getWindow(requestWindows, ip, cutoff);
      if (requests.length >= maxRequests) {
        const oldest = requests[0];
        const retryAfterMs = oldest + windowMs - now;
        return {
          allowed: false,
          retryAfterMs,
          reason: "REQUEST_RATE_EXCEEDED",
        };
      }

      // Record this request.
      requests.push(now);
      requestWindows.set(ip, requests);

      return { allowed: true, retryAfterMs: 0, reason: null };
    },

    /**
     * Records an authentication failure for rate limiting purposes.
     *
     * @param {string} ip - Client IP address.
     */
    recordAuthFailure(ip) {
      const now = Date.now();
      const cutoff = now - windowMs;
      const fails = getWindow(authFailWindows, ip, cutoff);
      fails.push(now);
      authFailWindows.set(ip, fails);
    },

    /**
     * Resets all rate limit state. Useful for testing.
     */
    reset() {
      requestWindows.clear();
      authFailWindows.clear();
    },

    /**
     * Returns rate limiter statistics for the metrics endpoint.
     */
    stats() {
      return {
        trackedIps: requestWindows.size,
        trackedAuthFailIps: authFailWindows.size,
        windowMs,
        maxRequests,
        authFailMax,
      };
    },

    /**
     * Stops the background cleanup timer. Call on shutdown or in tests.
     */
    destroy() {
      clearInterval(cleanupInterval);
    },
  };
}

/**
 * Gets the sliding window for a given IP, pruning expired entries.
 *
 * @param {Map<string, number[]>} map
 * @param {string} ip
 * @param {number} cutoff
 * @returns {number[]}
 */
function getWindow(map, ip, cutoff) {
  const existing = map.get(ip);
  if (!existing) {
    const fresh = [];
    map.set(ip, fresh);
    return fresh;
  }

  // Remove entries older than the window.
  const pruned = existing.filter((timestamp) => timestamp > cutoff);
  map.set(ip, pruned);
  return pruned;
}

/**
 * Prunes stale entries from a rate limit map.
 *
 * @param {Map<string, number[]>} map
 * @param {number} cutoff
 */
function pruneMap(map, cutoff) {
  for (const [ip, timestamps] of map) {
    const pruned = timestamps.filter((t) => t > cutoff);
    if (pruned.length === 0) {
      map.delete(ip);
    } else {
      map.set(ip, pruned);
    }
  }
}

/**
 * Parses a positive integer from an environment variable.
 *
 * @param {string} name
 * @param {number} defaultValue
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
