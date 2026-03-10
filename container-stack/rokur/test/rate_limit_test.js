// SPDX-License-Identifier: PMPL-1.0-or-later
// Unit tests for Rokur rate limiter.

import { assertEquals } from "@std/assert";
import { afterEach, beforeEach, describe, it } from "@std/testing/bdd";
import { createRateLimiter } from "../rate_limit.js";

let limiter;

function cleanRateLimitEnv() {
  for (
    const key of [
      "ROKUR_RATE_LIMIT_WINDOW_MS",
      "ROKUR_RATE_LIMIT_MAX",
      "ROKUR_RATE_LIMIT_AUTH_FAIL_MAX",
    ]
  ) {
    try {
      Deno.env.delete(key);
    } catch { /* noop */ }
  }
}

describe("rate limiter", () => {
  beforeEach(() => {
    cleanRateLimitEnv();
  });

  afterEach(() => {
    if (limiter) {
      limiter.destroy();
      limiter = null;
    }
    cleanRateLimitEnv();
  });

  it("allows requests under the limit", () => {
    Deno.env.set("ROKUR_RATE_LIMIT_MAX", "5");
    limiter = createRateLimiter();

    for (let i = 0; i < 5; i++) {
      const result = limiter.checkRequest("10.0.0.1");
      assertEquals(result.allowed, true);
    }
  });

  it("denies requests over the general limit", () => {
    Deno.env.set("ROKUR_RATE_LIMIT_MAX", "3");
    limiter = createRateLimiter();

    limiter.checkRequest("10.0.0.1");
    limiter.checkRequest("10.0.0.1");
    limiter.checkRequest("10.0.0.1");
    const result = limiter.checkRequest("10.0.0.1");

    assertEquals(result.allowed, false);
    assertEquals(result.reason, "REQUEST_RATE_EXCEEDED");
  });

  it("tracks IPs independently", () => {
    Deno.env.set("ROKUR_RATE_LIMIT_MAX", "2");
    limiter = createRateLimiter();

    limiter.checkRequest("10.0.0.1");
    limiter.checkRequest("10.0.0.1");
    const blocked = limiter.checkRequest("10.0.0.1");
    assertEquals(blocked.allowed, false);

    // Different IP should still be allowed.
    const different = limiter.checkRequest("10.0.0.2");
    assertEquals(different.allowed, true);
  });

  it("denies after auth failure threshold", () => {
    Deno.env.set("ROKUR_RATE_LIMIT_AUTH_FAIL_MAX", "2");
    Deno.env.set("ROKUR_RATE_LIMIT_MAX", "100");
    limiter = createRateLimiter();

    limiter.recordAuthFailure("10.0.0.1");
    limiter.recordAuthFailure("10.0.0.1");

    const result = limiter.checkRequest("10.0.0.1");
    assertEquals(result.allowed, false);
    assertEquals(result.reason, "AUTH_FAIL_RATE_EXCEEDED");
  });

  it("auth failures do not affect other IPs", () => {
    Deno.env.set("ROKUR_RATE_LIMIT_AUTH_FAIL_MAX", "1");
    limiter = createRateLimiter();

    limiter.recordAuthFailure("10.0.0.1");

    const blocked = limiter.checkRequest("10.0.0.1");
    assertEquals(blocked.allowed, false);

    const ok = limiter.checkRequest("10.0.0.2");
    assertEquals(ok.allowed, true);
  });

  it("reset clears all state", () => {
    Deno.env.set("ROKUR_RATE_LIMIT_MAX", "1");
    limiter = createRateLimiter();

    limiter.checkRequest("10.0.0.1");
    const blocked = limiter.checkRequest("10.0.0.1");
    assertEquals(blocked.allowed, false);

    limiter.reset();

    const afterReset = limiter.checkRequest("10.0.0.1");
    assertEquals(afterReset.allowed, true);
  });

  it("stats returns configuration and state", () => {
    Deno.env.set("ROKUR_RATE_LIMIT_MAX", "42");
    Deno.env.set("ROKUR_RATE_LIMIT_AUTH_FAIL_MAX", "7");
    Deno.env.set("ROKUR_RATE_LIMIT_WINDOW_MS", "30000");
    limiter = createRateLimiter();

    limiter.checkRequest("10.0.0.1");
    limiter.checkRequest("10.0.0.2");

    const stats = limiter.stats();
    assertEquals(stats.maxRequests, 42);
    assertEquals(stats.authFailMax, 7);
    assertEquals(stats.windowMs, 30000);
    assertEquals(stats.trackedIps, 2);
  });

  it("returns positive retryAfterMs when rate limited", () => {
    Deno.env.set("ROKUR_RATE_LIMIT_MAX", "1");
    limiter = createRateLimiter();

    limiter.checkRequest("10.0.0.1");
    const result = limiter.checkRequest("10.0.0.1");

    assertEquals(result.allowed, false);
    assertEquals(result.retryAfterMs > 0, true);
  });
});
