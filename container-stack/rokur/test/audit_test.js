// SPDX-License-Identifier: PMPL-1.0-or-later
// Unit tests for Rokur audit log.

import { assertEquals } from "@std/assert";
import { afterEach, describe, it } from "@std/testing/bdd";
import {
  closeAuditLog,
  openAuditLog,
  recordAuthFailure,
  recordDecision,
} from "../audit.js";

describe("audit log", () => {
  afterEach(() => {
    closeAuditLog();
  });

  it("recordDecision writes structured JSON to file", async () => {
    const tmpFile = await Deno.makeTempFile({ suffix: ".jsonl" });

    await openAuditLog({ enabled: true, path: tmpFile });
    await recordDecision({
      requestId: "req-001",
      action: "authorize-start",
      allowed: true,
      code: "AUTHORIZED",
      engine: "builtin",
      image: "alpine:3.19",
      name: "test-container",
      requiredSecretCount: 2,
      missingSecretCount: 0,
      clientIp: "10.0.0.1",
      authenticated: true,
    });
    closeAuditLog();

    const content = await Deno.readTextFile(tmpFile);
    const record = JSON.parse(content.trim());

    assertEquals(record.type, "authorization_decision");
    assertEquals(record.requestId, "req-001");
    assertEquals(record.action, "authorize-start");
    assertEquals(record.allowed, true);
    assertEquals(record.code, "AUTHORIZED");
    assertEquals(record.engine, "builtin");
    assertEquals(record.image, "alpine:3.19");
    assertEquals(record.name, "test-container");
    assertEquals(record.clientIp, "10.0.0.1");
    assertEquals(record.authenticated, true);
    assertEquals(typeof record.timestamp, "string");

    await Deno.remove(tmpFile);
  });

  it("recordAuthFailure writes authentication failure record", async () => {
    const tmpFile = await Deno.makeTempFile({ suffix: ".jsonl" });

    await openAuditLog({ enabled: true, path: tmpFile });
    await recordAuthFailure({
      requestId: "req-002",
      path: "/v1/authorize-start",
      clientIp: "192.168.1.100",
    });
    closeAuditLog();

    const content = await Deno.readTextFile(tmpFile);
    const record = JSON.parse(content.trim());

    assertEquals(record.type, "authentication_failure");
    assertEquals(record.requestId, "req-002");
    assertEquals(record.path, "/v1/authorize-start");
    assertEquals(record.clientIp, "192.168.1.100");

    await Deno.remove(tmpFile);
  });

  it("appends multiple records to the same file", async () => {
    const tmpFile = await Deno.makeTempFile({ suffix: ".jsonl" });

    await openAuditLog({ enabled: true, path: tmpFile });

    await recordDecision({
      requestId: "req-a",
      action: "secrets-status",
      allowed: true,
      code: "AUTHORIZED",
      engine: "builtin",
      requiredSecretCount: 1,
      missingSecretCount: 0,
      clientIp: "10.0.0.1",
      authenticated: true,
    });

    await recordDecision({
      requestId: "req-b",
      action: "authorize-start",
      allowed: false,
      code: "REQUIRED_SECRETS_MISSING",
      engine: "builtin",
      image: "nginx:latest",
      requiredSecretCount: 3,
      missingSecretCount: 2,
      clientIp: "10.0.0.2",
      authenticated: true,
    });

    closeAuditLog();

    const content = await Deno.readTextFile(tmpFile);
    const lines = content.trim().split("\n");
    assertEquals(lines.length, 2);

    const first = JSON.parse(lines[0]);
    const second = JSON.parse(lines[1]);
    assertEquals(first.requestId, "req-a");
    assertEquals(second.requestId, "req-b");
    assertEquals(second.allowed, false);

    await Deno.remove(tmpFile);
  });

  it("does nothing when audit log is disabled", async () => {
    await openAuditLog({ enabled: false });

    // These should silently do nothing.
    await recordDecision({
      requestId: "req-x",
      action: "test",
      allowed: true,
      code: "TEST",
      engine: "builtin",
      requiredSecretCount: 0,
      missingSecretCount: 0,
      clientIp: "10.0.0.1",
      authenticated: true,
    });
    await recordAuthFailure({
      requestId: "req-y",
      path: "/test",
      clientIp: "10.0.0.1",
    });

    // No crash = pass.
  });

  it("falls back to stderr when file path is invalid", async () => {
    // Should not throw — falls back to stderr.
    await openAuditLog({ enabled: true, path: "/nonexistent/dir/audit.jsonl" });
    await recordDecision({
      requestId: "req-z",
      action: "test",
      allowed: true,
      code: "TEST",
      engine: "builtin",
      requiredSecretCount: 0,
      missingSecretCount: 0,
      clientIp: "10.0.0.1",
      authenticated: true,
    });
  });
});
