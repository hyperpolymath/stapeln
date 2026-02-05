// SPDX-License-Identifier: PMPL-1.0-or-later
// dom_mounter_security.zig - Phase 2 security hardening

const std = @import("std");

// ============================================================================
// PHASE 2: SECURITY HARDENING
// ============================================================================

pub const CSPResult = enum(c_int) {
    Valid = 0,
    InvalidChars = 1,
    ScriptDetected = 2,
    TooLong = 3,
};

pub const AuditSeverity = enum(c_int) {
    Info = 0,
    Warning = 1,
    Error = 2,
    Critical = 3,
};

pub const SandboxMode = enum(c_int) {
    NoSandbox = 0,
    IframeSandbox = 1,
    ShadowDOMSandbox = 2,
};

// ============================================================================
// CSP VALIDATION
// ============================================================================

fn isValidChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or
           (c >= 'A' and c <= 'Z') or
           (c >= '0' and c <= '9') or
           c == '-' or c == '_';
}

fn containsScriptPattern(element_id: [*:0]const u8) bool {
    const patterns = [_][]const u8{
        "<script",
        "javascript:",
        "onerror=",
        "onclick=",
        "onload=",
        "eval(",
        "document.",
    };

    const id_len = std.mem.len(element_id);
    const id_slice = element_id[0..id_len];

    for (patterns) |pattern| {
        if (std.mem.indexOf(u8, id_slice, pattern)) |_| {
            return true;
        }
    }

    return false;
}

// Validate element ID against CSP rules
export fn validate_csp(element_id: [*:0]const u8) c_int {
    const id_len = std.mem.len(element_id);

    // Check 1: Empty or too long
    if (id_len == 0 or id_len > 255) {
        return @intFromEnum(CSPResult.TooLong);
    }

    // Check 2: Script injection patterns
    if (containsScriptPattern(element_id)) {
        return @intFromEnum(CSPResult.ScriptDetected);
    }

    // Check 3: Valid characters only
    for (0..id_len) |i| {
        if (!isValidChar(element_id[i])) {
            return @intFromEnum(CSPResult.InvalidChars);
        }
    }

    return @intFromEnum(CSPResult.Valid);
}

// Sanitize element ID (remove dangerous chars)
export fn sanitize_element_id(
    element_id: [*:0]const u8,
    output: [*]u8,
    output_len: c_int,
) c_int {
    const id_len = std.mem.len(element_id);
    var written: usize = 0;

    for (0..id_len) |i| {
        if (written >= @as(usize, @intCast(output_len - 1))) break;

        if (isValidChar(element_id[i])) {
            output[written] = element_id[i];
            written += 1;
        }
    }

    output[written] = 0; // null terminator
    return @intCast(written);
}

// ============================================================================
// AUDIT LOGGING
// ============================================================================

// Audit log entry structure
pub const AuditEntry = struct {
    timestamp: i64,
    operation: [32]u8,
    element_id: [256]u8,
    severity: AuditSeverity,
    message: [512]u8,
};

// Global audit log buffer (thread-local would be better)
var audit_log: [1000]AuditEntry = undefined;
var audit_log_index: usize = 0;

// Log an audit entry
export fn audit_log_entry(
    operation: [*:0]const u8,
    element_id: [*:0]const u8,
    severity: c_int,
    message: [*:0]const u8,
) c_int {
    if (audit_log_index >= audit_log.len) {
        return -1; // Log full
    }

    var entry = &audit_log[audit_log_index];

    // Set timestamp (would use real time in production)
    entry.timestamp = std.time.milliTimestamp();

    // Copy operation
    const op_len = @min(std.mem.len(operation), 31);
    @memcpy(entry.operation[0..op_len], operation[0..op_len]);
    entry.operation[op_len] = 0;

    // Copy element ID
    const id_len = @min(std.mem.len(element_id), 255);
    @memcpy(entry.element_id[0..id_len], element_id[0..id_len]);
    entry.element_id[id_len] = 0;

    // Set severity
    entry.severity = @enumFromInt(severity);

    // Copy message
    const msg_len = @min(std.mem.len(message), 511);
    @memcpy(entry.message[0..msg_len], message[0..msg_len]);
    entry.message[msg_len] = 0;

    audit_log_index += 1;
    return 0; // Success
}

// Get audit log count
export fn get_audit_log_count() c_int {
    return @intCast(audit_log_index);
}

// Clear audit log
export fn clear_audit_log() void {
    audit_log_index = 0;
}

// Get audit entry by index
export fn get_audit_entry(index: c_int, entry_out: *AuditEntry) c_int {
    if (index < 0 or index >= @as(c_int, @intCast(audit_log_index))) {
        return -1; // Invalid index
    }

    entry_out.* = audit_log[@intCast(index)];
    return 0; // Success
}

// ============================================================================
// SANDBOXING
// ============================================================================

// Create sandboxed mount environment
export fn create_sandboxed_mount(element_id: [*:0]const u8, mode: c_int) c_int {
    const id_len = std.mem.len(element_id);
    if (id_len == 0) {
        return -1; // Invalid element ID
    }

    // Validate sandbox mode
    if (mode < 0 or mode > 2) {
        return -2; // Invalid sandbox mode
    }

    const sandbox_mode: SandboxMode = @enumFromInt(mode);

    // Log sandboxed mount attempt
    const log_result = audit_log_entry(
        "sandboxed_mount",
        element_id,
        @intFromEnum(AuditSeverity.Info),
        "Creating sandboxed mount environment",
    );

    _ = log_result;
    _ = sandbox_mode;

    // In actual implementation, would create iframe or shadow DOM
    return 0; // Success
}

// Validate sandbox configuration
export fn validate_sandbox_config(
    allow_scripts: c_int,
    allow_same_origin: c_int,
    allow_forms: c_int,
) c_int {
    // Iframe sandbox must have at least one restriction
    if (allow_scripts != 0 and allow_same_origin != 0 and allow_forms != 0) {
        return -1; // Too permissive
    }

    return 0; // Valid
}

// ============================================================================
// SECURITY POLICY
// ============================================================================

pub const SecurityPolicy = struct {
    require_csp: bool,
    enable_audit_log: bool,
    sandbox_mode: SandboxMode,
    max_element_id_length: usize,
};

// Default secure policy (pass pointer for export)
export fn get_default_security_policy(policy_out: *SecurityPolicy) void {
    policy_out.* = SecurityPolicy{
        .require_csp = true,
        .enable_audit_log = true,
        .sandbox_mode = SandboxMode.NoSandbox,
        .max_element_id_length = 255,
    };
}

// Apply security policy to element ID
export fn apply_security_policy(
    policy: *const SecurityPolicy,
    element_id: [*:0]const u8,
) c_int {
    const id_len = std.mem.len(element_id);

    // Check length
    if (id_len > policy.max_element_id_length) {
        if (policy.enable_audit_log) {
            _ = audit_log_entry(
                "security_check",
                element_id,
                @intFromEnum(AuditSeverity.Error),
                "Element ID exceeds maximum length",
            );
        }
        return -1; // Length check failed
    }

    // Check CSP
    if (policy.require_csp) {
        const csp_result = validate_csp(element_id);
        if (csp_result != @intFromEnum(CSPResult.Valid)) {
            if (policy.enable_audit_log) {
                _ = audit_log_entry(
                    "security_check",
                    element_id,
                    @intFromEnum(AuditSeverity.Critical),
                    "CSP validation failed",
                );
            }
            return -2; // CSP check failed
        }
    }

    return 0; // All checks passed
}

// ============================================================================
// TESTS
// ============================================================================

test "CSP validates safe element ID" {
    const safe_id = "app-root";
    const result = validate_csp(safe_id.ptr);
    try std.testing.expectEqual(@intFromEnum(CSPResult.Valid), result);
}

test "CSP rejects script injection" {
    const id1 = "<script>alert(1)</script>";
    const id2 = "javascript:alert(1)";
    const id3 = "app-root onerror=alert(1)";
    const id4 = "onclick=steal()";

    try std.testing.expectEqual(@intFromEnum(CSPResult.ScriptDetected), validate_csp(id1.ptr));
    try std.testing.expectEqual(@intFromEnum(CSPResult.ScriptDetected), validate_csp(id2.ptr));
    try std.testing.expectEqual(@intFromEnum(CSPResult.ScriptDetected), validate_csp(id3.ptr));
    try std.testing.expectEqual(@intFromEnum(CSPResult.ScriptDetected), validate_csp(id4.ptr));
}

test "CSP rejects invalid characters" {
    const id1 = "app root"; // space
    const id2 = "app@root"; // @
    const id3 = "app#root"; // #
    const id4 = "app$root"; // $

    try std.testing.expectEqual(@intFromEnum(CSPResult.InvalidChars), validate_csp(id1.ptr));
    try std.testing.expectEqual(@intFromEnum(CSPResult.InvalidChars), validate_csp(id2.ptr));
    try std.testing.expectEqual(@intFromEnum(CSPResult.InvalidChars), validate_csp(id3.ptr));
    try std.testing.expectEqual(@intFromEnum(CSPResult.InvalidChars), validate_csp(id4.ptr));
}

test "sanitize removes dangerous characters" {
    const input = "app<script>root";
    var output: [256]u8 = undefined;

    const written = sanitize_element_id(input.ptr, &output, 256);
    const sanitized = output[0..@intCast(written)];

    try std.testing.expectEqualStrings("appscriptroot", sanitized);
}

test "audit log entry creation" {
    clear_audit_log();

    const result = audit_log_entry(
        "mount",
        "test-element",
        @intFromEnum(AuditSeverity.Info),
        "Test mount operation",
    );

    try std.testing.expectEqual(@as(c_int, 0), result);
    try std.testing.expectEqual(@as(c_int, 1), get_audit_log_count());
}

test "audit log retrieval" {
    clear_audit_log();

    _ = audit_log_entry(
        "mount",
        "test-element",
        @intFromEnum(AuditSeverity.Info),
        "Test message",
    );

    var entry: AuditEntry = undefined;
    const result = get_audit_entry(0, &entry);

    try std.testing.expectEqual(@as(c_int, 0), result);
    try std.testing.expectEqual(AuditSeverity.Info, entry.severity);
}

test "sandbox validation" {
    // Too permissive (all allowed)
    const invalid = validate_sandbox_config(1, 1, 1);
    try std.testing.expectEqual(@as(c_int, -1), invalid);

    // Valid (at least one restriction)
    const valid = validate_sandbox_config(1, 0, 0);
    try std.testing.expectEqual(@as(c_int, 0), valid);
}

test "security policy application" {
    var policy: SecurityPolicy = undefined;
    get_default_security_policy(&policy);

    // Valid element ID
    const valid_id = "app-root";
    const valid_result = apply_security_policy(&policy, valid_id.ptr);
    try std.testing.expectEqual(@as(c_int, 0), valid_result);

    // Invalid (script injection)
    const invalid_id = "<script>alert(1)</script>";
    const invalid_result = apply_security_policy(&policy, invalid_id.ptr);
    try std.testing.expectEqual(@as(c_int, -2), invalid_result);
}
