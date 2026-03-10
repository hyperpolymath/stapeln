// SPDX-License-Identifier: PMPL-1.0-or-later
// Vordr Integration Tests
//
// These tests verify that the Zig FFI correctly implements the Idris2 ABI
// declared in src/abi/Foreign.idr. They link against the shared library and
// exercise every exported symbol through the C ABI.

const std = @import("std");
const testing = std.testing;

// ---------------------------------------------------------------------------
// Import FFI functions (extern "C" — matches Foreign.idr declarations)
// ---------------------------------------------------------------------------

extern fn vordr_init() ?*anyopaque;
extern fn vordr_free(?*anyopaque) void;
extern fn vordr_process(?*anyopaque, u32) u32;
extern fn vordr_process_array(?*anyopaque, ?[*]const u8, u32) u32;
extern fn vordr_get_string(?*anyopaque) ?[*:0]const u8;
extern fn vordr_free_string(?[*:0]const u8) void;
extern fn vordr_last_error() ?[*:0]const u8;
extern fn vordr_version() [*:0]const u8;
extern fn vordr_build_info() [*:0]const u8;
extern fn vordr_is_initialized(?*anyopaque) u32;
extern fn vordr_register_callback(?*anyopaque, ?*const fn (u64, u32) callconv(.c) u32) u32;
extern fn vordr_verify_image(?*anyopaque, ?[*:0]const u8) u32;
extern fn vordr_create_container(?*anyopaque, ?[*:0]const u8, ?[*:0]const u8) u32;
extern fn vordr_start_container(?*anyopaque, ?[*:0]const u8) u32;
extern fn vordr_stop_container(?*anyopaque, ?[*:0]const u8) u32;
extern fn vordr_list_containers(?*anyopaque, ?*[*:0]u8) u32;

// ---------------------------------------------------------------------------
// Result codes (mirror of Idris2 ABI Types.idr)
// ---------------------------------------------------------------------------

const RESULT_OK: u32 = 0;
const RESULT_ERR: u32 = 1;
const RESULT_INVALID_PARAM: u32 = 2;
const RESULT_OUT_OF_MEMORY: u32 = 3;
const RESULT_NULL_POINTER: u32 = 4;

// ---------------------------------------------------------------------------
// Policy capability flags (mirror of FFI constants)
// ---------------------------------------------------------------------------

const CAP_NET: u32 = 1 << 0;
const CAP_MOUNT: u32 = 1 << 1;
const CAP_PRIVILEGE: u32 = 1 << 2;
const CAP_IPC: u32 = 1 << 3;
const CAP_PID_NS: u32 = 1 << 4;
const CAP_RAW_DEVICE: u32 = 1 << 5;
const CAP_KMOD: u32 = 1 << 6;
const CAP_PTRACE: u32 = 1 << 7;

// ===========================================================================
// Lifecycle Tests
// ===========================================================================

test "create and destroy handle" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    // If we got here, handle is non-null (orelse would have returned)
    try testing.expect(true);
}

test "handle is initialized" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const initialized = vordr_is_initialized(handle);
    try testing.expectEqual(@as(u32, 1), initialized);
}

test "null handle is not initialized" {
    const initialized = vordr_is_initialized(null);
    try testing.expectEqual(@as(u32, 0), initialized);
}

test "double free is safe" {
    const handle = vordr_init() orelse return error.InitFailed;
    vordr_free(handle);
    vordr_free(handle); // must not crash
}

test "free null is safe" {
    vordr_free(null); // must not crash
}

// ===========================================================================
// Policy Evaluation Tests (vordr_process)
// ===========================================================================

test "process — null handle returns zero" {
    const result = vordr_process(null, CAP_NET);
    try testing.expectEqual(@as(u32, 0), result);
}

test "process — empty request returns sentinel" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const result = vordr_process(handle, 0);
    try testing.expectEqual(@as(u32, 0xFFFF_FFFF), result);
}

test "process — allowed capability is granted" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    // Default policy allows CAP_NET
    const result = vordr_process(handle, CAP_NET);
    try testing.expectEqual(CAP_NET, result);
}

test "process — multiple allowed caps" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    // Default allows CAP_NET | CAP_IPC
    const result = vordr_process(handle, CAP_NET | CAP_IPC);
    try testing.expectEqual(CAP_NET | CAP_IPC, result);
}

test "process — always-denied caps rejected" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    // CAP_KMOD is always denied
    const result = vordr_process(handle, CAP_KMOD);
    try testing.expectEqual(@as(u32, 0), result);
}

test "process — RAW_DEVICE always denied" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const result = vordr_process(handle, CAP_RAW_DEVICE);
    try testing.expectEqual(@as(u32, 0), result);
}

test "process — mixed allowed and denied returns zero (always-denied present)" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    // CAP_NET is allowed but CAP_KMOD is always-denied — entire request denied
    const result = vordr_process(handle, CAP_NET | CAP_KMOD);
    try testing.expectEqual(@as(u32, 0), result);
}

test "process — unrecognised bits rejected" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const result = vordr_process(handle, 0x8000_0000);
    try testing.expectEqual(@as(u32, 0), result);
}

test "process — disallowed but valid cap returns zero" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    // CAP_MOUNT is valid but not in the default allowed set
    const result = vordr_process(handle, CAP_MOUNT);
    try testing.expectEqual(@as(u32, 0), result);
}

// ===========================================================================
// Policy Document Validation Tests (vordr_process_array)
// ===========================================================================

test "process_array — valid v1.0 policy document" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const doc = [_]u8{ 'V', 'P', 'O', 'L', 1, 0, 0, 0 };
    const result = vordr_process_array(handle, &doc, doc.len);
    try testing.expectEqual(RESULT_OK, result);
}

test "process_array — valid document with payload" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const doc = [_]u8{ 'V', 'P', 'O', 'L', 1, 0, 0, 0, 0xAA, 0xBB, 0xCC, 0xDD };
    const result = vordr_process_array(handle, &doc, doc.len);
    try testing.expectEqual(RESULT_OK, result);
}

test "process_array — invalid magic bytes" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const doc = [_]u8{ 'N', 'O', 'P', 'E', 1, 0, 0, 0 };
    const result = vordr_process_array(handle, &doc, doc.len);
    try testing.expectEqual(RESULT_INVALID_PARAM, result);
}

test "process_array — document too short" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const doc = [_]u8{ 'V', 'P', 'O', 'L' };
    const result = vordr_process_array(handle, &doc, doc.len);
    try testing.expectEqual(RESULT_INVALID_PARAM, result);
}

test "process_array — unsupported version" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const doc = [_]u8{ 'V', 'P', 'O', 'L', 2, 0, 0, 0 };
    const result = vordr_process_array(handle, &doc, doc.len);
    try testing.expectEqual(RESULT_INVALID_PARAM, result);
}

test "process_array — nonzero reserved flags" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const doc = [_]u8{ 'V', 'P', 'O', 'L', 1, 0, 0xFF, 0x00 };
    const result = vordr_process_array(handle, &doc, doc.len);
    try testing.expectEqual(RESULT_INVALID_PARAM, result);
}

test "process_array — null buffer" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const result = vordr_process_array(handle, null, 8);
    try testing.expectEqual(RESULT_NULL_POINTER, result);
}

test "process_array — null handle" {
    const doc = [_]u8{ 'V', 'P', 'O', 'L', 1, 0, 0, 0 };
    const result = vordr_process_array(null, &doc, doc.len);
    try testing.expectEqual(RESULT_NULL_POINTER, result);
}

// ===========================================================================
// String Operation Tests
// ===========================================================================

test "get_string — returns context status" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const str = vordr_get_string(handle) orelse return error.NoString;
    defer vordr_free_string(str);

    const slice = std.mem.span(str);
    try testing.expect(slice.len > 0);
    try testing.expect(std.mem.indexOf(u8, slice, "vordr context") != null);
    try testing.expect(std.mem.indexOf(u8, slice, "initialized=true") != null);
}

test "get_string — null handle returns null" {
    const str = vordr_get_string(null);
    try testing.expect(str == null);
}

// ===========================================================================
// Error Handling Tests
// ===========================================================================

test "last_error — captures error after null handle operation" {
    _ = vordr_process(null, 0);

    const err = vordr_last_error();
    try testing.expect(err != null);

    if (err) |e| {
        const err_str = std.mem.span(e);
        try testing.expect(err_str.len > 0);
        try testing.expect(std.mem.indexOf(u8, err_str, "null") != null);
    }
}

// ===========================================================================
// Version Tests
// ===========================================================================

test "version string is 0.1.0-alpha" {
    const ver = std.mem.span(vordr_version());
    try testing.expectEqualStrings("0.1.0-alpha", ver);
}

test "version contains dots (semver)" {
    const ver = std.mem.span(vordr_version());
    try testing.expect(std.mem.count(u8, ver, ".") >= 1);
}

test "build_info contains version and Zig" {
    const info = std.mem.span(vordr_build_info());
    try testing.expect(info.len > 0);
    try testing.expect(std.mem.indexOf(u8, info, "0.1.0-alpha") != null);
    try testing.expect(std.mem.indexOf(u8, info, "Zig") != null);
}

// ===========================================================================
// Callback Tests
// ===========================================================================

test "register and invoke callback" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const State = struct {
        var invoked: bool = false;
        var event_kind: u32 = 999;
    };

    const cb = struct {
        fn callback(_: u64, kind: u32) callconv(.c) u32 {
            State.invoked = true;
            State.event_kind = kind;
            return 0;
        }
    }.callback;

    const reg = vordr_register_callback(handle, cb);
    try testing.expectEqual(RESULT_OK, reg);

    // Trigger callback via policy evaluation
    _ = vordr_process(handle, CAP_NET);
    try testing.expect(State.invoked);
    try testing.expectEqual(@as(u32, 0), State.event_kind);
}

test "register callback — null handle rejected" {
    const cb = struct {
        fn callback(_: u64, _: u32) callconv(.c) u32 {
            return 0;
        }
    }.callback;

    const result = vordr_register_callback(null, cb);
    try testing.expectEqual(RESULT_NULL_POINTER, result);
}

test "unregister callback with null" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const result = vordr_register_callback(handle, null);
    try testing.expectEqual(RESULT_OK, result);
}

// ===========================================================================
// Container Operation Tests
// ===========================================================================

test "verify_image — tagged image accepted" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const result = vordr_verify_image(handle, "cgr.dev/chainguard/wolfi-base:latest");
    try testing.expectEqual(RESULT_OK, result);
}

test "verify_image — untagged image rejected" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const result = vordr_verify_image(handle, "alpine");
    try testing.expectEqual(RESULT_INVALID_PARAM, result);
}

test "verify_image — null name rejected" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const result = vordr_verify_image(handle, null);
    try testing.expectEqual(RESULT_NULL_POINTER, result);
}

test "create_container — valid inputs" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const result = vordr_create_container(handle, "my-app", "alpine:3.19");
    try testing.expectEqual(RESULT_OK, result);
}

test "create_container — empty name rejected" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const result = vordr_create_container(handle, "", "alpine:3.19");
    try testing.expectEqual(RESULT_INVALID_PARAM, result);
}

test "create_container — untagged image rejected" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const result = vordr_create_container(handle, "my-app", "alpine");
    try testing.expectEqual(RESULT_INVALID_PARAM, result);
}

test "start and stop container" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    try testing.expectEqual(RESULT_OK, vordr_start_container(handle, "abc123"));
    try testing.expectEqual(RESULT_OK, vordr_stop_container(handle, "abc123"));
}

test "start container — null ID rejected" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    try testing.expectEqual(RESULT_NULL_POINTER, vordr_start_container(handle, null));
}

test "list_containers — returns JSON" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    var output: [*:0]u8 = undefined;
    const result = vordr_list_containers(handle, &output);
    try testing.expectEqual(RESULT_OK, result);

    const json = std.mem.span(@as([*:0]const u8, output));
    try testing.expect(json.len > 0);
    try testing.expect(std.mem.indexOf(u8, json, "containers") != null);
    try testing.expect(std.mem.indexOf(u8, json, "allowed_caps") != null);

    vordr_free_string(output);
}

// ===========================================================================
// Memory Safety Tests
// ===========================================================================

test "multiple handles are independent" {
    const h1 = vordr_init() orelse return error.InitFailed;
    defer vordr_free(h1);

    const h2 = vordr_init() orelse return error.InitFailed;
    defer vordr_free(h2);

    try testing.expect(h1 != h2);

    // Error on h1 should not affect h2
    _ = vordr_process(h1, CAP_KMOD); // denied, sets error on h1
    const str2 = vordr_get_string(h2) orelse return error.NoString;
    defer vordr_free_string(str2);
    try testing.expect(std.mem.span(str2).len > 0);
}

test "concurrent operations" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const ThreadContext = struct {
        h: *anyopaque,
        caps: u32,
    };

    const thread_fn = struct {
        fn run(ctx: ThreadContext) void {
            var i: u32 = 0;
            while (i < 100) : (i += 1) {
                _ = vordr_process(ctx.h, ctx.caps);
            }
        }
    }.run;

    var threads: [4]std.Thread = undefined;
    const caps = [_]u32{ CAP_NET, CAP_IPC, CAP_NET | CAP_IPC, 0 };

    for (&threads, 0..) |*thread, i| {
        thread.* = try std.Thread.spawn(.{}, thread_fn, .{
            ThreadContext{ .h = handle, .caps = caps[i] },
        });
    }

    for (threads) |thread| {
        thread.join();
    }
}
