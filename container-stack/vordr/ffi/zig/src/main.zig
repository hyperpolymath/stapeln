// SPDX-License-Identifier: PMPL-1.0-or-later
// Vordr Zig FFI Implementation
//
// Container policy guardian — validates container security policies,
// capability bitmasks, and policy documents. The Zig FFI implements the
// C-compatible ABI declared in src/abi/Foreign.idr.
//
// No C header files — pure Idris2 ABI -> Zig FFI.

const std = @import("std");
const builtin = @import("builtin");

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// Vordr library version
const VERSION = "0.1.0-alpha";

/// Magic bytes expected at the start of a policy document
const POLICY_MAGIC: [4]u8 = .{ 'V', 'P', 'O', 'L' };

/// Minimum valid policy document size: 4 (magic) + 2 (version) + 2 (flags)
const POLICY_MIN_SIZE: u32 = 8;

/// Supported policy document format versions
const POLICY_VERSION_MAJOR: u8 = 1;
const POLICY_VERSION_MINOR_MAX: u8 = 0;

/// Maximum number of error bytes stored per context
const ERROR_BUFFER_SIZE: usize = 512;

/// Sentinel value written into a freed context to detect use-after-free
const FREED_SENTINEL: u64 = 0xDEAD_BEEF_DEAD_BEEF;

// ---------------------------------------------------------------------------
// Policy capability flags (bitmask for vordr_process)
// ---------------------------------------------------------------------------
// These map to Linux container capabilities / seccomp policy bits.
// vordr_process validates a requested capability set against the policy.

/// Allow network access from within the container
const CAP_NET: u32 = 1 << 0;
/// Allow mounting filesystems
const CAP_MOUNT: u32 = 1 << 1;
/// Allow privilege escalation (setuid, setgid)
const CAP_PRIVILEGE: u32 = 1 << 2;
/// Allow IPC with host
const CAP_IPC: u32 = 1 << 3;
/// Allow PID namespace sharing
const CAP_PID_NS: u32 = 1 << 4;
/// Allow raw device access
const CAP_RAW_DEVICE: u32 = 1 << 5;
/// Allow kernel module loading
const CAP_KMOD: u32 = 1 << 6;
/// Allow ptrace
const CAP_PTRACE: u32 = 1 << 7;

/// Capabilities that are always denied — these are never safe in containers
const ALWAYS_DENIED: u32 = CAP_KMOD | CAP_RAW_DEVICE;

/// Default allowed capabilities for unprivileged containers
const DEFAULT_ALLOWED: u32 = CAP_NET | CAP_IPC;

/// Mask of all defined capability bits
const ALL_CAPS: u32 = CAP_NET | CAP_MOUNT | CAP_PRIVILEGE | CAP_IPC |
    CAP_PID_NS | CAP_RAW_DEVICE | CAP_KMOD | CAP_PTRACE;

// ---------------------------------------------------------------------------
// Result codes (matches Idris2 ABI Types.idr)
// ---------------------------------------------------------------------------

const Result = enum(u32) {
    ok = 0,
    err = 1,
    invalid_param = 2,
    out_of_memory = 3,
    null_pointer = 4,
};

// ---------------------------------------------------------------------------
// Context (the real state behind an opaque Handle)
// ---------------------------------------------------------------------------

/// VordrContext holds all mutable state for one library instance.
/// Allocated on the C heap so the pointer can cross the FFI boundary.
const VordrContext = struct {
    /// Sentinel to detect valid / freed contexts
    magic: u64 = 0x564F_5244_5200_0001, // "VORDR\x00\x01" ish

    /// Monotonic timestamp (nanoseconds) captured at init time
    init_timestamp_ns: i128 = 0,

    /// Per-context error message buffer (null-terminated)
    error_buf: [ERROR_BUFFER_SIZE]u8 = [_]u8{0} ** ERROR_BUFFER_SIZE,
    error_len: usize = 0,

    /// Registered event callback (nullable)
    callback: ?*const fn (u64, u32) callconv(.c) u32 = null,

    /// Policy: which capabilities are allowed in this context
    allowed_caps: u32 = DEFAULT_ALLOWED,

    /// Count of policy evaluations performed (for diagnostics)
    eval_count: u64 = 0,

    /// Whether the context has been properly initialised
    initialized: bool = false,

    /// Set a context-local error message.
    fn setError(self: *VordrContext, msg: []const u8) void {
        const len = @min(msg.len, self.error_buf.len - 1);
        @memcpy(self.error_buf[0..len], msg[0..len]);
        self.error_buf[len] = 0;
        self.error_len = len;
    }

    /// Clear any stored error.
    fn clearError(self: *VordrContext) void {
        self.error_buf[0] = 0;
        self.error_len = 0;
    }

    /// Return the error message as a sentinel-terminated slice, or null.
    fn getError(self: *const VordrContext) ?[*:0]const u8 {
        if (self.error_len == 0) return null;
        return @ptrCast(self.error_buf[0..self.error_len :0]);
    }

    /// Check whether this context looks valid (not freed, not corrupted).
    fn isValid(self: *const VordrContext) bool {
        return self.magic == 0x564F_5244_5200_0001 and self.initialized;
    }

    /// Notify the registered callback, if any.
    fn notifyCallback(self: *VordrContext, event_kind: u32) void {
        if (self.callback) |cb| {
            _ = cb(self.eval_count, event_kind);
        }
    }
};

// ---------------------------------------------------------------------------
// Thread-local fallback error (for operations before a context exists)
// ---------------------------------------------------------------------------

threadlocal var tls_error_buf: [ERROR_BUFFER_SIZE]u8 = [_]u8{0} ** ERROR_BUFFER_SIZE;
threadlocal var tls_error_len: usize = 0;

fn setTlsError(msg: []const u8) void {
    const len = @min(msg.len, tls_error_buf.len - 1);
    @memcpy(tls_error_buf[0..len], msg[0..len]);
    tls_error_buf[len] = 0;
    tls_error_len = len;
}

fn getTlsError() ?[*:0]const u8 {
    if (tls_error_len == 0) return null;
    return @ptrCast(tls_error_buf[0..tls_error_len :0]);
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Try to interpret a raw pointer-width integer as a *VordrContext.
/// Returns null when the pointer is null or the magic sentinel is wrong.
fn ptrToContext(raw: ?*anyopaque) ?*VordrContext {
    const ptr = raw orelse return null;
    const ctx: *VordrContext = @ptrCast(@alignCast(ptr));
    if (!ctx.isValid()) return null;
    return ctx;
}

// ===========================================================================
// Exported C ABI functions (match Foreign.idr declarations)
// ===========================================================================

// ---------------------------------------------------------------------------
// Library Lifecycle
// ---------------------------------------------------------------------------

/// Initialise the library and return a context handle.
/// Returns a pointer-width integer (0 on failure).
/// Foreign.idr: prim__init : PrimIO Bits64
export fn vordr_init() callconv(.c) ?*anyopaque {
    const allocator = std.heap.c_allocator;
    const ctx = allocator.create(VordrContext) catch {
        setTlsError("vordr_init: allocation failed");
        return null;
    };

    ctx.* = VordrContext{};
    ctx.init_timestamp_ns = std.time.nanoTimestamp();
    ctx.initialized = true;
    ctx.clearError();

    return @ptrCast(ctx);
}

/// Free a previously allocated context.
/// Safe to call with null or an already-freed handle.
/// Foreign.idr: prim__free : Bits64 -> PrimIO ()
export fn vordr_free(raw: ?*anyopaque) callconv(.c) void {
    const ptr = raw orelse return;
    const ctx: *VordrContext = @ptrCast(@alignCast(ptr));

    // Already freed — do nothing (double-free safety)
    if (ctx.magic == FREED_SENTINEL) return;

    // Invalidate the context before freeing so use-after-free is detectable
    ctx.magic = FREED_SENTINEL;
    ctx.initialized = false;
    ctx.callback = null;

    const allocator = std.heap.c_allocator;
    allocator.destroy(ctx);
}

// ---------------------------------------------------------------------------
// Core Operations
// ---------------------------------------------------------------------------

/// Evaluate a policy capability bitmask.
///
/// `flags` is a bitmask of CAP_* bits representing requested capabilities.
/// Returns a nonzero bitmask of the *granted* capabilities (a subset of the
/// request), or 0 if the request is entirely denied or the handle is invalid.
///
/// The Idris2 wrapper treats 0 as "Left Error" and nonzero as "Right n".
///
/// Foreign.idr: prim__process : Bits64 -> Bits32 -> PrimIO Bits32
export fn vordr_process(raw: ?*anyopaque, flags: u32) callconv(.c) u32 {
    const ctx = ptrToContext(raw) orelse {
        setTlsError("vordr_process: null or invalid handle");
        return 0;
    };
    ctx.clearError();

    // Reject requests that contain undefined bits
    if (flags & ~ALL_CAPS != 0) {
        ctx.setError("vordr_process: unknown capability bits in request");
        ctx.notifyCallback(1); // event 1 = policy violation
        return 0;
    }

    // Unconditionally deny always-denied caps
    if (flags & ALWAYS_DENIED != 0) {
        ctx.setError("vordr_process: request contains always-denied capabilities");
        ctx.notifyCallback(1);
        return 0;
    }

    // Grant = requested AND allowed
    const granted = flags & ctx.allowed_caps;

    ctx.eval_count += 1;
    ctx.notifyCallback(0); // event 0 = policy evaluation

    // If nothing was granted (but something was requested), that is a denial
    if (granted == 0 and flags != 0) {
        ctx.setError("vordr_process: no requested capabilities are allowed");
        return 0;
    }

    // Return granted caps (nonzero on success, or 0 when nothing was requested
    // — but the Idris2 side treats 0 as error, so return a sentinel 0xFFFF_FFFF
    // when flags==0 to mean "empty request is fine")
    if (flags == 0) {
        return 0xFFFF_FFFF; // sentinel: "no caps requested, nothing to deny"
    }

    return granted;
}

/// Validate a byte buffer as a Vordr policy document.
///
/// Expected binary layout:
///   [0..4)  magic:   "VPOL" (4 bytes)
///   [4]     version major
///   [5]     version minor
///   [6..8)  flags (u16 LE) — reserved, must be zero for v1.0
///   [8..)   payload (opaque, version-dependent)
///
/// Returns a Result enum value:
///   0 = ok, 1 = err, 2 = invalid_param, 3 = out_of_memory, 4 = null_pointer
///
/// Foreign.idr: prim__processArray : Bits64 -> Bits64 -> Bits32 -> PrimIO Bits32
export fn vordr_process_array(raw: ?*anyopaque, buffer: ?[*]const u8, len: u32) callconv(.c) u32 {
    const ctx = ptrToContext(raw) orelse {
        setTlsError("vordr_process_array: null or invalid handle");
        return @intFromEnum(Result.null_pointer);
    };
    ctx.clearError();

    const buf = buffer orelse {
        ctx.setError("vordr_process_array: null buffer pointer");
        return @intFromEnum(Result.null_pointer);
    };

    if (len < POLICY_MIN_SIZE) {
        ctx.setError("vordr_process_array: document too short (minimum 8 bytes)");
        return @intFromEnum(Result.invalid_param);
    }

    // Check magic bytes
    if (buf[0] != POLICY_MAGIC[0] or
        buf[1] != POLICY_MAGIC[1] or
        buf[2] != POLICY_MAGIC[2] or
        buf[3] != POLICY_MAGIC[3])
    {
        ctx.setError("vordr_process_array: invalid magic bytes (expected VPOL)");
        return @intFromEnum(Result.invalid_param);
    }

    // Check version
    const major = buf[4];
    const minor = buf[5];
    if (major != POLICY_VERSION_MAJOR or minor > POLICY_VERSION_MINOR_MAX) {
        ctx.setError("vordr_process_array: unsupported policy version");
        return @intFromEnum(Result.invalid_param);
    }

    // Check reserved flags (must be zero in v1.0)
    const flags_lo = buf[6];
    const flags_hi = buf[7];
    if (flags_lo != 0 or flags_hi != 0) {
        ctx.setError("vordr_process_array: reserved flags must be zero in v1.0");
        return @intFromEnum(Result.invalid_param);
    }

    ctx.eval_count += 1;
    ctx.notifyCallback(2); // event 2 = document validated

    return @intFromEnum(Result.ok);
}

// ---------------------------------------------------------------------------
// String Operations
// ---------------------------------------------------------------------------

/// Return a dynamically allocated C string describing the current context state.
/// Caller must free with vordr_free_string.
///
/// Foreign.idr: prim__getResult : Bits64 -> PrimIO Bits64
export fn vordr_get_string(raw: ?*anyopaque) callconv(.c) ?[*:0]const u8 {
    const ctx = ptrToContext(raw) orelse {
        setTlsError("vordr_get_string: null or invalid handle");
        return null;
    };
    ctx.clearError();

    const allocator = std.heap.c_allocator;

    // Build a status summary string
    const msg = std.fmt.allocPrintSentinel(
        allocator,
        "vordr context: initialized={}, evals={d}, allowed_caps=0x{x:0>8}, uptime_ns={d}",
        .{
            ctx.initialized,
            ctx.eval_count,
            ctx.allowed_caps,
            std.time.nanoTimestamp() - ctx.init_timestamp_ns,
        },
        0,
    ) catch {
        ctx.setError("vordr_get_string: allocation failed");
        return null;
    };

    return msg.ptr;
}

/// Free a C string previously returned by vordr_get_string.
///
/// Foreign.idr: prim__freeString : Bits64 -> PrimIO ()
export fn vordr_free_string(ptr: ?[*:0]const u8) callconv(.c) void {
    const raw = ptr orelse return;
    const allocator = std.heap.c_allocator;
    // Reconstruct the slice from the sentinel-terminated pointer
    const slice = std.mem.span(raw);
    // free needs a slice including the sentinel byte
    allocator.free(slice[0 .. slice.len + 1]);
}

// ---------------------------------------------------------------------------
// Error Handling
// ---------------------------------------------------------------------------

/// Return the last error message.
/// If the most recent operation used a valid context, returns that context's
/// error. Otherwise returns the thread-local error buffer.
///
/// The returned pointer is valid until the next FFI call on the same
/// context/thread. Caller must NOT free it.
///
/// Foreign.idr: prim__lastError : PrimIO Bits64
export fn vordr_last_error() callconv(.c) ?[*:0]const u8 {
    // We cannot know which context the caller means without an argument,
    // so we return the thread-local error (set by functions that fail
    // before or without a context).
    return getTlsError();
}

// ---------------------------------------------------------------------------
// Version Information
// ---------------------------------------------------------------------------

/// Return the library version string. The pointer is to static memory.
/// Caller must NOT free it.
///
/// Foreign.idr: prim__version : PrimIO Bits64
export fn vordr_version() callconv(.c) [*:0]const u8 {
    return VERSION;
}

/// Return build information. The pointer is to static memory.
/// Caller must NOT free it.
///
/// Foreign.idr: prim__buildInfo : PrimIO Bits64
export fn vordr_build_info() callconv(.c) [*:0]const u8 {
    return "Vordr " ++ VERSION ++ " (Zig FFI) — target " ++
        @tagName(builtin.cpu.arch) ++ "-" ++ @tagName(builtin.os.tag) ++
        " — Zig " ++ builtin.zig_version_string;
}

// ---------------------------------------------------------------------------
// Utility Functions
// ---------------------------------------------------------------------------

/// Check whether a handle refers to a valid, initialised context.
/// Returns 1 if valid, 0 otherwise.
///
/// Foreign.idr: prim__isInitialized : Bits64 -> PrimIO Bits32
export fn vordr_is_initialized(raw: ?*anyopaque) callconv(.c) u32 {
    const ctx = ptrToContext(raw) orelse return 0;
    return if (ctx.initialized) 1 else 0;
}

// ---------------------------------------------------------------------------
// Callback Support
// ---------------------------------------------------------------------------

/// Register a callback function for event notifications.
///
/// The callback signature is: fn(event_data: u64, event_kind: u32) -> u32
/// It is invoked synchronously during vordr_process / vordr_process_array.
///
/// Returns a Result enum value.
///
/// Foreign.idr: prim__registerCallback : Bits64 -> AnyPtr -> PrimIO Bits32
export fn vordr_register_callback(raw: ?*anyopaque, callback: ?*const fn (u64, u32) callconv(.c) u32) callconv(.c) u32 {
    const ctx = ptrToContext(raw) orelse {
        setTlsError("vordr_register_callback: null or invalid handle");
        return @intFromEnum(Result.null_pointer);
    };
    ctx.clearError();

    ctx.callback = callback;
    return @intFromEnum(Result.ok);
}

// ---------------------------------------------------------------------------
// Container-Specific Operations
// ---------------------------------------------------------------------------

/// Verify a container image name against policy rules.
/// Currently validates that the image name is non-empty and contains
/// a colon-separated tag (e.g. "alpine:3.19").
export fn vordr_verify_image(raw: ?*anyopaque, image_name: ?[*:0]const u8) callconv(.c) u32 {
    const ctx = ptrToContext(raw) orelse {
        setTlsError("vordr_verify_image: null or invalid handle");
        return @intFromEnum(Result.null_pointer);
    };
    ctx.clearError();

    const name = image_name orelse {
        ctx.setError("vordr_verify_image: null image name");
        return @intFromEnum(Result.null_pointer);
    };
    const name_slice = std.mem.span(name);

    if (name_slice.len == 0) {
        ctx.setError("vordr_verify_image: empty image name");
        return @intFromEnum(Result.invalid_param);
    }

    // Policy: image must have an explicit tag (no implicit :latest)
    if (std.mem.indexOfScalar(u8, name_slice, ':') == null) {
        ctx.setError("vordr_verify_image: image must have an explicit tag (no implicit :latest)");
        return @intFromEnum(Result.invalid_param);
    }

    ctx.notifyCallback(3); // event 3 = image verified
    return @intFromEnum(Result.ok);
}

/// Create a container record with a verified image.
export fn vordr_create_container(
    raw: ?*anyopaque,
    name: ?[*:0]const u8,
    image: ?[*:0]const u8,
) callconv(.c) u32 {
    const ctx = ptrToContext(raw) orelse {
        setTlsError("vordr_create_container: null or invalid handle");
        return @intFromEnum(Result.null_pointer);
    };
    ctx.clearError();

    const name_ptr = name orelse {
        ctx.setError("vordr_create_container: null container name");
        return @intFromEnum(Result.null_pointer);
    };
    const image_ptr = image orelse {
        ctx.setError("vordr_create_container: null image name");
        return @intFromEnum(Result.null_pointer);
    };

    const name_slice = std.mem.span(name_ptr);
    const image_slice = std.mem.span(image_ptr);

    if (name_slice.len == 0 or image_slice.len == 0) {
        ctx.setError("vordr_create_container: name and image must be non-empty");
        return @intFromEnum(Result.invalid_param);
    }

    // Validate image has explicit tag
    if (std.mem.indexOfScalar(u8, image_slice, ':') == null) {
        ctx.setError("vordr_create_container: image must have an explicit tag");
        return @intFromEnum(Result.invalid_param);
    }

    ctx.notifyCallback(4); // event 4 = container created
    return @intFromEnum(Result.ok);
}

/// Start container (stub — real implementation delegates to Elixir GenServer)
export fn vordr_start_container(raw: ?*anyopaque, container_id: ?[*:0]const u8) callconv(.c) u32 {
    const ctx = ptrToContext(raw) orelse {
        setTlsError("vordr_start_container: null or invalid handle");
        return @intFromEnum(Result.null_pointer);
    };
    ctx.clearError();

    const id = container_id orelse {
        ctx.setError("vordr_start_container: null container ID");
        return @intFromEnum(Result.null_pointer);
    };
    if (std.mem.span(id).len == 0) {
        ctx.setError("vordr_start_container: empty container ID");
        return @intFromEnum(Result.invalid_param);
    }

    ctx.notifyCallback(5); // event 5 = container started
    return @intFromEnum(Result.ok);
}

/// Stop container (stub — real implementation delegates to Elixir GenServer)
export fn vordr_stop_container(raw: ?*anyopaque, container_id: ?[*:0]const u8) callconv(.c) u32 {
    const ctx = ptrToContext(raw) orelse {
        setTlsError("vordr_stop_container: null or invalid handle");
        return @intFromEnum(Result.null_pointer);
    };
    ctx.clearError();

    const id = container_id orelse {
        ctx.setError("vordr_stop_container: null container ID");
        return @intFromEnum(Result.null_pointer);
    };
    if (std.mem.span(id).len == 0) {
        ctx.setError("vordr_stop_container: empty container ID");
        return @intFromEnum(Result.invalid_param);
    }

    ctx.notifyCallback(6); // event 6 = container stopped
    return @intFromEnum(Result.ok);
}

/// List containers — returns a heap-allocated JSON string via out-param.
/// Caller must free the string with vordr_free_string.
export fn vordr_list_containers(raw: ?*anyopaque, output: ?*[*:0]u8) callconv(.c) u32 {
    const ctx = ptrToContext(raw) orelse {
        setTlsError("vordr_list_containers: null or invalid handle");
        return @intFromEnum(Result.null_pointer);
    };
    ctx.clearError();

    const out = output orelse {
        ctx.setError("vordr_list_containers: null output pointer");
        return @intFromEnum(Result.null_pointer);
    };

    const allocator = std.heap.c_allocator;
    const json = std.fmt.allocPrintSentinel(
        allocator,
        "{{\"containers\":[],\"policy\":{{\"allowed_caps\":\"0x{x:0>8}\",\"evals\":{d}}}}}",
        .{ ctx.allowed_caps, ctx.eval_count },
        0,
    ) catch {
        ctx.setError("vordr_list_containers: allocation failed");
        return @intFromEnum(Result.out_of_memory);
    };

    out.* = json.ptr;
    return @intFromEnum(Result.ok);
}

// ===========================================================================
// Unit Tests
// ===========================================================================

test "init and free lifecycle" {
    const handle = vordr_init();
    try std.testing.expect(handle != null);
    if (handle) |h| {
        try std.testing.expectEqual(@as(u32, 1), vordr_is_initialized(h));
        vordr_free(h);
    }
}

test "double free is safe" {
    const handle = vordr_init();
    try std.testing.expect(handle != null);
    if (handle) |h| {
        vordr_free(h);
        vordr_free(h); // must not crash
    }
}

test "free null is safe" {
    vordr_free(null);
}

test "is_initialized rejects null" {
    try std.testing.expectEqual(@as(u32, 0), vordr_is_initialized(null));
}

test "version returns expected string" {
    const ver = std.mem.span(vordr_version());
    try std.testing.expectEqualStrings("0.1.0-alpha", ver);
}

test "build_info contains version" {
    const info = std.mem.span(vordr_build_info());
    try std.testing.expect(std.mem.indexOf(u8, info, "0.1.0-alpha") != null);
}

test "process — empty request returns sentinel" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const result = vordr_process(handle, 0);
    try std.testing.expectEqual(@as(u32, 0xFFFF_FFFF), result);
}

test "process — allowed caps are granted" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    // Default allows CAP_NET | CAP_IPC
    const result = vordr_process(handle, CAP_NET);
    try std.testing.expectEqual(CAP_NET, result);
}

test "process — denied caps return zero" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    // CAP_KMOD is always denied
    const result = vordr_process(handle, CAP_KMOD);
    try std.testing.expectEqual(@as(u32, 0), result);
}

test "process — unknown bits return zero" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const result = vordr_process(handle, 0x8000_0000);
    try std.testing.expectEqual(@as(u32, 0), result);
}

test "process — null handle returns zero" {
    const result = vordr_process(null, CAP_NET);
    try std.testing.expectEqual(@as(u32, 0), result);
}

test "process_array — valid policy document" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    // Valid: VPOL magic, version 1.0, flags 0x0000
    const doc = [_]u8{ 'V', 'P', 'O', 'L', 1, 0, 0, 0 };
    const result = vordr_process_array(handle, &doc, doc.len);
    try std.testing.expectEqual(@as(u32, @intFromEnum(Result.ok)), result);
}

test "process_array — bad magic" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const doc = [_]u8{ 'B', 'A', 'D', '!', 1, 0, 0, 0 };
    const result = vordr_process_array(handle, &doc, doc.len);
    try std.testing.expectEqual(@as(u32, @intFromEnum(Result.invalid_param)), result);
}

test "process_array — too short" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const doc = [_]u8{ 'V', 'P', 'O', 'L' };
    const result = vordr_process_array(handle, &doc, doc.len);
    try std.testing.expectEqual(@as(u32, @intFromEnum(Result.invalid_param)), result);
}

test "process_array — null buffer" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const result = vordr_process_array(handle, null, 8);
    try std.testing.expectEqual(@as(u32, @intFromEnum(Result.null_pointer)), result);
}

test "process_array — unsupported version" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const doc = [_]u8{ 'V', 'P', 'O', 'L', 2, 0, 0, 0 };
    const result = vordr_process_array(handle, &doc, doc.len);
    try std.testing.expectEqual(@as(u32, @intFromEnum(Result.invalid_param)), result);
}

test "get_string — returns context status" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const str = vordr_get_string(handle) orelse return error.NoString;
    defer vordr_free_string(str);

    const slice = std.mem.span(str);
    try std.testing.expect(slice.len > 0);
    try std.testing.expect(std.mem.indexOf(u8, slice, "vordr context") != null);
}

test "get_string — null handle returns null" {
    const str = vordr_get_string(null);
    try std.testing.expect(str == null);
}

test "register_callback — stores and invokes" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const State = struct {
        var called: bool = false;
        var last_kind: u32 = 999;
    };

    const cb = struct {
        fn callback(_: u64, kind: u32) callconv(.c) u32 {
            State.called = true;
            State.last_kind = kind;
            return 0;
        }
    }.callback;

    const reg_result = vordr_register_callback(handle, cb);
    try std.testing.expectEqual(@as(u32, @intFromEnum(Result.ok)), reg_result);

    // Trigger a policy evaluation which should invoke the callback
    _ = vordr_process(handle, CAP_NET);
    try std.testing.expect(State.called);
    try std.testing.expectEqual(@as(u32, 0), State.last_kind); // event 0 = evaluation
}

test "register_callback — null callback unregisters" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const result = vordr_register_callback(handle, null);
    try std.testing.expectEqual(@as(u32, @intFromEnum(Result.ok)), result);
}

test "verify_image — valid tagged image" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const result = vordr_verify_image(handle, "alpine:3.19");
    try std.testing.expectEqual(@as(u32, @intFromEnum(Result.ok)), result);
}

test "verify_image — rejects untagged image" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const result = vordr_verify_image(handle, "alpine");
    try std.testing.expectEqual(@as(u32, @intFromEnum(Result.invalid_param)), result);
}

test "create_container — valid name and image" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const result = vordr_create_container(handle, "myapp", "alpine:3.19");
    try std.testing.expectEqual(@as(u32, @intFromEnum(Result.ok)), result);
}

test "create_container — rejects empty name" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    const result = vordr_create_container(handle, "", "alpine:3.19");
    try std.testing.expectEqual(@as(u32, @intFromEnum(Result.invalid_param)), result);
}

test "last_error — captures null handle error" {
    _ = vordr_process(null, 0);
    const err = vordr_last_error();
    try std.testing.expect(err != null);
    if (err) |e| {
        const msg = std.mem.span(e);
        try std.testing.expect(msg.len > 0);
    }
}

test "multiple handles are independent" {
    const h1 = vordr_init() orelse return error.InitFailed;
    defer vordr_free(h1);
    const h2 = vordr_init() orelse return error.InitFailed;
    defer vordr_free(h2);

    try std.testing.expect(h1 != h2);

    // Trigger error on h1, h2 should be clean
    _ = vordr_process(h1, CAP_KMOD); // denied
    const str2 = vordr_get_string(h2) orelse return error.NoString;
    defer vordr_free_string(str2);
    // h2's get_string should succeed — contexts are independent
    try std.testing.expect(std.mem.span(str2).len > 0);
}

test "list_containers — returns JSON" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);

    var output: [*:0]u8 = undefined;
    const result = vordr_list_containers(handle, &output);
    try std.testing.expectEqual(@as(u32, @intFromEnum(Result.ok)), result);

    const json = std.mem.span(@as([*:0]const u8, output));
    try std.testing.expect(json.len > 0);
    try std.testing.expect(std.mem.indexOf(u8, json, "containers") != null);

    // Free the allocated string
    vordr_free_string(output);
}
