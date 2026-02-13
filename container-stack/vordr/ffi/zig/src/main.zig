// SPDX-License-Identifier: PMPL-1.0-or-later
// Vörðr Zig FFI Implementation
// No C header files - pure Idris2 ABI → Zig FFI

const std = @import("std");

// Opaque handle type (matches Idris2 ABI)
const Handle = opaque {};

// Result codes (matches Idris2 ABI Types.idr)
const Result = enum(u32) {
    ok = 0,
    err = 1,
    invalid_param = 2,
    out_of_memory = 3,
    null_pointer = 4,
};

//------------------------------------------------------------------------------
// Library Lifecycle
//------------------------------------------------------------------------------

/// Initialize the library
/// Returns a handle to the library instance, or null on failure
export fn vordr_init() ?*Handle {
    const allocator = std.heap.c_allocator;
    const handle = allocator.create(Handle) catch return null;
    return handle;
}

/// Clean up library resources
export fn vordr_free(handle: *Handle) void {
    const allocator = std.heap.c_allocator;
    allocator.destroy(handle);
}

//------------------------------------------------------------------------------
// Core Operations
//------------------------------------------------------------------------------

/// Example operation: process data
export fn vordr_process(handle: *Handle, input: u32) u32 {
    _ = handle;
    // Placeholder implementation
    return input * 2;
}

/// Process array data
export fn vordr_process_array(handle: *Handle, buffer: [*]const u8, len: u32) Result {
    _ = handle;
    if (buffer == null) return .null_pointer;
    if (len == 0) return .invalid_param;
    
    // Placeholder: validate array
    for (0..len) |i| {
        _ = buffer[i];
    }
    
    return .ok;
}

//------------------------------------------------------------------------------
// String Operations
//------------------------------------------------------------------------------

/// Get string result from library
export fn vordr_get_string(handle: *Handle) ?[*:0]const u8 {
    _ = handle;
    // Placeholder: return static string
    return "Vörðr FFI v0.1.0";
}

/// Free C string
export fn vordr_free_string(ptr: [*:0]const u8) void {
    const allocator = std.heap.c_allocator;
    const slice = std.mem.span(ptr);
    allocator.free(slice);
}

//------------------------------------------------------------------------------
// Error Handling
//------------------------------------------------------------------------------

var last_error_buffer: [256]u8 = undefined;
var last_error_msg: ?[:0]const u8 = null;

/// Set last error message (internal)
fn setLastError(msg: []const u8) void {
    const len = @min(msg.len, last_error_buffer.len - 1);
    @memcpy(last_error_buffer[0..len], msg[0..len]);
    last_error_buffer[len] = 0;
    last_error_msg = last_error_buffer[0..len :0];
}

/// Get last error message
export fn vordr_last_error() ?[*:0]const u8 {
    return if (last_error_msg) |msg| msg.ptr else null;
}

//------------------------------------------------------------------------------
// Version Information
//------------------------------------------------------------------------------

export fn vordr_version() [*:0]const u8 {
    return "0.1.0";
}

export fn vordr_build_info() [*:0]const u8 {
    return "Vörðr Zig FFI - Built with Zig " ++ @import("builtin").zig_version_string;
}

//------------------------------------------------------------------------------
// Utility Functions
//------------------------------------------------------------------------------

/// Check if library is initialized
export fn vordr_is_initialized(handle: *Handle) u32 {
    return if (handle != null) 1 else 0;
}

//------------------------------------------------------------------------------
// Callback Support
//------------------------------------------------------------------------------

const Callback = *const fn (u64, u32) callconv(.C) u32;

var registered_callback: ?Callback = null;

/// Register a callback
export fn vordr_register_callback(handle: *Handle, callback: Callback) Result {
    _ = handle;
    registered_callback = callback;
    return .ok;
}

//------------------------------------------------------------------------------
// Container-Specific Operations (Vörðr)
//------------------------------------------------------------------------------

/// Verify container image signature
export fn vordr_verify_image(handle: *Handle, image_name: [*:0]const u8) Result {
    _ = handle;
    _ = image_name;
    // Placeholder: call Elixir GenServer for actual verification
    return .ok;
}

/// Create container with verified image
export fn vordr_create_container(
    handle: *Handle,
    name: [*:0]const u8,
    image: [*:0]const u8,
) Result {
    _ = handle;
    const name_slice = std.mem.span(name);
    const image_slice = std.mem.span(image);
    
    if (name_slice.len == 0 or image_slice.len == 0) {
        setLastError("Container name and image cannot be empty");
        return .invalid_param;
    }
    
    // Placeholder: call Elixir GenServer for container creation
    return .ok;
}

/// Start container
export fn vordr_start_container(handle: *Handle, container_id: [*:0]const u8) Result {
    _ = handle;
    _ = container_id;
    // Placeholder
    return .ok;
}

/// Stop container
export fn vordr_stop_container(handle: *Handle, container_id: [*:0]const u8) Result {
    _ = handle;
    _ = container_id;
    // Placeholder
    return .ok;
}

/// List containers
export fn vordr_list_containers(handle: *Handle, output: *[*:0]u8) Result {
    _ = handle;
    // Placeholder: return JSON array
    const json = "[{\"id\":\"abc123\",\"name\":\"test\",\"image\":\"alpine:latest\",\"status\":\"running\"}]";
    const allocator = std.heap.c_allocator;
    const allocated = allocator.dupeZ(u8, json) catch return .out_of_memory;
    output.* = allocated.ptr;
    return .ok;
}

//------------------------------------------------------------------------------
// Tests
//------------------------------------------------------------------------------

test "basic initialization" {
    const handle = vordr_init();
    try std.testing.expect(handle != null);
    if (handle) |h| vordr_free(h);
}

test "version info" {
    const version = std.mem.span(vordr_version());
    try std.testing.expect(version.len > 0);
}

test "process data" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);
    
    const result = vordr_process(handle, 42);
    try std.testing.expectEqual(@as(u32, 84), result);
}

test "string operations" {
    const handle = vordr_init() orelse return error.InitFailed;
    defer vordr_free(handle);
    
    const str = vordr_get_string(handle) orelse return error.NoString;
    const slice = std.mem.span(str);
    try std.testing.expect(slice.len > 0);
}
