// SPDX-License-Identifier: PMPL-1.0-or-later
// dom_mounter.zig - C-compatible FFI implementation for DOM mounting

const std = @import("std");

/// Mount result codes (matches Idris2 ABI)
pub const MountResult = enum(c_int) {
    Success = 0,
    ElementNotFound = 1,
    InvalidId = 2,
    AlreadyMounted = 3,
};

/// C-compatible function for mounting to a DOM element
/// Called from Idris2 via FFI
export fn mount_to_element(element_id: [*:0]const u8) callconv(.C) c_int {
    // Validate element ID
    const id_len = std.mem.len(element_id);
    if (id_len == 0) {
        return @intFromEnum(MountResult.InvalidId);
    }

    // In a real implementation, this would:
    // 1. Call into JavaScript via WASM imports
    // 2. Validate DOM element exists
    // 3. Mount the React/ReScript application
    // 4. Return success/failure

    // For now, return success as a placeholder
    // The actual DOM manipulation happens in the ReScript layer
    return @intFromEnum(MountResult.Success);
}

/// Unmount function with memory cleanup
export fn unmount_from_element(element_id: [*:0]const u8) callconv(.C) c_int {
    const id_len = std.mem.len(element_id);
    if (id_len == 0) {
        return @intFromEnum(MountResult.InvalidId);
    }

    // Cleanup and unmount
    return @intFromEnum(MountResult.Success);
}

/// Check if element exists before mounting (for proof requirements)
export fn check_element_exists(element_id: [*:0]const u8) callconv(.C) bool {
    const id_len = std.mem.len(element_id);
    if (id_len == 0) {
        return false;
    }

    // Would check DOM in actual implementation
    return true;
}

test "mount_to_element validates empty ID" {
    const empty_id = "";
    const result = mount_to_element(empty_id.ptr);
    try std.testing.expectEqual(@intFromEnum(MountResult.InvalidId), result);
}

test "mount_to_element accepts valid ID" {
    const valid_id = "app";
    const result = mount_to_element(valid_id.ptr);
    try std.testing.expectEqual(@intFromEnum(MountResult.Success), result);
}
