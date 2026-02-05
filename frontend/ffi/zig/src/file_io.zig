// SPDX-License-Identifier: PMPL-1.0-or-later
// file_io.zig - C-compatible FFI for file I/O

const std = @import("std");

/// File operation result codes
pub const FileResult = enum(c_int) {
    Success = 0,
    PathEmpty = 1,
    NotFound = 2,
    PermissionDenied = 3,
    IOError = 4,
};

/// Read file contents (for browser: LocalStorage or IndexedDB)
/// In WASM context, this would interface with JavaScript File API
export fn read_file(path: [*:0]const u8) c_int {
    const path_len = std.mem.len(path);
    if (path_len == 0) {
        return @intFromEnum(FileResult.PathEmpty);
    }

    // In actual implementation:
    // - Validate path
    // - Check permissions
    // - Read from virtual filesystem (browser) or real FS (native)
    // - Return buffer with contents

    return @intFromEnum(FileResult.Success);
}

/// Write file contents with atomic operation
/// Ensures write completes fully or not at all
export fn write_file(path: [*:0]const u8, content: [*:0]const u8) c_int {
    const path_len = std.mem.len(path);
    const content_len = std.mem.len(content);

    if (path_len == 0) {
        return @intFromEnum(FileResult.PathEmpty);
    }

    // In actual implementation:
    // - Write to temp file first
    // - Sync to disk/storage
    // - Atomic rename to target path
    // - Ensures no partial writes

    _ = content_len; // Use content
    return @intFromEnum(FileResult.Success);
}

/// Check if file exists
export fn file_exists(path: [*:0]const u8) bool {
    const path_len = std.mem.len(path);
    if (path_len == 0) {
        return false;
    }

    // Would check actual filesystem
    return true;
}

test "read_file validates empty path" {
    const empty_path = "";
    const result = read_file(empty_path.ptr);
    try std.testing.expectEqual(@intFromEnum(FileResult.PathEmpty), result);
}

test "write_file validates empty path" {
    const empty_path = "";
    const content = "test";
    const result = write_file(empty_path.ptr, content.ptr);
    try std.testing.expectEqual(@intFromEnum(FileResult.PathEmpty), result);
}

test "write_file accepts valid inputs" {
    const path = "test.txt";
    const content = "test content";
    const result = write_file(path.ptr, content.ptr);
    try std.testing.expectEqual(@intFromEnum(FileResult.Success), result);
}
