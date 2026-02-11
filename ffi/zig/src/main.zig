// SPDX-License-Identifier: AGPL-3.0-or-later
// Stapeln Zig FFI implementation for the Idris2 ABI contract.

const std = @import("std");

pub const ResultCode = enum(c_int) {
    ok = 0,
    @"error" = 1,
    invalid_param = 2,
    not_found = 3,
    out_of_memory = 4,
};

const contract_version = "stapeln-abi-v1";

fn writeOwnedJson(json: []const u8, out_ptr: *?[*]u8, out_len: *usize) ResultCode {
    const allocator = std.heap.page_allocator;
    const buffer = allocator.alloc(u8, json.len) catch return .out_of_memory;
    for (json, 0..) |byte, idx| {
        buffer[idx] = byte;
    }
    out_ptr.* = buffer.ptr;
    out_len.* = buffer.len;
    return .ok;
}

export fn stapeln_contract_version() [*:0]const u8 {
    return contract_version.ptr;
}

export fn stapeln_create_stack_json(
    input_ptr: ?[*]const u8,
    input_len: usize,
    out_ptr: *?[*]u8,
    out_len: *usize,
) c_int {
    _ = input_ptr;
    _ = input_len;
    const response = "{\"mode\":\"zig\",\"op\":\"create_stack\"}";
    return @intFromEnum(writeOwnedJson(response, out_ptr, out_len));
}

export fn stapeln_get_stack_json(
    stack_id: u64,
    out_ptr: *?[*]u8,
    out_len: *usize,
) c_int {
    if (stack_id == 0) return @intFromEnum(ResultCode.invalid_param);
    const response = "{\"mode\":\"zig\",\"op\":\"get_stack\"}";
    return @intFromEnum(writeOwnedJson(response, out_ptr, out_len));
}

export fn stapeln_update_stack_json(
    stack_id: u64,
    input_ptr: ?[*]const u8,
    input_len: usize,
    out_ptr: *?[*]u8,
    out_len: *usize,
) c_int {
    if (stack_id == 0) return @intFromEnum(ResultCode.invalid_param);
    _ = input_ptr;
    _ = input_len;
    const response = "{\"mode\":\"zig\",\"op\":\"update_stack\"}";
    return @intFromEnum(writeOwnedJson(response, out_ptr, out_len));
}

export fn stapeln_validate_stack_json(
    stack_id: u64,
    out_ptr: *?[*]u8,
    out_len: *usize,
) c_int {
    if (stack_id == 0) return @intFromEnum(ResultCode.invalid_param);
    const response = "{\"mode\":\"zig\",\"op\":\"validate_stack\"}";
    return @intFromEnum(writeOwnedJson(response, out_ptr, out_len));
}

export fn stapeln_list_stacks_json(
    out_ptr: *?[*]u8,
    out_len: *usize,
) c_int {
    const response = "{\"mode\":\"zig\",\"op\":\"list_stacks\"}";
    return @intFromEnum(writeOwnedJson(response, out_ptr, out_len));
}

export fn stapeln_free_buffer(ptr: ?[*]u8, len: usize) void {
    const allocator = std.heap.page_allocator;
    const p = ptr orelse return;
    allocator.free(p[0..len]);
}

test "contract version is present" {
    const value = std.mem.span(stapeln_contract_version());
    try std.testing.expect(value.len > 0);
}
