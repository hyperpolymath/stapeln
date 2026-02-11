// SPDX-License-Identifier: AGPL-3.0-or-later
// Integration checks for Stapeln FFI ABI exports.

const std = @import("std");
const testing = std.testing;

extern fn stapeln_contract_version() [*:0]const u8;
extern fn stapeln_list_stacks_json(*?[*]u8, *usize) c_int;
extern fn stapeln_free_buffer(?[*]u8, usize) void;

test "version export is valid" {
    const value = std.mem.span(stapeln_contract_version());
    try testing.expect(value.len > 0);
}

test "list_stacks returns json payload" {
    var out_ptr: ?[*]u8 = null;
    var out_len: usize = 0;

    const rc = stapeln_list_stacks_json(&out_ptr, &out_len);
    try testing.expectEqual(@as(c_int, 0), rc);
    try testing.expect(out_ptr != null);
    try testing.expect(out_len > 0);

    const buffer = out_ptr.?;
    defer stapeln_free_buffer(buffer, out_len);

    const json = buffer[0..out_len];
    try testing.expect(std.mem.indexOf(u8, json, "\"list_stacks\"") != null);
}

