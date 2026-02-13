// SPDX-License-Identifier: PMPL-1.0-or-later
// Integration checks for Stapeln FFI ABI exports.

const std = @import("std");
const testing = std.testing;

extern fn stapeln_contract_version() [*:0]const u8;
extern fn stapeln_list_stacks_json(*?[*]u8, *usize) c_int;
extern fn stapeln_create_stack_json(?[*]const u8, usize, *?[*]u8, *usize) c_int;
extern fn stapeln_validate_stack_json(u64, *?[*]u8, *usize) c_int;
extern fn stapeln_free_buffer(?[*]u8, usize) void;

test "version export is valid" {
    const value = std.mem.span(stapeln_contract_version());
    try testing.expect(value.len > 0);
    try testing.expectEqualStrings("stapeln-abi-v1", value);
}

test "list_stacks returns json array" {
    var out_ptr: ?[*]u8 = null;
    var out_len: usize = 0;

    const rc = stapeln_list_stacks_json(&out_ptr, &out_len);
    try testing.expectEqual(@as(c_int, 0), rc);
    try testing.expect(out_ptr != null);
    try testing.expect(out_len > 0);

    const buffer = out_ptr.?;
    defer stapeln_free_buffer(buffer, out_len);

    const json = buffer[0..out_len];
    // Response is a JSON array
    try testing.expect(json[0] == '[');
}

test "create_stack returns stack with id" {
    const input = "{\"name\":\"integration-test\",\"services\":[{\"name\":\"api\",\"kind\":\"web\",\"port\":3000}]}";
    var out_ptr: ?[*]u8 = null;
    var out_len: usize = 0;

    const rc = stapeln_create_stack_json(input.ptr, input.len, &out_ptr, &out_len);
    try testing.expectEqual(@as(c_int, 0), rc);
    try testing.expect(out_ptr != null);
    try testing.expect(out_len > 0);

    const buffer = out_ptr.?;
    defer stapeln_free_buffer(buffer, out_len);

    const json = buffer[0..out_len];
    try testing.expect(std.mem.indexOf(u8, json, "integration-test") != null);
    try testing.expect(std.mem.indexOf(u8, json, "\"id\"") != null);
}
