// SPDX-License-Identifier: PMPL-1.0-or-later
// Stapeln Zig FFI shared library for the Idris2 ABI contract.
// Implements CRUD operations and validation for container stacks.

const std = @import("std");

pub const ResultCode = enum(c_int) {
    ok = 0,
    @"error" = 1,
    invalid_param = 2,
    not_found = 3,
    out_of_memory = 4,
};

const contract_version: [:0]const u8 = "stapeln-abi-v1";
const page_alloc = std.heap.page_allocator;

// ---- Persistent store types ----

const PService = struct {
    name: []const u8,
    kind: []const u8,
    port: i32,
};

const PStack = struct {
    id: u64,
    name: []const u8,
    description: []const u8,
    services: []const PService,
};

const MAX_STACKS = 256;

var store: [MAX_STACKS]?PStack = [_]?PStack{null} ** MAX_STACKS;
var next_id: u64 = 1;

// ---- JSON input types ----

const SvcIn = struct {
    name: ?[]const u8 = null,
    kind: ?[]const u8 = null,
    port: ?i32 = null,
};

const CreateIn = struct {
    name: ?[]const u8 = null,
    description: ?[]const u8 = null,
    services: ?[]const SvcIn = null,
};

const UpdateIn = struct {
    name: ?[]const u8 = null,
    description: ?[]const u8 = null,
    services: ?[]const SvcIn = null,
};

// ---- Validation output types ----

const Finding = struct {
    id: []const u8,
    severity: []const u8,
    message: []const u8,
    hint: []const u8,
};

const Report = struct {
    score: i32,
    findings: []const Finding,
};

// ---- Helpers ----

fn dupe(s: ?[]const u8) []const u8 {
    const val = s orelse return "";
    if (val.len == 0) return "";
    return page_alloc.dupe(u8, val) catch "";
}

fn findSlot(id: u64) ?usize {
    for (&store, 0..) |slot, i| {
        if (slot.*) |st| {
            if (st.id == id) return i;
        }
    }
    return null;
}

fn freeSlot() ?usize {
    for (&store, 0..) |slot, i| {
        if (slot.* == null) return i;
    }
    return null;
}

fn convertServices(svcs: ?[]const SvcIn) ![]const PService {
    const input = svcs orelse return &[_]PService{};
    if (input.len == 0) return &[_]PService{};

    const result = try page_alloc.alloc(PService, input.len);
    for (input, 0..) |svc, i| {
        result[i] = .{
            .name = dupe(svc.name),
            .kind = dupe(svc.kind),
            .port = svc.port orelse 0,
        };
    }
    return result;
}

fn inputSlice(ptr: ?[*]const u8, len: usize) ?[]const u8 {
    const p = ptr orelse return null;
    if (len == 0) return null;
    return p[0..len];
}

fn writeResponse(value: anytype, out_ptr: *?[*]u8, out_len: *usize) ResultCode {
    // Serialize to temporary arena
    var arena = std.heap.ArenaAllocator.init(page_alloc);
    defer arena.deinit();
    const tmp = arena.allocator();

    const json = std.json.stringifyAlloc(tmp, value, .{}) catch return .out_of_memory;

    // Copy to persistent output buffer
    const buffer = page_alloc.alloc(u8, json.len) catch return .out_of_memory;
    @memcpy(buffer, json);
    out_ptr.* = buffer.ptr;
    out_len.* = buffer.len;
    return .ok;
}

// ---- Exported functions ----

export fn stapeln_contract_version() [*:0]const u8 {
    return contract_version.ptr;
}

export fn stapeln_create_stack_json(
    input_ptr: ?[*]const u8,
    input_len: usize,
    out_ptr: *?[*]u8,
    out_len: *usize,
) c_int {
    const input = inputSlice(input_ptr, input_len) orelse
        return @intFromEnum(ResultCode.invalid_param);

    var arena = std.heap.ArenaAllocator.init(page_alloc);
    defer arena.deinit();
    const tmp = arena.allocator();

    const parsed = std.json.parseFromSlice(CreateIn, tmp, input, .{
        .ignore_unknown_fields = true,
    }) catch return @intFromEnum(ResultCode.invalid_param);
    const payload = parsed.value;

    const slot = freeSlot() orelse return @intFromEnum(ResultCode.out_of_memory);
    const id = next_id;
    next_id += 1;

    const name_val = dupe(payload.name);
    const stack = PStack{
        .id = id,
        .name = if (name_val.len > 0) name_val else dupe("unnamed-stack"),
        .description = dupe(payload.description),
        .services = convertServices(payload.services) catch
            return @intFromEnum(ResultCode.out_of_memory),
    };

    store[slot] = stack;
    return @intFromEnum(writeResponse(stack, out_ptr, out_len));
}

export fn stapeln_get_stack_json(
    stack_id: u64,
    out_ptr: *?[*]u8,
    out_len: *usize,
) c_int {
    if (stack_id == 0) return @intFromEnum(ResultCode.invalid_param);
    const idx = findSlot(stack_id) orelse return @intFromEnum(ResultCode.not_found);
    const stack = store[idx].?;
    return @intFromEnum(writeResponse(stack, out_ptr, out_len));
}

export fn stapeln_update_stack_json(
    stack_id: u64,
    input_ptr: ?[*]const u8,
    input_len: usize,
    out_ptr: *?[*]u8,
    out_len: *usize,
) c_int {
    if (stack_id == 0) return @intFromEnum(ResultCode.invalid_param);
    const idx = findSlot(stack_id) orelse return @intFromEnum(ResultCode.not_found);

    const input = inputSlice(input_ptr, input_len) orelse
        return @intFromEnum(ResultCode.invalid_param);

    var arena = std.heap.ArenaAllocator.init(page_alloc);
    defer arena.deinit();
    const tmp = arena.allocator();

    const parsed = std.json.parseFromSlice(UpdateIn, tmp, input, .{
        .ignore_unknown_fields = true,
    }) catch return @intFromEnum(ResultCode.invalid_param);
    const payload = parsed.value;

    var current = store[idx].?;

    if (payload.name) |name| {
        if (name.len > 0) current.name = dupe(name);
    }
    if (payload.description) |desc| {
        current.description = dupe(desc);
    }
    if (payload.services != null) {
        current.services = convertServices(payload.services) catch
            return @intFromEnum(ResultCode.out_of_memory);
    }

    store[idx] = current;
    return @intFromEnum(writeResponse(current, out_ptr, out_len));
}

export fn stapeln_validate_stack_json(
    stack_id: u64,
    out_ptr: *?[*]u8,
    out_len: *usize,
) c_int {
    if (stack_id == 0) return @intFromEnum(ResultCode.invalid_param);
    const idx = findSlot(stack_id) orelse return @intFromEnum(ResultCode.not_found);
    const stack = store[idx].?;

    const report = validate(stack) catch return @intFromEnum(ResultCode.out_of_memory);
    return @intFromEnum(writeResponse(report, out_ptr, out_len));
}

export fn stapeln_list_stacks_json(
    out_ptr: *?[*]u8,
    out_len: *usize,
) c_int {
    var count: usize = 0;
    for (store) |slot| {
        if (slot != null) count += 1;
    }

    const list = page_alloc.alloc(PStack, count) catch return @intFromEnum(ResultCode.out_of_memory);
    var i: usize = 0;
    for (store) |slot| {
        if (slot) |st| {
            list[i] = st;
            i += 1;
        }
    }

    return @intFromEnum(writeResponse(list, out_ptr, out_len));
}

export fn stapeln_free_buffer(ptr: ?[*]u8, len: usize) void {
    const p = ptr orelse return;
    page_alloc.free(p[0..len]);
}

// ---- Validation logic ----

fn validate(stack: PStack) !Report {
    var arena = std.heap.ArenaAllocator.init(page_alloc);
    const tmp = arena.allocator();

    var findings = std.ArrayList(Finding).init(tmp);
    var penalty: i32 = 0;

    // Check: empty services
    if (stack.services.len == 0) {
        try findings.append(.{
            .id = "services.empty",
            .severity = "high",
            .message = "Stack has no services configured.",
            .hint = "Define at least one service.",
        });
        penalty += 20;
    }

    // Check each service
    for (stack.services, 0..) |svc, si| {
        // Missing kind
        if (svc.kind.len == 0 or std.mem.eql(u8, svc.kind, "unknown")) {
            try findings.append(.{
                .id = try std.fmt.allocPrint(tmp, "services.kind_missing.{d}", .{si + 1}),
                .severity = "medium",
                .message = try std.fmt.allocPrint(tmp, "Service #{d} has no kind.", .{si + 1}),
                .hint = "Set kind (e.g. web, worker, db).",
            });
            penalty += 10;
        }

        // Invalid port
        if (svc.port != 0 and (svc.port < 1 or svc.port > 65535)) {
            try findings.append(.{
                .id = try std.fmt.allocPrint(tmp, "services.invalid_port.{d}", .{si + 1}),
                .severity = "high",
                .message = try std.fmt.allocPrint(tmp, "Service #{d} has invalid port {d}.", .{ si + 1, svc.port }),
                .hint = "Use a TCP/UDP port in range 1..65535.",
            });
            penalty += 20;
        }
    }

    // Check duplicate names and ports
    for (stack.services, 0..) |left, li| {
        var ri: usize = li + 1;
        while (ri < stack.services.len) : (ri += 1) {
            const right = stack.services[ri];

            if (left.name.len > 0 and std.mem.eql(u8, left.name, right.name)) {
                try findings.append(.{
                    .id = try std.fmt.allocPrint(tmp, "services.duplicate_name.{s}", .{left.name}),
                    .severity = "high",
                    .message = try std.fmt.allocPrint(tmp, "Service name '{s}' appears multiple times.", .{left.name}),
                    .hint = "Give each service a unique name.",
                });
                penalty += 20;
            }

            if (left.port > 0 and left.port == right.port) {
                try findings.append(.{
                    .id = try std.fmt.allocPrint(tmp, "services.duplicate_port.{d}", .{left.port}),
                    .severity = "medium",
                    .message = try std.fmt.allocPrint(tmp, "Multiple services use port {d}.", .{left.port}),
                    .hint = "Assign unique service ports.",
                });
                penalty += 10;
            }
        }
    }

    var score: i32 = 100 - penalty;
    if (score < 0) score = 0;

    // Copy findings to persistent memory for serialization
    const owned_findings = try page_alloc.alloc(Finding, findings.items.len);
    @memcpy(owned_findings, findings.items);

    return .{
        .score = score,
        .findings = owned_findings,
    };
}

// ---- Tests ----

test "contract version is present" {
    const value = std.mem.span(stapeln_contract_version());
    try std.testing.expect(value.len > 0);
    try std.testing.expectEqualStrings("stapeln-abi-v1", value);
}

test "create and list stacks" {
    const input = "{\"name\":\"test-stack\",\"services\":[{\"name\":\"web\",\"kind\":\"web\",\"port\":8080}]}";
    var out_ptr: ?[*]u8 = null;
    var out_len: usize = 0;

    const rc = stapeln_create_stack_json(input.ptr, input.len, &out_ptr, &out_len);
    try std.testing.expectEqual(@as(c_int, 0), rc);
    try std.testing.expect(out_ptr != null);
    try std.testing.expect(out_len > 0);

    if (out_ptr) |p| {
        defer stapeln_free_buffer(p, out_len);
        const json = p[0..out_len];
        try std.testing.expect(std.mem.indexOf(u8, json, "test-stack") != null);
    }
}

test "list returns json array" {
    var out_ptr: ?[*]u8 = null;
    var out_len: usize = 0;

    const rc = stapeln_list_stacks_json(&out_ptr, &out_len);
    try std.testing.expectEqual(@as(c_int, 0), rc);
    try std.testing.expect(out_ptr != null);
    try std.testing.expect(out_len > 0);

    if (out_ptr) |p| {
        defer stapeln_free_buffer(p, out_len);
        const json = p[0..out_len];
        try std.testing.expect(json[0] == '[');
    }
}
