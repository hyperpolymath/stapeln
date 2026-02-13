// SPDX-License-Identifier: PMPL-1.0-or-later
// Native Zig bridge executable used by Stapeln.NativeBridge.

const std = @import("std");

const Service = struct {
    name: []const u8,
    kind: []const u8 = "unknown",
    port: ?i32 = null,
};

const Stack = struct {
    id: u64,
    name: []const u8,
    description: ?[]const u8 = null,
    services: []Service = &[_]Service{},
    created_at: []const u8,
    updated_at: []const u8,
};

const Store = struct {
    next_id: u64 = 1,
    stacks: []Stack = &[_]Stack{},
};

const ServiceInput = struct {
    name: ?[]const u8 = null,
    kind: ?[]const u8 = null,
    port: ?i32 = null,
};

const CreatePayload = struct {
    name: ?[]const u8 = null,
    description: ?[]const u8 = null,
    services: ?[]ServiceInput = null,
};

const UpdateAttrs = struct {
    name: ?[]const u8 = null,
    description: ?[]const u8 = null,
    services: ?[]ServiceInput = null,
};

const UpdatePayload = struct {
    id: u64 = 0,
    attrs: UpdateAttrs = .{},
};

const IdPayload = struct {
    id: u64 = 0,
};

const Finding = struct {
    id: []const u8,
    severity: []const u8,
    message: []const u8,
    hint: []const u8,
};

const ValidationResult = struct {
    score: i32,
    findings: []Finding,
    stack: Stack,
};

const EmptyPayload = struct {};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    if (args.len < 3) {
        try writeError("invalid_arguments");
        return;
    }

    const op = args[1];
    const payload_json = args[2];

    const store_path = std.process.getEnvVarOwned(allocator, "STAPELN_NATIVE_STORE") catch "/tmp/stapeln-native-store.json";
    var store = try readStore(allocator, store_path);

    if (std.mem.eql(u8, op, "list_stacks")) {
        _ = parsePayload(EmptyPayload, allocator, payload_json) catch {
            try writeError("invalid_payload");
            return;
        };

        try writeOk(.{ .ok = store.stacks });
        return;
    }

    if (std.mem.eql(u8, op, "create_stack")) {
        const payload = parsePayload(CreatePayload, allocator, payload_json) catch {
            try writeError("invalid_payload");
            return;
        };

        const now = try nowTimestamp(allocator);
        const new_id = if (store.next_id == 0) 1 else store.next_id;

        const stack = Stack{
            .id = new_id,
            .name = nonEmpty(payload.name) orelse try std.fmt.allocPrint(allocator, "stack-{d}", .{new_id}),
            .description = nonEmpty(payload.description),
            .services = try normalizeServices(allocator, payload.services),
            .created_at = now,
            .updated_at = now,
        };

        store.stacks = try appendStack(allocator, store.stacks, stack);
        store.next_id = new_id + 1;

        try writeStore(allocator, store_path, store);
        try writeOk(.{ .ok = stack });
        return;
    }

    if (std.mem.eql(u8, op, "get_stack")) {
        const payload = parsePayload(IdPayload, allocator, payload_json) catch {
            try writeError("invalid_payload");
            return;
        };

        const index = findStackIndex(store.stacks, payload.id) orelse {
            try writeError("not_found");
            return;
        };

        try writeOk(.{ .ok = store.stacks[index] });
        return;
    }

    if (std.mem.eql(u8, op, "update_stack")) {
        const payload = parsePayload(UpdatePayload, allocator, payload_json) catch {
            try writeError("invalid_payload");
            return;
        };

        const index = findStackIndex(store.stacks, payload.id) orelse {
            try writeError("not_found");
            return;
        };

        var current = store.stacks[index];

        if (nonEmpty(payload.attrs.name)) |name| {
            current.name = name;
        }

        if (nonEmpty(payload.attrs.description)) |description| {
            current.description = description;
        }

        if (payload.attrs.services) |services| {
            current.services = try normalizeServices(allocator, services);
        }

        current.updated_at = try nowTimestamp(allocator);
        store.stacks[index] = current;

        try writeStore(allocator, store_path, store);
        try writeOk(.{ .ok = current });
        return;
    }

    if (std.mem.eql(u8, op, "validate_stack")) {
        const payload = parsePayload(IdPayload, allocator, payload_json) catch {
            try writeError("invalid_payload");
            return;
        };

        const index = findStackIndex(store.stacks, payload.id) orelse {
            try writeError("not_found");
            return;
        };

        const report = try validateStack(allocator, store.stacks[index]);
        try writeOk(.{ .ok = report });
        return;
    }

    try writeError("unsupported_operation");
}

fn parsePayload(comptime T: type, allocator: std.mem.Allocator, json: []const u8) !T {
    const parsed = try std.json.parseFromSlice(T, allocator, json, .{ .ignore_unknown_fields = true });
    return parsed.value;
}

fn nonEmpty(value: ?[]const u8) ?[]const u8 {
    const input = value orelse return null;
    const trimmed = std.mem.trim(u8, input, " \n\r\t");
    if (trimmed.len == 0) return null;
    return trimmed;
}

fn nowTimestamp(allocator: std.mem.Allocator) ![]const u8 {
    return std.fmt.allocPrint(allocator, "{d}", .{std.time.timestamp()});
}

fn normalizeServices(allocator: std.mem.Allocator, services_opt: ?[]ServiceInput) ![]Service {
    const services = services_opt orelse return &[_]Service{};
    if (services.len == 0) return &[_]Service{};

    const normalized = try allocator.alloc(Service, services.len);
    for (services, 0..) |service, idx| {
        normalized[idx] = Service{
            .name = nonEmpty(service.name) orelse "unnamed-service",
            .kind = nonEmpty(service.kind) orelse "unknown",
            .port = if (service.port != null and service.port.? > 0) service.port.? else null,
        };
    }
    return normalized;
}

fn appendStack(allocator: std.mem.Allocator, stacks: []Stack, stack: Stack) ![]Stack {
    const out = try allocator.alloc(Stack, stacks.len + 1);
    std.mem.copyForwards(Stack, out[0..stacks.len], stacks);
    out[stacks.len] = stack;
    return out;
}

fn findStackIndex(stacks: []Stack, id: u64) ?usize {
    for (stacks, 0..) |stack, idx| {
        if (stack.id == id) {
            return idx;
        }
    }

    return null;
}

fn readStore(allocator: std.mem.Allocator, store_path: []const u8) !Store {
    const file = std.fs.cwd().openFile(store_path, .{}) catch |err| switch (err) {
        error.FileNotFound => return .{},
        else => return .{},
    };
    defer file.close();

    const data = try file.readToEndAlloc(allocator, 4 * 1024 * 1024);
    if (data.len == 0) {
        return .{};
    }

    const parsed = std.json.parseFromSlice(Store, allocator, data, .{ .ignore_unknown_fields = true }) catch {
        return .{};
    };

    return parsed.value;
}

fn writeStore(allocator: std.mem.Allocator, store_path: []const u8, store: Store) !void {
    const parent = std.fs.path.dirname(store_path);
    if (parent) |parent_dir| {
        std.fs.cwd().makePath(parent_dir) catch {};
    }

    const data = try std.json.stringifyAlloc(allocator, store, .{});
    const file = try std.fs.cwd().createFile(store_path, .{ .truncate = true });
    defer file.close();

    try file.writeAll(data);
}

fn writeOk(value: anytype) !void {
    const stdout = std.io.getStdOut().writer();
    try std.json.stringify(value, .{}, stdout);
}

fn writeError(reason: []const u8) !void {
    try writeOk(.{ .@"error" = reason });
}

fn validateStack(allocator: std.mem.Allocator, stack: Stack) !ValidationResult {
    var findings = std.ArrayList(Finding).init(allocator);
    var penalty: i32 = 0;

    if (stack.services.len == 0) {
        try findings.append(Finding{
            .id = "services.empty",
            .severity = "high",
            .message = "Stack has no services configured.",
            .hint = "Define at least one service before simulation or deployment.",
        });
        penalty += 20;
    }

    for (stack.services, 0..) |service, index| {
        if (std.mem.eql(u8, service.kind, "") or std.mem.eql(u8, service.kind, "unknown")) {
            try findings.append(Finding{
                .id = try std.fmt.allocPrint(allocator, "services.kind_missing.{d}", .{index + 1}),
                .severity = "medium",
                .message = try std.fmt.allocPrint(allocator, "Service #{d} has no kind.", .{index + 1}),
                .hint = "Set kind (for example web, worker, or db).",
            });
            penalty += 10;
        }

        if (service.port) |port| {
            if (port < 1 or port > 65535) {
                try findings.append(Finding{
                    .id = try std.fmt.allocPrint(allocator, "services.invalid_port.{d}", .{index + 1}),
                    .severity = "high",
                    .message = try std.fmt.allocPrint(allocator, "Service #{d} has invalid port {d}.", .{ index + 1, port }),
                    .hint = "Use a TCP/UDP port in range 1..65535.",
                });
                penalty += 20;
            }
        }
    }

    for (stack.services, 0..) |left, left_idx| {
        var right_idx: usize = left_idx + 1;
        while (right_idx < stack.services.len) : (right_idx += 1) {
            const right = stack.services[right_idx];

            if (std.mem.eql(u8, left.name, right.name)) {
                try findings.append(Finding{
                    .id = try std.fmt.allocPrint(allocator, "services.duplicate_name.{s}", .{left.name}),
                    .severity = "high",
                    .message = try std.fmt.allocPrint(allocator, "Service name `{s}` appears multiple times.", .{left.name}),
                    .hint = "Give each service a unique name.",
                });
                penalty += 20;
            }

            if (left.port != null and right.port != null and left.port.? == right.port.?) {
                try findings.append(Finding{
                    .id = try std.fmt.allocPrint(allocator, "services.duplicate_port.{d}", .{left.port.?}),
                    .severity = "medium",
                    .message = try std.fmt.allocPrint(allocator, "Multiple services use port {d}.", .{left.port.?}),
                    .hint = "Assign unique service ports where possible.",
                });
                penalty += 10;
            }
        }
    }

    var score: i32 = 100 - penalty;
    if (score < 0) score = 0;

    return ValidationResult{
        .score = score,
        .findings = try findings.toOwnedSlice(),
        .stack = stack,
    };
}
