// SPDX-License-Identifier: PMPL-1.0-or-later
// Stapeln Zig FFI shared library for the Idris2 ABI contract.
// Implements CRUD operations, validation, security scanning, gap analysis,
// and dispatch for container stacks.

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
    image: []const u8,
    cpu: f64,
    memory: i32,
    network: []const u8,
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
    image: ?[]const u8 = null,
    cpu: ?f64 = null,
    memory: ?i32 = null,
    network: ?[]const u8 = null,
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

// ---- Validation / analysis output types ----

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

const Vulnerability = struct {
    id: []const u8,
    title: []const u8,
    severity: []const u8,
    description: []const u8,
    affected_component: []const u8,
    fix_available: bool,
};

const SecurityReport = struct {
    grade: []const u8,
    score: i32,
    vulnerabilities: []const Vulnerability,
    check_count: i32,
    pass_count: i32,
};

const Gap = struct {
    id: []const u8,
    title: []const u8,
    category: []const u8,
    severity: []const u8,
    description: []const u8,
    fix_available: bool,
    estimated_effort: []const u8,
};

const GapReport = struct {
    total_gaps: i32,
    critical_count: i32,
    high_count: i32,
    gaps: []const Gap,
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
            .image = dupe(svc.image),
            .cpu = svc.cpu orelse 0.0,
            .memory = svc.memory orelse 0,
            .network = dupe(svc.network),
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
    var arena = std.heap.ArenaAllocator.init(page_alloc);
    defer arena.deinit();
    const tmp = arena.allocator();

    const json = std.json.stringifyAlloc(tmp, value, .{}) catch return .out_of_memory;

    const buffer = page_alloc.alloc(u8, json.len) catch return .out_of_memory;
    @memcpy(buffer, json);
    out_ptr.* = buffer.ptr;
    out_len.* = buffer.len;
    return .ok;
}

// ---- Exported CRUD functions ----

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

// ---- Validation ----

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

// ---- Security scan ----

export fn stapeln_security_scan_json(
    stack_id: u64,
    out_ptr: *?[*]u8,
    out_len: *usize,
) c_int {
    if (stack_id == 0) return @intFromEnum(ResultCode.invalid_param);
    const idx = findSlot(stack_id) orelse return @intFromEnum(ResultCode.not_found);
    const stack = store[idx].?;

    const report = securityScan(stack) catch return @intFromEnum(ResultCode.out_of_memory);
    return @intFromEnum(writeResponse(report, out_ptr, out_len));
}

// ---- Gap analysis ----

export fn stapeln_gap_analysis_json(
    stack_id: u64,
    out_ptr: *?[*]u8,
    out_len: *usize,
) c_int {
    if (stack_id == 0) return @intFromEnum(ResultCode.invalid_param);
    const idx = findSlot(stack_id) orelse return @intFromEnum(ResultCode.not_found);
    const stack = store[idx].?;

    const report = gapAnalysis(stack) catch return @intFromEnum(ResultCode.out_of_memory);
    return @intFromEnum(writeResponse(report, out_ptr, out_len));
}

// ---- Dispatch (generic command router) ----

export fn stapeln_dispatch_json(
    op_ptr: ?[*]const u8,
    op_len: usize,
    input_ptr: ?[*]const u8,
    input_len: usize,
    out_ptr: *?[*]u8,
    out_len: *usize,
) c_int {
    const op = inputSlice(op_ptr, op_len) orelse
        return @intFromEnum(ResultCode.invalid_param);

    if (std.mem.eql(u8, op, "list_stacks")) {
        return stapeln_list_stacks_json(out_ptr, out_len);
    }

    if (std.mem.eql(u8, op, "create_stack")) {
        return stapeln_create_stack_json(input_ptr, input_len, out_ptr, out_len);
    }

    // Operations that need a stack_id from JSON input
    const input = inputSlice(input_ptr, input_len) orelse
        return @intFromEnum(ResultCode.invalid_param);

    var arena = std.heap.ArenaAllocator.init(page_alloc);
    defer arena.deinit();
    const tmp = arena.allocator();

    const IdIn = struct { id: ?u64 = null };
    const id_parsed = std.json.parseFromSlice(IdIn, tmp, input, .{
        .ignore_unknown_fields = true,
    }) catch return @intFromEnum(ResultCode.invalid_param);
    const stack_id = id_parsed.value.id orelse return @intFromEnum(ResultCode.invalid_param);

    if (std.mem.eql(u8, op, "get_stack")) {
        return stapeln_get_stack_json(stack_id, out_ptr, out_len);
    }
    if (std.mem.eql(u8, op, "update_stack")) {
        return stapeln_update_stack_json(stack_id, input_ptr, input_len, out_ptr, out_len);
    }
    if (std.mem.eql(u8, op, "validate_stack") or std.mem.eql(u8, op, "validate")) {
        return stapeln_validate_stack_json(stack_id, out_ptr, out_len);
    }
    if (std.mem.eql(u8, op, "security_scan")) {
        return stapeln_security_scan_json(stack_id, out_ptr, out_len);
    }
    if (std.mem.eql(u8, op, "gap_analysis")) {
        return stapeln_gap_analysis_json(stack_id, out_ptr, out_len);
    }

    return @intFromEnum(ResultCode.invalid_param);
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

        // Privileged port warning
        if (svc.port > 0 and svc.port < 1024) {
            try findings.append(.{
                .id = try std.fmt.allocPrint(tmp, "services.privileged_port.{d}", .{si + 1}),
                .severity = "low",
                .message = try std.fmt.allocPrint(tmp, "Service #{d} uses privileged port {d}.", .{ si + 1, svc.port }),
                .hint = "Ports below 1024 require elevated privileges.",
            });
            penalty += 5;
        }

        // Image tag :latest warning
        if (svc.image.len > 0 and std.mem.endsWith(u8, svc.image, ":latest")) {
            try findings.append(.{
                .id = try std.fmt.allocPrint(tmp, "services.latest_tag.{d}", .{si + 1}),
                .severity = "medium",
                .message = try std.fmt.allocPrint(tmp, "Service #{d} uses :latest tag.", .{si + 1}),
                .hint = "Pin to a specific image version for reproducibility.",
            });
            penalty += 10;
        }

        // No resource limits
        if (svc.cpu == 0.0 and svc.memory == 0) {
            try findings.append(.{
                .id = try std.fmt.allocPrint(tmp, "services.no_resource_limits.{d}", .{si + 1}),
                .severity = "medium",
                .message = try std.fmt.allocPrint(tmp, "Service #{d} has no resource limits.", .{si + 1}),
                .hint = "Set CPU and memory limits to prevent resource starvation.",
            });
            penalty += 10;
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
                    .id = try std.fmt.allocPrint(tmp, "services.port_conflict.{d}", .{left.port}),
                    .severity = "high",
                    .message = try std.fmt.allocPrint(tmp, "Multiple services use port {d}.", .{left.port}),
                    .hint = "Assign unique service ports.",
                });
                penalty += 20;
            }
        }
    }

    var score: i32 = 100 - penalty;
    if (score < 0) score = 0;

    const owned_findings = try page_alloc.alloc(Finding, findings.items.len);
    @memcpy(owned_findings, findings.items);

    return .{
        .score = score,
        .findings = owned_findings,
    };
}

// ---- Security scan logic ----

fn securityScan(stack: PStack) !SecurityReport {
    var arena = std.heap.ArenaAllocator.init(page_alloc);
    const tmp = arena.allocator();

    var vulns = std.ArrayList(Vulnerability).init(tmp);
    var check_count: i32 = 0;
    var pass_count: i32 = 0;
    var penalty: i32 = 0;

    // Check 1: Ensure no service runs as root (heuristic: kind == "web" + port < 1024)
    check_count += 1;
    var has_privileged = false;
    for (stack.services) |svc| {
        if (svc.port > 0 and svc.port < 1024) {
            has_privileged = true;
            try vulns.append(.{
                .id = try std.fmt.allocPrint(tmp, "sec.privileged_port.{s}", .{svc.name}),
                .title = "Privileged port binding",
                .severity = "medium",
                .description = try std.fmt.allocPrint(tmp, "Service '{s}' binds to port {d} which requires root.", .{ svc.name, svc.port }),
                .affected_component = dupe(svc.name),
                .fix_available = true,
            });
            penalty += 10;
        }
    }
    if (!has_privileged) pass_count += 1;

    // Check 2: Image provenance (prefer cgr.dev/chainguard or ghcr.io/hyperpolymath)
    check_count += 1;
    var untrusted_images = false;
    for (stack.services) |svc| {
        if (svc.image.len > 0 and
            !std.mem.startsWith(u8, svc.image, "cgr.dev/") and
            !std.mem.startsWith(u8, svc.image, "ghcr.io/hyperpolymath/"))
        {
            untrusted_images = true;
            try vulns.append(.{
                .id = try std.fmt.allocPrint(tmp, "sec.untrusted_image.{s}", .{svc.name}),
                .title = "Untrusted image source",
                .severity = "high",
                .description = try std.fmt.allocPrint(tmp, "Service '{s}' uses image from untrusted registry.", .{svc.name}),
                .affected_component = dupe(svc.name),
                .fix_available = true,
            });
            penalty += 15;
        }
    }
    if (!untrusted_images) pass_count += 1;

    // Check 3: Network segmentation
    check_count += 1;
    var has_network_config = false;
    for (stack.services) |svc| {
        if (svc.network.len > 0) {
            has_network_config = true;
            break;
        }
    }
    if (!has_network_config and stack.services.len > 1) {
        try vulns.append(.{
            .id = "sec.no_network_segmentation",
            .title = "No network segmentation",
            .severity = "medium",
            .description = "Stack has multiple services with no network isolation.",
            .affected_component = "stack",
            .fix_available = true,
        });
        penalty += 10;
    } else {
        pass_count += 1;
    }

    // Check 4: Resource limits
    check_count += 1;
    var missing_limits = false;
    for (stack.services) |svc| {
        if (svc.cpu == 0.0 and svc.memory == 0) {
            missing_limits = true;
        }
    }
    if (missing_limits) {
        try vulns.append(.{
            .id = "sec.no_resource_limits",
            .title = "Missing resource limits",
            .severity = "low",
            .description = "One or more services have no CPU/memory limits, risking denial of service.",
            .affected_component = "stack",
            .fix_available = true,
        });
        penalty += 5;
    } else {
        pass_count += 1;
    }

    // Check 5: Empty stack
    check_count += 1;
    if (stack.services.len == 0) {
        try vulns.append(.{
            .id = "sec.empty_stack",
            .title = "Empty stack",
            .severity = "high",
            .description = "Stack has no services to scan.",
            .affected_component = "stack",
            .fix_available = false,
        });
        penalty += 20;
    } else {
        pass_count += 1;
    }

    // Calculate grade
    var score: i32 = 100 - penalty;
    if (score < 0) score = 0;

    const grade: []const u8 = if (score >= 95)
        "A+"
    else if (score >= 90)
        "A"
    else if (score >= 85)
        "A-"
    else if (score >= 80)
        "B+"
    else if (score >= 75)
        "B"
    else if (score >= 70)
        "B-"
    else if (score >= 65)
        "C+"
    else if (score >= 60)
        "C"
    else if (score >= 50)
        "D"
    else
        "F";

    const owned_vulns = try page_alloc.alloc(Vulnerability, vulns.items.len);
    @memcpy(owned_vulns, vulns.items);

    return .{
        .grade = grade,
        .score = score,
        .vulnerabilities = owned_vulns,
        .check_count = check_count,
        .pass_count = pass_count,
    };
}

// ---- Gap analysis logic ----

fn gapAnalysis(stack: PStack) !GapReport {
    var arena = std.heap.ArenaAllocator.init(page_alloc);
    const tmp = arena.allocator();

    var gaps = std.ArrayList(Gap).init(tmp);
    var critical_count: i32 = 0;
    var high_count: i32 = 0;

    // Gap 1: No health check configured
    for (stack.services) |svc| {
        try gaps.append(.{
            .id = try std.fmt.allocPrint(tmp, "gap.no_healthcheck.{s}", .{svc.name}),
            .title = "No health check",
            .category = "reliability",
            .severity = "medium",
            .description = try std.fmt.allocPrint(tmp, "Service '{s}' has no health check configured.", .{svc.name}),
            .fix_available = true,
            .estimated_effort = "15 minutes",
        });
    }

    // Gap 2: No logging configuration
    try gaps.append(.{
        .id = "gap.no_logging",
        .title = "No centralized logging",
        .category = "best_practice",
        .severity = "low",
        .description = "Stack has no centralized logging configuration.",
        .fix_available = true,
        .estimated_effort = "30 minutes",
    });

    // Gap 3: No backup strategy for stateful services
    for (stack.services) |svc| {
        if (std.mem.eql(u8, svc.kind, "db") or std.mem.eql(u8, svc.kind, "database") or
            std.mem.eql(u8, svc.kind, "redis"))
        {
            try gaps.append(.{
                .id = try std.fmt.allocPrint(tmp, "gap.no_backup.{s}", .{svc.name}),
                .title = "No backup strategy",
                .category = "reliability",
                .severity = "high",
                .description = try std.fmt.allocPrint(tmp, "Stateful service '{s}' has no backup configuration.", .{svc.name}),
                .fix_available = true,
                .estimated_effort = "1 hour",
            });
            high_count += 1;
        }
    }

    // Gap 4: No SBOM generation
    try gaps.append(.{
        .id = "gap.no_sbom",
        .title = "No SBOM generation",
        .category = "compliance",
        .severity = "medium",
        .description = "Stack does not generate Software Bill of Materials.",
        .fix_available = true,
        .estimated_effort = "20 minutes",
    });

    // Gap 5: No image signing
    try gaps.append(.{
        .id = "gap.no_image_signing",
        .title = "No container image signing",
        .category = "security",
        .severity = "high",
        .description = "Container images are not cryptographically signed.",
        .fix_available = true,
        .estimated_effort = "45 minutes",
    });
    high_count += 1;

    // Gap 6: No secret rotation
    try gaps.append(.{
        .id = "gap.no_secret_rotation",
        .title = "No secret rotation policy",
        .category = "security",
        .severity = "medium",
        .description = "No automated secret rotation configured for the stack.",
        .fix_available = true,
        .estimated_effort = "1 hour",
    });

    // Gap 7: No rate limiting
    for (stack.services) |svc| {
        if (std.mem.eql(u8, svc.kind, "web") or std.mem.eql(u8, svc.kind, "api")) {
            try gaps.append(.{
                .id = try std.fmt.allocPrint(tmp, "gap.no_rate_limit.{s}", .{svc.name}),
                .title = "No rate limiting",
                .category = "security",
                .severity = "medium",
                .description = try std.fmt.allocPrint(tmp, "Web service '{s}' has no rate limiting.", .{svc.name}),
                .fix_available = true,
                .estimated_effort = "30 minutes",
            });
        }
    }

    const total: i32 = @intCast(gaps.items.len);
    const owned_gaps = try page_alloc.alloc(Gap, gaps.items.len);
    @memcpy(owned_gaps, gaps.items);

    return .{
        .total_gaps = total,
        .critical_count = critical_count,
        .high_count = high_count,
        .gaps = owned_gaps,
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

test "security scan returns report" {
    // Create a stack first
    const input = "{\"name\":\"sec-test\",\"services\":[{\"name\":\"api\",\"kind\":\"web\",\"port\":443}]}";
    var create_ptr: ?[*]u8 = null;
    var create_len: usize = 0;
    _ = stapeln_create_stack_json(input.ptr, input.len, &create_ptr, &create_len);
    if (create_ptr) |p| stapeln_free_buffer(p, create_len);

    // Find the stack id (use next_id - 1)
    const id = next_id - 1;
    var out_ptr: ?[*]u8 = null;
    var out_len: usize = 0;
    const rc = stapeln_security_scan_json(id, &out_ptr, &out_len);
    try std.testing.expectEqual(@as(c_int, 0), rc);
    try std.testing.expect(out_ptr != null);

    if (out_ptr) |p| {
        defer stapeln_free_buffer(p, out_len);
        const json = p[0..out_len];
        try std.testing.expect(std.mem.indexOf(u8, json, "grade") != null);
    }
}

test "gap analysis returns report" {
    const input = "{\"name\":\"gap-test\",\"services\":[{\"name\":\"db\",\"kind\":\"db\",\"port\":5432}]}";
    var create_ptr: ?[*]u8 = null;
    var create_len: usize = 0;
    _ = stapeln_create_stack_json(input.ptr, input.len, &create_ptr, &create_len);
    if (create_ptr) |p| stapeln_free_buffer(p, create_len);

    const id = next_id - 1;
    var out_ptr: ?[*]u8 = null;
    var out_len: usize = 0;
    const rc = stapeln_gap_analysis_json(id, &out_ptr, &out_len);
    try std.testing.expectEqual(@as(c_int, 0), rc);
    try std.testing.expect(out_ptr != null);

    if (out_ptr) |p| {
        defer stapeln_free_buffer(p, out_len);
        const json = p[0..out_len];
        try std.testing.expect(std.mem.indexOf(u8, json, "total_gaps") != null);
    }
}

test "dispatch routes correctly" {
    var out_ptr: ?[*]u8 = null;
    var out_len: usize = 0;

    const op = "list_stacks";
    const rc = stapeln_dispatch_json(op.ptr, op.len, null, 0, &out_ptr, &out_len);
    try std.testing.expectEqual(@as(c_int, 0), rc);
    try std.testing.expect(out_ptr != null);

    if (out_ptr) |p| {
        defer stapeln_free_buffer(p, out_len);
        const json = p[0..out_len];
        try std.testing.expect(json[0] == '[');
    }
}
