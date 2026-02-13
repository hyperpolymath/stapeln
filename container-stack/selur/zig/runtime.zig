// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
//
// Zig WASM runtime for selur bridge
// Compiles Ephapax-linear types to WASM32

const std = @import("std");

// WASM linear memory (shared between Svalinn and Vörðr)
var memory: [1024 * 1024]u8 = undefined;  // 1 MB
var memory_offset: usize = 0;

// Maximum request/response size
const MAX_REQUEST_SIZE: usize = 1024 * 1024;  // 1 MB

// Request/Response structures (matches Ephapax types)
const Request = struct {
    command: u32,        // Command type
    payload_ptr: u32,    // Offset in linear memory
    payload_len: u32,    // Payload length
    correlation_id: u64, // For matching requests/responses
};

const Response = struct {
    status: u32,         // 0 = success, 1+ = error
    payload_ptr: u32,    // Offset in linear memory
    payload_len: u32,    // Payload length
    correlation_id: u64, // Matches request correlation_id
};

// Command types (must match Ephapax enum)
const Command = enum(u32) {
    CreateContainer = 1,
    StartContainer = 2,
    StopContainer = 3,
    InspectContainer = 4,
    DeleteContainer = 5,
    ListContainers = 6,
};

// Error codes
const ErrorCode = enum(u32) {
    Success = 0,
    InvalidRequest = 1,
    ContainerNotFound = 2,
    ContainerAlreadyExists = 3,
    ResourceExhausted = 4,
    PermissionDenied = 5,
    InternalError = 6,
};

// Export: Allocate memory in WASM linear memory
export fn allocate(size: u32)  u32 {
    if (size > MAX_REQUEST_SIZE) {
        @panic("Allocation too large");
    }

    const ptr = memory_offset;
    memory_offset += size;

    if (memory_offset > memory.len) {
        @panic("Out of memory");
    }

    return @intCast(ptr);
}

// Export: Free memory (placeholder - linear types handle this)
export fn deallocate(ptr: u32, size: u32)  void {
    _ = ptr;
    _ = size;
    // TODO: Implement proper deallocation when needed
}

// Export: Send request from Svalinn to Vörðr
export fn send_request(request_ptr: u32, request_len: u32)  u32 {
    // Bounds check
    if (request_len > MAX_REQUEST_SIZE) {
        return @intFromEnum(ErrorCode.InvalidRequest);
    }

    if (request_ptr + request_len > memory.len) {
        return @intFromEnum(ErrorCode.InvalidRequest);
    }

    // TODO: Parse request, validate, delegate to Vörðr
    // For now, return success
    return @intFromEnum(ErrorCode.Success);
}

// Export: Get response from Vörðr to Svalinn
export fn get_response(response_ptr: u32)  u32 {
    // Bounds check
    if (response_ptr + @sizeOf(Response) > memory.len) {
        return @intFromEnum(ErrorCode.InvalidRequest);
    }

    // TODO: Build response from Vörðr result
    // For now, return a placeholder response

    const response: *Response = @ptrCast(@alignCast(&memory[response_ptr]));
    response.status = @intFromEnum(ErrorCode.Success);
    response.payload_ptr = 0;
    response.payload_len = 0;
    response.correlation_id = 0;

    return @intFromEnum(ErrorCode.Success);
}

// Export: Get memory pointer (for debugging)
export fn get_memory_ptr()  [*]u8 {
    return &memory;
}

// Export: Get memory size
export fn get_memory_size()  u32 {
    return memory.len;
}
