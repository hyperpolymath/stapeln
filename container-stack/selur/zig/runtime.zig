// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
//
// Zig WASM runtime for selur bridge
// Compiles Ephapax-linear types to WASM32
//
// Protocol format:
//   Request:  [command:1B][payloadLen:4B LE][payload:NB]
//   Response: [status:1B][dataLen:4B LE][data:NB]

const std = @import("std");

// ---------------------------------------------------------------------------
// Linear memory region (shared between Svalinn and Vordr)
// ---------------------------------------------------------------------------

const MEMORY_SIZE: usize = 1024 * 1024; // 1 MB
var memory: [MEMORY_SIZE]u8 = undefined;
var memory_offset: usize = 0;

// Maximum request/response size
const MAX_REQUEST_SIZE: usize = MEMORY_SIZE;

// Minimum request header size: 1B command + 4B payload length
const REQUEST_HEADER_SIZE: usize = 5;

// Response header size: 1B status + 4B data length
const RESPONSE_HEADER_SIZE: usize = 5;

// ---------------------------------------------------------------------------
// Free list for deallocation
// ---------------------------------------------------------------------------

const FreeBlock = struct {
    offset: u32,
    size: u32,
};

const MAX_FREE_BLOCKS: usize = 256;
var free_list: [MAX_FREE_BLOCKS]FreeBlock = undefined;
var free_list_len: usize = 0;

// ---------------------------------------------------------------------------
// Request queue for host bridging
// ---------------------------------------------------------------------------

const QueuedRequest = struct {
    command: u8,
    payload_offset: u32, // offset in linear memory where payload lives
    payload_len: u32,
    handle: u32, // unique handle for this request
    fulfilled: bool, // true once host has written a response
    response_offset: u32, // offset in linear memory where host wrote response
    response_len: u32, // total response size (header + data)
};

const MAX_QUEUED_REQUESTS: usize = 64;
var request_queue: [MAX_QUEUED_REQUESTS]QueuedRequest = undefined;
var request_queue_len: usize = 0;
var next_handle: u32 = 1;

// ---------------------------------------------------------------------------
// Request/Response structures (matches Ephapax types)
// ---------------------------------------------------------------------------

const Request = struct {
    command: u32,
    payload_ptr: u32,
    payload_len: u32,
    correlation_id: u64,
};

const Response = struct {
    status: u32,
    payload_ptr: u32,
    payload_len: u32,
    correlation_id: u64,
};

// ---------------------------------------------------------------------------
// Command types (must match Ephapax enum)
// ---------------------------------------------------------------------------

const Command = enum(u8) {
    CreateContainer = 1,
    StartContainer = 2,
    StopContainer = 3,
    InspectContainer = 4,
    DeleteContainer = 5,
    ListContainers = 6,
};

// ---------------------------------------------------------------------------
// Status/error codes
// ---------------------------------------------------------------------------

const StatusCode = enum(u8) {
    Success = 0,
    InvalidRequest = 1,
    ContainerNotFound = 2,
    ContainerAlreadyExists = 3,
    ResourceExhausted = 4,
    PermissionDenied = 5,
    InternalError = 6,
};

// Legacy error code enum (kept for backward compat with struct-based API)
const ErrorCode = enum(u32) {
    Success = 0,
    InvalidRequest = 1,
    ContainerNotFound = 2,
    ContainerAlreadyExists = 3,
    ResourceExhausted = 4,
    PermissionDenied = 5,
    InternalError = 6,
};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Read a 4-byte little-endian u32 from memory at the given offset.
fn readU32LE(offset: usize) u32 {
    if (offset + 4 > memory.len) return 0;
    return @as(u32, memory[offset]) |
        (@as(u32, memory[offset + 1]) << 8) |
        (@as(u32, memory[offset + 2]) << 16) |
        (@as(u32, memory[offset + 3]) << 24);
}

/// Write a 4-byte little-endian u32 to memory at the given offset.
fn writeU32LE(offset: usize, value: u32) void {
    if (offset + 4 > memory.len) return;
    memory[offset] = @truncate(value);
    memory[offset + 1] = @truncate(value >> 8);
    memory[offset + 2] = @truncate(value >> 16);
    memory[offset + 3] = @truncate(value >> 24);
}

/// Validate that a command byte is a known command.
fn isValidCommand(cmd: u8) bool {
    return switch (cmd) {
        1, 2, 3, 4, 5, 6 => true,
        else => false,
    };
}

// ---------------------------------------------------------------------------
// Export: Allocate memory in WASM linear memory
// ---------------------------------------------------------------------------

/// Allocate a contiguous region from linear memory. First checks the free list
/// for a suitable block (first-fit), then falls back to bump allocation.
export fn allocate(size: u32) u32 {
    if (size == 0) return 0;
    if (size > MAX_REQUEST_SIZE) {
        return 0; // signal failure with null pointer
    }

    // First-fit search in free list
    var i: usize = 0;
    while (i < free_list_len) : (i += 1) {
        if (free_list[i].size >= size) {
            const ptr = free_list[i].offset;
            const remaining = free_list[i].size - size;
            if (remaining > 0) {
                // Shrink the free block
                free_list[i].offset += size;
                free_list[i].size = remaining;
            } else {
                // Remove the block by swapping with last
                free_list_len -= 1;
                if (i < free_list_len) {
                    free_list[i] = free_list[free_list_len];
                }
            }
            return ptr;
        }
    }

    // Bump allocation
    const ptr = memory_offset;
    const new_offset = memory_offset + size;
    if (new_offset > memory.len) {
        return 0; // out of memory
    }
    memory_offset = new_offset;
    return @intCast(ptr);
}

// ---------------------------------------------------------------------------
// Export: Deallocate memory — add to free list
// ---------------------------------------------------------------------------

/// Return a region to the free list. Adjacent free blocks are not coalesced
/// (kept simple; linear types should prevent most fragmentation).
export fn deallocate(ptr: u32, size: u32) void {
    if (size == 0) return;
    if (ptr + size > memory.len) return; // invalid region

    // If this region is at the top of the bump allocator, just shrink it
    if (ptr + size == memory_offset) {
        memory_offset -= size;
        return;
    }

    // Otherwise add to free list
    if (free_list_len >= MAX_FREE_BLOCKS) {
        // Free list full — leak the memory rather than corrupt state.
        // In practice, linear types should prevent this from happening.
        return;
    }

    free_list[free_list_len] = FreeBlock{
        .offset = ptr,
        .size = size,
    };
    free_list_len += 1;
}

// ---------------------------------------------------------------------------
// Export: Send request from Svalinn to Vordr
// ---------------------------------------------------------------------------

/// Parse a binary request from linear memory, validate it, and enqueue it for
/// the host (Rust wasmtime) to bridge to Vordr.
///
/// The request at `request_ptr` must be: [command:1B][payloadLen:4B LE][payload:NB]
///
/// Returns a handle (>= 1) on success that the host uses to write back a
/// response. Returns 0 on validation failure (check get_last_error for code).
export fn send_request(request_ptr: u32, request_len: u32) u32 {
    // Basic bounds check
    if (request_len < REQUEST_HEADER_SIZE) {
        return 0;
    }
    if (request_ptr + request_len > memory.len) {
        return 0;
    }

    // Parse header
    const cmd_byte: u8 = memory[request_ptr];
    const payload_len = readU32LE(request_ptr + 1);

    // Validate command
    if (!isValidCommand(cmd_byte)) {
        return 0;
    }

    // Validate payload length matches actual data
    if (REQUEST_HEADER_SIZE + payload_len != request_len) {
        return 0;
    }

    // Validate payload fits in memory
    if (request_ptr + REQUEST_HEADER_SIZE + payload_len > memory.len) {
        return 0;
    }

    // Check queue capacity
    if (request_queue_len >= MAX_QUEUED_REQUESTS) {
        return 0;
    }

    // Assign handle and enqueue
    const handle = next_handle;
    next_handle +%= 1;
    if (next_handle == 0) next_handle = 1; // skip 0 (reserved for error)

    request_queue[request_queue_len] = QueuedRequest{
        .command = cmd_byte,
        .payload_offset = request_ptr + REQUEST_HEADER_SIZE,
        .payload_len = payload_len,
        .handle = handle,
        .fulfilled = false,
        .response_offset = 0,
        .response_len = 0,
    };
    request_queue_len += 1;

    return handle;
}

// ---------------------------------------------------------------------------
// Export: Get response from Vordr to Svalinn
// ---------------------------------------------------------------------------

/// Read the response for a given handle. The host (Rust) calls
/// fulfill_request() to write the response into linear memory, then Svalinn
/// calls get_response() to retrieve it.
///
/// `response_ptr` is where to write the Response struct for backward compat.
/// Returns StatusCode (0=Success) or an error code.
///
/// The actual response data is at the offset stored in the Response struct's
/// payload_ptr field.
export fn get_response(response_ptr: u32) u32 {
    // Bounds check for Response struct
    if (response_ptr + @sizeOf(Response) > memory.len) {
        return @intFromEnum(ErrorCode.InvalidRequest);
    }

    // Find the most recently fulfilled request (simple FIFO)
    var found_idx: ?usize = null;
    var i: usize = 0;
    while (i < request_queue_len) : (i += 1) {
        if (request_queue[i].fulfilled) {
            found_idx = i;
            break;
        }
    }

    if (found_idx == null) {
        // No fulfilled request — write an empty response
        const response: *Response = @ptrCast(@alignCast(&memory[response_ptr]));
        response.status = @intFromEnum(ErrorCode.InternalError);
        response.payload_ptr = 0;
        response.payload_len = 0;
        response.correlation_id = 0;
        return @intFromEnum(ErrorCode.InternalError);
    }

    const idx = found_idx.?;
    const queued = request_queue[idx];

    // Write the Response struct
    const response: *Response = @ptrCast(@alignCast(&memory[response_ptr]));

    if (queued.response_len >= RESPONSE_HEADER_SIZE) {
        // Parse the response the host wrote: [status:1B][dataLen:4B LE][data:NB]
        const status_byte: u8 = memory[queued.response_offset];
        const data_len = readU32LE(queued.response_offset + 1);
        const data_offset = queued.response_offset + RESPONSE_HEADER_SIZE;

        response.status = @as(u32, status_byte);
        response.payload_ptr = data_offset;
        response.payload_len = data_len;
        response.correlation_id = @as(u64, queued.handle);
    } else {
        // Host wrote a malformed response
        response.status = @intFromEnum(ErrorCode.InternalError);
        response.payload_ptr = 0;
        response.payload_len = 0;
        response.correlation_id = @as(u64, queued.handle);
    }

    // Remove fulfilled request from queue by swapping with last
    request_queue_len -= 1;
    if (idx < request_queue_len) {
        request_queue[idx] = request_queue[request_queue_len];
    }

    return response.status;
}

// ---------------------------------------------------------------------------
// Export: Host-side API — fulfill a queued request
// ---------------------------------------------------------------------------

/// Called by the Rust host after it has bridged the request to Vordr and
/// received a result. The host writes the response into linear memory at
/// `response_offset` in the binary protocol format, then calls this to mark
/// the request as fulfilled.
export fn fulfill_request(handle: u32, response_offset: u32, response_len: u32) u32 {
    if (response_offset + response_len > memory.len) {
        return @intFromEnum(ErrorCode.InvalidRequest);
    }

    var i: usize = 0;
    while (i < request_queue_len) : (i += 1) {
        if (request_queue[i].handle == handle) {
            request_queue[i].fulfilled = true;
            request_queue[i].response_offset = response_offset;
            request_queue[i].response_len = response_len;
            return @intFromEnum(ErrorCode.Success);
        }
    }

    return @intFromEnum(ErrorCode.ContainerNotFound); // handle not found
}

// ---------------------------------------------------------------------------
// Export: Queue introspection (for host and debugging)
// ---------------------------------------------------------------------------

/// Return the number of pending (unfulfilled) requests in the queue.
export fn get_pending_count() u32 {
    var count: u32 = 0;
    var i: usize = 0;
    while (i < request_queue_len) : (i += 1) {
        if (!request_queue[i].fulfilled) {
            count += 1;
        }
    }
    return count;
}

/// Return the handle of the Nth pending request (0-indexed), or 0 if out of range.
/// The host uses this to iterate pending requests and fulfill them.
export fn get_pending_handle(index: u32) u32 {
    var seen: u32 = 0;
    var i: usize = 0;
    while (i < request_queue_len) : (i += 1) {
        if (!request_queue[i].fulfilled) {
            if (seen == index) {
                return request_queue[i].handle;
            }
            seen += 1;
        }
    }
    return 0;
}

/// Return the command byte for a queued request by handle, or 0 if not found.
export fn get_request_command(handle: u32) u32 {
    var i: usize = 0;
    while (i < request_queue_len) : (i += 1) {
        if (request_queue[i].handle == handle) {
            return @as(u32, request_queue[i].command);
        }
    }
    return 0;
}

/// Return the payload offset for a queued request by handle, or 0 if not found.
export fn get_request_payload_ptr(handle: u32) u32 {
    var i: usize = 0;
    while (i < request_queue_len) : (i += 1) {
        if (request_queue[i].handle == handle) {
            return request_queue[i].payload_offset;
        }
    }
    return 0;
}

/// Return the payload length for a queued request by handle, or 0 if not found.
export fn get_request_payload_len(handle: u32) u32 {
    var i: usize = 0;
    while (i < request_queue_len) : (i += 1) {
        if (request_queue[i].handle == handle) {
            return request_queue[i].payload_len;
        }
    }
    return 0;
}

// ---------------------------------------------------------------------------
// Export: Memory introspection
// ---------------------------------------------------------------------------

/// Get pointer to the start of linear memory (for debugging).
export fn get_memory_ptr() [*]u8 {
    return &memory;
}

/// Get total linear memory size.
export fn get_memory_size() u32 {
    return memory.len;
}

/// Get current bump allocator offset (bytes used, not counting free list).
export fn get_memory_used() u32 {
    return @intCast(memory_offset);
}

/// Get number of blocks in the free list.
export fn get_free_block_count() u32 {
    return @intCast(free_list_len);
}
