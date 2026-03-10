// SPDX-License-Identifier: PMPL-1.0-or-later
//! eBPF tracepoint program for raw_syscalls/sys_enter
//!
//! This program attaches to the sys_enter tracepoint and captures
//! syscall events for monitored containers, sending them to userspace
//! via a ring buffer.
//!
//! # Kernel-side constraints
//!
//! eBPF programs run inside the kernel verifier sandbox. This means:
//! - No heap allocation (no Vec, String, Box, etc.)
//! - No floating point
//! - Fixed-size stack (512 bytes max)
//! - All loops must be bounded
//! - All memory accesses must be bounds-checked
//! - Only BPF helper functions available (no libc, no std)
//!
//! # Data flow
//!
//! ```text
//! sys_enter tracepoint
//!   -> vordr_sys_enter()
//!     -> check CONTAINER_FILTER (is this cgroup monitored?)
//!     -> check SYSCALL_FILTER (do we care about this syscall?)
//!     -> fill SyscallEventBpf from task context
//!     -> submit to EVENTS ring buffer
//!   -> userspace Monitor reads ring buffer
//! ```

#![no_std]
#![no_main]

use aya_ebpf::{
    helpers::{bpf_get_current_cgroup_id, bpf_get_current_comm, bpf_get_current_pid_tgid,
              bpf_get_current_uid_gid, bpf_ktime_get_ns},
    macros::{map, tracepoint},
    maps::{HashMap, RingBuf},
    programs::TracePointContext,
};
use aya_log_ebpf::info;

// ---------------------------------------------------------------------------
// Shared types (kernel-side, #[repr(C)] for ring buffer transport)
// ---------------------------------------------------------------------------

/// Syscall event as seen by the BPF program.
///
/// This is the kernel-side counterpart of `events::SyscallEvent`.
/// Key differences from the userspace struct:
/// - `comm` is a fixed-size `[u8; 16]` (not String)
/// - `ret` is always 0 here (sys_enter has no return value yet)
/// - No Option types (BPF has no enum discriminants for Option)
///
/// The userspace reader must convert this to `SyscallEvent` after
/// reading from the ring buffer.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct SyscallEventBpf {
    /// Process ID (tgid in kernel terms)
    pub pid: u32,

    /// Thread ID (the actual pid in kernel terms)
    pub tid: u32,

    /// User ID
    pub uid: u32,

    /// Group ID
    pub gid: u32,

    /// Syscall number (x86_64 syscall table)
    pub syscall_nr: i64,

    /// First 6 syscall arguments from registers
    pub args: [u64; 6],

    /// Return value (always 0 for sys_enter; populated by sys_exit)
    pub ret: i64,

    /// Timestamp in nanoseconds since boot (CLOCK_MONOTONIC)
    pub timestamp_ns: u64,

    /// Command name from task_struct->comm (null-terminated, max 16 bytes)
    pub comm: [u8; 16],

    /// cgroup ID for container identification
    pub cgroup_id: u64,
}

/// Container filter entry, matching `probes::ContainerFilter`.
///
/// Stored in the CONTAINER_FILTER map. The userspace ProbeManager
/// populates this when `set_container_filter()` is called.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct ContainerFilterBpf {
    /// CGroup ID to match (0 = wildcard, match all)
    pub cgroup_id: u64,

    /// PID namespace ID to match (0 = wildcard)
    pub pid_ns: u64,

    /// Whether this filter entry is active (1 = yes, 0 = no)
    pub enabled: u8,

    /// Padding to align struct to 8 bytes (must match userspace)
    pub _padding: [u8; 7],
}

// ---------------------------------------------------------------------------
// BPF maps
// ---------------------------------------------------------------------------

/// Ring buffer for sending events to userspace.
///
/// Size: 256KB (262144 bytes). This is a power-of-two as required by
/// the kernel. At ~120 bytes per event, this holds ~2000 events before
/// the oldest is overwritten. The userspace Monitor should drain this
/// fast enough to avoid drops.
#[map]
static EVENTS: RingBuf = RingBuf::with_byte_size(256 * 1024, 0);

/// Container filter map.
///
/// Key: slot index (u32). Slot 0 is the primary filter.
/// Value: ContainerFilterBpf describing which cgroup(s) to monitor.
///
/// When empty or when the entry at slot 0 has `enabled == 0`, all
/// containers are monitored (no filtering).
///
/// The userspace ProbeManager writes to this map via
/// `set_container_filter()` / `clear_container_filter()`.
#[map]
static CONTAINER_FILTER: HashMap<u32, ContainerFilterBpf> =
    HashMap::with_max_entries(64, 0);

/// Syscall allow-list filter.
///
/// Key: syscall number (i64, matching x86_64 syscall table).
/// Value: action byte:
///   - 0 = ignore (do not emit event)
///   - 1 = log (emit event to ring buffer)
///   - 2 = audit (emit event + set audit flag)
///
/// When this map is EMPTY, ALL syscalls are captured (no filtering).
/// The userspace side populates this from `SyscallPolicy` filters.
#[map]
static SYSCALL_FILTER: HashMap<i64, u8> =
    HashMap::with_max_entries(512, 0);

// ---------------------------------------------------------------------------
// Tracepoint program: raw_syscalls/sys_enter
// ---------------------------------------------------------------------------

/// Main eBPF tracepoint handler for syscall entry.
///
/// This fires on every syscall from every process on the system.
/// We filter early to minimise overhead:
///
/// 1. Read the syscall number from the tracepoint context
/// 2. Check if the calling process is in a monitored cgroup
/// 3. Check if this syscall number is in our interest set
/// 4. If both pass, build SyscallEventBpf and submit to ring buffer
///
/// # Tracepoint format (raw_syscalls/sys_enter)
///
/// The context layout is defined by the kernel at:
///   /sys/kernel/debug/tracing/events/raw_syscalls/sys_enter/format
///
/// Fields:
///   - `id` (offset 8): syscall number (long)
///   - `args[0..5]` (offset 16): syscall arguments (unsigned long[6])
#[tracepoint]
pub fn vordr_sys_enter(ctx: TracePointContext) -> u32 {
    // Safety: we return 0 on all error paths so the tracepoint
    // never blocks the actual syscall.
    match try_sys_enter(&ctx) {
        Ok(ret) => ret,
        Err(_) => 0,
    }
}

/// Inner implementation with Result return for cleaner error handling.
///
/// Separated from the tracepoint entry point so we can use `?` for
/// fallible BPF helper calls without panicking.
#[inline(always)]
fn try_sys_enter(ctx: &TracePointContext) -> Result<u32, i64> {
    // ---------------------------------------------------------------
    // Step 1: Read syscall number from tracepoint context
    // ---------------------------------------------------------------
    // The `id` field is at offset 8 in the raw_syscalls/sys_enter
    // tracepoint format. This is a `long` (i64 on x86_64).
    let syscall_nr: i64 = unsafe { ctx.read_at(8)? };

    // ---------------------------------------------------------------
    // Step 2: Container (cgroup) filtering
    // ---------------------------------------------------------------
    // Get the cgroup ID of the calling process. This is the v2 cgroup
    // ID which uniquely identifies the container's cgroup hierarchy.
    let cgroup_id = unsafe { bpf_get_current_cgroup_id() };

    // Check if we have a container filter active in slot 0
    if let Some(filter) = unsafe { CONTAINER_FILTER.get(&0) } {
        if filter.enabled != 0 {
            // Filter is active — check if this cgroup matches
            if filter.cgroup_id != 0 && filter.cgroup_id != cgroup_id {
                // Not our container, skip silently
                return Ok(0);
            }
        }
    }
    // If no filter is set or filter.enabled == 0, we monitor everything

    // ---------------------------------------------------------------
    // Step 3: Syscall number filtering
    // ---------------------------------------------------------------
    // If the SYSCALL_FILTER map has entries, only emit events for
    // syscalls present in the map. If the map is empty, emit all.
    //
    // We check for the key's existence. If found, the value tells us
    // the action (1 = log, 2 = audit). If not found and the map has
    // entries, skip this syscall.
    let _action: u8 = if let Some(action) = unsafe { SYSCALL_FILTER.get(&syscall_nr) } {
        if *action == 0 {
            // Explicitly marked as "ignore"
            return Ok(0);
        }
        *action
    } else {
        // Key not in map — if map is used for allowlisting, we'd skip.
        // For now, default to logging everything not explicitly ignored.
        // The userspace side controls this by populating the map.
        1
    };

    // ---------------------------------------------------------------
    // Step 4: Gather process context from BPF helpers
    // ---------------------------------------------------------------

    // pid_tgid: upper 32 bits = tgid (userspace PID), lower 32 = tid
    let pid_tgid = bpf_get_current_pid_tgid();
    let pid = (pid_tgid >> 32) as u32;
    let tid = pid_tgid as u32;

    // uid_gid: upper 32 bits = gid, lower 32 = uid
    let uid_gid = bpf_get_current_uid_gid();
    let uid = uid_gid as u32;
    let gid = (uid_gid >> 32) as u32;

    // Command name (task_struct->comm, 16 bytes max)
    let comm = bpf_get_current_comm().map_err(|e| e as i64)?;

    // Timestamp (nanoseconds since boot, CLOCK_MONOTONIC)
    let timestamp_ns = unsafe { bpf_ktime_get_ns() };

    // Read syscall arguments from the tracepoint context.
    // args[0..5] start at offset 16 in the raw_syscalls/sys_enter format,
    // each is an unsigned long (u64 on x86_64), 8 bytes apart.
    let args: [u64; 6] = [
        unsafe { ctx.read_at(16).unwrap_or(0) },
        unsafe { ctx.read_at(24).unwrap_or(0) },
        unsafe { ctx.read_at(32).unwrap_or(0) },
        unsafe { ctx.read_at(40).unwrap_or(0) },
        unsafe { ctx.read_at(48).unwrap_or(0) },
        unsafe { ctx.read_at(56).unwrap_or(0) },
    ];

    // ---------------------------------------------------------------
    // Step 5: Build event and submit to ring buffer
    // ---------------------------------------------------------------
    let event = SyscallEventBpf {
        pid,
        tid,
        uid,
        gid,
        syscall_nr,
        args,
        ret: 0, // sys_enter has no return value
        timestamp_ns,
        comm,
        cgroup_id,
    };

    // Reserve space in the ring buffer and write the event.
    // If the ring buffer is full, this returns None and the event
    // is silently dropped. The userspace side tracks drop counts
    // via the ProbeStats::events_dropped counter.
    if let Some(mut entry) = EVENTS.reserve::<SyscallEventBpf>(0) {
        // Write the event into the reserved slot
        entry.write(event);
        // Submit makes the entry visible to userspace consumers
        entry.submit(0);
    }
    // else: ring buffer full, event dropped (acceptable under load)

    Ok(0)
}

// ---------------------------------------------------------------------------
// Panic handler (required for #![no_std] + #![no_main])
// ---------------------------------------------------------------------------

/// Panic handler stub.
///
/// eBPF programs cannot panic in any meaningful way. The kernel
/// verifier ensures all code paths terminate, so this is purely
/// to satisfy the Rust compiler's requirement for a panic handler
/// in `#![no_std]` crates.
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    unsafe { core::hint::unreachable_unchecked() }
}
