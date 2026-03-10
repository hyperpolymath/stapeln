// SPDX-License-Identifier: PMPL-1.0-or-later
//! eBPF tracepoint program for sched/sched_process_exec
//!
//! This program captures process execution events — every time a new
//! binary is exec'd inside a monitored container. This is one of the
//! most security-critical tracepoints: it reveals what code is actually
//! running, not just what syscalls are being made.
//!
//! # Why a separate program?
//!
//! While `sys_enter` catches execve(2) syscall *entry*, this tracepoint
//! fires after the kernel has committed to the exec — the new binary is
//! loaded, the comm field is updated, and the process is about to start
//! executing. This gives us:
//!
//! - The *new* comm (not the parent's)
//! - Confirmation that exec succeeded (sys_enter fires even if exec fails)
//! - Access to the filename from the linux_binprm struct
//!
//! # Data flow
//!
//! ```text
//! sched_process_exec tracepoint
//!   -> vordr_sched_process_exec()
//!     -> check CONTAINER_FILTER (cgroup match)
//!     -> read pid, uid, comm from task context
//!     -> read filename from tracepoint args
//!     -> submit ExecEventBpf to EXEC_EVENTS ring buffer
//!   -> userspace Monitor processes exec events
//! ```

#![no_std]
#![no_main]

use aya_ebpf::{
    helpers::{bpf_get_current_cgroup_id, bpf_get_current_comm, bpf_get_current_pid_tgid,
              bpf_get_current_uid_gid, bpf_ktime_get_ns, bpf_probe_read_kernel_str_bytes},
    macros::{map, tracepoint},
    maps::{HashMap, RingBuf},
    programs::TracePointContext,
};
use aya_log_ebpf::info;

// Import shared types from the sys_enter module.
// In a real aya-bpf project these would likely live in a shared crate,
// but for clarity we define exec-specific types here and re-use the
// container filter map type.

// ---------------------------------------------------------------------------
// Shared types
// ---------------------------------------------------------------------------

/// Container filter entry — same layout as in sys_enter.rs.
///
/// Both programs reference the same CONTAINER_FILTER map (shared via
/// the userspace Bpf object), so the struct layout must be identical.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct ContainerFilterBpf {
    pub cgroup_id: u64,
    pub pid_ns: u64,
    pub enabled: u8,
    pub _padding: [u8; 7],
}

/// Process execution event.
///
/// Captures the moment a new binary begins execution inside a container.
/// This is richer than a raw execve syscall event because:
/// - `comm` reflects the *new* process name (post-exec)
/// - `filename` contains the path to the executed binary
/// - The event only fires on successful exec (failed execs are silent)
///
/// The userspace side converts this to a higher-level event for the
/// anomaly detector, which maintains a baseline of expected executables
/// per container.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct ExecEventBpf {
    /// Process ID (tgid — what userspace calls "PID")
    pub pid: u32,

    /// Thread ID (the kernel-level pid)
    pub tid: u32,

    /// User ID of the process performing the exec
    pub uid: u32,

    /// Group ID
    pub gid: u32,

    /// Timestamp in nanoseconds since boot
    pub timestamp_ns: u64,

    /// Command name after exec (task_struct->comm, 16 bytes)
    ///
    /// This is the *new* process name. For example, if bash execs
    /// `/usr/bin/curl`, this will be "curl", not "bash".
    pub comm: [u8; 16],

    /// Filename of the executed binary (up to 256 bytes).
    ///
    /// Read from the tracepoint's `filename` field, which comes from
    /// the linux_binprm struct. This is the path as resolved by the
    /// kernel (may be relative to the process's root/cwd).
    ///
    /// Null-terminated; unused bytes are zeroed.
    pub filename: [u8; 256],

    /// Length of the filename (excluding null terminator).
    /// Allows userspace to slice without scanning for null.
    pub filename_len: u32,

    /// cgroup ID for container identification
    pub cgroup_id: u64,
}

// ---------------------------------------------------------------------------
// BPF maps
// ---------------------------------------------------------------------------

/// Ring buffer for exec events, separate from the syscall event buffer.
///
/// Exec events are less frequent than raw syscalls but more important
/// for security analysis. A separate ring buffer ensures exec events
/// are never crowded out by high-frequency syscall noise.
///
/// Size: 64KB — exec events are larger (~300 bytes with filename) but
/// much less frequent. 64KB holds ~200 events, which is plenty for
/// any realistic exec burst.
#[map]
static EXEC_EVENTS: RingBuf = RingBuf::with_byte_size(64 * 1024, 0);

/// Container filter map — shared with sys_enter.rs.
///
/// Both programs read from the same underlying map (the userspace
/// Bpf loader pins both to the same map FD). The key/value layout
/// must be identical across all programs that share this map.
#[map]
static CONTAINER_FILTER: HashMap<u32, ContainerFilterBpf> =
    HashMap::with_max_entries(64, 0);

// ---------------------------------------------------------------------------
// Tracepoint program: sched/sched_process_exec
// ---------------------------------------------------------------------------

/// eBPF handler for process execution events.
///
/// Fires every time the kernel commits to executing a new binary
/// (after the point of no return in do_execveat_common).
///
/// # Tracepoint format (sched/sched_process_exec)
///
/// From /sys/kernel/debug/tracing/events/sched/sched_process_exec/format:
///
/// ```text
/// field:unsigned short common_type;  offset:0;  size:2;
/// field:unsigned char common_flags;  offset:2;  size:1;
/// field:unsigned char common_preempt_count; offset:3; size:1;
/// field:int common_pid;              offset:4;  size:4;
///
/// field:__data_loc char[] filename;  offset:8;  size:4;
/// field:pid_t pid;                   offset:12; size:4;
/// field:pid_t old_pid;               offset:16; size:4;
/// ```
///
/// The `filename` field uses `__data_loc` encoding: the lower 16 bits
/// give the offset from the start of the tracepoint entry, and the
/// upper 16 bits give the length.
#[tracepoint]
pub fn vordr_sched_process_exec(ctx: TracePointContext) -> u32 {
    match try_sched_process_exec(&ctx) {
        Ok(ret) => ret,
        Err(_) => 0,
    }
}

/// Inner implementation for sched_process_exec handling.
#[inline(always)]
fn try_sched_process_exec(ctx: &TracePointContext) -> Result<u32, i64> {
    // ---------------------------------------------------------------
    // Step 1: Container (cgroup) filtering
    // ---------------------------------------------------------------
    let cgroup_id = unsafe { bpf_get_current_cgroup_id() };

    if let Some(filter) = unsafe { CONTAINER_FILTER.get(&0) } {
        if filter.enabled != 0 && filter.cgroup_id != 0 && filter.cgroup_id != cgroup_id {
            return Ok(0);
        }
    }

    // ---------------------------------------------------------------
    // Step 2: Gather process context
    // ---------------------------------------------------------------
    let pid_tgid = bpf_get_current_pid_tgid();
    let pid = (pid_tgid >> 32) as u32;
    let tid = pid_tgid as u32;

    let uid_gid = bpf_get_current_uid_gid();
    let uid = uid_gid as u32;
    let gid = (uid_gid >> 32) as u32;

    let comm = bpf_get_current_comm().map_err(|e| e as i64)?;
    let timestamp_ns = unsafe { bpf_ktime_get_ns() };

    // ---------------------------------------------------------------
    // Step 3: Read filename from tracepoint context
    // ---------------------------------------------------------------
    // The filename uses __data_loc encoding at offset 8.
    // Read the __data_loc descriptor first to find where the string is.
    let data_loc: u32 = unsafe { ctx.read_at(8)? };
    let filename_offset = (data_loc & 0xFFFF) as usize;
    let filename_len_raw = ((data_loc >> 16) & 0xFFFF) as usize;

    // Cap the filename length to our buffer size
    let filename_len = if filename_len_raw > 255 {
        255
    } else {
        filename_len_raw
    };

    // Read the filename string from the tracepoint data.
    // We read into a stack buffer (BPF stack is 512 bytes, so we
    // must be careful — this struct is too large for the stack alone,
    // which is why we write directly into the ring buffer entry).
    let mut filename_buf: [u8; 256] = [0u8; 256];
    if filename_len > 0 {
        // Use bpf_probe_read_kernel_str_bytes to safely read the
        // null-terminated filename from the tracepoint data area.
        let src_ptr = unsafe {
            (ctx.as_ptr() as *const u8).add(filename_offset)
        };
        let _ = unsafe {
            bpf_probe_read_kernel_str_bytes(src_ptr, &mut filename_buf[..filename_len])
        };
    }

    // ---------------------------------------------------------------
    // Step 4: Build event and submit to ring buffer
    // ---------------------------------------------------------------
    let event = ExecEventBpf {
        pid,
        tid,
        uid,
        gid,
        timestamp_ns,
        comm,
        filename: filename_buf,
        filename_len: filename_len as u32,
        cgroup_id,
    };

    if let Some(mut entry) = EXEC_EVENTS.reserve::<ExecEventBpf>(0) {
        entry.write(event);
        entry.submit(0);
    }

    Ok(0)
}

// ---------------------------------------------------------------------------
// Panic handler
// ---------------------------------------------------------------------------

#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    unsafe { core::hint::unreachable_unchecked() }
}
