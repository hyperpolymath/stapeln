// SPDX-License-Identifier: PMPL-1.0-or-later
//! Event types for eBPF monitoring
//!
//! These types represent events captured by eBPF probes and
//! communicated to userspace via ring buffer.

use serde::{Deserialize, Serialize};
use std::time::SystemTime;

/// A syscall event captured by eBPF probes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SyscallEvent {
    /// Process ID
    pub pid: u32,

    /// Thread ID
    pub tid: u32,

    /// User ID
    pub uid: u32,

    /// Group ID
    pub gid: u32,

    /// Syscall number
    pub syscall_nr: i64,

    /// Syscall arguments (up to 6)
    pub args: [u64; 6],

    /// Return value (if exit event)
    pub ret: Option<i64>,

    /// Timestamp (nanoseconds since boot)
    pub timestamp_ns: u64,

    /// Command name (up to 16 chars)
    pub comm: String,

    /// cgroup ID (for container identification)
    pub cgroup_id: u64,
}

impl SyscallEvent {
    /// Get the syscall name from the number
    pub fn syscall_name(&self) -> &'static str {
        syscall_name(self.syscall_nr)
    }

    /// Check if this is a potentially dangerous syscall
    pub fn is_sensitive(&self) -> bool {
        matches!(
            self.syscall_nr,
            // Process/execution
            59 | 322 |  // execve, execveat
            // Filesystem (dangerous paths)
            2 | 257 |   // open, openat
            161 | 163 | // chroot, pivot_root
            // Namespace manipulation
            272 |       // unshare
            308 |       // setns
            // Kernel module
            175 | 176 | 313 |  // init_module, delete_module, finit_module
            // System
            169 | 170 | 171    // reboot, sethostname, setdomainname
        )
    }
}



/// Network event captured by eBPF probes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkEvent {
    /// Process ID
    pub pid: u32,

    /// Source IP (if applicable)
    pub src_ip: Option<[u8; 16]>,

    /// Destination IP (if applicable)
    pub dst_ip: Option<[u8; 16]>,

    /// Source port
    pub src_port: u16,

    /// Destination port
    pub dst_port: u16,

    /// Protocol (TCP=6, UDP=17)
    pub protocol: u8,

    /// Bytes sent/received
    pub bytes: u64,

    /// Direction (true = outbound)
    pub outbound: bool,

    /// Timestamp
    pub timestamp_ns: u64,
}

/// File access event
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileEvent {
    /// Process ID
    pub pid: u32,

    /// File path (up to 256 chars)
    pub path: String,

    /// Access flags
    pub flags: u32,

    /// File mode
    pub mode: u32,

    /// Operation type
    pub operation: FileOperation,

    /// Timestamp
    pub timestamp_ns: u64,
}

/// File operation type
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum FileOperation {
    Open,
    Read,
    Write,
    Close,
    Unlink,
    Rename,
    Mkdir,
    Rmdir,
    Chmod,
    Chown,
}

/// Type of event
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EventType {
    Syscall(SyscallEvent),
    Network(NetworkEvent),
    File(FileEvent),
}

/// A container event (wrapper with metadata)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContainerEvent {
    /// Container ID
    pub container_id: String,

    /// Event timestamp
    pub timestamp: SystemTime,

    /// Event type and data
    pub event_type: EventType,
}

impl ContainerEvent {
    /// Create a new syscall event
    pub fn syscall(container_id: String, event: SyscallEvent) -> Self {
        Self {
            container_id,
            timestamp: SystemTime::now(),
            event_type: EventType::Syscall(event),
        }
    }

    /// Create a new network event
    pub fn network(container_id: String, event: NetworkEvent) -> Self {
        Self {
            container_id,
            timestamp: SystemTime::now(),
            event_type: EventType::Network(event),
        }
    }

    /// Create a new file event
    pub fn file(container_id: String, event: FileEvent) -> Self {
        Self {
            container_id,
            timestamp: SystemTime::now(),
            event_type: EventType::File(event),
        }
    }
}

/// Returns the name of a syscall given its number.
/// This is a minimal implementation and can be expanded as needed.
fn syscall_name(syscall_nr: i64) -> &'static str {
    match syscall_nr {
        1 => "write",
        2 => "open",
        3 => "close",
        4 => "stat",
        5 => "fstat",
        8 => "lseek",
        9 => "mmap",
        10 => "mprotect",
        11 => "munmap",
        12 => "brk",
        16 => "ioctl",
        21 => "access",
        39 => "getpid",
        41 => "socket",
        42 => "connect",
        43 => "accept",
        44 => "sendto",
        45 => "recvfrom",
        46 => "sendmsg",
        47 => "recvmsg",
        48 => "shutdown",
        49 => "bind",
        50 => "listen",
        54 => "setsockopt",
        55 => "execve",
        56 => "exit",
        57 => "wait4",
        59 => "execve",
        62 => "kill",
        63 => "rename",
        79 => "getcwd",
        80 => "chdir",
        90 => "mkdir",
        91 => "rmdir",
        96 => "gettimeofday",
        133 => "mknod",
        135 => "fchmod",
        136 => "fchown",
        158 => "setuid",
        159 => "setgid",
        160 => "setreuid",
        161 => "setregid",
        162 => "getresuid",
        163 => "getresgid",
        165 => "capget",
        166 => "capset",
        169 => "reboot",
        170 => "sethostname",
        171 => "setdomainname",
        172 => "iopl",
        173 => "ioperm",
        174 => "create_module",
        175 => "init_module",
        176 => "delete_module",
        217 => "getdents64",
        257 => "openat",
        272 => "unshare",
        308 => "setns",
        322 => "execveat",
        332 => "finit_module",
        _ => "unknown",
    }
}