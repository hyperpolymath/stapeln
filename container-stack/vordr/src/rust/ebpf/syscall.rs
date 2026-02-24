// SPDX-License-Identifier: PMPL-1.0-or-later
//! Syscall filtering and policy enforcement
//!
//! This module provides policy-based filtering for syscalls,
//! allowing fine-grained control over which syscalls trigger events
//! or are blocked entirely.

use serde::{Deserialize, Serialize};
use std::collections::HashSet;

/// Action to take when a syscall matches a filter
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum FilterAction {
    /// Allow the syscall (default)
    Allow,
    /// Log the syscall but allow it
    Log,
    /// Emit an event for anomaly detection
    Audit,
    /// Block the syscall with EPERM
    Block,
    /// Kill the process
    Kill,
}

/// A syscall filter rule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SyscallFilter {
    /// Syscall numbers this filter applies to
    pub syscalls: HashSet<i64>,

    /// Action to take
    pub action: FilterAction,

    /// Only apply to these UIDs (empty = all)
    pub uids: HashSet<u32>,

    /// Only apply to these GIDs (empty = all)
    pub gids: HashSet<u32>,

    /// Only apply to commands matching this pattern
    pub comm_pattern: Option<String>,

    /// Description of this filter
    pub description: String,
}

impl SyscallFilter {
    /// Create a new filter for specific syscalls
    pub fn new(syscalls: impl IntoIterator<Item = i64>, action: FilterAction) -> Self {
        Self {
            syscalls: syscalls.into_iter().collect(),
            action,
            uids: HashSet::new(),
            gids: HashSet::new(),
            comm_pattern: None,
            description: String::new(),
        }
    }

    /// Add a UID restriction
    #[allow(dead_code)]
    pub fn with_uid(mut self, uid: u32) -> Self {
        self.uids.insert(uid);
        self
    }

    /// Add a GID restriction
    #[allow(dead_code)]
    pub fn with_gid(mut self, gid: u32) -> Self {
        self.gids.insert(gid);
        self
    }

    /// Add a command pattern restriction
    #[allow(dead_code)]
    pub fn with_comm(mut self, pattern: String) -> Self {
        self.comm_pattern = Some(pattern);
        self
    }

    /// Add a description
    pub fn with_description(mut self, desc: impl Into<String>) -> Self {
        self.description = desc.into();
        self
    }

    /// Check if this filter matches an event
    #[allow(dead_code)]
    pub fn matches(&self, syscall_nr: i64, uid: u32, gid: u32, comm: &str) -> bool {
        // Must match syscall number
        if !self.syscalls.contains(&syscall_nr) {
            return false;
        }

        // Check UID restriction
        if !self.uids.is_empty() && !self.uids.contains(&uid) {
            return false;
        }

        // Check GID restriction
        if !self.gids.is_empty() && !self.gids.contains(&gid) {
            return false;
        }

        // Check command pattern
        if let Some(ref pattern) = self.comm_pattern {
            if !comm.contains(pattern) {
                return false;
            }
        }

        true
    }
}

/// A complete syscall policy
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SyscallPolicy {
    /// Policy name
    pub name: String,

    /// Policy version
    pub version: String,

    /// Default action for unmatched syscalls
    pub default_action: FilterAction,

    /// Ordered list of filter rules (first match wins)
    pub filters: Vec<SyscallFilter>,

    /// Enable process restriction (only apply to specific pids/containers)
    pub container_ids: HashSet<String>,
}

impl Default for SyscallPolicy {
    fn default() -> Self {
        Self {
            name: "default".to_string(),
            version: "1.0".to_string(),
            default_action: FilterAction::Allow,
            filters: Vec::new(),
            container_ids: HashSet::new(),
        }
    }
}

impl SyscallPolicy {
    /// Create a strict policy that blocks dangerous syscalls
    pub fn strict() -> Self {
        let mut policy = Self {
            name: "strict".to_string(),
            version: "1.0".to_string(),
            default_action: FilterAction::Allow,
            filters: Vec::new(),
            container_ids: HashSet::new(),
        };

        // Block kernel module operations
        policy.filters.push(
            SyscallFilter::new([174, 175, 176, 313], FilterAction::Block)
                .with_description("Block kernel module operations"),
        );

        // Block system modification
        policy.filters.push(
            SyscallFilter::new([169, 170, 171], FilterAction::Block)
                .with_description("Block reboot, hostname changes"),
        );

        // Audit chroot/pivot_root
        policy.filters.push(
            SyscallFilter::new([161, 163], FilterAction::Audit)
                .with_description("Audit filesystem root changes"),
        );

        // Audit namespace operations
        policy.filters.push(
            SyscallFilter::new([272, 308], FilterAction::Audit)
                .with_description("Audit namespace operations"),
        );

        // Audit execve/execveat
        policy.filters.push(
            SyscallFilter::new([59, 322], FilterAction::Audit)
                .with_description("Audit process execution"),
        );

        // Audit mount operations
        policy.filters.push(
            SyscallFilter::new([165, 166, 167, 168], FilterAction::Audit)
                .with_description("Audit mount operations"),
        );

        // Audit capability changes
        policy.filters.push(
            SyscallFilter::new([125, 126], FilterAction::Audit)
                .with_description("Audit capability changes"),
        );

        // Audit ptrace
        policy.filters.push(
            SyscallFilter::new([101], FilterAction::Audit)
                .with_description("Audit ptrace"),
        );

        policy
    }

    /// Create a minimal audit policy
    pub fn minimal_audit() -> Self {
        let mut policy = Self {
            name: "minimal-audit".to_string(),
            version: "1.0".to_string(),
            default_action: FilterAction::Allow,
            filters: Vec::new(),
            container_ids: HashSet::new(),
        };

        // Only audit execve
        policy.filters.push(
            SyscallFilter::new([59, 322], FilterAction::Audit)
                .with_description("Audit process execution"),
        );

        policy
    }

    /// Evaluate a syscall against this policy
    #[allow(dead_code)]
    pub fn evaluate(&self, syscall_nr: i64, uid: u32, gid: u32, comm: &str) -> FilterAction {
        for filter in &self.filters {
            if filter.matches(syscall_nr, uid, gid, comm) {
                return filter.action;
            }
        }
        self.default_action
    }

    /// Add a filter to this policy
    #[allow(dead_code)]
    pub fn add_filter(&mut self, filter: SyscallFilter) {
        self.filters.push(filter);
    }

    /// Restrict this policy to specific containers
    #[allow(dead_code)]
    pub fn for_containers(mut self, containers: impl IntoIterator<Item = String>) -> Self {
        self.container_ids = containers.into_iter().collect();
        self
    }
}

/// Predefined syscall groups for common use cases
#[allow(dead_code)]
pub struct SyscallGroups;

impl SyscallGroups {
    /// Process execution syscalls
    #[allow(dead_code)]
    pub fn process_exec() -> Vec<i64> {
        vec![59, 322] // execve, execveat
    }

    /// Process creation syscalls
    #[allow(dead_code)]
    pub fn process_create() -> Vec<i64> {
        vec![56, 57, 58, 435] // clone, fork, vfork, clone3
    }

    /// File operations
    #[allow(dead_code)]
    pub fn file_ops() -> Vec<i64> {
        vec![
            2, 257, 85, 86, 87, 88, 82, 83, 84, // open, openat, creat, link, unlink, symlink, rename, mkdir, rmdir
            263, 264, 265, 266, // unlinkat, renameat, linkat, symlinkat
        ]
    }

    /// Network operations
    #[allow(dead_code)]
    pub fn network_ops() -> Vec<i64> {
        vec![
            41, 42, 43, 44, 45, 46, 47, 48, 49, 50, // socket, connect, accept, sendto, recvfrom, etc.
        ]
    }

    /// Namespace operations
    #[allow(dead_code)]
    pub fn namespace_ops() -> Vec<i64> {
        vec![272, 308] // unshare, setns
    }

    /// Privilege escalation related
    #[allow(dead_code)]
    pub fn privilege_ops() -> Vec<i64> {
        vec![
            105, 106, 113, 114, 117, 119, // setuid, setgid, setreuid, setregid, setresuid, setresgid
            125, 126, // capget, capset
        ]
    }

    /// System administration
    #[allow(dead_code)]
    pub fn sysadmin_ops() -> Vec<i64> {
        vec![
            161, 163, 165, 166, // chroot, pivot_root, mount, umount
            169, 170, 171,       // reboot, sethostname, setdomainname
            174, 175, 176, 313,  // module operations
        ]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_filter_match() {
        let filter = SyscallFilter::new([59], FilterAction::Audit);

        assert!(filter.matches(59, 0, 0, "test"));
        assert!(!filter.matches(60, 0, 0, "test"));
    }

    #[test]
    fn test_filter_uid_restriction() {
        let filter = SyscallFilter::new([59], FilterAction::Audit).with_uid(1000);

        assert!(filter.matches(59, 1000, 0, "test"));
        assert!(!filter.matches(59, 1001, 0, "test"));
    }

    #[test]
    fn test_policy_evaluation() {
        let policy = SyscallPolicy::strict();

        // init_module should be blocked
        assert_eq!(policy.evaluate(175, 0, 0, "test"), FilterAction::Block);

        // execve should be audited
        assert_eq!(policy.evaluate(59, 0, 0, "test"), FilterAction::Audit);

        // read should be allowed (default)
        assert_eq!(policy.evaluate(0, 0, 0, "test"), FilterAction::Allow);
    }

    #[test]
    fn test_syscall_groups() {
        let exec = SyscallGroups::process_exec();
        assert!(exec.contains(&59)); // execve
        assert!(exec.contains(&322)); // execveat
    }
}
