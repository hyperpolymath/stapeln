// SPDX-License-Identifier: PMPL-1.0-or-later
//! Aya eBPF probe definitions
//!
//! This module contains the userspace side of eBPF probe management.
//! The actual eBPF programs are in a separate aya-bpf crate.

use std::collections::HashMap;
use std::sync::Arc;

use anyhow::Result;
#[cfg(feature = "bpf")]
use aya::maps::{HashMap as AyaHashMap, MapData};
#[cfg(feature = "bpf")]
use aya::programs::{RawTracePoint, TracePoint};
#[cfg(feature = "bpf")]
use aya::{include_bytes_aligned, Bpf};
use tokio::sync::RwLock;
use tracing::{debug, info, warn};

/// Probe types supported by Vörðr
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ProbeType {
    /// sys_enter raw tracepoint
    SysEnter,
    /// sys_exit raw tracepoint
    #[allow(dead_code)]
    SysExit,
    /// sched_process_exec tracepoint
    SchedProcessExec,
    /// sched_process_exit tracepoint
    SchedProcessExit,
    /// net_dev_xmit tracepoint
    #[allow(dead_code)]
    NetDevXmit,
    /// sock_sendmsg kprobe
    #[allow(dead_code)]
    SockSendMsg,
    /// vfs_read kprobe
    #[allow(dead_code)]
    VfsRead,
    /// vfs_write kprobe
    #[allow(dead_code)]
    VfsWrite,
}

impl ProbeType {
    /// Get the BPF program name for this probe
    #[allow(dead_code)]
    pub fn program_name(&self) -> &'static str {
        match self {
            ProbeType::SysEnter => "vordr_sys_enter",
            ProbeType::SysExit => "vordr_sys_exit",
            ProbeType::SchedProcessExec => "vordr_sched_process_exec",
            ProbeType::SchedProcessExit => "vordr_sched_process_exit",
            ProbeType::NetDevXmit => "vordr_net_dev_xmit",
            ProbeType::SockSendMsg => "vordr_sock_sendmsg",
            ProbeType::VfsRead => "vordr_vfs_read",
            ProbeType::VfsWrite => "vordr_vfs_write",
        }
    }

    /// Get the tracepoint/kprobe category and name
    pub fn attach_point(&self) -> (&'static str, &'static str) {
        match self {
            ProbeType::SysEnter => ("raw_syscalls", "sys_enter"),
            ProbeType::SysExit => ("raw_syscalls", "sys_exit"),
            ProbeType::SchedProcessExec => ("sched", "sched_process_exec"),
            ProbeType::SchedProcessExit => ("sched", "sched_process_exit"),
            ProbeType::NetDevXmit => ("net", "net_dev_xmit"),
            ProbeType::SockSendMsg => ("kprobe", "sock_sendmsg"),
            ProbeType::VfsRead => ("kprobe", "vfs_read"),
            ProbeType::VfsWrite => ("kprobe", "vfs_write"),
        }
    }
}

/// Configuration for container filtering in eBPF
#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
pub struct ContainerFilter {
    /// CGroup ID to filter (0 = all)
    pub cgroup_id: u64,
    /// PID namespace to filter (0 = all)
    pub pid_ns: u64,
    /// Whether filtering is enabled
    pub enabled: u8,
    _padding: [u8; 7],
}

/// Statistics from eBPF programs
#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
pub struct ProbeStats {
    /// Events generated
    pub events_generated: u64,
    /// Events dropped (ring buffer full)
    pub events_dropped: u64,
    /// Filtered events (not matching container)
    pub events_filtered: u64,
}

/// Manager for eBPF probes
pub struct ProbeManager {
    /// Loaded BPF object
    #[cfg(feature = "bpf")]
    bpf: Option<aya::Bpf>,
    /// Active probes
    active_probes: HashMap<ProbeType, bool>,
    /// Container filter map
    #[cfg(feature = "bpf")]
    filter_map: Option<AyaHashMap<MapData, u32, ContainerFilter>>,
    /// Statistics
    stats: Arc<RwLock<ProbeStats>>,
}

impl ProbeManager {
    /// Create a new probe manager
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self {
            #[cfg(feature = "bpf")]
            bpf: None,
            active_probes: HashMap::new(),
            #[cfg(feature = "bpf")]
            filter_map: None,
            stats: Arc::new(RwLock::new(ProbeStats::default())),
        }
    }

    /// Load the eBPF programs from embedded bytecode
    #[allow(dead_code)]
    pub async fn load(&mut self) -> Result<()> {
        info!("Loading Vörðr eBPF programs");

        // In production, this would load the actual eBPF bytecode
        // For now, we check if we can create a BPF object
        #[cfg(feature = "bpf")]
        {
            // Load embedded BPF bytecode (compiled from aya-bpf)
            let bpf = Bpf::load(include_bytes_aligned!(
                concat!(env!("OUT_DIR"), "/vordr")
            ))
            .context("Failed to load BPF bytecode")?;

            self.bpf = Some(bpf);
            info!("BPF programs loaded successfully");
        }

        #[cfg(not(feature = "bpf"))]
        {
            warn!("BPF feature not enabled, running in stub mode");
        }

        Ok(())
    }

    /// Attach a specific probe
    #[allow(dead_code)]
    pub async fn attach(&mut self, probe: ProbeType) -> Result<()> {
        let (category, name) = probe.attach_point();
        debug!("Attaching probe {}/{}", category, name);

        #[cfg(feature = "bpf")]
        if let Some(ref mut bpf) = self.bpf {
            match category {
                "raw_syscalls" => {
                    let program: &mut RawTracePoint = bpf
                        .program_mut(probe.program_name())
                        .unwrap()
                        .try_into()?;
                    program.load()?;
                    program.attach(name)?;
                }
                "sched" | "net" => {
                    let program: &mut TracePoint = bpf
                        .program_mut(probe.program_name())
                        .unwrap()
                        .try_into()?;
                    program.load()?;
                    program.attach(category, name)?;
                }
                "kprobe" => {
                    // Kprobe attachment would go here
                    warn!("Kprobe attachment not yet implemented for {}", name);
                }
                _ => {
                    anyhow::bail!("Unknown probe category: {}", category);
                }
            }
        }

        self.active_probes.insert(probe, true);
        info!("Probe {}/{} attached", category, name);
        Ok(())
    }

    /// Detach a specific probe
    #[allow(dead_code)]
    pub async fn detach(&mut self, probe: ProbeType) -> Result<()> {
        let (category, name) = probe.attach_point();
        debug!("Detaching probe {}/{}", category, name);

        // Mark as inactive (actual detachment happens on drop)
        self.active_probes.insert(probe, false);
        Ok(())
    }

    /// Attach all default probes for container monitoring
    #[allow(dead_code)]
    pub async fn attach_default_probes(&mut self) -> Result<()> {
        let default_probes = [
            ProbeType::SysEnter,
            ProbeType::SchedProcessExec,
            ProbeType::SchedProcessExit,
        ];

        for probe in default_probes {
            self.attach(probe).await?;
        }

        Ok(())
    }

    /// Set container filter
    #[allow(dead_code)]
    pub async fn set_container_filter(&mut self, cgroup_id: u64, pid_ns: u64) -> Result<()> {
        let _filter = ContainerFilter {
            cgroup_id,
            pid_ns,
            enabled: 1,
            _padding: [0; 7],
        };

        #[cfg(feature = "bpf")]
        if let Some(ref mut map) = self.filter_map {
            map.insert(0, _filter, 0)?;
        }

        debug!(
            "Container filter set: cgroup_id={}, pid_ns={}",
            cgroup_id, pid_ns
        );
        Ok(())
    }

    /// Clear container filter (monitor all containers)
    #[allow(dead_code)]
    pub async fn clear_container_filter(&mut self) -> Result<()> {
        let _filter = ContainerFilter::default();

        #[cfg(feature = "bpf")]
        if let Some(ref mut map) = self.filter_map {
            map.insert(0, _filter, 0)?;
        }

        debug!("Container filter cleared");
        Ok(())
    }

    /// Get probe statistics
    #[allow(dead_code)]
    pub async fn get_stats(&self) -> ProbeStats {
        *self.stats.read().await
    }

    /// Check if a probe is active
    #[allow(dead_code)]
    pub fn is_active(&self, probe: ProbeType) -> bool {
        self.active_probes.get(&probe).copied().unwrap_or(false)
    }

    /// List all active probes
    #[allow(dead_code)]
    pub fn active_probes(&self) -> Vec<ProbeType> {
        self.active_probes
            .iter()
            .filter(|(_, active)| **active)
            .map(|(probe, _)| *probe)
            .collect()
    }
}

impl Default for ProbeManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_probe_type_names() {
        assert_eq!(ProbeType::SysEnter.program_name(), "vordr_sys_enter");
        assert_eq!(
            ProbeType::SchedProcessExec.attach_point(),
            ("sched", "sched_process_exec")
        );
    }

    #[test]
    fn test_probe_manager_creation() {
        let manager = ProbeManager::new();
        assert!(manager.active_probes().is_empty());
    }

    #[test]
    fn test_container_filter_size() {
        // Ensure the filter struct is the right size for BPF
        assert_eq!(std::mem::size_of::<ContainerFilter>(), 24);
    }
}
