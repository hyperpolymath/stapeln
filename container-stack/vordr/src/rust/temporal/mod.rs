// SPDX-License-Identifier: PMPL-1.0-or-later

//! Temporal Isolation Engine
//!
//! Implements ADR-007: temporal isolation as a primary security primitive.
//! Instead of spatial isolation (containers, VMs), this module provides
//! temporal isolation — the monitoring system operates on a fundamentally
//! faster timescale than the code being observed.
//!
//! # Architecture
//!
//! ```text
//! ┌──────────────────────────────────────────────────────┐
//! │  ERLANG/BEAM CONTROL PLANE  (real speed)             │
//! │  ┌────────────────────────────────────────────┐      │
//! │  │  Port Interceptor Swarm (65,535 processes) │      │
//! │  └────────────────────────────────────────────┘      │
//! │  ┌────────────────────────────────────────────┐      │
//! │  │  eBPF Event Consumer                       │      │
//! │  └──────────────┬─────────────────────────────┘      │
//! ├─────────────────┼────────────────────────────────────┤
//! │  KERNEL SPACE   │                                    │
//! │  ┌──────────────┴─────────────────────────────┐      │
//! │  │  eBPF Probes (invisible to userspace)      │      │
//! │  └──────────────┬─────────────────────────────┘      │
//! │  ┌──────────────┴─────────────────────────────┐      │
//! │  │  TARGET CODE  (dilated time, 1/10,000x)    │      │
//! │  └────────────────────────────────────────────┘      │
//! └──────────────────────────────────────────────────────┘
//! ```
//!
//! # Tiers
//!
//! - **Standard**: libfaketime + fake NTP (covers 99% of code)
//! - **Temporal**: BEAM + eBPF single-instance with time dilation
//! - **Tomograph**: Distributed CT-scan across Kubernetes pods
//!
//! # Reference
//!
//! See `docs/ADR-007-temporal-isolation.md` for the full design document.

pub mod isolation;
pub mod time_control;

use serde::{Deserialize, Serialize};

/// Temporal isolation tier
///
/// Each tier increases the depth and undetectability of observation,
/// at the cost of more compute resources.
#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum IsolationTier {
    /// Tier 1: Filesystem isolation + libfaketime (current abduct level).
    /// Copies files, locks them, shifts time via LD_PRELOAD.
    Standard,

    /// Tier 2: BEAM + eBPF, single instance, temporal dilation.
    /// Monitoring runs at real speed while target's clock is dilated.
    /// eBPF probes invisible to userspace; port interceptor swarm
    /// covers all 65,535 ports before target starts.
    Temporal,

    /// Tier 3: CT-scan tomographic mode, distributed across Kubernetes.
    /// N copies (1,000–100,000), each monitoring ONE thing.
    /// Results recombined into a complete behavioural tomograph.
    /// For high-assurance scanning (nuclear, aviation, power grid).
    Tomograph,
}

impl std::fmt::Display for IsolationTier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Standard => write!(f, "standard"),
            Self::Temporal => write!(f, "temporal"),
            Self::Tomograph => write!(f, "tomograph"),
        }
    }
}

/// Time sweep strategy for scanning through date ranges
#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SweepStrategy {
    /// Linear sweep: fixed step increments through the range
    Linear,

    /// Calendar-aware: extra density around trigger dates
    /// (New Year, epoch overflow 2038-01-19, quarter boundaries, known CVE dates)
    CalendarAware,

    /// Binary search: once a trigger window is found, narrow to exact second
    BinarySearch,

    /// Oscillating: forward-back-forward to catch hysteresis effects
    Oscillating,
}

/// Time sweep range specification
#[allow(dead_code)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimeSweepRange {
    /// Start of sweep range (ISO 8601)
    pub start: String,

    /// End of sweep range (ISO 8601)
    pub end: String,

    /// Step size in seconds (e.g. 86400 = 1 day)
    pub step_seconds: u64,

    /// Sweep strategy
    pub strategy: SweepStrategy,
}

/// Environment mirror specification
///
/// Provides target deployment characteristics so the analysis
/// environment matches what the code expects. Prevents
/// environment-fingerprinting evasion.
#[allow(dead_code)]
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct EnvironmentMirror {
    /// Contents of /etc/os-release from the target system
    pub os_release: Option<String>,

    /// Kernel version string (uname -r)
    pub kernel_version: Option<String>,

    /// Installed package list (name=version)
    pub packages: Vec<String>,

    /// Network topology description (reachable hosts/services)
    pub network_topology: Option<String>,

    /// CPU model string (/proc/cpuinfo model name)
    pub cpu_model: Option<String>,

    /// Total memory in bytes
    pub memory_bytes: Option<u64>,
}

/// Configuration for temporal isolation
#[allow(dead_code)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemporalConfig {
    /// Isolation tier
    pub tier: IsolationTier,

    /// Time dilation ratio (e.g. 10_000.0 means monitor is 10,000x faster)
    pub time_dilation_ratio: f64,

    /// Target date/time to simulate (ISO 8601)
    pub time_target: Option<String>,

    /// Time sweep range for scanning through dates
    pub sweep_range: Option<TimeSweepRange>,

    /// Environment mirror for defeating fingerprint evasion
    pub mirror_env: Option<EnvironmentMirror>,

    /// Number of port interceptors (default 65535)
    pub port_interceptor_count: u16,

    /// Number of tomographic instances (Tier 3 only)
    pub tomograph_instances: u32,

    /// Use Firecracker microVM for RDTSC virtualisation
    pub use_firecracker: bool,

    /// Kubernetes context for tomographic scanning
    pub k8s_context: Option<String>,
}

impl Default for TemporalConfig {
    fn default() -> Self {
        Self {
            tier: IsolationTier::Standard,
            time_dilation_ratio: 10_000.0,
            time_target: None,
            sweep_range: None,
            mirror_env: None,
            port_interceptor_count: 65535,
            tomograph_instances: 1000,
            use_firecracker: false,
            k8s_context: None,
        }
    }
}

/// Result of a temporal isolation scan
#[allow(dead_code)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemporalScanResult {
    /// Whether any time-triggered behaviour was detected
    pub triggered: bool,

    /// The date/time that triggered activation (if found)
    pub trigger_time: Option<String>,

    /// Description of the triggered behaviour
    pub trigger_behaviour: Option<String>,

    /// Syscalls observed during trigger
    pub trigger_syscalls: Vec<String>,

    /// Network connections attempted during trigger
    pub trigger_connections: Vec<String>,

    /// Files accessed during trigger
    pub trigger_files: Vec<String>,

    /// Total time points scanned
    pub time_points_scanned: u64,

    /// Anomalies detected across the sweep
    pub anomaly_count: u64,

    /// Tier used for scanning
    pub tier: IsolationTier,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = TemporalConfig::default();
        assert_eq!(config.tier, IsolationTier::Standard);
        assert_eq!(config.time_dilation_ratio, 10_000.0);
        assert_eq!(config.port_interceptor_count, 65535);
        assert!(!config.use_firecracker);
    }

    #[test]
    fn test_tier_display() {
        assert_eq!(IsolationTier::Standard.to_string(), "standard");
        assert_eq!(IsolationTier::Temporal.to_string(), "temporal");
        assert_eq!(IsolationTier::Tomograph.to_string(), "tomograph");
    }

    #[test]
    fn test_tomograph_config() {
        let config = TemporalConfig {
            tier: IsolationTier::Tomograph,
            tomograph_instances: 10_000,
            k8s_context: Some("prod-scanner".to_string()),
            sweep_range: Some(TimeSweepRange {
                start: "2025-01-01T00:00:00".to_string(),
                end: "2040-01-01T00:00:00".to_string(),
                step_seconds: 86400,
                strategy: SweepStrategy::CalendarAware,
            }),
            ..Default::default()
        };
        assert_eq!(config.tomograph_instances, 10_000);
        assert!(config.k8s_context.is_some());
    }
}
