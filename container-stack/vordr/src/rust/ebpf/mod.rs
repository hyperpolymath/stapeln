// SPDX-License-Identifier: PMPL-1.0-or-later
//! eBPF-based runtime monitoring for container security
//!
//! This module provides syscall-level monitoring using eBPF probes.
//! It integrates with the Aya eBPF framework for pure-Rust eBPF programs.
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────┐
//! │                    Container Process                         │
//! │  ┌──────────┐  ┌──────────┐  ┌──────────┐                  │
//! │  │ syscall  │  │ syscall  │  │ syscall  │                  │
//! │  └────┬─────┘  └────┬─────┘  └────┬─────┘                  │
//! └───────┼─────────────┼─────────────┼─────────────────────────┘
//!         │             │             │
//!         ▼             ▼             ▼
//! ┌─────────────────────────────────────────────────────────────┐
//! │                    eBPF Tracepoints                          │
//! │  ┌──────────────────────────────────────────────────────┐   │
//! │  │ sys_enter_* / sys_exit_* / raw_tracepoints           │   │
//! │  └──────────────────────────────────────────────────────┘   │
//! └────────────────────────┬────────────────────────────────────┘
//!                          │
//!                          ▼
//! ┌─────────────────────────────────────────────────────────────┐
//! │                    Ring Buffer                               │
//! │  ┌──────────────────────────────────────────────────────┐   │
//! │  │ SyscallEvent { pid, syscall_nr, args, timestamp }    │   │
//! │  └──────────────────────────────────────────────────────┘   │
//! └────────────────────────┬────────────────────────────────────┘
//!                          │
//!                          ▼
//! ┌─────────────────────────────────────────────────────────────┐
//! │                    Userspace Monitor                         │
//! │  ┌────────────┐  ┌────────────┐  ┌────────────────────┐    │
//! │  │ Event Loop │→ │ Anomaly    │→ │ Alert Dispatch     │    │
//! │  │            │  │ Detector   │  │ (webhook/log)      │    │
//! │  └────────────┘  └────────────┘  └────────────────────┘    │
//! └─────────────────────────────────────────────────────────────┘
//! ```

pub mod anomaly;
pub mod events;
pub mod probes;
pub mod syscall;

use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;

use anyhow::{Context, Result};
use tokio::sync::mpsc;
use tracing::{debug, error, info, warn};

pub use anomaly::{AnomalyDetector, AnomalyReport};
pub use events::{ContainerEvent, EventType};
pub use syscall::SyscallPolicy;

/// Configuration for the eBPF monitor
#[derive(Debug, Clone)]
pub struct MonitorConfig {
    /// Container IDs to monitor (empty = all containers)
    pub container_ids: Vec<String>,

    /// Syscalls to specifically track
    pub tracked_syscalls: Vec<i64>,

    /// Enable anomaly detection
    pub anomaly_detection: bool,

    /// Anomaly detection sensitivity (0.0 - 1.0)
    pub anomaly_sensitivity: f64,

    /// Webhook URL for alerts
    pub webhook_url: Option<String>,

    /// Buffer size for event ring buffer
    pub buffer_size: usize,

    /// Sampling rate for high-frequency syscalls (1 = all, 10 = 1/10)
    pub sampling_rate: u32,
    pub learning_period: Option<Duration>,
}

impl Default for MonitorConfig {
    fn default() -> Self {
        Self {
            container_ids: Vec::new(),
            tracked_syscalls: Vec::new(),
            anomaly_detection: true,
            anomaly_sensitivity: 0.8,
            webhook_url: None,
            buffer_size: 16384,
            sampling_rate: 1,
            learning_period: None,
        }
    }
}

/// Status of the eBPF monitor
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MonitorStatus {
    /// Monitor is stopped
    Stopped,
    /// Monitor is initializing (loading eBPF programs)
    Initializing,
    /// Monitor is running
    Running,
    /// Monitor encountered an error
    Error,
}

/// Statistics from the eBPF monitor
#[derive(Debug, Clone, Default)]
pub struct MonitorStats {
    /// Total events received
    pub events_received: u64,
    /// Events dropped due to buffer overflow
    pub events_dropped: u64,
    /// Anomalies detected
    pub anomalies_detected: u64,
    /// Alerts sent
    pub alerts_sent: u64,
    /// Events per syscall number
    pub syscall_counts: HashMap<i64, u64>,
    /// Events per container
    pub container_counts: HashMap<String, u64>,
}

/// eBPF-based runtime monitor
///
/// This is the main entry point for container runtime monitoring.
/// It coordinates eBPF program loading, event collection, and anomaly detection.
pub struct Monitor {
    config: MonitorConfig,
    status: MonitorStatus,
    stats: MonitorStats,
    event_tx: Option<mpsc::Sender<ContainerEvent>>,
    event_rx: Option<mpsc::Receiver<ContainerEvent>>,
    anomaly_detector: Option<Arc<AnomalyDetector>>,
}

impl Monitor {
    /// Create a new monitor with the given configuration
    pub fn new(config: MonitorConfig) -> Self {
        let (tx, rx) = mpsc::channel(config.buffer_size);

        let anomaly_detector = if config.anomaly_detection {
            let mut detector = AnomalyDetector::new(config.anomaly_sensitivity);
            if let Some(period) = config.learning_period {
                detector = detector.with_learning_period(period);
            }
            Some(Arc::new(detector))
        } else {
            None
        };

        Self {
            config,
            status: MonitorStatus::Stopped,
            stats: MonitorStats::default(),
            event_tx: Some(tx),
            event_rx: Some(rx),
            anomaly_detector,
        }
    }

    /// Check if the system supports eBPF
    pub fn check_ebpf_support() -> Result<bool> {
        // Check for BPF syscall support
        #[cfg(target_os = "linux")]
        {
            use std::fs;

            // Check kernel version (need 4.15+ for most eBPF features)
            let version = fs::read_to_string("/proc/version").context("Failed to read kernel version")?;
            debug!("Kernel: {}", version.trim());

            // Check for BPF filesystem
            let bpf_fs = std::path::Path::new("/sys/fs/bpf");
            if !bpf_fs.exists() {
                warn!("BPF filesystem not mounted at /sys/fs/bpf");
                return Ok(false);
            }

            // Check for required capabilities
            // In production, we'd check CAP_BPF and CAP_PERFMON
            Ok(true)
        }

        #[cfg(not(target_os = "linux"))]
        {
            warn!("eBPF is only supported on Linux");
            Ok(false)
        }
    }

    /// Start the monitor
    ///
    /// This loads eBPF programs and begins collecting events.
    pub async fn start(&mut self) -> Result<()> {
        info!("Starting eBPF monitor");
        self.status = MonitorStatus::Initializing;

        // Verify eBPF support
        if !Self::check_ebpf_support()? {
            self.status = MonitorStatus::Error;
            anyhow::bail!("eBPF not supported on this system");
        }

        // Load eBPF programs (stub - actual implementation requires aya-bpf)
        self.load_ebpf_programs().await?;

        self.status = MonitorStatus::Running;
        info!("eBPF monitor started successfully");

        Ok(())
    }

    /// Stop the monitor
    pub async fn stop(&mut self) -> Result<()> {
        info!("Stopping eBPF monitor");
        self.status = MonitorStatus::Stopped;
        Ok(())
    }

    /// Get current monitor status
    pub fn status(&self) -> MonitorStatus {
        self.status
    }

    /// Get current statistics
    pub fn stats(&self) -> &MonitorStats {
        &self.stats
    }

    /// Load eBPF programs into the kernel
    async fn load_ebpf_programs(&mut self) -> Result<()> {
        info!("Loading eBPF programs...");

        // In the full implementation, this would:
        // 1. Load compiled eBPF bytecode (from aya-bpf)
        // 2. Attach to tracepoints/kprobes
        // 3. Set up ring buffer for event communication

        // Stub: Just log what we would do
        debug!(
            "Would attach to syscall tracepoints for containers: {:?}",
            if self.config.container_ids.is_empty() {
                vec!["all".to_string()]
            } else {
                self.config.container_ids.clone()
            }
        );

        if !self.config.tracked_syscalls.is_empty() {
            debug!("Tracking specific syscalls: {:?}", self.config.tracked_syscalls);
        }

        Ok(())
    }

    /// Process incoming events from the ring buffer
    pub async fn process_events(&mut self) -> Result<()> {
        // Collect pending anomaly reports to handle after the borrow ends
        let mut pending_anomalies: Vec<AnomalyReport> = Vec::new();

        {
            let Some(ref mut rx) = self.event_rx else {
                anyhow::bail!("Event receiver not initialized");
            };

            let mut event_counter: u64 = 0; // Counter for sampling

            while let Some(event) = rx.recv().await {
                event_counter += 1;

                // Apply sampling rate if greater than 1
                if self.config.sampling_rate > 1 && event_counter % self.config.sampling_rate as u64 != 0 {
                    self.stats.events_dropped += 1;
                    continue; // Skip processing this event due to sampling
                }

                self.stats.events_received += 1;

                // Update per-syscall stats
                if let EventType::Syscall(ref syscall) = event.event_type {
                    *self
                        .stats
                        .syscall_counts
                        .entry(syscall.syscall_nr)
                        .or_insert(0) += 1;
                }

                // Update per-container stats
                *self
                    .stats
                    .container_counts
                    .entry(event.container_id.clone())
                    .or_insert(0) += 1;

                // Run anomaly detection if enabled
                if let Some(ref detector) = self.anomaly_detector {
                    if let Some(report) = detector.as_ref().check_event(&event) {
                        self.stats.anomalies_detected += 1;
                        pending_anomalies.push(report);
                    }
                }
            }
        }

        // Handle collected anomalies after the rx borrow is released
        for report in pending_anomalies {
            self.handle_anomaly(report).await?;
        }

        Ok(())
    }

    /// Handle a detected anomaly
    async fn handle_anomaly(&mut self, report: AnomalyReport) -> Result<()> {
        warn!("Anomaly detected: {:?}", report);

        // Send webhook alert if configured
        if let Some(ref url) = self.config.webhook_url {
            match self.send_webhook_alert(url, &report).await {
                Ok(_) => {
                    self.stats.alerts_sent += 1;
                    info!("Alert sent to webhook");
                }
                Err(e) => {
                    error!("Failed to send webhook alert: {}", e);
                }
            }
        }

        Ok(())
    }

    /// Send an alert to a webhook URL
    async fn send_webhook_alert(&self, url: &str, report: &AnomalyReport) -> Result<()> {
        let client = reqwest::Client::new();

        let payload = serde_json::json!({
            "type": "vordr_anomaly",
            "timestamp": chrono::Utc::now().to_rfc3339(),
            "level": format!("{:?}", report.level),
            "container_id": report.container_id,
            "description": report.description,
            "details": report.details,
        });

        client
            .post(url)
            .json(&payload)
            .timeout(Duration::from_secs(10))
            .send()
            .await
            .context("Failed to send webhook")?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_monitor_config_default() {
        let config = MonitorConfig::default();
        assert!(config.container_ids.is_empty());
        assert!(config.anomaly_detection);
        assert_eq!(config.sampling_rate, 1);
    }

    #[test]
    fn test_monitor_creation() {
        let config = MonitorConfig::default();
        let monitor = Monitor::new(config);
        assert_eq!(monitor.status(), MonitorStatus::Stopped);
    }
}
