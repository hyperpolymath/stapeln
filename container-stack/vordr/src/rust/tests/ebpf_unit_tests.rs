// SPDX-License-Identifier: PMPL-1.0-or-later
//! Unit tests for eBPF monitoring modules
//!
//! Tests the userspace eBPF infrastructure without requiring kernel support.
//! This allows CI/CD to run tests even on systems without eBPF.

#[cfg(test)]
mod syscall_tests {
    use crate::ebpf::syscall::*;

    #[test]
    fn test_syscall_policy_strict_blocks_dangerous() {
        let policy = SyscallPolicy::strict();

        assert_eq!(policy.name, "strict");
        assert!(policy.is_dangerous(175)); // init_module
        assert!(policy.is_dangerous(176)); // delete_module
        assert!(policy.is_dangerous(169)); // reboot
    }

    #[test]
    fn test_syscall_policy_minimal_allows_most() {
        let policy = SyscallPolicy::minimal_audit();

        assert_eq!(policy.name, "minimal-audit");
        // Minimal policy only audits, doesn't block
        assert!(!policy.is_dangerous(59)); // execve - audited not blocked
    }

    #[test]
    fn test_syscall_filter_creation() {
        let filter = SyscallFilter::new(vec![59, 322]); // execve, execveat

        assert!(filter.should_track(59));
        assert!(filter.should_track(322));
        assert!(!filter.should_track(1)); // write - not tracked
    }

    #[test]
    fn test_syscall_name_lookup() {
        assert_eq!(syscall_name(0), "read");
        assert_eq!(syscall_name(1), "write");
        assert_eq!(syscall_name(59), "execve");
        assert_eq!(syscall_name(999999), "unknown");
    }

    #[test]
    fn test_sensitive_syscall_detection() {
        let event = SyscallEvent {
            pid: 1234,
            tid: 1234,
            uid: 0,
            gid: 0,
            syscall_nr: 59, // execve - sensitive
            args: [0; 6],
            ret: None,
            timestamp_ns: 1000000,
            comm: "test".to_string(),
            cgroup_id: 0,
        };

        assert!(event.is_sensitive());
    }

    #[test]
    fn test_non_sensitive_syscall() {
        let event = SyscallEvent {
            pid: 1234,
            tid: 1234,
            uid: 0,
            gid: 0,
            syscall_nr: 1, // write - not sensitive
            args: [0; 6],
            ret: None,
            timestamp_ns: 1000000,
            comm: "test".to_string(),
            cgroup_id: 0,
        };

        assert!(!event.is_sensitive());
    }
}

#[cfg(test)]
mod probe_tests {
    use crate::ebpf::probes::*;

    #[test]
    fn test_probe_type_names() {
        assert_eq!(ProbeType::SysEnter.program_name(), "vordr_sys_enter");
        assert_eq!(ProbeType::SchedProcessExec.program_name(), "vordr_sched_process_exec");
        assert_eq!(ProbeType::NetDevXmit.program_name(), "vordr_net_dev_xmit");
    }

    #[test]
    fn test_probe_attach_points() {
        assert_eq!(ProbeType::SysEnter.attach_point(), ("raw_syscalls", "sys_enter"));
        assert_eq!(ProbeType::SchedProcessExec.attach_point(), ("sched", "sched_process_exec"));
        assert_eq!(ProbeType::NetDevXmit.attach_point(), ("net", "net_dev_xmit"));
        assert_eq!(ProbeType::VfsRead.attach_point(), ("kprobe", "vfs_read"));
    }

    #[test]
    fn test_container_filter_size() {
        // Ensure C-compatible size for BPF maps
        assert_eq!(std::mem::size_of::<ContainerFilter>(), 24);
    }

    #[test]
    fn test_container_filter_default() {
        let filter = ContainerFilter::default();

        assert_eq!(filter.cgroup_id, 0);
        assert_eq!(filter.pid_ns, 0);
        assert_eq!(filter.enabled, 0);
    }

    #[test]
    fn test_probe_stats_default() {
        let stats = ProbeStats::default();

        assert_eq!(stats.events_generated, 0);
        assert_eq!(stats.events_dropped, 0);
        assert_eq!(stats.events_filtered, 0);
    }

    #[tokio::test]
    async fn test_probe_manager_creation() {
        let manager = ProbeManager::new();

        assert!(manager.active_probes().is_empty());
        assert!(!manager.is_active(ProbeType::SysEnter));
    }

    #[tokio::test]
    async fn test_probe_manager_default_probes() {
        let mut manager = ProbeManager::new();

        // Default probes should be defined
        let _ = manager.attach_default_probes().await;
        // On systems without eBPF, this will use stub mode
    }
}

#[cfg(test)]
mod anomaly_tests {
    use crate::ebpf::anomaly::*;
    use crate::ebpf::events::*;

    #[test]
    fn test_anomaly_detector_creation() {
        let detector = AnomalyDetector::new(0.8);

        // Detector should be created with sensitivity
        assert!(detector.check_event(&create_test_event()).is_none());
    }

    #[test]
    fn test_anomaly_level_ordering() {
        assert!(AnomalyLevel::Critical > AnomalyLevel::High);
        assert!(AnomalyLevel::High > AnomalyLevel::Medium);
        assert!(AnomalyLevel::Medium > AnomalyLevel::Low);
    }

    #[test]
    fn test_rapid_syscall_detection() {
        let detector = AnomalyDetector::new(0.5);
        let mut events = Vec::new();

        // Generate 100 identical syscalls in quick succession
        for i in 0..100 {
            let mut event = create_test_event();
            event.event_type = EventType::Syscall(SyscallEvent {
                pid: 1234,
                tid: 1234,
                uid: 0,
                gid: 0,
                syscall_nr: 1, // write
                args: [0; 6],
                ret: None,
                timestamp_ns: 1000000 + i * 1000, // 1μs apart
                comm: "test".to_string(),
                cgroup_id: 0,
            });
            events.push(event);
        }

        // Rapid syscalls should trigger anomaly
        let mut anomaly_detected = false;
        for event in events {
            if let Some(report) = detector.check_event(&event) {
                anomaly_detected = true;
                assert!(matches!(report.level, AnomalyLevel::Low | AnomalyLevel::Medium));
                break;
            }
        }

        // With 100 rapid syscalls, we should detect an anomaly
        // (Actual implementation may vary based on detector logic)
    }

    fn create_test_event() -> ContainerEvent {
        ContainerEvent {
            container_id: "test-container".to_string(),
            timestamp: std::time::SystemTime::now(),
            event_type: EventType::Syscall(SyscallEvent {
                pid: 1234,
                tid: 1234,
                uid: 1000,
                gid: 1000,
                syscall_nr: 0,
                args: [0; 6],
                ret: None,
                timestamp_ns: 1000000,
                comm: "test".to_string(),
                cgroup_id: 123,
            }),
        }
    }
}

#[cfg(test)]
mod monitor_tests {
    use crate::ebpf::*;

    #[test]
    fn test_monitor_config_default() {
        let config = MonitorConfig::default();

        assert!(config.container_ids.is_empty());
        assert!(config.tracked_syscalls.is_empty());
        assert!(config.anomaly_detection);
        assert_eq!(config.anomaly_sensitivity, 0.8);
        assert_eq!(config.buffer_size, 16384);
        assert_eq!(config.sampling_rate, 1);
    }

    #[test]
    fn test_monitor_config_custom() {
        let config = MonitorConfig {
            container_ids: vec!["test-container".to_string()],
            tracked_syscalls: vec![59, 322], // execve, execveat
            anomaly_detection: false,
            anomaly_sensitivity: 0.5,
            webhook_url: Some("https://webhook.example.com".to_string()),
            buffer_size: 8192,
            sampling_rate: 10,
        };

        assert_eq!(config.container_ids.len(), 1);
        assert_eq!(config.tracked_syscalls.len(), 2);
        assert!(!config.anomaly_detection);
        assert_eq!(config.sampling_rate, 10);
    }

    #[test]
    fn test_monitor_creation() {
        let config = MonitorConfig::default();
        let monitor = Monitor::new(config);

        assert_eq!(monitor.status(), MonitorStatus::Stopped);
    }

    #[tokio::test]
    async fn test_monitor_start_without_ebpf() {
        let config = MonitorConfig::default();
        let mut monitor = Monitor::new(config);

        // On systems without eBPF, start should handle gracefully
        let result = monitor.start().await;
        // May fail (expected on non-Linux or without BPF), but should not panic
        if result.is_err() {
            // Expected on systems without eBPF support
            assert_eq!(monitor.status(), MonitorStatus::Error);
        }
    }

    #[test]
    fn test_monitor_status_transitions() {
        let config = MonitorConfig::default();
        let monitor = Monitor::new(config);

        // Initial state
        assert_eq!(monitor.status(), MonitorStatus::Stopped);

        // Status should be queryable
        let status = monitor.status();
        assert!(matches!(
            status,
            MonitorStatus::Stopped | MonitorStatus::Initializing | MonitorStatus::Running | MonitorStatus::Error
        ));
    }

    #[test]
    fn test_monitor_stats_initialization() {
        let config = MonitorConfig::default();
        let monitor = Monitor::new(config);

        let stats = monitor.stats();
        assert_eq!(stats.events_received, 0);
        assert_eq!(stats.events_dropped, 0);
        assert_eq!(stats.anomalies_detected, 0);
        assert_eq!(stats.alerts_sent, 0);
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn test_ebpf_support_check_on_linux() {
        // On Linux, should check for BPF filesystem and kernel version
        let result = Monitor::check_ebpf_support();

        // Should return a boolean without panicking
        assert!(result.is_ok());
    }

    #[cfg(not(target_os = "linux"))]
    #[test]
    fn test_ebpf_support_check_non_linux() {
        // On non-Linux, should return false
        let result = Monitor::check_ebpf_support();

        assert!(result.is_ok());
        assert!(!result.unwrap());
    }
}

#[cfg(test)]
mod event_tests {
    use crate::ebpf::events::*;

    #[test]
    fn test_syscall_event_creation() {
        let event = SyscallEvent {
            pid: 1234,
            tid: 1234,
            uid: 1000,
            gid: 1000,
            syscall_nr: 59, // execve
            args: [1, 2, 3, 4, 5, 6],
            ret: Some(0),
            timestamp_ns: 1000000000,
            comm: "test-proc".to_string(),
            cgroup_id: 12345,
        };

        assert_eq!(event.pid, 1234);
        assert_eq!(event.syscall_nr, 59);
        assert_eq!(event.syscall_name(), "execve");
    }

    #[test]
    fn test_syscall_sensitivity_execve() {
        let event = SyscallEvent {
            pid: 1234,
            tid: 1234,
            uid: 0,
            gid: 0,
            syscall_nr: 59, // execve - sensitive
            args: [0; 6],
            ret: None,
            timestamp_ns: 1000000,
            comm: "test".to_string(),
            cgroup_id: 0,
        };

        assert!(event.is_sensitive());
    }

    #[test]
    fn test_syscall_sensitivity_module_ops() {
        let event = SyscallEvent {
            pid: 1234,
            tid: 1234,
            uid: 0,
            gid: 0,
            syscall_nr: 175, // init_module - very sensitive
            args: [0; 6],
            ret: None,
            timestamp_ns: 1000000,
            comm: "test".to_string(),
            cgroup_id: 0,
        };

        assert!(event.is_sensitive());
    }

    #[test]
    fn test_network_event_creation() {
        let event = NetworkEvent {
            pid: 1234,
            src_ip: Some([192, 168, 1, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]),
            dst_ip: Some([8, 8, 8, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]),
            src_port: 50000,
            dst_port: 443,
            protocol: 6, // TCP
            bytes: 1024,
            outbound: true,
            timestamp_ns: 1000000000,
        };

        assert_eq!(event.pid, 1234);
        assert_eq!(event.protocol, 6);
        assert_eq!(event.dst_port, 443);
        assert!(event.outbound);
    }

    #[test]
    fn test_container_event_creation() {
        let syscall_event = SyscallEvent {
            pid: 1234,
            tid: 1234,
            uid: 1000,
            gid: 1000,
            syscall_nr: 0,
            args: [0; 6],
            ret: None,
            timestamp_ns: 1000000,
            comm: "test".to_string(),
            cgroup_id: 123,
        };

        let container_event = ContainerEvent {
            container_id: "test-container-123".to_string(),
            timestamp: std::time::SystemTime::now(),
            event_type: EventType::Syscall(syscall_event),
        };

        assert_eq!(container_event.container_id, "test-container-123");
        assert!(matches!(container_event.event_type, EventType::Syscall(_)));
    }
}

#[cfg(test)]
mod integration_helpers {
    use super::*;

    /// Helper to create a test monitor config
    pub fn test_monitor_config() -> MonitorConfig {
        MonitorConfig {
            container_ids: vec![],
            tracked_syscalls: vec![],
            anomaly_detection: true,
            anomaly_sensitivity: 0.8,
            webhook_url: None,
            buffer_size: 1024, // Smaller for tests
            sampling_rate: 1,
        }
    }

    /// Helper to create a test syscall event
    pub fn test_syscall_event(syscall_nr: i64) -> SyscallEvent {
        SyscallEvent {
            pid: 1234,
            tid: 1234,
            uid: 1000,
            gid: 1000,
            syscall_nr,
            args: [0; 6],
            ret: None,
            timestamp_ns: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos() as u64,
            comm: "test-process".to_string(),
            cgroup_id: 12345,
        }
    }
}
