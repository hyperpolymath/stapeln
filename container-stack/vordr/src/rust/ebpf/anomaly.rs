// SPDX-License-Identifier: PMPL-1.0-or-later
//! Statistical anomaly detection for container behavior
//!
//! This module provides baseline-based anomaly detection using
//! statistical methods to identify unusual container behavior.

use crate::ebpf::events::{ContainerEvent, EventType};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::{Duration, Instant};

/// Anomaly severity level
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AnomalyLevel {
    Critical,
    High,
    Medium,
    Low,
}

/// Represents a detected anomaly
#[derive(Debug, Clone, Serialize)]
pub struct AnomalyReport {
    pub level: AnomalyLevel,
    pub container_id: String,
    pub description: String,
    pub details: HashMap<String, String>,
}

impl AnomalyReport {
    pub fn new(level: AnomalyLevel, container_id: String, description: String) -> Self {
        Self {
            level,
            container_id,
            description,
            details: HashMap::new(),
        }
    }

    pub fn with_detail(mut self, key: &str, value: &str) -> Self {
        self.details.insert(key.to_string(), value.to_string());
        self
    }
}

/// Anomaly detection engine
pub struct AnomalyDetector {
    sensitivity: f64,
    learning_period: Option<Duration>,
    start_time: Instant,
    // Add fields to store baselines or statistical models here
    // For now, a simple blacklist for demonstration
    blacklist: HashMap<i64, AnomalyLevel>,
}

impl AnomalyDetector {
    pub fn new(sensitivity: f64) -> Self {
        Self {
            sensitivity,
            learning_period: None,
            start_time: Instant::now(),
            blacklist: {
                let mut map = HashMap::new();
                // Example blacklisted syscall: init_module
                map.insert(175, AnomalyLevel::Critical);
                map
            },
        }
    }

    pub fn with_learning_period(mut self, period: Duration) -> Self {
        self.learning_period = Some(period);
        self
    }

    pub fn check_event(&self, event: &ContainerEvent) -> Option<AnomalyReport> {
        // If still in learning period, don't report anomalies unless blacklisted
        if let Some(period) = self.learning_period {
            if self.start_time.elapsed() < period {
                if let EventType::Syscall(ref syscall_event) = event.event_type {
                    if let Some(level) = self.blacklist.get(&syscall_event.syscall_nr) {
                        return Some(AnomalyReport::new(
                            *level,
                            event.container_id.clone(),
                            format!("Blacklisted syscall detected: {}", syscall_event.syscall_nr),
                        )
                        .with_detail("pid", &syscall_event.pid.to_string())
                        .with_detail("comm", &syscall_event.comm)
                        .with_detail("syscall_nr", &syscall_event.syscall_nr.to_string()));
                    }
                }
                return None; // Still learning, no anomaly unless blacklisted
            }
        }

        match event.event_type {
            EventType::Syscall(ref syscall_event) => {
                if let Some(level) = self.blacklist.get(&syscall_event.syscall_nr) {
                    Some(AnomalyReport::new(
                        *level,
                        event.container_id.clone(),
                        format!("Blacklisted syscall detected: {}", syscall_event.syscall_nr),
                    )
                    .with_detail("pid", &syscall_event.pid.to_string())
                    .with_detail("comm", &syscall_event.comm)
                    .with_detail("syscall_nr", &syscall_event.syscall_nr.to_string()))
                } else if syscall_event.is_sensitive() && self.sensitivity > 0.5 {
                    // Example: sensitive syscalls are anomalies with high sensitivity
                    Some(AnomalyReport::new(
                        AnomalyLevel::High,
                        event.container_id.clone(),
                        format!(
                            "Potentially sensitive syscall detected: {}",
                            syscall_event.syscall_nr
                        ),
                    )
                    .with_detail("pid", &syscall_event.pid.to_string())
                    .with_detail("comm", &syscall_event.comm)
                    .with_detail("syscall_nr", &syscall_event.syscall_nr.to_string()))
                } else {
                    None
                }
            }
            _ => None, // Only syscall anomalies for now
        }
    }
}


// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn test_anomaly_report() {
//         let report = AnomalyReport::new(
//             AnomalyLevel::High,
//             "container-123".to_string(),
//             "Test anomaly".to_string(),
//         )
//         .with_detail("key", "value");

//         assert_eq!(report.level, AnomalyLevel::High);
//         assert_eq!(report.details.get("key"), Some(&"value".to_string()));
//     }

//     #[test]
//     fn test_detector_blacklist() {
//         let detector = AnomalyDetector::new(0.5);

//         let event = ContainerEvent::syscall(
//             "test".to_string(),
//             SyscallEvent {
//                 pid: 1,
//                 tid: 1,
//                 uid: 0,
//                 gid: 0,
//                 syscall_nr: 175, // init_module (blacklisted)
//                 args: [0; 6],
//                 ret: None,
//                 timestamp_ns: 0,
//                 comm: "test".to_string(),
//                 cgroup_id: 0,
//             },
//         );

//         let report = detector.check_event(&event);
//         assert!(report.is_some());
//         assert_eq!(report.unwrap().level, AnomalyLevel::Critical);
//     }

//     #[test]
//     fn test_learning_period() {
//         let detector = AnomalyDetector::new(0.5).with_learning_period(Duration::from_millis(100));

//         // During learning, nothing should be flagged (except blacklist)
//         let event = ContainerEvent::syscall(
//             "test".to_string(),
//             SyscallEvent {
//                 pid: 1,
//                 tid: 1,
//                 uid: 0,
//                 gid: 0,
//                 syscall_nr: 59, // execve
//                 args: [0; 6],
//                 ret: None,
//                 timestamp_ns: 0,
//                 comm: "test".to_string(),
//                 cgroup_id: 0,
//             },
//         );

//         let report = detector.check_event(&event);
//         assert!(report.is_none()); // Still learning
//     }
// }