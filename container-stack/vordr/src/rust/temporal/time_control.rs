// SPDX-License-Identifier: PMPL-1.0-or-later

//! Time manipulation stack for temporal isolation
//!
//! Provides layered time control to make the target code perceive
//! time differently from reality. Each layer covers different
//! time sources that code might check.
//!
//! # Layers
//!
//! | Layer          | Mechanism                 | What it fakes                    |
//! |----------------|---------------------------|----------------------------------|
//! | NTP            | Fake NTP server           | ntpdate, chronyd queries         |
//! | PTP            | IEEE 1588 fake grandmaster| Hardware clock sync, /dev/ptp*   |
//! | libfaketime    | LD_PRELOAD                | gettimeofday(), clock_gettime()  |
//! | CLOCK_MONOTONIC| Via libfaketime           | Uptime-relative timing           |
//! | RDTSC          | Firecracker microVM       | CPU cycle counter (hardware)     |
//!
//! # Detection resistance
//!
//! - Standard tier: libfaketime is detectable via /proc/self/maps
//! - Temporal tier: eBPF probes are invisible + PTP manipulation is
//!   indistinguishable from normal clock sync jitter
//! - Tomograph tier: each instance uses a different layer combination

use serde::{Deserialize, Serialize};

use super::TemporalConfig;

/// A time manipulation layer
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TimeLayer {
    /// Fake NTP server responding to time queries
    Ntp,

    /// IEEE 1588 Precision Time Protocol fake grandmaster clock
    Ptp,

    /// LD_PRELOAD intercepting libc time functions
    Libfaketime,

    /// CLOCK_MONOTONIC manipulation (via libfaketime)
    ClockMonotonic,

    /// RDTSC virtualisation (requires Firecracker microVM)
    Rdtsc,

    /// Full VM with virtualised TSC (covers everything)
    FullVm,
}

impl TimeLayer {
    /// Whether this layer requires a virtual machine
    pub fn requires_vm(&self) -> bool {
        matches!(self, Self::Rdtsc | Self::FullVm)
    }

    /// Whether this layer is detectable by userspace
    pub fn is_detectable(&self) -> bool {
        matches!(self, Self::Libfaketime | Self::ClockMonotonic)
    }
}

/// Configuration for a single time layer
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimeLayerConfig {
    /// The layer type
    pub layer: TimeLayer,

    /// Whether this layer is active
    pub enabled: bool,

    /// Time offset from real time (seconds, can be negative)
    pub offset_seconds: i64,

    /// Dilation ratio (1.0 = real speed, 0.0001 = 10,000x slower)
    pub dilation: f64,
}

/// Manages the time manipulation stack
///
/// Coordinates multiple time layers to present a consistent
/// fake time to the target process. The key insight is that
/// all layers must agree — if NTP says 2038-01-19 but
/// CLOCK_MONOTONIC says uptime is 30 seconds, sophisticated
/// code will detect the inconsistency.
pub struct TimeController {
    /// Active time layers
    layers: Vec<TimeLayerConfig>,

    /// Base real time when controller started
    base_real_time: std::time::SystemTime,

    /// Current simulated time offset from base
    offset_seconds: i64,

    /// Current dilation ratio
    dilation_ratio: f64,
}

impl TimeController {
    /// Create a new time controller from temporal config
    pub fn from_config(config: &TemporalConfig) -> Self {
        let mut layers = Vec::new();

        // Always include libfaketime for Standard tier
        layers.push(TimeLayerConfig {
            layer: TimeLayer::Libfaketime,
            enabled: true,
            offset_seconds: 0,
            dilation: 1.0 / config.time_dilation_ratio,
        });

        // Add NTP and PTP for Temporal and Tomograph tiers
        if config.tier != super::IsolationTier::Standard {
            layers.push(TimeLayerConfig {
                layer: TimeLayer::Ntp,
                enabled: true,
                offset_seconds: 0,
                dilation: 1.0 / config.time_dilation_ratio,
            });

            layers.push(TimeLayerConfig {
                layer: TimeLayer::Ptp,
                enabled: true,
                offset_seconds: 0,
                dilation: 1.0 / config.time_dilation_ratio,
            });

            layers.push(TimeLayerConfig {
                layer: TimeLayer::ClockMonotonic,
                enabled: true,
                offset_seconds: 0,
                dilation: 1.0 / config.time_dilation_ratio,
            });
        }

        // Add RDTSC virtualisation if Firecracker is enabled
        if config.use_firecracker {
            layers.push(TimeLayerConfig {
                layer: TimeLayer::Rdtsc,
                enabled: true,
                offset_seconds: 0,
                dilation: 1.0 / config.time_dilation_ratio,
            });
        }

        Self {
            layers,
            base_real_time: std::time::SystemTime::now(),
            offset_seconds: 0,
            dilation_ratio: config.time_dilation_ratio,
        }
    }

    /// Set the simulated time to a specific point
    ///
    /// Updates all active layers to present a consistent time.
    pub fn set_time_offset(&mut self, offset_seconds: i64) {
        self.offset_seconds = offset_seconds;
        for layer in &mut self.layers {
            if layer.enabled {
                layer.offset_seconds = offset_seconds;
            }
        }
    }

    /// Advance the simulated time by a step
    pub fn advance(&mut self, step_seconds: i64) {
        self.set_time_offset(self.offset_seconds + step_seconds);
    }

    /// Reverse the simulated time by a step
    pub fn reverse(&mut self, step_seconds: i64) {
        self.set_time_offset(self.offset_seconds - step_seconds);
    }

    /// Get the current simulated time offset
    pub fn current_offset(&self) -> i64 {
        self.offset_seconds
    }

    /// Get the dilation ratio
    pub fn dilation_ratio(&self) -> f64 {
        self.dilation_ratio
    }

    /// Get active layers
    pub fn active_layers(&self) -> Vec<&TimeLayerConfig> {
        self.layers.iter().filter(|l| l.enabled).collect()
    }

    /// Check if any detectable layers are active
    ///
    /// Returns true if the time manipulation could be detected
    /// by the target process (e.g. libfaketime via /proc/self/maps).
    pub fn has_detectable_layers(&self) -> bool {
        self.layers
            .iter()
            .any(|l| l.enabled && l.layer.is_detectable())
    }

    /// Check if VM-dependent layers are active without a VM
    pub fn requires_vm(&self) -> bool {
        self.layers
            .iter()
            .any(|l| l.enabled && l.layer.requires_vm())
    }

    /// Generate libfaketime environment variables
    ///
    /// Returns the env vars needed to inject time manipulation
    /// via LD_PRELOAD into the target process.
    pub fn libfaketime_env(&self) -> Vec<(String, String)> {
        let libfaketime_layer = self.layers.iter().find(|l| {
            l.enabled && l.layer == TimeLayer::Libfaketime
        });

        match libfaketime_layer {
            Some(layer) => {
                let mut env = Vec::new();

                // FAKETIME format: offset from real time
                // "+15d" = 15 days ahead, "-30d" = 30 days behind
                // Or absolute: "2038-01-19 03:14:07"
                if layer.offset_seconds >= 0 {
                    env.push((
                        "FAKETIME".to_string(),
                        format!("+{}s", layer.offset_seconds),
                    ));
                } else {
                    env.push((
                        "FAKETIME".to_string(),
                        format!("{}s", layer.offset_seconds),
                    ));
                }

                // FAKETIME_NO_CACHE=1 to prevent caching issues
                env.push(("FAKETIME_NO_CACHE".to_string(), "1".to_string()));

                // Dilation: FAKETIME_TIMESTAMP_FILE or speed factor
                if (layer.dilation - 1.0).abs() > f64::EPSILON {
                    env.push((
                        "FAKETIME_SPEED_FACTOR".to_string(),
                        format!("{}", layer.dilation),
                    ));
                }

                env
            }
            None => Vec::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_standard_tier_layers() {
        let config = TemporalConfig::default();
        let controller = TimeController::from_config(&config);

        // Standard tier should only have libfaketime
        let active = controller.active_layers();
        assert_eq!(active.len(), 1);
        assert_eq!(active[0].layer, TimeLayer::Libfaketime);
    }

    #[test]
    fn test_temporal_tier_layers() {
        let config = TemporalConfig {
            tier: super::super::IsolationTier::Temporal,
            ..Default::default()
        };
        let controller = TimeController::from_config(&config);

        // Temporal tier: libfaketime + NTP + PTP + ClockMonotonic
        let active = controller.active_layers();
        assert_eq!(active.len(), 4);
    }

    #[test]
    fn test_time_advance_reverse() {
        let config = TemporalConfig::default();
        let mut controller = TimeController::from_config(&config);

        controller.advance(86400); // 1 day forward
        assert_eq!(controller.current_offset(), 86400);

        controller.reverse(43200); // 12 hours back
        assert_eq!(controller.current_offset(), 43200);
    }

    #[test]
    fn test_libfaketime_env() {
        let config = TemporalConfig::default();
        let mut controller = TimeController::from_config(&config);
        controller.set_time_offset(86400 * 365); // 1 year ahead

        let env = controller.libfaketime_env();
        assert!(!env.is_empty());
        assert!(env.iter().any(|(k, _)| k == "FAKETIME"));
    }

    #[test]
    fn test_detectability() {
        let standard_config = TemporalConfig::default();
        let controller = TimeController::from_config(&standard_config);
        // Standard tier uses libfaketime which is detectable
        assert!(controller.has_detectable_layers());
    }

    #[test]
    fn test_layer_requires_vm() {
        assert!(TimeLayer::Rdtsc.requires_vm());
        assert!(TimeLayer::FullVm.requires_vm());
        assert!(!TimeLayer::Ntp.requires_vm());
        assert!(!TimeLayer::Ptp.requires_vm());
        assert!(!TimeLayer::Libfaketime.requires_vm());
    }
}
