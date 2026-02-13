// SPDX-License-Identifier: PMPL-1.0-or-later
//! Benchmarks for eBPF monitoring overhead

use criterion::{black_box, criterion_group, criterion_main, Criterion};

use vordr::ebpf::{AnomalyDetector, MonitorConfig, SyscallEvent, SyscallPolicy};

fn bench_event_processing(c: &mut Criterion) {
    c.bench_function("syscall_event_create", |b| {
        b.iter(|| {
            black_box(SyscallEvent {
                syscall_nr: 1,
                pid: 1234,
                uid: 1000,
                timestamp: std::time::SystemTime::now(),
            });
        })
    });
}

fn bench_anomaly_detection(c: &mut Criterion) {
    let mut detector = AnomalyDetector::new();

    // Establish baseline with 100 events
    for i in 0..100 {
        let event = SyscallEvent {
            syscall_nr: i % 20, // 20 different syscalls
            pid: 1234,
            uid: 1000,
            timestamp: std::time::SystemTime::now(),
        };
        detector.record_event(&event);
    }

    c.bench_function("anomaly_detect_normal", |b| {
        let event = SyscallEvent {
            syscall_nr: 10, // Normal syscall
            pid: 1234,
            uid: 1000,
            timestamp: std::time::SystemTime::now(),
        };
        b.iter(|| {
            black_box(detector.is_anomalous(&event));
        })
    });

    c.bench_function("anomaly_detect_suspicious", |b| {
        let event = SyscallEvent {
            syscall_nr: 999, // Rare syscall
            pid: 1234,
            uid: 1000,
            timestamp: std::time::SystemTime::now(),
        };
        b.iter(|| {
            black_box(detector.is_anomalous(&event));
        })
    });
}

fn bench_syscall_policy(c: &mut Criterion) {
    let policy = SyscallPolicy::default_secure();

    c.bench_function("policy_check_allowed", |b| {
        let event = SyscallEvent {
            syscall_nr: 0, // read() - usually allowed
            pid: 1234,
            uid: 1000,
            timestamp: std::time::SystemTime::now(),
        };
        b.iter(|| {
            black_box(policy.is_allowed(&event));
        })
    });

    c.bench_function("policy_check_denied", |b| {
        let event = SyscallEvent {
            syscall_nr: 139, // reboot() - usually denied
            pid: 1234,
            uid: 1000,
            timestamp: std::time::SystemTime::now(),
        };
        b.iter(|| {
            black_box(policy.is_allowed(&event));
        })
    });
}

fn bench_monitor_config(c: &mut Criterion) {
    c.bench_function("monitor_config_default", |b| {
        b.iter(|| {
            black_box(MonitorConfig::default());
        })
    });
}

criterion_group!(
    benches,
    bench_event_processing,
    bench_anomaly_detection,
    bench_syscall_policy,
    bench_monitor_config
);
criterion_main!(benches);
