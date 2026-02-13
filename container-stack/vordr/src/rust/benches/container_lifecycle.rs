// SPDX-License-Identifier: PMPL-1.0-or-later
//! Benchmarks for container lifecycle operations

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::path::PathBuf;
use tempfile::TempDir;

use vordr::engine::{ContainerLifecycle, ContainerState, StateManager};
use vordr::ffi::{NetworkMode, ValidatedConfig};

fn setup_lifecycle() -> (TempDir, ContainerLifecycle) {
    let temp_dir = TempDir::new().unwrap();
    let db_path = temp_dir.path().join("bench.db");
    let root_dir = temp_dir.path().join("root");

    let lifecycle = ContainerLifecycle::new(&db_path, &root_dir, "youki").unwrap();

    // Create test image (required for foreign key constraint)
    lifecycle
        .state
        .create_image(
            "bench-image",
            "sha256:bench123",
            Some("alpine"),
            &["latest".to_string()],
            10_000_000,
        )
        .unwrap();

    (temp_dir, lifecycle)
}

fn bench_state_manager_creation(c: &mut Criterion) {
    c.bench_function("state_manager_create", |b| {
        b.iter(|| {
            let temp_dir = TempDir::new().unwrap();
            let db_path = temp_dir.path().join("bench.db");
            black_box(StateManager::open(&db_path).unwrap());
        })
    });
}

fn bench_container_creation(c: &mut Criterion) {
    let (_temp, lifecycle) = setup_lifecycle();

    let config = ValidatedConfig {
        privileged: false,
        user_namespace: true,
        user_id: 1000,
        network_mode: NetworkMode::Unprivileged,
        capabilities: vec![],
        no_new_privileges: true,
        readonly_rootfs: false,
    };

    let mut counter = 0;
    c.bench_function("container_create", |b| {
        b.iter(|| {
            counter += 1;
            let id = format!("bench-{}", counter);
            let name = format!("bench-container-{}", counter);
            black_box(
                lifecycle
                    .create(&id, &name, "bench-image", &config, None, None)
                    .unwrap(),
            );
        })
    });
}

fn bench_container_list(c: &mut Criterion) {
    let (_temp, lifecycle) = setup_lifecycle();

    // Create 100 containers
    let config = ValidatedConfig {
        privileged: false,
        user_namespace: true,
        user_id: 1000,
        network_mode: NetworkMode::Unprivileged,
        capabilities: vec![],
        no_new_privileges: true,
        readonly_rootfs: false,
    };

    for i in 0..100 {
        lifecycle
            .create(
                &format!("bench-{}", i),
                &format!("container-{}", i),
                "bench-image",
                &config,
                None,
                None,
            )
            .unwrap();
    }

    c.bench_function("container_list_100", |b| {
        b.iter(|| {
            black_box(lifecycle.state.list_containers(None).unwrap());
        })
    });
}

fn bench_container_get(c: &mut Criterion) {
    let (_temp, lifecycle) = setup_lifecycle();

    let config = ValidatedConfig {
        privileged: false,
        user_namespace: true,
        user_id: 1000,
        network_mode: NetworkMode::Unprivileged,
        capabilities: vec![],
        no_new_privileges: true,
        readonly_rootfs: false,
    };

    lifecycle
        .create("bench-get", "bench-get-container", "bench-image", &config, None, None)
        .unwrap();

    c.bench_function("container_get_by_id", |b| {
        b.iter(|| {
            black_box(lifecycle.state.get_container("bench-get").unwrap());
        })
    });

    c.bench_function("container_get_by_name", |b| {
        b.iter(|| {
            black_box(lifecycle.state.get_container("bench-get-container").unwrap());
        })
    });
}

fn bench_state_transitions(c: &mut Criterion) {
    let (_temp, lifecycle) = setup_lifecycle();

    let config = ValidatedConfig {
        privileged: false,
        user_namespace: true,
        user_id: 1000,
        network_mode: NetworkMode::Unprivileged,
        capabilities: vec![],
        no_new_privileges: true,
        readonly_rootfs: false,
    };

    c.bench_function("state_transition_create_to_running", |b| {
        let mut counter = 0;
        b.iter(|| {
            counter += 1;
            let id = format!("trans-{}", counter);
            lifecycle
                .create(&id, &format!("trans-{}", counter), "bench-image", &config, None, None)
                .unwrap();
            // Simulate state transition to running
            black_box(
                lifecycle
                    .state
                    .set_container_state(&id, ContainerState::Running, Some(1234))
                    .unwrap(),
            );
        })
    });
}

criterion_group!(
    benches,
    bench_state_manager_creation,
    bench_container_creation,
    bench_container_list,
    bench_container_get,
    bench_state_transitions
);
criterion_main!(benches);
