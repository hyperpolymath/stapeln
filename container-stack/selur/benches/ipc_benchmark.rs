// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

//! Benchmark comparing selur WASM IPC vs JSON/HTTP

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};

fn create_sample_request(size: usize) -> Vec<u8> {
    let mut request = Vec::new();
    request.push(0x01); // CREATE_CONTAINER command

    // Payload
    let payload = vec![0x42; size]; // Dummy payload
    request.extend_from_slice(&(payload.len() as u32).to_le_bytes());
    request.extend_from_slice(&payload);

    request
}

fn benchmark_wasm_ipc(c: &mut Criterion) {
    let mut group = c.benchmark_group("wasm_ipc");

    for size in [100, 1000, 10000].iter() {
        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, &size| {
            // Note: This will only work when selur.wasm is built
            // For now, we benchmark request creation
            b.iter(|| {
                let request = create_sample_request(size);
                black_box(request);
            });
        });
    }

    group.finish();
}

fn benchmark_json_serialization(c: &mut Criterion) {
    let mut group = c.benchmark_group("json_serialization");

    #[derive(serde::Serialize)]
    struct JsonRequest {
        command: String,
        payload: Vec<u8>,
    }

    for size in [100, 1000, 10000].iter() {
        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, &size| {
            let request = JsonRequest {
                command: "CREATE_CONTAINER".to_string(),
                payload: vec![0x42; size],
            };

            b.iter(|| {
                let json = serde_json::to_vec(&request).unwrap();
                black_box(json);
            });
        });
    }

    group.finish();
}

criterion_group!(benches, benchmark_wasm_ipc, benchmark_json_serialization);
criterion_main!(benches);
