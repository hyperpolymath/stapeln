// SPDX-License-Identifier: PMPL-1.0-or-later
//! Kernel-side eBPF programs for Vordr container monitoring.
//!
//! # Module overview
//!
//! | Module        | Tracepoint                    | Purpose                          |
//! |---------------|-------------------------------|----------------------------------|
//! | `sys_enter`   | `raw_syscalls/sys_enter`      | Capture all syscall entry events  |
//! | `sched_exec`  | `sched/sched_process_exec`    | Capture process execution events  |
//!
//! # Compilation
//!
//! These modules are **not** compiled as part of the normal `cargo build`.
//! They require the eBPF target and nightly toolchain:
//!
//! ```bash
//! cargo +nightly build \
//!     --target bpfel-unknown-none \
//!     -Z build-std=core \
//!     --release
//! ```
//!
//! The resulting ELF objects are then embedded into the userspace binary
//! via `include_bytes_aligned!` in `probes.rs`.
//!
//! # Shared BPF maps
//!
//! The `CONTAINER_FILTER` map is shared across all programs. When the
//! userspace `ProbeManager` loads multiple BPF programs, aya pins them
//! to the same underlying map FD so that a single call to
//! `set_container_filter()` applies to all tracepoints.
//!
//! # Adding new programs
//!
//! To add a new eBPF program (e.g., for kprobes or XDP):
//!
//! 1. Create a new `.rs` file in this directory
//! 2. Follow the pattern: `#![no_std]`, `#![no_main]`, tracepoint fn, panic handler
//! 3. Re-use `ContainerFilterBpf` and `CONTAINER_FILTER` for container filtering
//! 4. Define a new ring buffer map for the event type (or re-use `EVENTS`)
//! 5. Add the corresponding `ProbeType` variant in `probes.rs`
//! 6. Update the userspace `ProbeManager::attach()` to handle the new program

pub mod sched_exec;
pub mod sys_enter;
