# Vordr eBPF Kernel Programs

<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->

These are **kernel-side** eBPF programs compiled with [aya-bpf](https://github.com/aya-rs/aya). They run inside the Linux kernel's BPF virtual machine and communicate with the userspace `Monitor` (in `src/rust/ebpf/`) via ring buffer maps.

## Programs

| File | Tracepoint | Purpose |
|------|-----------|---------|
| `sys_enter.rs` | `raw_syscalls/sys_enter` | Captures all syscall entry events for monitored containers |
| `sched_exec.rs` | `sched/sched_process_exec` | Captures process execution events (new binaries being exec'd) |

## Building

eBPF programs target the BPF virtual machine, not a regular CPU architecture. They require a nightly Rust toolchain and a cross-compilation setup:

```bash
# Install the nightly toolchain with the BPF target
rustup toolchain install nightly
rustup target add --toolchain nightly bpfel-unknown-none

# Build the eBPF programs
cargo +nightly build \
    --target bpfel-unknown-none \
    -Z build-std=core \
    --release
```

The build produces ELF object files containing BPF bytecode. These are embedded into the userspace binary at compile time via `include_bytes_aligned!` in `probes.rs`.

## Architecture

```
                          Kernel Space
  ┌─────────────────────────────────────────────────────────┐
  │                                                         │
  │  sys_enter.rs ──────┐                                   │
  │  (tracepoint)       ├──> EVENTS (RingBuf, 256KB)        │
  │                     │                                   │
  │  sched_exec.rs ─────┤──> EXEC_EVENTS (RingBuf, 64KB)   │
  │  (tracepoint)       │                                   │
  │                     │                                   │
  │        CONTAINER_FILTER ◄── shared map (64 slots)       │
  │        SYSCALL_FILTER   ◄── shared map (512 entries)    │
  │                                                         │
  └─────────────────────┬───────────────────────────────────┘
                        │ ring buffer
                        ▼
                     Userspace
  ┌─────────────────────────────────────────────────────────┐
  │  ProbeManager (probes.rs)                               │
  │    - Loads BPF bytecode                                 │
  │    - Attaches to tracepoints                            │
  │    - Populates filter maps                              │
  │                                                         │
  │  Monitor (mod.rs)                                       │
  │    - Reads ring buffers                                 │
  │    - Converts BPF structs to Rust events                │
  │    - Feeds AnomalyDetector                              │
  └─────────────────────────────────────────────────────────┘
```

## Feature flag

The `bpf` feature flag in `Cargo.toml` controls whether the userspace code includes aya dependencies for loading these programs. Without the flag, the `Monitor` runs in stub mode (no kernel tracing).

```toml
[features]
bpf = ["aya", "aya-log"]
```

## BPF map summary

| Map | Type | Key | Value | Purpose |
|-----|------|-----|-------|---------|
| `EVENTS` | RingBuf (256KB) | -- | `SyscallEventBpf` | Syscall events to userspace |
| `EXEC_EVENTS` | RingBuf (64KB) | -- | `ExecEventBpf` | Exec events to userspace |
| `CONTAINER_FILTER` | HashMap | `u32` (slot) | `ContainerFilterBpf` | Which cgroups to monitor |
| `SYSCALL_FILTER` | HashMap | `i64` (syscall nr) | `u8` (action) | Which syscalls to capture |

## Constraints

eBPF programs run inside the kernel verifier sandbox:

- **No heap allocation** -- no `Vec`, `String`, `Box`, or any `alloc` types
- **512-byte stack limit** -- all local variables must fit in 512 bytes total
- **Bounded loops only** -- the verifier rejects unbounded iteration
- **No floating point** -- BPF VM has no FPU
- **No panics** -- the panic handler is a stub that the verifier proves unreachable
- **Helper functions only** -- no libc, no std, only `bpf_*` helpers from the kernel

## Kernel requirements

- Linux 5.8+ (for ring buffer support)
- `CONFIG_BPF=y`, `CONFIG_BPF_SYSCALL=y`
- `CAP_BPF` and `CAP_PERFMON` capabilities (or root)
- BPF filesystem mounted at `/sys/fs/bpf`
