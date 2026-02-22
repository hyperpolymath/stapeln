# ADR-007: Temporal Isolation Engine

**Status:** Proposed
**Date:** 2026-02-22
**Author:** Jonathan D.A. Jewell
**Deciders:** hyperpolymath
**Context:** vordr container guardian / panic-attack abduct integration

---

## Summary

Replace traditional container-based isolation (namespace + cgroup + seccomp)
with a **temporal isolation** architecture where the monitoring system operates
on a fundamentally faster timescale than the code being observed. Instead of
putting suspicious code in a box, we put it in a **time bubble**.

This is a direct engineering solution to the observer effect problem in
security: the act of monitoring normally interferes with the thing being
monitored, making the monitoring detectable. Temporal isolation eliminates
this by making the interference fall below the target's temporal resolution.

---

## Problem

### The Observer Effect in Security

Every existing monitoring approach introduces detectable interference:

| Technique | Detection vector |
|-----------|-----------------|
| strace / ptrace | `/proc/self/status` TracerPid field, syscall latency |
| LD_PRELOAD (libfaketime) | `/proc/self/maps`, `/proc/self/environ` |
| Containers | `/.dockerenv`, `/run/.containerenv`, cgroup hierarchy |
| gVisor | 10-100x syscall latency (timing side-channel) |
| Network monitors | Added latency, port scanning artefacts |
| Debuggers | INT3 breakpoints, debug registers |

Sophisticated malware (especially time-bombs targeting critical infrastructure)
exploits these detection vectors to stay dormant when observed.

### The Time-Bomb Threat Model

A well-designed time-bomb:

- **Never phones home** — no C2 heartbeat, no callback, no network trail
- **Checks local time only** — `gettimeofday()`, `clock_gettime()`, NTP
- **Activates on a date condition** — then executes payload (sabotage, exfiltration, destruction)
- **Is indistinguishable from normal code** when dormant
- **Cannot be recalled** — the attacker has no visibility into whether it was found

This means:
- We can analyse freely without the attacker knowing
- But we must sweep through time to find the activation condition
- And we must observe the payload WITHOUT the code detecting observation

### Environment Fingerprinting Evasion

Advanced payloads also check their environment:

- `/etc/os-release` — wrong OS? Stay dormant
- Installed packages, kernel version, CPU model
- Network topology — is the expected internal network reachable?
- Geographic IP — wrong country? Don't activate

Testing in a generic Chainguard/Wolfi container may cause the code to stay
dormant because it knows it's not in the target environment. False negative.

---

## Solution: Temporal Isolation

### Core Concept

Instead of spatial isolation (containers, VMs), use **temporal isolation**:

1. The monitoring system (Erlang/BEAM + eBPF) runs at real speed
2. The suspicious code's perception of time is dilated via PTP/NTP manipulation
3. The speed differential (e.g. 10,000:1) means monitoring overhead falls
   below the code's temporal resolution
4. Every action passes through an interception layer, but the interception
   is imperceptibly fast from the target's perspective

### Architecture

```
┌──────────────────────────────────────────────────────────┐
│  ERLANG/BEAM CONTROL PLANE  (real speed, microsecond     │
│  process spawning, 65,535 port interceptors)             │
│                                                          │
│  ┌─────────────────────────────────────────────────┐     │
│  │  Port Interceptor Swarm                         │     │
│  │  65,535 lightweight BEAM processes              │     │
│  │  Each listening on one port                     │     │
│  │  Pre-spawned before target starts               │     │
│  │  Logs all connection attempts                   │     │
│  └─────────────────────────────────────────────────┘     │
│                                                          │
│  ┌─────────────────────────────────────────────────┐     │
│  │  eBPF Event Consumer                            │     │
│  │  Reads from kernel ring buffer                  │     │
│  │  Syscall events, file events, network events    │     │
│  │  Feeds into BEAM processes for analysis         │     │
│  └──────────────┬──────────────────────────────────┘     │
├─────────────────┼────────────────────────────────────────┤
│  KERNEL SPACE   │                                        │
│                 │                                        │
│  ┌──────────────┴──────────────────────────────────┐     │
│  │  eBPF Probes (invisible to userspace)           │     │
│  │  sys_enter / sys_exit hooks                     │     │
│  │  sched_process_exec / sched_process_exit        │     │
│  │  vfs_read / vfs_write                           │     │
│  │  sock_sendmsg / net_dev_xmit                    │     │
│  │                                                 │     │
│  │  Can PAUSE a syscall mid-flight while BEAM      │     │
│  │  decides whether to allow, deny, or log it      │     │
│  └──────────────┬──────────────────────────────────┘     │
│                 │                                        │
│  ┌──────────────┴──────────────────────────────────┐     │
│  │  TARGET CODE  (dilated time)                    │     │
│  │                                                 │     │
│  │  PTP/NTP says clock runs at 1/10,000x           │     │
│  │  libfaketime intercepts gettimeofday()          │     │
│  │  Code thinks it's moving at normal speed         │     │
│  │  Actually frozen between clock ticks            │     │
│  │                                                 │     │
│  │  Environment mirrors target deployment:          │     │
│  │  - Matching /etc/os-release                     │     │
│  │  - Matching package list, kernel version         │     │
│  │  - Matching network topology (simulated)         │     │
│  └─────────────────────────────────────────────────┘     │
└──────────────────────────────────────────────────────────┘
```

### Why This Defeats Detection

1. **eBPF probes are in kernel space** — no `/proc` entry, no ptrace flag,
   no LD_PRELOAD in `/proc/self/maps`. The target process cannot see them.

2. **Port interceptors look like normal services** — in any real deployment,
   most ports have something listening. This is environmental noise.

3. **Timing side-channels vanish** — the 50ns eBPF overhead is unmeasurable
   when the target's clock thinks a microsecond takes 10 seconds.

4. **No container fingerprints** — no `/.dockerenv`, no container cgroups.
   The code runs in a real (or faithfully mirrored) environment.

5. **PTP/NTP manipulation is undetectable** — programs routinely deal with
   NTP jitter, port conflicts, and variable syscall latency. This is normal.

---

## CT-Scan Tomographic Mode (Industrial/Critical Infrastructure)

For high-assurance scanning (nuclear, aviation, power grid, military):

### Concept

Like a CT scanner that takes thousands of thin slices to build a 3D image,
each capturing almost nothing individually:

1. Make N copies of the suspicious code (N = 1,000 to 100,000)
2. Each copy runs in its own sealed environment (Kubernetes pod)
3. Each copy monitors **ONE thing only**:
   - One port, or one syscall number, or one file descriptor,
     or one network destination, or one signal
4. Time sweeps at high speed, forward and backward, oscillating
5. No single instance knows the full scan is happening
6. Combine all N results into a complete behavioural tomograph

### Why This Is Undetectable

- Each instance's monitoring footprint is **one data point** — statistically
  indistinguishable from normal environmental noise
- The code cannot warn other instances — they are isolated
- Even if the code detects ONE instance, the other N-1 still collect data
- The attacker would need to detect and evade **distributed partial
  observation**, which is orders of magnitude harder than detecting strace

### Kubernetes Orchestration

```yaml
# Pseudocode — actual implementation in Elixir/BEAM
for syscall_nr in 0..500:
  for time_offset in sweep_range:
    spawn_pod(
      image: mirror_of_target_environment,
      monitor: single_syscall(syscall_nr),
      time: base_time + time_offset,
      collect_to: central_ring_buffer
    )

# Recombine results
tomograph = assemble(all_pod_results)
anomalies = diff(tomograph, baseline_tomograph)
```

### Time Sweep Strategy

- **Linear sweep**: 1 day increments, 10 years forward, 5 years backward
- **Calendar-aware**: extra density around common trigger dates
  (New Year, epoch overflow 2038-01-19, quarter boundaries, known CVE dates)
- **Binary search**: once a trigger window is found, narrow to exact second
- **Oscillating**: forward-back-forward to catch hysteresis effects

---

## Time Manipulation Stack

### Layers of Time Control

| Layer | Mechanism | What it fakes | Limitation |
|-------|-----------|---------------|------------|
| **NTP** | Fake NTP server on forwarded port | `ntpdate`, `chronyd` queries | Code could use other NTP servers |
| **PTP** | IEEE 1588 fake grandmaster clock | Hardware clock sync, `/dev/ptp*` | Requires network control |
| **libfaketime** | LD_PRELOAD intercepting libc | `gettimeofday()`, `clock_gettime(CLOCK_REALTIME)` | Detectable via `/proc/self/maps` |
| **CLOCK_MONOTONIC** | Must also fake via libfaketime | Uptime-relative timing | Inconsistency with `/proc/uptime` |
| **RDTSC** | CPU cycle counter | Raw hardware timer | **Cannot be faked except in a VM** |
| **Full VM (Firecracker)** | Virtualised TSC | Everything including RDTSC | Higher overhead (~125ms boot) |

### Recommended Approach

- **Standard tier**: libfaketime + fake NTP + PTP manipulation (covers 99% of code)
- **High-assurance tier**: Firecracker microVM with virtualised TSC (covers RDTSC checks)
- **CT-scan tier**: Kubernetes pods with mixed approaches across instances

### PTP as Processor Speed Simulation

PTP normally synchronises clocks to sub-microsecond accuracy. By controlling
the PTP grandmaster, we can make the code's clock sync report that the
processor is running slower than it actually is. This creates a perception
that time is passing slowly, giving our BEAM interceptors more relative time
to react.

Combined with libfaketime for CLOCK_REALTIME and CLOCK_MONOTONIC, the code
sees consistent slow time across all sources. Only RDTSC (hardware cycle
counter) would reveal the deception, and that requires a VM to fake.

---

## Environment Mirroring

To prevent environment-fingerprinting evasion:

### `--mirror-env` Flag

```
panic-attack abduct --mirror-env /path/to/reference-system.tar.gz target.bin
```

The reference system provides:
- `/etc/os-release` — exact OS identity
- Package manifest — installed packages and versions
- `/proc/version` — kernel version string
- Network topology description — what hosts/services are reachable
- Hardware profile — CPU model, memory size, disk layout

Abduct builds a runtime environment that matches these properties, so the
code sees its expected deployment environment, not a generic analysis sandbox.

---

## Integration with panic-attack

### abduct Subcommand Tiers

```
# Tier 1: Current (filesystem isolation, local)
panic-attack abduct target.bin --time-mode frozen

# Tier 2: Temporal isolation (BEAM + eBPF, single instance)
panic-attack abduct target.bin --mode temporal --time-target "2038-01-19"

# Tier 3: CT-scan (distributed, Kubernetes)
panic-attack abduct target.bin --mode tomograph --instances 10000 \
  --sweep "2025-01-01..2040-01-01" --k8s-context prod-scanner
```

### Integration with vordr

- vordr provides the eBPF probe infrastructure
- vordr's Elixir orchestrator manages the BEAM interceptor swarm
- vordr's reversibility module (Bennett-reversible operations) handles
  safe rollback if anything goes wrong during scanning
- selur provides IPC between the BEAM control plane and eBPF kernel probes

---

## Threat Model Assumptions

1. **Time-bombs don't phone home** — no C2 heartbeat to fake, no callback
   server to simulate. The attacker has no visibility into whether their
   bomb was found.

2. **Payloads are self-contained** — activation depends only on local
   conditions (time, date, environment state), not external signals.

3. **Dead man's switches are rare** — requiring inbound signals is equally
   traceable and creates operational security risk for the attacker.

4. **Port contention is normal** — programs already tolerate busy ports,
   slow NTP, variable syscall latency. Our interception looks like normal
   operational noise.

---

## Novelty Assessment

Individual components exist (eBPF monitoring, BEAM concurrency, NTP/PTP,
libfaketime, container sandboxing). The novel contributions are:

1. **Temporal isolation as a primary security primitive** — using clock
   manipulation as the isolation mechanism, not containers/VMs
2. **Asymmetric speed advantage** — manufacturing a 10,000:1 time ratio
   so monitoring reacts faster than the target can act
3. **BEAM process swarm as reactive interception fabric** — 65,000
   pre-spawned interceptors covering all ports before target starts
4. **CT-scan tomographic distributed observation** — splitting monitoring
   across thousands of instances so each monitors one thing, recombining
   results, making observation undetectable
5. **The combination** — eBPF + BEAM + PTP + tomographic scanning as
   one coherent system

---

## Proposed Subcommands for panic-attack

| Subcommand | Purpose | Status |
|------------|---------|--------|
| **abduct** | Isolate code, manipulate time, observe | Tier 1 exists |
| **assassinate** | Dead code detection: copy, kill, verify, obliterate | Proposed |
| **autopsy** | Deep forensic post-mortem of crash artefacts | Proposed |
| **aftermath** | Campaign-wide summary dashboard after full sweep | Proposed |

All names follow the all-A convention and panic/danger theme.

---

## References

- IEEE 1588 (PTP): https://standards.ieee.org/ieee/1588/6825/
- libfaketime: https://github.com/wolfcw/libfaketime
- Aya (Rust eBPF): https://aya-rs.dev/
- BEAM VM internals: https://blog.stenmans.org/theBeamBook/
- Bennett reversible computation: doi:10.1007/BF02166677
- Firecracker microVM: https://firecracker-microvm.github.io/
