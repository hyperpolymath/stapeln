// SPDX-License-Identifier: PMPL-1.0-or-later
//! Container runtime integration
//!
//! Two execution paths are provided:
//!
//! 1. **`DirectRuntime`** (`crun` module) — Preferred. Shells out to crun or
//!    youki as a subprocess, exactly like podman does. This is the primary
//!    path for `vordr run`.
//!
//! 2. **`ShimClient`** (`shim` module) — Fallback. Uses the same subprocess
//!    approach but with a slightly different API. Retained for backward
//!    compatibility and for the `ContainerLifecycle` engine.
//!
//! 3. **`TtrpcClient`** (`ttrpc` module) — For containerd shim v2 protocol
//!    communication over Unix domain sockets.

pub mod crun;
pub mod shim;
pub mod ttrpc;

pub use crun::DirectRuntime;
pub use shim::ShimClient;
