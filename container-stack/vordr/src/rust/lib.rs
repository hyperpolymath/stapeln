// SPDX-License-Identifier: PMPL-1.0-or-later
//! Vordr - High-Assurance Daemonless Container Engine
//!
//! The Warden component of the Svalinn ecosystem.
//! Provides secure container execution with formally verified security policies.

pub mod cli;
pub mod ebpf;
pub mod engine;
pub mod ffi;
pub mod mcp;
pub mod network;
pub mod registry;
pub mod runtime;
pub mod temporal;
