// SPDX-License-Identifier: PMPL-1.0-or-later
//! Foreign Function Interface bindings

pub mod gatekeeper;

pub use gatekeeper::{
    init as init_gatekeeper, version as gatekeeper_version,
    ConfigValidator, NetworkMode, ValidatedConfig,
};
