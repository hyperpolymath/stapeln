// SPDX-License-Identifier: PMPL-1.0-or-later
//! Vordr - High-Assurance Daemonless Container Engine
//!
//! The Warden component of the Svalinn ecosystem.
//! Provides secure container execution with formally verified security policies.

use anyhow::Result;
use clap::Parser;
use tracing::info;
use tracing_subscriber::{fmt, EnvFilter};

use ebpf::{Monitor, MonitorConfig};

mod cli;
mod ebpf;
mod engine;
mod ffi;
mod mcp;
mod network;
mod registry;
mod runtime;

use cli::Cli;

fn main() -> Result<()> {
    // Initialize logging
    let filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info"));

    fmt()
        .with_env_filter(filter)
        .with_target(false)
        .init();

    // Initialize the gatekeeper
    ffi::init_gatekeeper()?;
    info!("Gatekeeper initialized (version {})", ffi::gatekeeper_version());

    // Parse CLI arguments
    let cli = Cli::parse();

    // Execute command
    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()?
        .block_on(async {
            // Initialize and start eBPF Monitor in the background
            let monitor_config = MonitorConfig::default();
            let mut monitor = Monitor::new(monitor_config);

            // Start the monitor's event processing in a separate task
            let _monitor_handle = tokio::spawn(async move {
                if let Err(e) = monitor.process_events().await {
                    tracing::error!("eBPF Monitor error: {:?}", e);
                }
            });

            // Execute CLI command
            let cli_result = cli::execute(cli).await;

            // Optionally, wait for the monitor to finish if needed before exiting
            // For now, let it run in the background until the main process exits
            // monitor_handle.await.expect("eBPF Monitor task failed");

            cli_result
        })
}
