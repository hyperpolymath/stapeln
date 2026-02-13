// SPDX-License-Identifier: PMPL-1.0-or-later
//! `vordr inspect` command implementation

use anyhow::{Context, Result};
use clap::Args;
use serde_json::json;
use std::path::Path;

use crate::cli::Cli;
use crate::engine::StateManager;

/// Arguments for the `inspect` command
#[derive(Args, Debug)]
pub struct InspectArgs {
    /// Container ID or name
    pub container: String,

    /// Format output using a Go template (limited support)
    #[arg(short, long)]
    pub format: Option<String>,

    /// Display total file sizes
    #[arg(short, long)]
    pub size: bool,
}

pub async fn execute(args: InspectArgs, cli: &Cli) -> Result<()> {
    // Open state database
    let state = StateManager::open(Path::new(&cli.db_path))
        .context("Failed to open state database")?;

    // Get container info
    let container = state.get_container(&args.container)
        .context("Container not found")?;

    // Parse stored config if available
    let config: serde_json::Value = container.config
        .as_ref()
        .and_then(|c| serde_json::from_str(c).ok())
        .unwrap_or(json!({}));

    // Build inspection output
    let output = json!({
        "Id": container.id,
        "Name": container.name,
        "Created": container.created_at,
        "State": {
            "Status": container.state.as_str(),
            "Running": container.state == crate::engine::ContainerState::Running,
            "Paused": container.state == crate::engine::ContainerState::Paused,
            "Pid": container.pid,
            "ExitCode": container.exit_code,
            "StartedAt": container.started_at,
            "FinishedAt": container.finished_at,
        },
        "Image": container.image_id,
        "Config": config,
        "HostConfig": {
            "Privileged": config.get("privileged").and_then(|v| v.as_bool()).unwrap_or(false),
            "UsernsMode": if config.get("userns").and_then(|v| v.as_bool()).unwrap_or(true) {
                "private"
            } else {
                "host"
            },
        },
        "Mounts": config.get("volumes").unwrap_or(&json!([])),
        "NetworkSettings": {
            "Ports": config.get("ports").unwrap_or(&json!([])),
        },
        "Path": container.bundle_path,
    });

    if let Some(ref format) = args.format {
        // Simple format string support
        let formatted = format
            .replace("{{.Id}}", &container.id)
            .replace("{{.Name}}", &container.name)
            .replace("{{.State.Status}}", container.state.as_str())
            .replace("{{.Image}}", &container.image_id);
        println!("{}", formatted);
    } else {
        // Pretty print JSON
        println!("{}", serde_json::to_string_pretty(&output)?);
    }

    Ok(())
}
