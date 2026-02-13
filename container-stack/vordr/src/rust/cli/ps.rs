// SPDX-License-Identifier: PMPL-1.0-or-later
//! `vordr ps` command implementation

use anyhow::{Context, Result};
use clap::Args;
use std::path::Path;

use crate::cli::Cli;
use crate::engine::{ContainerState, StateManager};

/// Arguments for the `ps` command
#[derive(Args, Debug)]
pub struct PsArgs {
    /// Show all containers (default shows only running)
    #[arg(short, long)]
    pub all: bool,

    /// Only display container IDs
    #[arg(short, long)]
    pub quiet: bool,

    /// Filter by state
    #[arg(long)]
    pub filter: Option<String>,

    /// Format output
    #[arg(long)]
    pub format: Option<String>,
}

pub async fn execute(args: PsArgs, cli: &Cli) -> Result<()> {
    // Open state database
    let db_path = Path::new(&cli.db_path);

    // If database doesn't exist yet, show empty list
    if !db_path.exists() {
        if args.quiet {
            return Ok(());
        }
        println!("CONTAINER ID        NAME                STATUS              IMAGE");
        return Ok(());
    }

    let state = StateManager::open(db_path)
        .context("Failed to open state database")?;

    // Determine state filter
    let state_filter = if args.all {
        None
    } else if let Some(ref filter) = args.filter {
        Some(ContainerState::from_str(filter).unwrap_or(ContainerState::Running))
    } else {
        Some(ContainerState::Running)
    };

    // Get containers
    let containers = state.list_containers(state_filter)?;

    if args.quiet {
        // Only print IDs
        for container in &containers {
            println!("{}", &container.id[..12]);
        }
    } else if let Some(ref format) = args.format {
        // Custom format
        for container in &containers {
            let output = format
                .replace("{{.ID}}", &container.id[..12])
                .replace("{{.Name}}", &container.name)
                .replace("{{.Status}}", container.state.as_str())
                .replace("{{.Image}}", &container.image_id[..12]);
            println!("{}", output);
        }
    } else {
        // Default table format
        println!("{:<20} {:<20} {:<20} {:<20}",
            "CONTAINER ID", "NAME", "STATUS", "IMAGE");

        for container in &containers {
            let status = format_status(&container);
            println!("{:<20} {:<20} {:<20} {:<20}",
                &container.id[..12.min(container.id.len())],
                truncate(&container.name, 20),
                truncate(&status, 20),
                &container.image_id[..12.min(container.image_id.len())]);
        }
    }

    Ok(())
}

fn format_status(container: &crate::engine::ContainerInfo) -> String {
    match container.state {
        ContainerState::Running => {
            if let Some(pid) = container.pid {
                format!("Up (PID {})", pid)
            } else {
                "Up".to_string()
            }
        }
        ContainerState::Created => "Created".to_string(),
        ContainerState::Paused => "Paused".to_string(),
        ContainerState::Stopped => {
            if let Some(code) = container.exit_code {
                format!("Exited ({})", code)
            } else {
                "Exited".to_string()
            }
        }
    }
}

fn truncate(s: &str, max: usize) -> String {
    if s.len() <= max {
        s.to_string()
    } else {
        format!("{}...", &s[..max - 3])
    }
}
