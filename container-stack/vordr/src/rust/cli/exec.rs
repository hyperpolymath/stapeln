// SPDX-License-Identifier: PMPL-1.0-or-later
//! `vordr exec` command implementation

use anyhow::{Context, Result};
use clap::Args;
use std::path::Path;

use crate::cli::{Cli, validation};
use crate::engine::{ContainerState, StateManager};
use crate::runtime::ShimClient;

/// Arguments for the `exec` command
#[derive(Args, Debug)]
pub struct ExecArgs {
    /// Container ID or name
    pub container: String,

    /// Run in detached mode
    #[arg(short, long)]
    pub detach: bool,

    /// Set environment variables
    #[arg(short = 'e', long = "env", action = clap::ArgAction::Append)]
    pub env: Vec<String>,

    /// Allocate a pseudo-TTY
    #[arg(short = 't', long)]
    pub tty: bool,

    /// Keep STDIN open
    #[arg(short = 'i', long)]
    pub interactive: bool,

    /// Working directory inside the container
    #[arg(short = 'w', long)]
    pub workdir: Option<String>,

    /// Run as specific user
    #[arg(short = 'u', long)]
    pub user: Option<String>,

    /// Command and arguments to execute
    #[arg(required = true, trailing_var_arg = true)]
    pub command: Vec<String>,
}

pub async fn execute(args: ExecArgs, cli: &Cli) -> Result<()> {
    // Validate container ID to prevent path traversal
    validation::validate_container_id(&args.container)
        .context("Invalid container ID")?;

    // Open state database
    let state = StateManager::open(Path::new(&cli.db_path))
        .context("Failed to open state database")?;

    // Get container info
    let container = state.get_container(&args.container)
        .context("Container not found")?;

    // Check container is running
    if container.state != ContainerState::Running {
        anyhow::bail!("Container {} is not running (state: {:?})",
            container.name, container.state);
    }

    println!("Executing in container: {} ({})", container.name, container.id);

    // Build process spec for exec
    let uid: u32 = if let Some(ref user) = args.user {
        user.parse()
            .context("Invalid user ID - must be a valid UID number")?
    } else {
        1000  // Default to non-root user instead of 0
    };

    let process_spec = serde_json::json!({
        "terminal": args.tty,
        "user": {
            "uid": uid,
            "gid": uid
        },
        "args": args.command,
        "env": args.env.iter()
            .chain(std::iter::once(&"PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin".to_string()))
            .collect::<Vec<_>>(),
        "cwd": args.workdir.as_deref().unwrap_or("/")
    });

    // Create shim client
    let shim = ShimClient::new(&cli.runtime, &container.bundle_path);

    // Execute the process
    let _exec_pid = shim.exec(
        &container.id,
        &process_spec.to_string(),
        args.tty,
    ).await.context("Failed to exec in container")?;

    // In foreground mode (not detached), we wait for the process
    // The exec call already inherits stdin/stdout/stderr

    Ok(())
}
