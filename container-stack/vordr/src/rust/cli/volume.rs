// SPDX-License-Identifier: PMPL-1.0-or-later
//! Volume management commands

use anyhow::{Context, Result};
use clap::Subcommand;
use std::path::Path;

use crate::cli::Cli;
use crate::engine::StateManager;

#[derive(Subcommand, Debug)]
pub enum VolumeCommands {
    /// Create a volume
    Create {
        /// Volume name
        name: String,

        /// Volume driver
        #[arg(short, long, default_value = "local")]
        driver: String,

        /// Set metadata label (key=value)
        #[arg(short, long, action = clap::ArgAction::Append)]
        label: Vec<String>,

        /// Set driver options (key=value)
        #[arg(short, long, action = clap::ArgAction::Append)]
        opt: Vec<String>,
    },

    /// List volumes
    Ls {
        /// Only show volume names
        #[arg(short, long)]
        quiet: bool,

        /// Filter volumes
        #[arg(short, long)]
        filter: Option<String>,
    },

    /// Remove a volume
    Rm {
        /// Volume name
        volume: String,

        /// Force removal
        #[arg(short, long)]
        force: bool,
    },

    /// Show volume details
    Inspect {
        /// Volume name
        volume: String,
    },

    /// Remove unused volumes
    Prune {
        /// Remove all unused volumes, not just anonymous ones
        #[arg(short, long)]
        all: bool,

        /// Do not prompt for confirmation
        #[arg(short, long)]
        force: bool,
    },
}

pub async fn execute(cmd: VolumeCommands, cli: &Cli) -> Result<()> {
    match cmd {
        VolumeCommands::Create {
            name,
            driver,
            label,
            opt,
        } => create_volume(&name, &driver, &label, &opt, cli).await,
        VolumeCommands::Ls { quiet, filter: _ } => list_volumes(quiet, cli).await,
        VolumeCommands::Rm { volume, force: _ } => remove_volume(&volume, cli).await,
        VolumeCommands::Inspect { volume } => inspect_volume(&volume, cli).await,
        VolumeCommands::Prune { all: _, force: _ } => prune_volumes(cli).await,
    }
}

async fn create_volume(
    name: &str,
    driver: &str,
    labels: &[String],
    options: &[String],
    cli: &Cli,
) -> Result<()> {
    let db_path = Path::new(&cli.db_path);
    if let Some(parent) = db_path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    let state = StateManager::open(db_path).context("Failed to open state database")?;

    let volume_id = uuid::Uuid::new_v4().to_string();

    // Create mountpoint
    let root_path = Path::new(&cli.root);
    let mountpoint = root_path.join("volumes").join(name);
    std::fs::create_dir_all(&mountpoint).context("Failed to create volume mountpoint")?;

    // Parse labels and options to JSON
    let labels_json = if labels.is_empty() {
        None
    } else {
        let map: std::collections::HashMap<String, String> = labels
            .iter()
            .filter_map(|l| {
                let parts: Vec<&str> = l.splitn(2, '=').collect();
                if parts.len() == 2 {
                    Some((parts[0].to_string(), parts[1].to_string()))
                } else {
                    None
                }
            })
            .collect();
        Some(serde_json::to_string(&map).unwrap())
    };

    let options_json = if options.is_empty() {
        None
    } else {
        let map: std::collections::HashMap<String, String> = options
            .iter()
            .filter_map(|o| {
                let parts: Vec<&str> = o.splitn(2, '=').collect();
                if parts.len() == 2 {
                    Some((parts[0].to_string(), parts[1].to_string()))
                } else {
                    None
                }
            })
            .collect();
        Some(serde_json::to_string(&map).unwrap())
    };

    state.create_volume(
        &volume_id,
        name,
        driver,
        mountpoint.to_str().unwrap(),
        options_json.as_deref(),
        labels_json.as_deref(),
    )?;

    println!("{}", name);
    Ok(())
}

async fn list_volumes(quiet: bool, cli: &Cli) -> Result<()> {
    let db_path = Path::new(&cli.db_path);

    if !db_path.exists() {
        if quiet {
            return Ok(());
        }
        println!("DRIVER              VOLUME NAME");
        return Ok(());
    }

    let state = StateManager::open(db_path).context("Failed to open state database")?;

    let volumes = state.list_volumes()?;

    if quiet {
        for volume in &volumes {
            println!("{}", volume.name);
        }
    } else {
        println!("{:<20} {:<40}", "DRIVER", "VOLUME NAME");

        for volume in &volumes {
            println!("{:<20} {:<40}", volume.driver, volume.name);
        }
    }

    Ok(())
}

async fn remove_volume(volume_name: &str, cli: &Cli) -> Result<()> {
    let state =
        StateManager::open(Path::new(&cli.db_path)).context("Failed to open state database")?;

    let volume = state.get_volume(volume_name)?;

    // Remove mountpoint
    let mountpoint = Path::new(&volume.mountpoint);
    if mountpoint.exists() {
        std::fs::remove_dir_all(mountpoint).context("Failed to remove volume mountpoint")?;
    }

    state.delete_volume(&volume.id)?;

    println!("{}", volume.name);
    Ok(())
}

async fn inspect_volume(volume_name: &str, cli: &Cli) -> Result<()> {
    let state =
        StateManager::open(Path::new(&cli.db_path)).context("Failed to open state database")?;

    let volume = state.get_volume(volume_name)?;

    let labels: serde_json::Value = volume
        .labels
        .as_ref()
        .and_then(|l| serde_json::from_str(l).ok())
        .unwrap_or(serde_json::json!({}));

    let options: serde_json::Value = volume
        .options
        .as_ref()
        .and_then(|o| serde_json::from_str(o).ok())
        .unwrap_or(serde_json::json!({}));

    let output = serde_json::json!({
        "Name": volume.name,
        "Driver": volume.driver,
        "Mountpoint": volume.mountpoint,
        "Labels": labels,
        "Options": options,
        "CreatedAt": volume.created_at,
        "Scope": "local",
    });

    println!("{}", serde_json::to_string_pretty(&output)?);
    Ok(())
}

async fn prune_volumes(_cli: &Cli) -> Result<()> {
    println!("Volume pruning not yet implemented");
    Ok(())
}
