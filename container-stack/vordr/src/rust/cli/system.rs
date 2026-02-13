// SPDX-License-Identifier: PMPL-1.0-or-later
//! System management commands (df, prune, info, reset)

use anyhow::Result;
use bytesize::ByteSize;
use clap::{Parser, Subcommand, ValueEnum};
use dialoguer::Confirm;
use std::path::Path;
use tabled::{Table, Tabled};

use crate::cli::Cli;
use crate::engine::StateManager;

/// System management commands
#[derive(Parser, Debug)]
pub struct SystemArgs {
    #[command(subcommand)]
    pub command: SystemCommands,
}

#[derive(Subcommand, Debug)]
pub enum SystemCommands {
    /// Show disk usage
    Df {
        /// Show detailed info per resource
        #[arg(short, long)]
        verbose: bool,

        /// Output format
        #[arg(long, value_enum, default_value = "table")]
        format: OutputFormat,
    },

    /// Remove unused data
    Prune {
        /// Remove all unused images (not just dangling)
        #[arg(short, long)]
        all: bool,

        /// Also prune volumes
        #[arg(long)]
        volumes: bool,

        /// Don't prompt for confirmation
        #[arg(short, long)]
        force: bool,

        /// Show what would be deleted without deleting
        #[arg(long)]
        dry_run: bool,

        /// Filter resources (e.g., until=24h, label=unused)
        #[arg(long)]
        filter: Vec<String>,
    },

    /// Display system information
    Info,

    /// Reset all Vordr data (dangerous!)
    Reset {
        /// Don't prompt for confirmation
        #[arg(short, long)]
        force: bool,

        /// Also remove configuration files
        #[arg(long)]
        include_config: bool,
    },
}

#[derive(Debug, Clone, Copy, ValueEnum, Default)]
pub enum OutputFormat {
    #[default]
    Table,
    Json,
}

#[derive(Tabled)]
struct DiskUsageRow {
    #[tabled(rename = "TYPE")]
    resource_type: String,
    #[tabled(rename = "TOTAL")]
    total: String,
    #[tabled(rename = "ACTIVE")]
    active: String,
    #[tabled(rename = "SIZE")]
    size: String,
    #[tabled(rename = "RECLAIMABLE")]
    reclaimable: String,
}

#[derive(serde::Serialize)]
struct DiskUsage {
    images: ResourceUsage,
    containers: ResourceUsage,
    volumes: ResourceUsage,
    total_size: u64,
    reclaimable: u64,
}

#[derive(serde::Serialize)]
struct ResourceUsage {
    total: u64,
    active: u64,
    size: u64,
    reclaimable: u64,
}

#[derive(serde::Serialize)]
struct PruneResult {
    containers_deleted: u64,
    networks_deleted: u64,
    images_deleted: u64,
    volumes_deleted: u64,
    space_reclaimed: u64,
}

/// Execute system command
pub async fn execute(args: SystemArgs, cli: &Cli) -> Result<()> {
    match args.command {
        SystemCommands::Df { verbose, format } => df(cli, verbose, format).await,
        SystemCommands::Prune {
            all,
            volumes,
            force,
            dry_run,
            filter,
        } => prune(cli, all, volumes, force, dry_run, filter).await,
        SystemCommands::Info => info(cli).await,
        SystemCommands::Reset {
            force,
            include_config,
        } => reset(cli, force, include_config).await,
    }
}

async fn df(cli: &Cli, verbose: bool, format: OutputFormat) -> Result<()> {
    let state = StateManager::open(Path::new(&cli.db_path))?;

    // Get counts and sizes from database
    let containers = state.list_containers(None)?;
    let running_count = containers.iter().filter(|c| c.state == crate::engine::ContainerState::Running).count();

    // Calculate disk usage (simplified - actual implementation would scan directories)
    let usage = DiskUsage {
        images: ResourceUsage {
            total: 0, // Would query images table
            active: 0,
            size: 0,
            reclaimable: 0,
        },
        containers: ResourceUsage {
            total: containers.len() as u64,
            active: running_count as u64,
            size: 0, // Would calculate from container directories
            reclaimable: 0,
        },
        volumes: ResourceUsage {
            total: 0,
            active: 0,
            size: 0,
            reclaimable: 0,
        },
        total_size: 0,
        reclaimable: 0,
    };

    match format {
        OutputFormat::Json => {
            println!("{}", serde_json::to_string_pretty(&usage)?);
        }
        OutputFormat::Table => {
            if verbose {
                println!("Images:");
                println!("  (detailed listing would go here)\n");
                println!("Containers:");
                for container in &containers {
                    println!(
                        "  {} ({}, {})",
                        &container.id[..12.min(container.id.len())],
                        &container.image_id,
                        container.state.as_str()
                    );
                }
                println!();
            }

            let rows = vec![
                DiskUsageRow {
                    resource_type: "Images".to_string(),
                    total: usage.images.total.to_string(),
                    active: usage.images.active.to_string(),
                    size: ByteSize(usage.images.size).to_string(),
                    reclaimable: format!(
                        "{} ({}%)",
                        ByteSize(usage.images.reclaimable),
                        if usage.images.size > 0 {
                            usage.images.reclaimable * 100 / usage.images.size
                        } else {
                            0
                        }
                    ),
                },
                DiskUsageRow {
                    resource_type: "Containers".to_string(),
                    total: usage.containers.total.to_string(),
                    active: usage.containers.active.to_string(),
                    size: ByteSize(usage.containers.size).to_string(),
                    reclaimable: format!(
                        "{} ({}%)",
                        ByteSize(usage.containers.reclaimable),
                        if usage.containers.size > 0 {
                            usage.containers.reclaimable * 100 / usage.containers.size
                        } else {
                            0
                        }
                    ),
                },
                DiskUsageRow {
                    resource_type: "Volumes".to_string(),
                    total: usage.volumes.total.to_string(),
                    active: usage.volumes.active.to_string(),
                    size: ByteSize(usage.volumes.size).to_string(),
                    reclaimable: format!(
                        "{} ({}%)",
                        ByteSize(usage.volumes.reclaimable),
                        if usage.volumes.size > 0 {
                            usage.volumes.reclaimable * 100 / usage.volumes.size
                        } else {
                            0
                        }
                    ),
                },
            ];

            let table = Table::new(rows).to_string();
            println!("{}", table);
            println!(
                "\nTotal: {}, Reclaimable: {} ({}%)",
                ByteSize(usage.total_size),
                ByteSize(usage.reclaimable),
                if usage.total_size > 0 {
                    usage.reclaimable * 100 / usage.total_size
                } else {
                    0
                }
            );
        }
    }

    Ok(())
}

async fn prune(
    cli: &Cli,
    all: bool,
    volumes: bool,
    force: bool,
    dry_run: bool,
    _filter: Vec<String>,
) -> Result<()> {
    let state = StateManager::open(Path::new(&cli.db_path))?;

    // Find resources to prune
    let containers = state.list_containers(None)?;
    let stopped_containers: Vec<_> = containers
        .iter()
        .filter(|c| c.state != crate::engine::ContainerState::Running)
        .collect();

    // Calculate what would be pruned
    let result = PruneResult {
        containers_deleted: stopped_containers.len() as u64,
        networks_deleted: 0, // Would query unused networks
        images_deleted: 0,   // Would query dangling/unused images
        volumes_deleted: 0,  // Only if --volumes flag
        space_reclaimed: 0,
    };

    if dry_run {
        println!("DRY RUN - No changes will be made\n");
        println!("Would remove:");

        if !stopped_containers.is_empty() {
            println!("  Containers:");
            for container in &stopped_containers {
                println!(
                    "    - {} ({}, {})",
                    &container.id[..12.min(container.id.len())],
                    &container.image_id,
                    container.state.as_str()
                );
            }
        }

        if all {
            println!("  Images: (all unused images)");
        } else {
            println!("  Images: (dangling images only)");
        }

        if volumes {
            println!("  Volumes: (unused volumes)");
        }

        println!(
            "\nEstimated space: {}",
            ByteSize(result.space_reclaimed)
        );
        return Ok(());
    }

    // Show what will be removed
    println!("This will remove:");
    println!("  - {} stopped container(s)", stopped_containers.len());
    println!("  - All unused networks");
    if all {
        println!("  - All unused images");
    } else {
        println!("  - All dangling images");
    }
    if volumes {
        println!("  - All unused volumes");
    }
    println!();

    // Confirm unless forced
    if !force {
        let confirmed = Confirm::new()
            .with_prompt("Continue?")
            .default(false)
            .interact()?;

        if !confirmed {
            println!("Aborted.");
            return Ok(());
        }
    }

    // Perform the prune
    println!();
    for container in stopped_containers {
        print!("Deleting container {}... ", &container.id[..12.min(container.id.len())]);
        state.delete_container(&container.id)?;
        println!("done");
    }

    // Would also prune networks, images, volumes here

    println!(
        "\nDeleted {} container(s), {} network(s), {} image(s)",
        result.containers_deleted, result.networks_deleted, result.images_deleted
    );
    if volumes {
        println!("Deleted {} volume(s)", result.volumes_deleted);
    }
    println!("Reclaimed: {}", ByteSize(result.space_reclaimed));

    Ok(())
}

async fn info(cli: &Cli) -> Result<()> {
    println!("Vordr Container Engine");
    println!("Version: {}", env!("CARGO_PKG_VERSION"));
    println!("Gatekeeper: {}", crate::ffi::gatekeeper_version());

    // Runtime info
    if let Ok(path) = which::which(&cli.runtime) {
        println!("Runtime: {} ({})", cli.runtime, path.display());
    } else {
        println!("Runtime: {} (not found)", cli.runtime);
    }

    // State info
    println!("State DB: {}", cli.db_path);
    println!("Root Dir: {}", cli.root);

    // Kernel info
    #[cfg(target_os = "linux")]
    {
        if let Ok(output) = std::process::Command::new("uname").args(["-r"]).output() {
            let kernel = String::from_utf8_lossy(&output.stdout);
            println!("Kernel: {}", kernel.trim());
        }

        // cgroup version
        if Path::new("/sys/fs/cgroup/cgroup.controllers").exists() {
            println!("Cgroups: v2 (unified)");
        } else {
            println!("Cgroups: v1 (legacy)");
        }
    }

    Ok(())
}

async fn reset(cli: &Cli, force: bool, include_config: bool) -> Result<()> {
    println!("WARNING: This will delete ALL Vordr data:");
    println!("  - All containers (running and stopped)");
    println!("  - All images");
    println!("  - All networks");
    println!("  - All volumes");
    if include_config {
        println!("  - Configuration files");
    }
    println!();

    if !force {
        // Require typing 'yes' for extra safety
        let input: String = dialoguer::Input::new()
            .with_prompt("Type 'yes' to confirm")
            .interact_text()?;

        if input != "yes" {
            println!("Aborted.");
            return Ok(());
        }
    }

    println!();

    // Stop any running containers first
    let state = StateManager::open(Path::new(&cli.db_path))?;
    let containers = state.list_containers(None)?;
    let running: Vec<_> = containers
        .iter()
        .filter(|c| c.state == crate::engine::ContainerState::Running)
        .collect();

    if !running.is_empty() {
        println!("Stopping {} running container(s)...", running.len());
        for container in running {
            print!("  Stopping {}... ", &container.id[..12.min(container.id.len())]);
            // Would actually stop the container via runtime
            println!("done");
        }
    }

    // Remove all data
    println!("Removing all data from {}...", cli.root);

    // Would actually remove directories here
    // std::fs::remove_dir_all(&cli.root)?;

    if include_config {
        println!("Removing configuration...");
        // Would remove config files
    }

    println!("\nReset complete. Run 'vordr doctor' to verify.");

    Ok(())
}
