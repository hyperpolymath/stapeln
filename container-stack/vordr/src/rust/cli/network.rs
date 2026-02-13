// SPDX-License-Identifier: PMPL-1.0-or-later
//! Network management commands

use anyhow::{Context, Result};
use clap::Subcommand;
use std::path::Path;

use crate::cli::Cli;
use crate::engine::StateManager;

#[derive(Subcommand, Debug)]
pub enum NetworkCommands {
    /// Create a network
    Create {
        /// Network name
        name: String,

        /// Network driver
        #[arg(short, long, default_value = "bridge")]
        driver: String,

        /// Subnet in CIDR format
        #[arg(long)]
        subnet: Option<String>,

        /// Gateway IP address
        #[arg(long)]
        gateway: Option<String>,
    },

    /// List networks
    Ls {
        /// Only show network IDs
        #[arg(short, long)]
        quiet: bool,
    },

    /// Remove a network
    Rm {
        /// Network name or ID
        network: String,
    },

    /// Show network details
    Inspect {
        /// Network name or ID
        network: String,
    },

    /// Connect a container to a network
    Connect {
        /// Network name or ID
        network: String,

        /// Container name or ID
        container: String,

        /// Alias for the container on the network
        #[arg(long)]
        alias: Option<String>,

        /// Static IP address
        #[arg(long)]
        ip: Option<String>,
    },

    /// Disconnect a container from a network
    Disconnect {
        /// Network name or ID
        network: String,

        /// Container name or ID
        container: String,
    },

    /// Remove unused networks
    Prune {
        /// Do not prompt for confirmation
        #[arg(short, long)]
        force: bool,
    },
}

pub async fn execute(cmd: NetworkCommands, cli: &Cli) -> Result<()> {
    match cmd {
        NetworkCommands::Create {
            name,
            driver,
            subnet,
            gateway,
        } => create_network(&name, &driver, subnet.as_deref(), gateway.as_deref(), cli).await,
        NetworkCommands::Ls { quiet } => list_networks(quiet, cli).await,
        NetworkCommands::Rm { network } => remove_network(&network, cli).await,
        NetworkCommands::Inspect { network } => inspect_network(&network, cli).await,
        NetworkCommands::Connect {
            network,
            container,
            alias,
            ip,
        } => connect_network(&network, &container, alias.as_deref(), ip.as_deref(), cli).await,
        NetworkCommands::Disconnect { network, container } => {
            disconnect_network(&network, &container, cli).await
        }
        NetworkCommands::Prune { force: _ } => prune_networks(cli).await,
    }
}

async fn create_network(
    name: &str,
    driver: &str,
    subnet: Option<&str>,
    gateway: Option<&str>,
    cli: &Cli,
) -> Result<()> {
    let db_path = Path::new(&cli.db_path);
    if let Some(parent) = db_path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    let state = StateManager::open(db_path).context("Failed to open state database")?;

    let network_id = uuid::Uuid::new_v4().to_string();

    state.create_network(&network_id, name, driver, subnet, gateway, None)?;

    println!("{}", network_id);
    Ok(())
}

async fn list_networks(quiet: bool, cli: &Cli) -> Result<()> {
    let db_path = Path::new(&cli.db_path);

    if !db_path.exists() {
        if quiet {
            return Ok(());
        }
        println!("NETWORK ID          NAME                DRIVER              SCOPE");
        return Ok(());
    }

    let state = StateManager::open(db_path).context("Failed to open state database")?;

    let networks = state.list_networks()?;

    if quiet {
        for network in &networks {
            println!("{}", &network.id[..12]);
        }
    } else {
        println!(
            "{:<20} {:<20} {:<20} {:<10}",
            "NETWORK ID", "NAME", "DRIVER", "SCOPE"
        );

        for network in &networks {
            println!(
                "{:<20} {:<20} {:<20} {:<10}",
                &network.id[..12.min(network.id.len())],
                truncate(&network.name, 20),
                &network.driver,
                "local"
            );
        }
    }

    Ok(())
}

async fn remove_network(network_id: &str, cli: &Cli) -> Result<()> {
    let state =
        StateManager::open(Path::new(&cli.db_path)).context("Failed to open state database")?;

    let network = state.get_network(network_id)?;
    state.delete_network(&network.id)?;

    println!("{}", &network.id[..12]);
    Ok(())
}

async fn inspect_network(network_id: &str, cli: &Cli) -> Result<()> {
    let state =
        StateManager::open(Path::new(&cli.db_path)).context("Failed to open state database")?;

    let network = state.get_network(network_id)?;

    let output = serde_json::json!({
        "Id": network.id,
        "Name": network.name,
        "Driver": network.driver,
        "Subnet": network.subnet,
        "Gateway": network.gateway,
        "Created": network.created_at,
        "Scope": "local",
    });

    println!("{}", serde_json::to_string_pretty(&output)?);
    Ok(())
}

async fn connect_network(
    network_id: &str,
    container_id: &str,
    alias: Option<&str>,
    ip: Option<&str>,
    cli: &Cli,
) -> Result<()> {
    let state =
        StateManager::open(Path::new(&cli.db_path)).context("Failed to open state database")?;

    let network = state.get_network(network_id)?;
    let container = state.get_container(container_id)?;

    let aliases = alias.map(|a| vec![a.to_string()]).unwrap_or_default();

    state.connect_container_network(&container.id, &network.id, ip, None, &aliases)?;

    println!("Connected {} to {}", container.name, network.name);
    Ok(())
}

async fn disconnect_network(network_id: &str, container_id: &str, cli: &Cli) -> Result<()> {
    let state =
        StateManager::open(Path::new(&cli.db_path)).context("Failed to open state database")?;

    let network = state.get_network(network_id)?;
    let container = state.get_container(container_id)?;

    state.disconnect_container_network(&container.id, &network.id)?;

    println!("Disconnected {} from {}", container.name, network.name);
    Ok(())
}

async fn prune_networks(_cli: &Cli) -> Result<()> {
    println!("Network pruning not yet implemented");
    Ok(())
}

fn truncate(s: &str, max: usize) -> String {
    if s.len() <= max {
        s.to_string()
    } else {
        format!("{}...", &s[..max - 3])
    }
}
