// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose network` command implementation
//!
//! Manages networks: list, create, and remove via Vordr MCP.

use anyhow::Result;
use prettytable::{Cell, Row, Table, format};

use crate::vordr_mcp::VordrMcpClient;

/// List all networks
pub async fn network_ls() -> Result<()> {
    tracing::info!("Listing networks");

    let vordr_mcp_url =
        std::env::var("VORDR_MCP_URL").unwrap_or_else(|_| "http://localhost:8081".to_string());
    let vordr_mcp = VordrMcpClient::new(vordr_mcp_url);

    let networks = vordr_mcp.list_networks().await?;

    if networks.is_empty() {
        println!("No networks found");
        return Ok(());
    }

    let mut table = Table::new();
    table.set_format(*format::consts::FORMAT_BOX_CHARS);

    table.add_row(Row::new(vec![
        Cell::new("NAME"),
        Cell::new("DRIVER"),
        Cell::new("SCOPE"),
    ]));

    for network in &networks {
        table.add_row(Row::new(vec![
            Cell::new(&network.name),
            Cell::new(&network.driver),
            Cell::new(&network.scope),
        ]));
    }

    table.printstd();
    println!();
    println!("Total: {} network(s)", networks.len());

    Ok(())
}

/// Create a new network
pub async fn network_create(name: &str, driver: &str, subnet: Option<String>) -> Result<()> {
    tracing::info!("Creating network '{}' with driver '{}'", name, driver);

    let vordr_mcp_url =
        std::env::var("VORDR_MCP_URL").unwrap_or_else(|_| "http://localhost:8081".to_string());
    let vordr_mcp = VordrMcpClient::new(vordr_mcp_url);

    vordr_mcp.create_network(name, driver, subnet.clone()).await?;

    println!("Network '{}' created", name);
    println!("  Driver: {}", driver);
    if let Some(ref s) = subnet {
        println!("  Subnet: {}", s);
    }

    Ok(())
}

/// Remove a network by name
pub async fn network_rm(name: &str) -> Result<()> {
    tracing::info!("Removing network '{}'", name);

    let vordr_mcp_url =
        std::env::var("VORDR_MCP_URL").unwrap_or_else(|_| "http://localhost:8081".to_string());
    let vordr_mcp = VordrMcpClient::new(vordr_mcp_url);

    vordr_mcp.remove_network(name).await?;

    println!("Network '{}' removed", name);

    Ok(())
}
