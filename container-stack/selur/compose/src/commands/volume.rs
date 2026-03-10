// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose volume` command implementation
//!
//! Manages volumes: list, create, and remove via Vordr MCP.

use anyhow::Result;
use prettytable::{Cell, Row, Table, format};

use crate::vordr_mcp::VordrMcpClient;

/// List all volumes
pub async fn volume_ls() -> Result<()> {
    tracing::info!("Listing volumes");

    let vordr_mcp_url =
        std::env::var("VORDR_MCP_URL").unwrap_or_else(|_| "http://localhost:8081".to_string());
    let vordr_mcp = VordrMcpClient::new(vordr_mcp_url);

    let volumes = vordr_mcp.list_volumes().await?;

    if volumes.is_empty() {
        println!("No volumes found");
        return Ok(());
    }

    let mut table = Table::new();
    table.set_format(*format::consts::FORMAT_BOX_CHARS);

    table.add_row(Row::new(vec![
        Cell::new("NAME"),
        Cell::new("DRIVER"),
        Cell::new("MOUNTPOINT"),
    ]));

    for volume in &volumes {
        table.add_row(Row::new(vec![
            Cell::new(&volume.name),
            Cell::new(&volume.driver),
            Cell::new(&volume.mountpoint),
        ]));
    }

    table.printstd();
    println!();
    println!("Total: {} volume(s)", volumes.len());

    Ok(())
}

/// Create a new volume
pub async fn volume_create(name: &str, driver: &str) -> Result<()> {
    tracing::info!("Creating volume '{}' with driver '{}'", name, driver);

    let vordr_mcp_url =
        std::env::var("VORDR_MCP_URL").unwrap_or_else(|_| "http://localhost:8081".to_string());
    let vordr_mcp = VordrMcpClient::new(vordr_mcp_url);

    vordr_mcp.create_volume(name, driver).await?;

    println!("Volume '{}' created", name);
    println!("  Driver: {}", driver);

    Ok(())
}

/// Remove a volume by name
pub async fn volume_rm(name: &str) -> Result<()> {
    tracing::info!("Removing volume '{}'", name);

    let vordr_mcp_url =
        std::env::var("VORDR_MCP_URL").unwrap_or_else(|_| "http://localhost:8081".to_string());
    let vordr_mcp = VordrMcpClient::new(vordr_mcp_url);

    vordr_mcp.remove_volume(name).await?;

    println!("Volume '{}' removed", name);

    Ok(())
}
