// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose ps` command implementation

use anyhow::Result;
use prettytable::{Table, Row, Cell, format};

use crate::vordr::VordrClient;

/// Run `ps` command - list services
pub async fn ps(
    project_name: &str,
    show_all: bool,
    output_format: &str,
) -> Result<()> {
    tracing::info!("Listing services for project '{}'", project_name);

    // Initialize Vörðr client
    let vordr_url = std::env::var("VORDR_URL")
        .unwrap_or_else(|_| "http://localhost:9090".to_string());
    let vordr_client = VordrClient::new(vordr_url);

    // Get all containers
    let containers = vordr_client.list_containers().await?;

    // Filter by project name
    let project_containers: Vec<_> = containers
        .iter()
        .filter(|c| {
            c.container_id.starts_with(&format!("{}_", project_name))
        })
        .filter(|c| show_all || c.state == "running")
        .collect();

    // Output based on format
    match output_format {
        "json" => {
            let json = serde_json::to_string_pretty(&project_containers)?;
            println!("{}", json);
        }
        "table" | _ => {
            let mut table = Table::new();
            table.set_format(*format::consts::FORMAT_BOX_CHARS);

            // Header
            table.add_row(Row::new(vec![
                Cell::new("NAME"),
                Cell::new("CONTAINER ID"),
                Cell::new("STATE"),
                Cell::new("CREATED"),
            ]));

            // Rows
            for container in &project_containers {
                let service_name = extract_service_name(&container.container_id, project_name);

                table.add_row(Row::new(vec![
                    Cell::new(&service_name),
                    Cell::new(&container.container_id[..12]), // Short ID
                    Cell::new(&container.state),
                    Cell::new("N/A"), // TODO: Add created timestamp
                ]));
            }

            table.printstd();

            // Summary
            println!();
            println!("Total: {} containers", project_containers.len());
        }
    }

    Ok(())
}

/// Extract service name from container ID
fn extract_service_name(container_id: &str, project_name: &str) -> String {
    container_id
        .strip_prefix(&format!("{}_", project_name))
        .and_then(|s| s.split('_').next())
        .unwrap_or(container_id)
        .to_string()
}
