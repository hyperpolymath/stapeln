// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose top` command implementation

use anyhow::Result;
use prettytable::{Cell, Row, Table};

use crate::compose::ComposeFile;
use crate::vordr::VordrClient;

/// Run `top` command - display running processes
pub async fn top(
    compose: &ComposeFile,
    project_name: &str,
    services: Vec<String>,
) -> Result<()> {
    tracing::info!("Displaying running processes for project '{}'", project_name);

    // Determine which services to show
    let target_services: Vec<String> = if services.is_empty() {
        compose.services.keys().cloned().collect()
    } else {
        // Validate that all requested services exist
        for service in &services {
            if !compose.services.contains_key(service) {
                anyhow::bail!("Unknown service: {}", service);
            }
        }
        services
    };

    // Initialize Vörðr client
    let vordr_url = std::env::var("VORDR_URL")
        .unwrap_or_else(|_| "http://localhost:9090".to_string());
    let vordr_client = VordrClient::new(vordr_url);

    // Get all containers for this project
    let all_containers = vordr_client.list_containers().await?;

    // Filter containers for target services
    let mut project_containers = Vec::new();
    for container in all_containers {
        let service_name = extract_service_name(&container.container_id, project_name);
        if target_services.contains(&service_name) {
            project_containers.push((service_name, container.container_id));
        }
    }

    if project_containers.is_empty() {
        println!("No running containers found for the specified services");
        return Ok(());
    }

    // Display processes for each container
    for (service_name, container_id) in project_containers {
        println!();
        println!("{}", service_name);

        match vordr_client.get_top(&container_id).await {
            Ok(top_info) => {
                // Create table for process information
                let mut table = Table::new();
                table.add_row(Row::new(vec![
                    Cell::new("PID"),
                    Cell::new("USER"),
                    Cell::new("%CPU"),
                    Cell::new("%MEM"),
                    Cell::new("VSZ"),
                    Cell::new("RSS"),
                    Cell::new("TTY"),
                    Cell::new("STAT"),
                    Cell::new("START"),
                    Cell::new("TIME"),
                    Cell::new("COMMAND"),
                ]));

                for process in &top_info.processes {
                    table.add_row(Row::new(vec![
                        Cell::new(&process.pid),
                        Cell::new(&process.user),
                        Cell::new(&process.cpu),
                        Cell::new(&process.mem),
                        Cell::new(&process.vsz),
                        Cell::new(&process.rss),
                        Cell::new(&process.tty),
                        Cell::new(&process.stat),
                        Cell::new(&process.start),
                        Cell::new(&process.time),
                        Cell::new(&process.command),
                    ]));
                }

                table.printstd();
            }
            Err(e) => {
                eprintln!("  Error getting processes: {}", e);
            }
        }
    }

    Ok(())
}

/// Extract service name from container ID
fn extract_service_name(container_id: &str, project_name: &str) -> String {
    // Container ID format: {project}_{service}_{replica}
    // Example: myapp_web_1 -> web
    container_id
        .strip_prefix(&format!("{}_", project_name))
        .and_then(|s| s.split('_').next())
        .unwrap_or(container_id)
        .to_string()
}
