// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose stop` command implementation

use anyhow::Result;

use crate::compose::ComposeFile;
use crate::vordr::VordrClient;

/// Run `stop` command - stop running containers
pub async fn stop(
    compose: &ComposeFile,
    project_name: &str,
    services: Vec<String>,
    _timeout: u64,
) -> Result<()> {
    tracing::info!("Stopping services for project '{}'", project_name);

    // Determine which services to stop
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

    let mut stopped = 0;
    let mut already_stopped = 0;
    let mut not_found = 0;

    for service_name in &target_services {
        // Find running containers for this service
        let running_containers: Vec<_> = all_containers
            .iter()
            .filter(|c| {
                let name = extract_service_name(&c.container_id, project_name);
                name == *service_name && c.state == "running"
            })
            .collect();

        if running_containers.is_empty() {
            // Check if there are stopped containers
            let stopped_exists = all_containers.iter().any(|c| {
                let name = extract_service_name(&c.container_id, project_name);
                name == *service_name && c.state != "running"
            });

            if stopped_exists {
                tracing::info!("{}: already stopped", service_name);
                already_stopped += 1;
            } else {
                tracing::warn!("{}: no containers found", service_name);
                not_found += 1;
            }
            continue;
        }

        // Stop each running container
        for container in running_containers {
            tracing::info!("  Stopping {}...", container.container_id);

            // TODO: Use timeout parameter for graceful shutdown
            match vordr_client.stop_container(&container.container_id).await {
                Ok(_) => {
                    println!("✓ {} stopped", service_name);
                    stopped += 1;
                }
                Err(e) => {
                    eprintln!("✗ Failed to stop {}: {}", service_name, e);
                }
            }
        }
    }

    // Summary
    println!();
    if stopped > 0 {
        println!("✓ Stopped {} container(s)", stopped);
    }
    if already_stopped > 0 {
        println!("⚠ {} already stopped", already_stopped);
    }
    if not_found > 0 {
        println!("⚠ {} not found", not_found);
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
