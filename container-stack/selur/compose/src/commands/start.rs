// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose start` command implementation

use anyhow::Result;

use crate::compose::ComposeFile;
use crate::vordr::VordrClient;

/// Run `start` command - start stopped containers
pub async fn start(
    compose: &ComposeFile,
    project_name: &str,
    services: Vec<String>,
) -> Result<()> {
    tracing::info!("Starting stopped services for project '{}'", project_name);

    // Determine which services to start
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

    let mut started = 0;
    let mut already_running = 0;
    let mut not_found = 0;

    for service_name in &target_services {
        // Find stopped containers for this service
        let stopped_containers: Vec<_> = all_containers
            .iter()
            .filter(|c| {
                let name = extract_service_name(&c.container_id, project_name);
                name == *service_name && c.state != "running"
            })
            .collect();

        if stopped_containers.is_empty() {
            // Check if there are running containers
            let running_exists = all_containers.iter().any(|c| {
                let name = extract_service_name(&c.container_id, project_name);
                name == *service_name && c.state == "running"
            });

            if running_exists {
                tracing::info!("{}: already running", service_name);
                already_running += 1;
            } else {
                tracing::warn!("{}: no containers found", service_name);
                not_found += 1;
            }
            continue;
        }

        // Start each stopped container
        for container in stopped_containers {
            tracing::info!("  Starting {}...", container.container_id);

            match vordr_client.start_container(&container.container_id).await {
                Ok(_) => {
                    println!("✓ {} started", service_name);
                    started += 1;
                }
                Err(e) => {
                    eprintln!("✗ Failed to start {}: {}", service_name, e);
                }
            }
        }
    }

    // Summary
    println!();
    if started > 0 {
        println!("✓ Started {} container(s)", started);
    }
    if already_running > 0 {
        println!("⚠ {} already running", already_running);
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
