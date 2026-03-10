// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose pause` and `selur-compose unpause` command implementations
//!
//! Pauses and unpauses running containers via the Vordr container runtime.

use anyhow::Result;

use crate::compose::ComposeFile;
use crate::vordr::VordrClient;

/// Run `pause` command - pause running containers
pub async fn pause(
    compose: &ComposeFile,
    project_name: &str,
    services: Vec<String>,
) -> Result<()> {
    tracing::info!("Pausing services for project '{}'", project_name);

    let target_services: Vec<String> = if services.is_empty() {
        compose.services.keys().cloned().collect()
    } else {
        for service in &services {
            if !compose.services.contains_key(service) {
                anyhow::bail!("Unknown service: {}", service);
            }
        }
        services
    };

    let vordr_url = std::env::var("VORDR_URL")
        .unwrap_or_else(|_| "http://localhost:9090".to_string());
    let vordr_client = VordrClient::new(vordr_url);

    let all_containers = vordr_client.list_containers().await?;

    let mut paused = 0;
    let mut skipped = 0;

    for service_name in &target_services {
        let running_containers: Vec<_> = all_containers
            .iter()
            .filter(|c| {
                let name = extract_service_name(&c.container_id, project_name);
                name == *service_name && c.state == "running"
            })
            .collect();

        if running_containers.is_empty() {
            tracing::info!("{}: not running, skipping", service_name);
            skipped += 1;
            continue;
        }

        for container in running_containers {
            tracing::info!("  Pausing {}...", container.container_id);

            match vordr_client.pause_container(&container.container_id).await {
                Ok(_) => {
                    println!("Paused {}", service_name);
                    paused += 1;
                }
                Err(e) => {
                    eprintln!("Failed to pause {}: {}", service_name, e);
                }
            }
        }
    }

    println!();
    if paused > 0 {
        println!("Paused {} container(s)", paused);
    }
    if skipped > 0 {
        println!("{} service(s) not running", skipped);
    }

    Ok(())
}

/// Run `unpause` command - unpause paused containers
pub async fn unpause(
    compose: &ComposeFile,
    project_name: &str,
    services: Vec<String>,
) -> Result<()> {
    tracing::info!("Unpausing services for project '{}'", project_name);

    let target_services: Vec<String> = if services.is_empty() {
        compose.services.keys().cloned().collect()
    } else {
        for service in &services {
            if !compose.services.contains_key(service) {
                anyhow::bail!("Unknown service: {}", service);
            }
        }
        services
    };

    let vordr_url = std::env::var("VORDR_URL")
        .unwrap_or_else(|_| "http://localhost:9090".to_string());
    let vordr_client = VordrClient::new(vordr_url);

    let all_containers = vordr_client.list_containers().await?;

    let mut unpaused = 0;
    let mut skipped = 0;

    for service_name in &target_services {
        let paused_containers: Vec<_> = all_containers
            .iter()
            .filter(|c| {
                let name = extract_service_name(&c.container_id, project_name);
                name == *service_name && c.state == "paused"
            })
            .collect();

        if paused_containers.is_empty() {
            tracing::info!("{}: not paused, skipping", service_name);
            skipped += 1;
            continue;
        }

        for container in paused_containers {
            tracing::info!("  Unpausing {}...", container.container_id);

            match vordr_client.unpause_container(&container.container_id).await {
                Ok(_) => {
                    println!("Unpaused {}", service_name);
                    unpaused += 1;
                }
                Err(e) => {
                    eprintln!("Failed to unpause {}: {}", service_name, e);
                }
            }
        }
    }

    println!();
    if unpaused > 0 {
        println!("Unpaused {} container(s)", unpaused);
    }
    if skipped > 0 {
        println!("{} service(s) not paused", skipped);
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
