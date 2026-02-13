// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose inspect` command implementation

use anyhow::Result;

use crate::compose::ComposeFile;
use crate::vordr::VordrClient;

/// Run `inspect` command - view detailed service information
pub async fn inspect(
    compose: &ComposeFile,
    project_name: &str,
    service: String,
    format: &str,
) -> Result<()> {
    tracing::info!("Inspecting service '{}' for project '{}'", service, project_name);

    // Validate service exists
    if !compose.services.contains_key(&service) {
        anyhow::bail!("Unknown service: {}", service);
    }

    // Initialize Vörðr client
    let vordr_url = std::env::var("VORDR_URL")
        .unwrap_or_else(|_| "http://localhost:9090".to_string());
    let vordr_client = VordrClient::new(vordr_url);

    // Get all containers for this project
    let all_containers = vordr_client.list_containers().await?;

    // Find containers for this service
    let service_containers: Vec<_> = all_containers
        .iter()
        .filter(|c| {
            let name = extract_service_name(&c.container_id, project_name);
            name == service
        })
        .collect();

    if service_containers.is_empty() {
        println!("No containers found for service '{}'", service);
        return Ok(());
    }

    // Get detailed information for each container
    for container in service_containers {
        match vordr_client.inspect_container(&container.container_id).await {
            Ok(details) => {
                match format {
                    "json" => {
                        println!("{}", serde_json::to_string_pretty(&details)?);
                    }
                    "pretty" => {
                        print_pretty_inspect(&service, &details);
                    }
                    _ => {
                        anyhow::bail!("Unknown format: {}", format);
                    }
                }
            }
            Err(e) => {
                eprintln!("Error inspecting {}: {}", container.container_id, e);
            }
        }
    }

    Ok(())
}

/// Print inspection details in human-readable format
fn print_pretty_inspect(service_name: &str, details: &crate::vordr::ContainerDetails) {
    println!();
    println!("Service: {}", service_name);
    println!("Container ID: {}", details.id);
    println!("Image: {}", details.image);
    println!("State: {}", details.state);
    println!("Status: {}", details.status);

    if let Some(created) = &details.created {
        println!("Created: {}", created);
    }

    if let Some(started) = &details.started {
        println!("Started: {}", started);
    }

    // Network information
    if !details.networks.is_empty() {
        println!();
        println!("Networks:");
        for (name, network) in &details.networks {
            println!("  {}:", name);
            println!("    IP Address: {}", network.ip_address);
            if let Some(gateway) = &network.gateway {
                println!("    Gateway: {}", gateway);
            }
        }
    }

    // Port mappings
    if !details.ports.is_empty() {
        println!();
        println!("Ports:");
        for port in &details.ports {
            println!("  {} -> {}", port.host_port, port.container_port);
        }
    }

    // Environment variables
    if !details.environment.is_empty() {
        println!();
        println!("Environment:");
        for (key, value) in &details.environment {
            println!("  {}={}", key, value);
        }
    }

    // Volumes/Mounts
    if !details.mounts.is_empty() {
        println!();
        println!("Mounts:");
        for mount in &details.mounts {
            println!("  {} -> {} ({})", mount.source, mount.destination, mount.mode);
        }
    }

    // Resource usage (if available)
    if let Some(cpu) = &details.cpu_usage {
        println!();
        println!("Resource Usage:");
        println!("  CPU: {}", cpu);
        if let Some(mem) = &details.memory_usage {
            println!("  Memory: {}", mem);
        }
    }

    println!();
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
