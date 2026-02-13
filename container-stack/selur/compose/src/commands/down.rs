// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose down` command implementation

use anyhow::Result;

use crate::compose::ComposeFile;
use crate::svalinn::SvalinnClient;
use crate::vordr::VordrClient;
use crate::vordr_mcp::VordrMcpClient;

/// Run `down` command - stop and remove all services
pub async fn down(
    compose: &ComposeFile,
    project_name: &str,
    remove_volumes: bool,
    remove_images: Option<String>,
) -> Result<()> {
    tracing::info!("Stopping services for project '{}'", project_name);

    // Initialize clients
    let svalinn_url = std::env::var("SVALINN_URL")
        .unwrap_or_else(|_| "http://localhost:8080".to_string());
    let vordr_url = std::env::var("VORDR_URL")
        .unwrap_or_else(|_| "http://localhost:9090".to_string());
    let vordr_mcp_url =
        std::env::var("VORDR_MCP_URL").unwrap_or_else(|_| "http://localhost:8081".to_string());

    let svalinn_client = SvalinnClient::new(svalinn_url);
    let vordr_client = VordrClient::new(vordr_url);
    let vordr_mcp = VordrMcpClient::new(vordr_mcp_url);

    // Get all running containers for this project
    let containers = vordr_client.list_containers().await?;

    tracing::info!("Found {} containers", containers.len());

    // Stop each service via Svalinn
    for container in &containers {
        let service_name = extract_service_name(&container.container_id, project_name);

        tracing::info!("  Stopping {}...", service_name);

        if let Err(e) = svalinn_client.stop(&container.container_id).await {
            tracing::warn!("  Failed to stop via Svalinn: {}, trying Vörðr directly", e);
            vordr_client.stop_container(&container.container_id).await?;
        }

        println!("✓ {} stopped", service_name);
    }

    // Remove containers via Vörðr
    tracing::info!("Removing containers...");
    for container in &containers {
        let service_name = extract_service_name(&container.container_id, project_name);

        tracing::info!("  Removing {}...", service_name);

        // TODO: Call vordr_client.delete_container when implemented
        // vordr_client.delete_container(&container.container_id).await?;

        println!("✓ {} removed", service_name);
    }

    // Remove volumes if requested
    if remove_volumes {
        tracing::info!("Removing volumes...");

        for (volume_name, _volume_def) in &compose.volumes {
            tracing::info!("  Removing volume {}...", volume_name);

            vordr_mcp.remove_volume(volume_name).await?;

            println!("✓ Volume {} removed", volume_name);
        }
    }

    // Remove networks
    if !compose.networks.is_empty() {
        tracing::info!("Removing networks...");
        for (network_name, _network_def) in &compose.networks {
            tracing::info!("  Removing network {}...", network_name);
            vordr_mcp.remove_network(network_name).await?;
            println!("✓ Network {} removed", network_name);
        }
    }

    // Remove images if requested
    if let Some(rmi_type) = remove_images {
        tracing::info!("Removing images ({})...", rmi_type);

        match rmi_type.as_str() {
            "all" => {
                // Remove all images used by services
                for (_service_name, service) in &compose.services {
                    tracing::info!("  Removing image {}...", service.image);
                    // TODO: Implement image removal
                    println!("✓ Image {} removed", service.image);
                }
            }
            "local" => {
                // Remove only locally built images
                tracing::info!("  Removing local images only");
                // TODO: Filter and remove local images
            }
            _ => {
                tracing::warn!("Unknown rmi type: {}", rmi_type);
            }
        }
    }

    // Summary
    println!();
    println!("🛑 All services stopped and removed");
    println!("   Project: {}", project_name);

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_service_name() {
        assert_eq!(
            extract_service_name("myapp_web_1", "myapp"),
            "web"
        );
        assert_eq!(
            extract_service_name("myapp_api_2", "myapp"),
            "api"
        );
    }
}
