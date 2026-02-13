// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose logs` command implementation

use anyhow::Result;
use std::collections::HashMap;
use tokio::time::{sleep, Duration};

use crate::compose::ComposeFile;
use crate::vordr::VordrClient;

/// Run `logs` command - view service logs
pub async fn logs(
    compose: &ComposeFile,
    project_name: &str,
    follow: bool,
    tail: Option<usize>,
    services: Vec<String>,
) -> Result<()> {
    tracing::info!("Fetching logs for project '{}'", project_name);

    // Initialize Vörðr client
    let vordr_url = std::env::var("VORDR_URL")
        .unwrap_or_else(|_| "http://localhost:9090".to_string());
    let vordr_client = VordrClient::new(vordr_url);

    // Determine which services to show logs for
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

    tracing::info!("Showing logs for {} service(s)", target_services.len());

    // Get running containers for these services
    let all_containers = vordr_client.list_containers().await?;
    let mut service_containers: HashMap<String, Vec<String>> = HashMap::new();

    for container in all_containers {
        let service_name = extract_service_name(&container.container_id, project_name);
        if target_services.contains(&service_name) {
            service_containers
                .entry(service_name)
                .or_insert_with(Vec::new)
                .push(container.container_id);
        }
    }

    if service_containers.is_empty() {
        println!("No running containers found for the specified services");
        return Ok(());
    }

    // If follow mode, continuously fetch logs
    if follow {
        println!("Following logs (Ctrl+C to stop)...\n");
        loop {
            for (service_name, containers) in &service_containers {
                for container_id in containers {
                    match fetch_logs(&vordr_client, container_id, tail, service_name).await {
                        Ok(logs) => {
                            if !logs.is_empty() {
                                print!("{}", logs);
                            }
                        }
                        Err(e) => {
                            tracing::warn!("Failed to fetch logs for {}: {}", container_id, e);
                        }
                    }
                }
            }
            sleep(Duration::from_secs(1)).await;
        }
    } else {
        // One-time log fetch
        for (service_name, containers) in &service_containers {
            for container_id in containers {
                match fetch_logs(&vordr_client, container_id, tail, service_name).await {
                    Ok(logs) => {
                        if !logs.is_empty() {
                            print!("{}", logs);
                        }
                    }
                    Err(e) => {
                        eprintln!("Error fetching logs for {}: {}", container_id, e);
                    }
                }
            }
        }
    }

    Ok(())
}

/// Fetch logs from a container
async fn fetch_logs(
    client: &VordrClient,
    container_id: &str,
    tail: Option<usize>,
    service_name: &str,
) -> Result<String> {
    tracing::debug!("Fetching logs for container {}", container_id);

    // TODO: Implement actual log streaming via Vörðr API
    // For now, simulate with placeholder
    let logs = client.get_logs(container_id, tail).await?;

    // Format logs with service name prefix
    let formatted_logs: Vec<String> = logs
        .lines()
        .map(|line| format!("{} | {}", service_name, line))
        .collect();

    Ok(formatted_logs.join("\n") + "\n")
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
        assert_eq!(
            extract_service_name("demo_frontend_3", "demo"),
            "frontend"
        );
    }
}
