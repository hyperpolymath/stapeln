// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose wait` command implementation
//!
//! Waits for containers to stop and returns their exit codes.

use anyhow::Result;

use crate::compose::ComposeFile;
use crate::vordr::VordrClient;

/// Run `wait` command - wait for containers to stop, return exit codes
pub async fn wait_cmd(
    compose: &ComposeFile,
    project_name: &str,
    services: Vec<String>,
) -> Result<()> {
    tracing::info!("Waiting for services to stop in project '{}'", project_name);

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

    // Find running containers for the target services
    let all_containers = vordr_client.list_containers().await?;

    let mut wait_targets: Vec<(String, String)> = Vec::new();

    for service_name in &target_services {
        let containers: Vec<_> = all_containers
            .iter()
            .filter(|c| {
                let name = extract_service_name(&c.container_id, project_name);
                name == *service_name
            })
            .collect();

        if containers.is_empty() {
            eprintln!("No containers found for service '{}'", service_name);
            continue;
        }

        for container in containers {
            wait_targets.push((service_name.clone(), container.container_id.clone()));
        }
    }

    if wait_targets.is_empty() {
        println!("No containers to wait for");
        return Ok(());
    }

    println!("Waiting for {} container(s) to stop...", wait_targets.len());
    println!();

    // Wait for each container concurrently
    let mut handles = Vec::new();

    for (service_name, container_id) in wait_targets {
        let vordr_url_clone = std::env::var("VORDR_URL")
            .unwrap_or_else(|_| "http://localhost:9090".to_string());
        let client = VordrClient::new(vordr_url_clone);

        let handle = tokio::spawn(async move {
            match client.wait_container(&container_id).await {
                Ok(result) => {
                    println!(
                        "{} ({}) exited with code {}",
                        service_name,
                        &container_id[..std::cmp::min(12, container_id.len())],
                        result.exit_code
                    );
                    if let Some(ref error) = result.error {
                        eprintln!("  Error: {}", error);
                    }
                    result.exit_code
                }
                Err(e) => {
                    eprintln!("Failed to wait for {} ({}): {}", service_name, container_id, e);
                    -1
                }
            }
        });

        handles.push(handle);
    }

    // Collect results
    let mut max_exit_code = 0;
    for handle in handles {
        match handle.await {
            Ok(exit_code) => {
                if exit_code > max_exit_code {
                    max_exit_code = exit_code;
                }
            }
            Err(e) => {
                eprintln!("Task join error: {}", e);
                max_exit_code = -1;
            }
        }
    }

    println!();

    if max_exit_code != 0 {
        anyhow::bail!("One or more containers exited with non-zero code (max: {})", max_exit_code);
    }

    println!("All containers exited successfully");

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
