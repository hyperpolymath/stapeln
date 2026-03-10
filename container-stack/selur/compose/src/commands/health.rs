// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose health` command implementation
//!
//! Shows health check status for all services with configured health checks.

use anyhow::Result;
use prettytable::{Cell, Row, Table, format};

use crate::compose::ComposeFile;
use crate::vordr::VordrClient;

/// Run `health` command - show health check status for all services
pub async fn health(
    compose: &ComposeFile,
    project_name: &str,
    services: Vec<String>,
) -> Result<()> {
    tracing::info!("Checking health status for project '{}'", project_name);

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

    let mut table = Table::new();
    table.set_format(*format::consts::FORMAT_BOX_CHARS);

    table.add_row(Row::new(vec![
        Cell::new("SERVICE"),
        Cell::new("CONTAINER"),
        Cell::new("STATE"),
        Cell::new("HEALTH"),
        Cell::new("FAILURES"),
        Cell::new("LAST CHECK"),
    ]));

    let mut found = 0;

    for service_name in &target_services {
        let service = &compose.services[service_name];

        // Find containers for this service
        let service_containers: Vec<_> = all_containers
            .iter()
            .filter(|c| {
                let name = extract_service_name(&c.container_id, project_name);
                name == *service_name
            })
            .collect();

        if service_containers.is_empty() {
            // Service has no running containers
            let has_healthcheck = service.healthcheck.is_some();
            table.add_row(Row::new(vec![
                Cell::new(service_name),
                Cell::new("-"),
                Cell::new("not running"),
                Cell::new(if has_healthcheck { "N/A" } else { "no healthcheck" }),
                Cell::new("-"),
                Cell::new("-"),
            ]));
            found += 1;
            continue;
        }

        for container in service_containers {
            let short_id = if container.container_id.len() >= 12 {
                &container.container_id[..12]
            } else {
                &container.container_id
            };

            if service.healthcheck.is_none() {
                // No health check configured
                table.add_row(Row::new(vec![
                    Cell::new(service_name),
                    Cell::new(short_id),
                    Cell::new(&container.state),
                    Cell::new("no healthcheck"),
                    Cell::new("-"),
                    Cell::new("-"),
                ]));
                found += 1;
                continue;
            }

            // Fetch health status from Vordr
            match vordr_client.get_health(&container.container_id).await {
                Ok(health_status) => {
                    let failing_streak = health_status
                        .failing_streak
                        .map(|n| n.to_string())
                        .unwrap_or_else(|| "0".to_string());

                    let last_check = health_status
                        .log
                        .last()
                        .map(|entry| {
                            format!(
                                "exit={} ({})",
                                entry.exit_code,
                                &entry.end
                            )
                        })
                        .unwrap_or_else(|| "none yet".to_string());

                    let health_display = match health_status.status.as_str() {
                        "healthy" => "healthy",
                        "unhealthy" => "UNHEALTHY",
                        "starting" => "starting...",
                        _ => &health_status.status,
                    };

                    table.add_row(Row::new(vec![
                        Cell::new(service_name),
                        Cell::new(short_id),
                        Cell::new(&container.state),
                        Cell::new(health_display),
                        Cell::new(&failing_streak),
                        Cell::new(&last_check),
                    ]));
                }
                Err(e) => {
                    tracing::debug!("Failed to get health for {}: {}", container.container_id, e);
                    table.add_row(Row::new(vec![
                        Cell::new(service_name),
                        Cell::new(short_id),
                        Cell::new(&container.state),
                        Cell::new("unavailable"),
                        Cell::new("-"),
                        Cell::new(&format!("error: {}", e)),
                    ]));
                }
            }

            found += 1;
        }
    }

    table.printstd();

    println!();
    println!("Checked {} service(s)", found);

    // Count services with health checks configured
    let with_healthcheck: Vec<&String> = target_services
        .iter()
        .filter(|s| compose.services[*s].healthcheck.is_some())
        .collect();

    if with_healthcheck.is_empty() {
        println!("No services have health checks configured");
    } else {
        println!(
            "{} of {} service(s) have health checks",
            with_healthcheck.len(),
            target_services.len()
        );
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
