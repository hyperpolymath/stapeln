// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose up` command implementation

use anyhow::{Context, Result};
use std::collections::HashMap;
use std::time::Duration;
use tokio::time::sleep;

use crate::compose::ComposeFile;
use crate::ct::CtClient;
use crate::graph::build_dependency_graph;
use crate::svalinn::{HealthCheckV2, RunConfigV2, RunRequestV2, SvalinnClient};
use crate::vordr_mcp::VordrMcpClient;

/// Run `up` command - start all services
pub async fn up(
    compose: &ComposeFile,
    project_name: &str,
    detach: bool,
    build: bool,
    services: Vec<String>,
) -> Result<()> {
    tracing::info!("Starting services for project '{}'", project_name);

    // Filter services if specific ones requested
    let target_services: Vec<String> = if services.is_empty() {
        compose.services.keys().cloned().collect()
    } else {
        services
    };

    // Build dependency order
    let deployment_order = build_dependency_graph(compose)?;
    let ordered_services: Vec<String> = deployment_order
        .into_iter()
        .filter(|s| target_services.contains(s))
        .collect();

    tracing::info!("Deployment order: {:?}", ordered_services);

    // Initialize clients
    let ct_client = CtClient::new();
    let svalinn_url =
        std::env::var("SVALINN_URL").unwrap_or_else(|_| "http://localhost:8080".to_string());
    let svalinn_client = SvalinnClient::new(svalinn_url);
    let vordr_mcp_url =
        std::env::var("VORDR_MCP_URL").unwrap_or_else(|_| "http://localhost:8081".to_string());
    let vordr_mcp = VordrMcpClient::new(vordr_mcp_url);

    // Track deployed service IDs
    let mut deployed: HashMap<String, String> = HashMap::new();

    // Create networks and volumes upfront (fail-closed)
    for (network_name, network) in &compose.networks {
        let driver = network.driver.as_str();
        let subnet = network.ipam.as_ref().map(|ipam| ipam.subnet.clone());
        vordr_mcp
            .create_network(network_name, driver, subnet)
            .await
            .with_context(|| format!("Failed to create network {}", network_name))?;
    }

    for (volume_name, volume) in &compose.volumes {
        vordr_mcp
            .create_volume(volume_name, &volume.driver)
            .await
            .with_context(|| format!("Failed to create volume {}", volume_name))?;
    }

    // Deploy each service in dependency order
    for service_name in ordered_services {
        let service = &compose.services[&service_name];

        tracing::info!("📦 Deploying service '{}'", service_name);

        // Step 1: Build if requested
        if build {
            tracing::info!("  Building {}", service.image);
            // TODO: Implement build - pack source into .ctp
            // ct_client.pack(&source_dir, &bundle_path).await?;
        }

        // Step 2: Verify .ctp bundle signature
        tracing::info!("  Verifying {}", service.image);
        ct_client
            .verify(&service.image)
            .await
            .with_context(|| format!("Verification failed for {}", service.image))?;

        // Step 3: Deploy via Svalinn v2
        tracing::info!("  Deploying via Svalinn v2...");

        // Build environment map
        let mut environment = HashMap::new();
        for (key, value) in &service.environment {
            match value {
                crate::compose::EnvValue::String(s) => {
                    environment.insert(key.clone(), s.clone());
                }
                crate::compose::EnvValue::Secret { secret } => {
                    if dev_secrets_enabled() {
                        let secret_path = resolve_secret_file(compose, secret)?;
                        let secret_value = std::fs::read_to_string(&secret_path)
                            .with_context(|| format!("Failed to read secret {}", secret))?;
                        tracing::warn!(
                            "DEV MODE: injecting secret '{}' into env for {}",
                            secret,
                            service_name
                        );
                        environment.insert(key.clone(), secret_value.trim_end().to_string());
                    } else {
                        anyhow::bail!(
                            "Secret '{}' used in env for '{}' (set SELUR_DEV_SECRETS=1 to allow)",
                            secret,
                            service_name
                        );
                    }
                }
            }
        }

        // Map secrets to mounts
        let mut secret_mounts: Vec<String> = Vec::new();
        for secret in &service.secrets {
            let secret_path = resolve_secret_file(compose, secret)?;
            let mount = format!("{}:/run/secrets/{}:ro", secret_path, secret);
            secret_mounts.push(mount);
        }

        // Compose volumes (include secret mounts)
        let mut volumes = service.volumes.clone();
        volumes.extend(secret_mounts.iter().cloned());

        let healthcheck = service.healthcheck.as_ref().map(|hc| HealthCheckV2 {
            test: hc.test.clone(),
            interval: hc.interval.clone(),
            timeout: hc.timeout.clone(),
            retries: hc.retries,
            start_period: hc.start_period.clone(),
        });

        let restart = if service.restart == "no" {
            None
        } else {
            Some(service.restart.clone())
        };

        let config = RunConfigV2 {
            env: environment,
            ports: service.ports.clone(),
            volumes,
            networks: service.networks.clone(),
            secrets: service.secrets.clone(),
            restart,
            healthcheck,
            command: service.command.clone(),
        };

        let image_digest = ct_client
            .bundle_digest(&service.image)
            .with_context(|| format!("Failed to digest {}", service.image))?;

        let run_request = RunRequestV2 {
            image_name: service.image.clone(),
            image_digest,
            name: Some(service_name.clone()),
            detach: Some(true),
            config: Some(config),
        };

        let response = svalinn_client
            .run_v2(run_request)
            .await
            .with_context(|| format!("Failed to deploy {}", service_name))?;

        let container_id = response
            .container_id
            .clone()
            .unwrap_or_else(|| service_name.clone());
        deployed.insert(service_name.clone(), container_id.clone());
        tracing::info!("  ✓ Deployed as {}", container_id);

        // Step 4: Wait for health check (if configured)
        if let Some(healthcheck) = &service.healthcheck {
            tracing::info!("  Waiting for health check...");

            let interval_secs = parse_duration(&healthcheck.interval)?;
            let timeout_secs = parse_duration(&healthcheck.timeout)?;
            let max_retries = healthcheck.retries;

            let mut retries = 0;
            loop {
                sleep(Duration::from_secs(interval_secs)).await;

                match tokio::time::timeout(
                    Duration::from_secs(timeout_secs),
                    svalinn_client.health_check(&container_id),
                )
                .await
                {
                    Ok(Ok(true)) => {
                        tracing::info!("  ✓ Health check passed");
                        break;
                    }
                    Ok(Ok(false)) | Ok(Err(_)) | Err(_) => {
                        retries += 1;
                        if retries >= max_retries {
                            anyhow::bail!(
                                "Health check failed for {} after {} retries",
                                service_name,
                                max_retries
                            );
                        }
                        tracing::warn!("  Health check failed, retry {}/{}", retries, max_retries);
                    }
                }
            }
        }

        println!("✓ {} deployed successfully", service_name);
    }

    // Summary
    println!();
    println!("🚀 All services deployed!");
    println!("   Project: {}", project_name);
    println!("   Services: {}", deployed.len());

    if !detach {
        println!();
        println!("Press Ctrl+C to stop all services");
        // TODO: Stream logs in non-detached mode
        tokio::signal::ctrl_c().await?;
        println!("\nStopping services...");
        // TODO: Call down command
    }

    Ok(())
}

fn dev_secrets_enabled() -> bool {
    std::env::var("SELUR_DEV_SECRETS")
        .map(|v| v == "1" || v.eq_ignore_ascii_case("true"))
        .unwrap_or(false)
}

fn resolve_secret_file(compose: &ComposeFile, name: &str) -> Result<String> {
    let secret = compose
        .secrets
        .get(name)
        .with_context(|| format!("Unknown secret '{}'", name))?;

    if secret.external {
        anyhow::bail!("External secrets not supported: {}", name);
    }

    if let Some(path) = &secret.file {
        Ok(path.to_string_lossy().to_string())
    } else {
        anyhow::bail!("Secret '{}' missing file path", name);
    }
}

/// Parse duration string like "30s" into seconds
fn parse_duration(s: &str) -> Result<u64> {
    let s = s.trim();
    if let Some(num) = s.strip_suffix('s') {
        Ok(num.parse()?)
    } else if let Some(num) = s.strip_suffix('m') {
        Ok(num.parse::<u64>()? * 60)
    } else if let Some(num) = s.strip_suffix('h') {
        Ok(num.parse::<u64>()? * 3600)
    } else {
        Ok(s.parse()?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_duration() {
        assert_eq!(parse_duration("30s").unwrap(), 30);
        assert_eq!(parse_duration("2m").unwrap(), 120);
        assert_eq!(parse_duration("1h").unwrap(), 3600);
    }
}
