// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose create` command implementation
//!
//! Creates containers for all services without starting them. Useful for
//! pre-provisioning before a coordinated start.

use anyhow::{Context, Result};
use std::collections::HashMap;

use crate::compose::ComposeFile;
use crate::ct::CtClient;
use crate::graph::build_dependency_graph;
use crate::svalinn::{HealthCheckV2, RunConfigV2, RunRequestV2, SvalinnClient};
use crate::vordr_mcp::VordrMcpClient;

/// Run `create` command - create containers without starting them
pub async fn create(
    compose: &ComposeFile,
    project_name: &str,
    services: Vec<String>,
) -> Result<()> {
    tracing::info!("Creating containers for project '{}'", project_name);

    // Filter services
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

    // Build dependency order
    let deployment_order = build_dependency_graph(compose)?;
    let ordered_services: Vec<String> = deployment_order
        .into_iter()
        .filter(|s| target_services.contains(s))
        .collect();

    // Initialize clients
    let ct_client = CtClient::new();
    let svalinn_url =
        std::env::var("SVALINN_URL").unwrap_or_else(|_| "http://localhost:8080".to_string());
    let svalinn_client = SvalinnClient::new(svalinn_url);
    let vordr_mcp_url =
        std::env::var("VORDR_MCP_URL").unwrap_or_else(|_| "http://localhost:8081".to_string());
    let vordr_mcp = VordrMcpClient::new(vordr_mcp_url);

    // Ensure networks and volumes exist
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

    let mut created = 0;

    for service_name in ordered_services {
        let service = &compose.services[&service_name];

        tracing::info!("Creating container for service '{}'", service_name);

        // Verify bundle
        ct_client
            .verify(&service.image)
            .await
            .with_context(|| format!("Verification failed for {}", service.image))?;

        // Build environment
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

        // Map secrets to volume mounts
        let mut secret_mounts: Vec<String> = Vec::new();
        for secret in &service.secrets {
            let secret_path = resolve_secret_file(compose, secret)?;
            let mount = format!("{}:/run/secrets/{}:ro", secret_path, secret);
            secret_mounts.push(mount);
        }

        let mut volumes = service.volumes.clone();
        volumes.extend(secret_mounts);

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

        // Create but do not start (detach=false signals create-only via Svalinn)
        let run_request = RunRequestV2 {
            image_name: service.image.clone(),
            image_digest,
            name: Some(format!("{}_{}_1", project_name, service_name)),
            detach: Some(false),
            config: Some(config),
        };

        svalinn_client
            .run_v2(run_request)
            .await
            .with_context(|| format!("Failed to create container for {}", service_name))?;

        println!("Created {}", service_name);
        created += 1;
    }

    println!();
    println!("{} container(s) created (not started)", created);
    println!("Use `selur-compose start` to start them");

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
        .ok_or_else(|| anyhow::anyhow!("Unknown secret '{}'", name))?;

    if secret.external {
        anyhow::bail!("External secrets not supported: {}", name);
    }

    if let Some(path) = &secret.file {
        Ok(path.to_string_lossy().to_string())
    } else {
        anyhow::bail!("Secret '{}' missing file path", name);
    }
}
