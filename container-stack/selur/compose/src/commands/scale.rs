// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose scale` command implementation

use anyhow::Result;
use std::collections::HashMap;

use crate::compose::ComposeFile;
use crate::ct::CtClient;
use crate::svalinn::{HealthCheckV2, RunConfigV2, RunRequestV2, SvalinnClient};
use crate::vordr::VordrClient;

/// Run `scale` command - scale service to N replicas
pub async fn scale(
    compose: &ComposeFile,
    project_name: &str,
    service_specs: Vec<String>,
) -> Result<()> {
    tracing::info!("Scaling services for project '{}'", project_name);

    if service_specs.is_empty() {
        anyhow::bail!("No services specified. Usage: selur-compose scale SERVICE=REPLICAS");
    }

    // Parse service=replicas pairs
    let mut scale_map: HashMap<String, usize> = HashMap::new();
    for spec in &service_specs {
        let parts: Vec<&str> = spec.split('=').collect();
        if parts.len() != 2 {
            anyhow::bail!("Invalid format: '{}'. Expected SERVICE=REPLICAS", spec);
        }

        let service = parts[0].to_string();
        let replicas = parts[1]
            .parse::<usize>()
            .map_err(|_| anyhow::anyhow!("Invalid replica count: '{}'", parts[1]))?;

        // Validate service exists
        if !compose.services.contains_key(&service) {
            anyhow::bail!("Unknown service: {}", service);
        }

        scale_map.insert(service, replicas);
    }

    // Initialize clients
    let svalinn_url = std::env::var("SVALINN_URL")
        .unwrap_or_else(|_| "http://localhost:8080".to_string());
    let vordr_url = std::env::var("VORDR_URL")
        .unwrap_or_else(|_| "http://localhost:9090".to_string());

    let svalinn_client = SvalinnClient::new(svalinn_url);
    let vordr_client = VordrClient::new(vordr_url);
    let ct_client = CtClient::new();

    // Get current containers
    let all_containers = vordr_client.list_containers().await?;

    // Group containers by service
    let mut current_replicas: HashMap<String, Vec<String>> = HashMap::new();
    for container in all_containers {
        let service_name = extract_service_name(&container.container_id, project_name);
        if scale_map.contains_key(&service_name) {
            current_replicas
                .entry(service_name)
                .or_insert_with(Vec::new)
                .push(container.container_id);
        }
    }

    // Scale each service
    for (service, target_replicas) in &scale_map {
        let current_count = current_replicas.get(service).map(|v| v.len()).unwrap_or(0);

        tracing::info!(
            "Service '{}': {} → {} replicas",
            service,
            current_count,
            target_replicas
        );

        if *target_replicas > current_count {
            // Scale up
            let to_add = target_replicas - current_count;
            println!("Scaling up {} by {} replicas...", service, to_add);

            for i in 0..to_add {
                let replica_num = current_count + i + 1;
                let container_name = format!("{}_{}_{}", project_name, service, replica_num);

                tracing::info!("  Creating replica: {}", container_name);

                // Get service config
                let service_config = &compose.services[service];

                // Build environment map
                let mut env_map: HashMap<String, String> = HashMap::new();
                for (key, value) in &service_config.environment {
                    match value {
                        crate::compose::EnvValue::String(s) => {
                            env_map.insert(key.clone(), s.clone());
                        }
                        crate::compose::EnvValue::Secret { secret } => {
                            if dev_secrets_enabled() {
                                let secret_path = resolve_secret_file(compose, secret)?;
                                let secret_value = std::fs::read_to_string(&secret_path)?;
                                tracing::warn!(
                                    "DEV MODE: injecting secret '{}' into env for {}",
                                    secret,
                                    container_name
                                );
                                env_map.insert(key.clone(), secret_value.trim_end().to_string());
                            } else {
                                anyhow::bail!(
                                    "Secret '{}' used in env for '{}' (set SELUR_DEV_SECRETS=1 to allow)",
                                    secret,
                                    container_name
                                );
                            }
                        }
                    }
                }

                // Map secrets to mounts
                let mut secret_mounts: Vec<String> = Vec::new();
                for secret in &service_config.secrets {
                    let secret_path = resolve_secret_file(compose, secret)?;
                    let mount = format!("{}:/run/secrets/{}:ro", secret_path, secret);
                    secret_mounts.push(mount);
                }

                let mut volumes = service_config.volumes.clone();
                volumes.extend(secret_mounts.iter().cloned());

                let healthcheck = service_config.healthcheck.as_ref().map(|hc| HealthCheckV2 {
                    test: hc.test.clone(),
                    interval: hc.interval.clone(),
                    timeout: hc.timeout.clone(),
                    retries: hc.retries,
                    start_period: hc.start_period.clone(),
                });

                let config = RunConfigV2 {
                    env: env_map,
                    ports: service_config.ports.clone(),
                    volumes,
                    networks: service_config.networks.clone(),
                    secrets: service_config.secrets.clone(),
                    healthcheck,
                    command: service_config.command.clone(),
                    ..Default::default()
                };

                let image_digest = ct_client.bundle_digest(&service_config.image)?;

                let run_request = RunRequestV2 {
                    image_name: service_config.image.clone(),
                    image_digest,
                    name: Some(container_name.clone()),
                    detach: Some(true),
                    config: Some(config),
                };

                match svalinn_client.run_v2(run_request).await {
                    Ok(_) => {
                        println!("  ✓ Replica {} created", replica_num);
                    }
                    Err(e) => {
                        eprintln!("  ✗ Failed to create replica {}: {}", replica_num, e);
                    }
                }
            }
        } else if *target_replicas < current_count {
            // Scale down
            let to_remove = current_count - target_replicas;
            println!("Scaling down {} by {} replicas...", service, to_remove);

            // Remove the last N replicas
            if let Some(containers) = current_replicas.get(service) {
                let to_remove_containers = &containers[containers.len() - to_remove..];

                for container_id in to_remove_containers {
                    tracing::info!("  Removing container: {}", container_id);

                    // Stop via Svalinn
                    match svalinn_client.stop(container_id).await {
                        Ok(_) => {
                            // Remove via Vörðr
                            // TODO: Implement delete_container when Vörðr API is ready
                            println!("  ✓ Replica stopped: {}", container_id);
                        }
                        Err(e) => {
                            eprintln!("  ✗ Failed to stop {}: {}", container_id, e);
                        }
                    }
                }
            }
        } else {
            println!("Service '{}' already at {} replicas", service, target_replicas);
        }
    }

    println!();
    println!("✓ Scaling complete");

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
        assert_eq!(extract_service_name("myapp_web_1", "myapp"), "web");
        assert_eq!(extract_service_name("myapp_api_2", "myapp"), "api");
    }

    #[test]
    fn test_parse_scale_spec() {
        let specs = vec!["web=3".to_string(), "api=5".to_string()];
        let mut map = HashMap::new();

        for spec in specs {
            let parts: Vec<&str> = spec.split('=').collect();
            if parts.len() == 2 {
                if let Ok(count) = parts[1].parse::<usize>() {
                    map.insert(parts[0].to_string(), count);
                }
            }
        }

        assert_eq!(map.get("web"), Some(&3));
        assert_eq!(map.get("api"), Some(&5));
    }
}
