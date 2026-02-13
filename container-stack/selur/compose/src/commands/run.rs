// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose run` command implementation

use anyhow::Result;
use std::collections::HashMap;

use crate::compose::ComposeFile;
use crate::ct::CtClient;
use crate::svalinn::{HealthCheckV2, RunConfigV2, RunRequestV2, SvalinnClient};
use crate::vordr::VordrClient;

/// Run `run` command - run a one-off command in a new container
pub async fn run(
    compose: &ComposeFile,
    project_name: &str,
    service: String,
    command: Vec<String>,
    rm: bool,
) -> Result<()> {
    tracing::info!(
        "Running one-off command in service '{}' for project '{}'",
        service,
        project_name
    );

    // Validate service exists
    if !compose.services.contains_key(&service) {
        anyhow::bail!("Unknown service: {}", service);
    }

    if command.is_empty() {
        anyhow::bail!("No command specified");
    }

    let service_config = &compose.services[&service];

    // Initialize clients
    let svalinn_url = std::env::var("SVALINN_URL")
        .unwrap_or_else(|_| "http://localhost:8080".to_string());
    let vordr_url = std::env::var("VORDR_URL")
        .unwrap_or_else(|_| "http://localhost:9090".to_string());

    let svalinn_client = SvalinnClient::new(svalinn_url);
    let vordr_client = VordrClient::new(vordr_url);
    let ct_client = CtClient::new();

    // Generate unique container name
    let container_name = format!("{}_{}_run_{}", project_name, service, uuid::Uuid::new_v4());

    tracing::info!("Creating temporary container: {}", container_name);

    // Convert environment variables
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
        command: Some(command.clone()),
    };

    let image_digest = ct_client.bundle_digest(&service_config.image)?;

    let run_request = RunRequestV2 {
        image_name: service_config.image.clone(),
        image_digest,
        name: Some(container_name.clone()),
        detach: Some(true),
        config: Some(config),
    };

    println!("Creating and starting container...");
    let response = svalinn_client.run_v2(run_request).await?;

    let container_id = response
        .container_id
        .clone()
        .unwrap_or_else(|| container_name.clone());

    tracing::info!("Container created: {}", container_id);

    // Execute command in the container
    println!("Executing command: {}", command.join(" "));

    let output = vordr_client.exec(&container_id, &command).await?;

    // Print output
    if !output.stdout.is_empty() {
        print!("{}", output.stdout);
    }
    if !output.stderr.is_empty() {
        eprint!("{}", output.stderr);
    }

    // Remove container if --rm flag is set
    if rm {
        tracing::info!("Removing temporary container: {}", container_id);
        println!();
        println!("Removing temporary container...");

        if let Err(e) = svalinn_client.stop(&container_id).await {
            eprintln!("Warning: Failed to remove container: {}", e);
        }
    } else {
        println!();
        println!("Container {} left running", container_name);
        println!("Use `selur-compose down` to remove it");
    }

    // Exit with the same code as the command
    if output.exit_code != 0 {
        anyhow::bail!("Command exited with code {}", output.exit_code);
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
