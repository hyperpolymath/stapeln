// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose exec` command implementation

use anyhow::Result;

use crate::compose::ComposeFile;
use crate::vordr::VordrClient;

/// Run `exec` command - execute command in a running service
pub async fn exec(
    compose: &ComposeFile,
    project_name: &str,
    service: String,
    command: Vec<String>,
) -> Result<()> {
    tracing::info!(
        "Executing command in service '{}' for project '{}'",
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

    // Initialize Vörðr client
    let vordr_url = std::env::var("VORDR_URL")
        .unwrap_or_else(|_| "http://localhost:9090".to_string());
    let vordr_client = VordrClient::new(vordr_url);

    // Find running container for this service
    let all_containers = vordr_client.list_containers().await?;
    let container_id = all_containers
        .iter()
        .find(|c| {
            let name = extract_service_name(&c.container_id, project_name);
            name == service && c.state == "running"
        })
        .map(|c| c.container_id.clone())
        .ok_or_else(|| anyhow::anyhow!("No running container found for service '{}'", service))?;

    tracing::info!("Found container: {}", container_id);

    // Execute command in container
    println!(
        "Executing {} in {}...",
        command.join(" "),
        service
    );

    let output = vordr_client.exec(&container_id, &command).await?;

    // Print output
    if !output.stdout.is_empty() {
        print!("{}", output.stdout);
    }
    if !output.stderr.is_empty() {
        eprint!("{}", output.stderr);
    }

    // Exit with the same code as the command
    if output.exit_code != 0 {
        anyhow::bail!("Command exited with code {}", output.exit_code);
    }

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
        assert_eq!(extract_service_name("myapp_web_1", "myapp"), "web");
        assert_eq!(extract_service_name("myapp_api_2", "myapp"), "api");
    }
}
