// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose cp` command implementation
//!
//! Copy files to or from running containers via Vordr.

use anyhow::{Context, Result};

use crate::compose::ComposeFile;
use crate::vordr::VordrClient;

/// Run `cp` command - copy files between host and container
///
/// Paths follow the format:
/// - Host path: `/path/to/file`
/// - Container path: `SERVICE:/path/in/container`
pub async fn cp(
    compose: &ComposeFile,
    project_name: &str,
    src: &str,
    dest: &str,
) -> Result<()> {
    tracing::info!("Copying {} -> {}", src, dest);

    let vordr_url = std::env::var("VORDR_URL")
        .unwrap_or_else(|_| "http://localhost:9090".to_string());
    let vordr_client = VordrClient::new(vordr_url);

    // Determine direction of copy
    let src_parts = parse_container_path(src);
    let dest_parts = parse_container_path(dest);

    match (src_parts, dest_parts) {
        (Some((service, container_path)), None) => {
            // Container -> Host
            copy_from_container(
                compose,
                project_name,
                &vordr_client,
                &service,
                &container_path,
                dest,
            )
            .await?;
        }
        (None, Some((service, container_path))) => {
            // Host -> Container
            copy_to_container(
                compose,
                project_name,
                &vordr_client,
                src,
                &service,
                &container_path,
            )
            .await?;
        }
        (Some(_), Some(_)) => {
            anyhow::bail!("Cannot copy between two containers directly. Copy to host first, then to the destination container.");
        }
        (None, None) => {
            anyhow::bail!(
                "At least one path must be a container path (SERVICE:/path). Got: {} -> {}",
                src, dest
            );
        }
    }

    Ok(())
}

/// Copy a file from a container to the host
async fn copy_from_container(
    compose: &ComposeFile,
    project_name: &str,
    vordr_client: &VordrClient,
    service_name: &str,
    container_path: &str,
    host_path: &str,
) -> Result<()> {
    // Validate service exists
    if !compose.services.contains_key(service_name) {
        anyhow::bail!("Unknown service: {}", service_name);
    }

    // Find running container for this service
    let container_id = find_running_container(vordr_client, project_name, service_name).await?;

    println!("Copying from {}:{} to {}", service_name, container_path, host_path);

    let data = vordr_client
        .copy_from_container(&container_id, container_path)
        .await
        .with_context(|| {
            format!(
                "Failed to copy from {}:{} in container {}",
                service_name, container_path, container_id
            )
        })?;

    // Write data to host path
    std::fs::write(host_path, &data)
        .with_context(|| format!("Failed to write to {}", host_path))?;

    println!(
        "Copied {} bytes from {}:{} -> {}",
        data.len(),
        service_name,
        container_path,
        host_path
    );

    Ok(())
}

/// Copy a file from the host to a container
async fn copy_to_container(
    compose: &ComposeFile,
    project_name: &str,
    vordr_client: &VordrClient,
    host_path: &str,
    service_name: &str,
    container_path: &str,
) -> Result<()> {
    // Validate service exists
    if !compose.services.contains_key(service_name) {
        anyhow::bail!("Unknown service: {}", service_name);
    }

    // Validate host file exists
    if !std::path::Path::new(host_path).exists() {
        anyhow::bail!("Source file does not exist: {}", host_path);
    }

    // Find running container for this service
    let container_id = find_running_container(vordr_client, project_name, service_name).await?;

    println!("Copying {} to {}:{}", host_path, service_name, container_path);

    vordr_client
        .copy_to_container(&container_id, host_path, container_path)
        .await
        .with_context(|| {
            format!(
                "Failed to copy {} to {}:{} in container {}",
                host_path, service_name, container_path, container_id
            )
        })?;

    println!(
        "Copied {} -> {}:{}",
        host_path, service_name, container_path
    );

    Ok(())
}

/// Parse a path to check if it is a container path (SERVICE:/path)
/// Returns Some((service_name, path)) if it is, None if it is a host path
fn parse_container_path(path: &str) -> Option<(String, String)> {
    // Container paths look like "service_name:/path/in/container"
    // Host paths start with / or ./ or are relative without a colon
    if let Some(colon_pos) = path.find(':') {
        let service_part = &path[..colon_pos];
        let path_part = &path[colon_pos + 1..];

        // Make sure the service part does not look like a drive letter (Windows)
        // or an absolute path. Service names are alphanumeric + hyphens + underscores.
        if !service_part.is_empty()
            && service_part
                .chars()
                .all(|c| c.is_alphanumeric() || c == '-' || c == '_')
            && path_part.starts_with('/')
        {
            return Some((service_part.to_string(), path_part.to_string()));
        }
    }

    None
}

/// Find a running container for a given service name
async fn find_running_container(
    vordr_client: &VordrClient,
    project_name: &str,
    service_name: &str,
) -> Result<String> {
    let all_containers = vordr_client.list_containers().await?;

    let container_id = all_containers
        .iter()
        .find(|c| {
            let name = extract_service_name(&c.container_id, project_name);
            name == service_name && c.state == "running"
        })
        .map(|c| c.container_id.clone())
        .ok_or_else(|| {
            anyhow::anyhow!(
                "No running container found for service '{}'",
                service_name
            )
        })?;

    Ok(container_id)
}

/// Extract service name from container ID
fn extract_service_name(container_id: &str, project_name: &str) -> String {
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
    fn test_parse_container_path() {
        assert_eq!(
            parse_container_path("web:/var/log/app.log"),
            Some(("web".to_string(), "/var/log/app.log".to_string()))
        );
        assert_eq!(
            parse_container_path("my-service:/etc/config"),
            Some(("my-service".to_string(), "/etc/config".to_string()))
        );
        assert_eq!(parse_container_path("/tmp/local-file"), None);
        assert_eq!(parse_container_path("./relative/path"), None);
        assert_eq!(parse_container_path("just-a-name"), None);
    }
}
