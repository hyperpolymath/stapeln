// SPDX-License-Identifier: PMPL-1.0-or-later
//! Vörðr orchestrator client

use anyhow::Result;
use serde::{Deserialize, Serialize};

/// Vörðr HTTP client (or Elixir port protocol)
pub struct VordrClient {
    base_url: String,
    client: reqwest::Client,
}

#[derive(Debug, Serialize)]
pub struct ContainerRequest {
    pub image: String,
    pub name: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ContainerResponse {
    pub container_id: String,
    pub state: String,
}

impl VordrClient {
    /// Create new Vörðr client
    pub fn new(base_url: impl Into<String>) -> Self {
        Self {
            base_url: base_url.into(),
            client: reqwest::Client::new(),
        }
    }

    /// Create container via Vörðr
    pub async fn create_container(&self, request: ContainerRequest) -> Result<ContainerResponse> {
        tracing::info!("Creating container {} via Vörðr", request.name);

        let response = self
            .client
            .post(format!("{}/api/v1/containers", self.base_url))
            .json(&request)
            .send()
            .await?
            .json::<ContainerResponse>()
            .await?;

        Ok(response)
    }

    /// Start container
    pub async fn start_container(&self, container_id: &str) -> Result<()> {
        tracing::info!("Starting container {} via Vörðr", container_id);

        let response = self
            .client
            .post(format!(
                "{}/api/v1/containers/{}/start",
                self.base_url, container_id
            ))
            .send()
            .await?;

        if !response.status().is_success() {
            anyhow::bail!("Failed to start container {}", container_id);
        }

        Ok(())
    }

    /// Stop container
    pub async fn stop_container(&self, container_id: &str) -> Result<()> {
        tracing::info!("Stopping container {} via Vörðr", container_id);

        let response = self
            .client
            .post(format!(
                "{}/api/v1/containers/{}/stop",
                self.base_url, container_id
            ))
            .send()
            .await?;

        if !response.status().is_success() {
            anyhow::bail!("Failed to stop container {}", container_id);
        }

        Ok(())
    }

    /// List containers
    pub async fn list_containers(&self) -> Result<Vec<ContainerResponse>> {
        let response = self
            .client
            .get(format!("{}/api/v1/containers", self.base_url))
            .send()
            .await?
            .json::<Vec<ContainerResponse>>()
            .await?;

        Ok(response)
    }

    /// Get container logs
    pub async fn get_logs(&self, container_id: &str, tail: Option<usize>) -> Result<String> {
        tracing::debug!("Getting logs for container {}", container_id);

        let mut url = format!("{}/api/v1/containers/{}/logs", self.base_url, container_id);
        if let Some(n) = tail {
            url = format!("{}?tail={}", url, n);
        }

        let response = self
            .client
            .get(&url)
            .send()
            .await?
            .text()
            .await?;

        Ok(response)
    }

    /// Execute command in container
    pub async fn exec(&self, container_id: &str, command: &[String]) -> Result<ExecOutput> {
        tracing::info!("Executing command in container {}: {:?}", container_id, command);

        let request = ExecRequest {
            command: command.to_vec(),
        };

        let response = self
            .client
            .post(format!(
                "{}/api/v1/containers/{}/exec",
                self.base_url, container_id
            ))
            .json(&request)
            .send()
            .await?
            .json::<ExecOutput>()
            .await?;

        Ok(response)
    }

    /// Get top (process list) for container
    pub async fn get_top(&self, container_id: &str) -> Result<TopInfo> {
        tracing::debug!("Getting process list for container {}", container_id);

        let response = self
            .client
            .get(format!(
                "{}/api/v1/containers/{}/top",
                self.base_url, container_id
            ))
            .send()
            .await?
            .json::<TopInfo>()
            .await?;

        Ok(response)
    }

    /// Get events for project
    pub async fn get_events(&self, project_name: &str, since_id: i64) -> Result<Vec<Event>> {
        tracing::debug!("Getting events for project {} since {}", project_name, since_id);

        let response = self
            .client
            .get(format!(
                "{}/api/v1/events?project={}&since={}",
                self.base_url, project_name, since_id
            ))
            .send()
            .await?
            .json::<Vec<Event>>()
            .await?;

        Ok(response)
    }

    /// Inspect container details
    pub async fn inspect_container(&self, container_id: &str) -> Result<ContainerDetails> {
        tracing::info!("Inspecting container {}", container_id);

        let response = self
            .client
            .get(format!(
                "{}/api/v1/containers/{}/inspect",
                self.base_url, container_id
            ))
            .send()
            .await?
            .json::<ContainerDetails>()
            .await?;

        Ok(response)
    }

    /// Delete container
    pub async fn delete_container(&self, id: &str) -> Result<()> {
        tracing::info!("Deleting container {} via Vörðr", id);

        let response = self
            .client
            .delete(format!(
                "{}/api/v1/containers/{}",
                self.base_url, id
            ))
            .send()
            .await?;

        if !response.status().is_success() {
            anyhow::bail!("Failed to delete container {}", id);
        }

        Ok(())
    }

    /// Pause container
    pub async fn pause_container(&self, id: &str) -> Result<()> {
        tracing::info!("Pausing container {} via Vörðr", id);

        let response = self
            .client
            .post(format!(
                "{}/api/v1/containers/{}/pause",
                self.base_url, id
            ))
            .send()
            .await?;

        if !response.status().is_success() {
            anyhow::bail!("Failed to pause container {}", id);
        }

        Ok(())
    }

    /// Unpause container
    pub async fn unpause_container(&self, id: &str) -> Result<()> {
        tracing::info!("Unpausing container {} via Vörðr", id);

        let response = self
            .client
            .post(format!(
                "{}/api/v1/containers/{}/unpause",
                self.base_url, id
            ))
            .send()
            .await?;

        if !response.status().is_success() {
            anyhow::bail!("Failed to unpause container {}", id);
        }

        Ok(())
    }

    /// Wait for container to stop, returns exit code
    pub async fn wait_container(&self, id: &str) -> Result<WaitResult> {
        tracing::info!("Waiting for container {} to stop via Vörðr", id);

        let response = self
            .client
            .post(format!(
                "{}/api/v1/containers/{}/wait",
                self.base_url, id
            ))
            .send()
            .await?
            .json::<WaitResult>()
            .await?;

        Ok(response)
    }

    /// Get container health check status
    pub async fn get_health(&self, id: &str) -> Result<HealthStatus> {
        tracing::debug!("Getting health status for container {}", id);

        let response = self
            .client
            .get(format!(
                "{}/api/v1/containers/{}/health",
                self.base_url, id
            ))
            .send()
            .await?
            .json::<HealthStatus>()
            .await?;

        Ok(response)
    }

    /// Copy files to container via tar archive
    pub async fn copy_to_container(
        &self,
        id: &str,
        src_path: &str,
        dest_path: &str,
    ) -> Result<()> {
        tracing::info!(
            "Copying {} to container {}:{} via Vörðr",
            src_path, id, dest_path
        );

        let tar_data = std::fs::read(src_path)?;

        let response = self
            .client
            .post(format!(
                "{}/api/v1/containers/{}/copy",
                self.base_url, id
            ))
            .query(&[("dest", dest_path)])
            .header("Content-Type", "application/x-tar")
            .body(tar_data)
            .send()
            .await?;

        if !response.status().is_success() {
            anyhow::bail!(
                "Failed to copy {} to container {}:{}",
                src_path, id, dest_path
            );
        }

        Ok(())
    }

    /// Copy files from container, returns tar archive bytes
    pub async fn copy_from_container(&self, id: &str, src_path: &str) -> Result<Vec<u8>> {
        tracing::info!(
            "Copying from container {}:{} via Vörðr",
            id, src_path
        );

        let response = self
            .client
            .get(format!(
                "{}/api/v1/containers/{}/copy",
                self.base_url, id
            ))
            .query(&[("path", src_path)])
            .send()
            .await?;

        if !response.status().is_success() {
            anyhow::bail!(
                "Failed to copy from container {}:{}",
                id, src_path
            );
        }

        let bytes = response.bytes().await?.to_vec();
        Ok(bytes)
    }

    /// List images
    pub async fn list_images(&self) -> Result<Vec<ImageInfo>> {
        tracing::debug!("Listing images via Vörðr");

        let response = self
            .client
            .get(format!("{}/api/v1/images", self.base_url))
            .send()
            .await?
            .json::<Vec<ImageInfo>>()
            .await?;

        Ok(response)
    }

    /// Get container port mappings
    pub async fn get_container_ports(&self, id: &str) -> Result<Vec<PortMapping>> {
        tracing::debug!("Getting port mappings for container {}", id);

        let response = self
            .client
            .get(format!(
                "{}/api/v1/containers/{}/ports",
                self.base_url, id
            ))
            .send()
            .await?
            .json::<Vec<PortMapping>>()
            .await?;

        Ok(response)
    }
}

#[derive(Debug, Serialize)]
struct ExecRequest {
    command: Vec<String>,
}

#[derive(Debug, Deserialize)]
pub struct ExecOutput {
    pub stdout: String,
    pub stderr: String,
    pub exit_code: i32,
}

/// Process information for top command
#[derive(Debug, Deserialize)]
pub struct TopInfo {
    pub processes: Vec<Process>,
}

#[derive(Debug, Deserialize)]
pub struct Process {
    pub pid: String,
    pub user: String,
    pub cpu: String,
    pub mem: String,
    pub vsz: String,
    pub rss: String,
    pub tty: String,
    pub stat: String,
    pub start: String,
    pub time: String,
    pub command: String,
}

/// Event from Vörðr event stream
#[derive(Debug, Deserialize)]
pub struct Event {
    pub id: i64,
    pub timestamp: String,
    pub event_type: String,
    pub container_id: String,
    pub action: String,
    pub attributes: std::collections::HashMap<String, String>,
}

/// Detailed container information
#[derive(Debug, Deserialize, Serialize)]
pub struct ContainerDetails {
    pub id: String,
    pub image: String,
    pub state: String,
    pub status: String,
    pub created: Option<String>,
    pub started: Option<String>,
    pub networks: std::collections::HashMap<String, NetworkInfo>,
    pub ports: Vec<PortInfo>,
    pub environment: std::collections::HashMap<String, String>,
    pub mounts: Vec<MountInfo>,
    pub cpu_usage: Option<String>,
    pub memory_usage: Option<String>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct NetworkInfo {
    pub ip_address: String,
    pub gateway: Option<String>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct PortInfo {
    pub host_port: u16,
    pub container_port: u16,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct MountInfo {
    pub source: String,
    pub destination: String,
    pub mode: String,
}

/// Result from waiting for a container to stop
#[derive(Debug, Deserialize)]
pub struct WaitResult {
    pub exit_code: i32,
    pub error: Option<String>,
}

/// Container health check status
#[derive(Debug, Deserialize)]
pub struct HealthStatus {
    pub status: String,
    pub failing_streak: Option<u32>,
    pub log: Vec<HealthLogEntry>,
}

/// Individual health check log entry
#[derive(Debug, Deserialize)]
pub struct HealthLogEntry {
    pub start: String,
    pub end: String,
    pub exit_code: i32,
    pub output: String,
}

/// Image information
#[derive(Debug, Deserialize)]
pub struct ImageInfo {
    pub id: String,
    pub repository: String,
    pub tag: String,
    pub size: u64,
    pub created: String,
}

/// Port mapping with protocol information
#[derive(Debug, Deserialize)]
pub struct PortMapping {
    pub host_ip: String,
    pub host_port: u16,
    pub container_port: u16,
    pub protocol: String,
}
