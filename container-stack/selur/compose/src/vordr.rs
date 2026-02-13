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
