// SPDX-License-Identifier: PMPL-1.0-or-later
//! Svalinn gateway client

use anyhow::Result;
use serde::{Deserialize, Serialize};
use serde_json::Value;

/// Svalinn HTTP client
pub struct SvalinnClient {
    base_url: String,
    client: reqwest::Client,
}

#[derive(Debug, Serialize)]
pub struct DeployRequest {
    pub bundle_path: String,
    pub service_name: String,
    pub environment: std::collections::HashMap<String, String>,
    pub ports: Vec<PortMapping>,
}

#[derive(Debug, Serialize)]
pub struct PortMapping {
    pub host: u16,
    pub container: u16,
}

#[derive(Debug, Deserialize)]
pub struct DeployResponse {
    pub service_id: String,
    pub status: String,
}

#[derive(Debug, Serialize)]
pub struct RunConfigV2 {
    pub env: std::collections::HashMap<String, String>,
    pub ports: Vec<String>,
    pub volumes: Vec<String>,
    pub networks: Vec<String>,
    pub secrets: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub restart: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub healthcheck: Option<HealthCheckV2>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub command: Option<Vec<String>>,
}

#[derive(Debug, Serialize)]
pub struct HealthCheckV2 {
    pub test: String,
    pub interval: String,
    pub timeout: String,
    pub retries: u32,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub start_period: Option<String>,
}

#[derive(Debug, Serialize)]
pub struct RunRequestV2 {
    pub image_name: String,
    pub image_digest: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub detach: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub config: Option<RunConfigV2>,
}

#[derive(Debug, Deserialize)]
pub struct RunResponseV2 {
    pub container_id: Option<String>,
    pub status: Option<String>,
}

impl SvalinnClient {
    /// Create new Svalinn client
    pub fn new(base_url: impl Into<String>) -> Self {
        Self {
            base_url: base_url.into(),
            client: reqwest::Client::new(),
        }
    }

    /// Deploy service via Svalinn
    pub async fn deploy(&self, request: DeployRequest) -> Result<DeployResponse> {
        tracing::info!("Deploying {} via Svalinn", request.service_name);

        let response = self
            .client
            .post(format!("{}/api/v1/deploy", self.base_url))
            .json(&request)
            .send()
            .await?
            .json::<DeployResponse>()
            .await?;

        Ok(response)
    }

    /// Run container via Svalinn v2 API
    pub async fn run_v2(&self, request: RunRequestV2) -> Result<RunResponseV2> {
        tracing::info!("Running {} via Svalinn v2", request.image_name);

        let response = self
            .client
            .post(format!("{}/api/v2/run", self.base_url))
            .json(&request)
            .send()
            .await?
            .json::<RunResponseV2>()
            .await?;

        Ok(response)
    }

    /// Check service health
    pub async fn health_check(&self, service_id: &str) -> Result<bool> {
        let response = self
            .client
            .get(format!("{}/api/v1/containers/{}", self.base_url, service_id))
            .send()
            .await?;

        if !response.status().is_success() {
            return Ok(false);
        }

        let body: Value = response.json().await.unwrap_or(Value::Null);
        let status = body
            .get("status")
            .and_then(|v| v.as_str())
            .unwrap_or("unknown");

        Ok(matches!(status, "running" | "healthy"))
    }

    /// Stop service
    pub async fn stop(&self, service_id: &str) -> Result<()> {
        tracing::info!("Stopping service {} via Svalinn", service_id);

        let response = self
            .client
            .post(format!(
                "{}/api/v1/containers/{}/stop",
                self.base_url, service_id
            ))
            .send()
            .await?;

        if !response.status().is_success() {
            anyhow::bail!("Failed to stop service {}", service_id);
        }

        Ok(())
    }
}
