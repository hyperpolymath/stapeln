// SPDX-License-Identifier: PMPL-1.0-or-later
//! Vordr MCP JSON-RPC client

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::sync::atomic::{AtomicU64, Ordering};

pub struct VordrMcpClient {
    base_url: String,
    client: reqwest::Client,
    next_id: AtomicU64,
}

#[derive(Debug, Serialize)]
struct JsonRpcRequest {
    jsonrpc: &'static str,
    method: &'static str,
    params: Value,
    id: u64,
}

#[derive(Debug, Deserialize)]
struct JsonRpcResponse {
    result: Option<Value>,
    error: Option<JsonRpcError>,
}

#[derive(Debug, Deserialize)]
struct JsonRpcError {
    code: i32,
    message: String,
}

impl VordrMcpClient {
    pub fn new(base_url: impl Into<String>) -> Self {
        Self {
            base_url: base_url.into(),
            client: reqwest::Client::new(),
            next_id: AtomicU64::new(1),
        }
    }

    pub async fn call_tool(&self, name: &str, arguments: Value) -> Result<Value> {
        let id = self.next_id.fetch_add(1, Ordering::Relaxed);
        let request = JsonRpcRequest {
            jsonrpc: "2.0",
            method: "tools/call",
            params: json!({
                "name": name,
                "arguments": arguments,
            }),
            id,
        };

        let response = self
            .client
            .post(&self.base_url)
            .json(&request)
            .send()
            .await
            .context("Failed to send MCP request")?
            .json::<JsonRpcResponse>()
            .await
            .context("Failed to parse MCP response")?;

        if let Some(error) = response.error {
            anyhow::bail!("MCP error {}: {}", error.code, error.message);
        }

        Ok(response.result.unwrap_or(Value::Null))
    }

    pub async fn create_network(&self, name: &str, driver: &str, subnet: Option<String>) -> Result<()> {
        let args = json!({
            "name": name,
            "driver": driver,
            "subnet": subnet,
        });
        self.call_tool("vordr_network_create", args).await?;
        Ok(())
    }

    pub async fn remove_network(&self, name: &str) -> Result<()> {
        let args = json!({ "name": name });
        self.call_tool("vordr_network_rm", args).await?;
        Ok(())
    }

    pub async fn create_volume(&self, name: &str, driver: &str) -> Result<()> {
        let args = json!({
            "name": name,
            "driver": driver,
        });
        self.call_tool("vordr_volume_create", args).await?;
        Ok(())
    }

    pub async fn remove_volume(&self, name: &str) -> Result<()> {
        let args = json!({ "id": name });
        self.call_tool("vordr_volume_rm", args).await?;
        Ok(())
    }
}
