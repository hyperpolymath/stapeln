// SPDX-License-Identifier: PMPL-1.0-or-later
//! TTRPC client for shim communication
//!
//! This module provides TTRPC-based communication with container shims,
//! compatible with the containerd shim v2 protocol.

use std::path::Path;
use std::time::Duration;
use thiserror::Error;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::UnixStream;

#[allow(dead_code)]
#[derive(Error, Debug)]
pub enum TtrpcError {
    #[error("Failed to connect to shim: {0}")]
    ConnectionFailed(String),
    #[error("RPC error: {0}")]
    RpcError(String),
    #[error("Timeout after {0}s")]
    Timeout(u64),
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

/// TTRPC client for shim communication.
///
/// Implements the containerd shim v2 task service protocol over Unix domain sockets.
/// Wire format: 10-byte header (length u32-LE + stream_id u32-LE + msg_type u8 + flags u8)
/// followed by JSON-encoded request/response body.
#[allow(dead_code)]
pub struct TtrpcClient {
    #[allow(dead_code)]
    socket_path: String,
    #[allow(dead_code)]
    timeout: Duration,
}

impl TtrpcClient {
    /// Create a new TTRPC client
    #[allow(dead_code)]
    pub fn new(socket_path: impl AsRef<Path>, timeout_secs: u64) -> Self {
        Self {
            socket_path: socket_path.as_ref().to_string_lossy().into_owned(),
            timeout: Duration::from_secs(timeout_secs),
        }
    }

    /// Connect to the shim socket
    #[allow(dead_code)]
    pub async fn connect(&self) -> Result<(), TtrpcError> {
        let path = Path::new(&self.socket_path);
        if !path.exists() {
            return Err(TtrpcError::ConnectionFailed(format!(
                "socket not found: {}",
                self.socket_path
            )));
        }

        // Verify we can open the socket
        let _stream = tokio::time::timeout(
            self.timeout,
            UnixStream::connect(path),
        )
        .await
        .map_err(|_| TtrpcError::Timeout(self.timeout.as_secs()))?
        .map_err(|e| TtrpcError::ConnectionFailed(format!("connect failed: {}", e)))?;

        Ok(())
    }

    /// Send a TTRPC request and receive the response.
    ///
    /// Uses a simplified JSON-over-Unix-socket protocol compatible with
    /// containerd shim v2 task service.
    #[allow(dead_code)]
    async fn send_request(
        &self,
        service: &str,
        method: &str,
        payload: &serde_json::Value,
    ) -> Result<serde_json::Value, TtrpcError> {
        let mut stream = tokio::time::timeout(
            self.timeout,
            UnixStream::connect(&self.socket_path),
        )
        .await
        .map_err(|_| TtrpcError::Timeout(self.timeout.as_secs()))?
        .map_err(|e| TtrpcError::ConnectionFailed(format!("connect failed: {}", e)))?;

        // Build TTRPC frame: header (10 bytes) + body
        let request_envelope = serde_json::json!({
            "service": service,
            "method": method,
            "payload": payload,
        });
        let body = serde_json::to_vec(&request_envelope)
            .map_err(|e| TtrpcError::RpcError(format!("serialize error: {}", e)))?;

        // Write header: length (4 bytes LE) + stream_id (4 bytes LE) + type (1) + flags (1)
        let len = body.len() as u32;
        let mut header = [0u8; 10];
        header[0..4].copy_from_slice(&len.to_le_bytes());
        // stream_id = 1, type = 0 (request), flags = 0
        header[4..8].copy_from_slice(&1u32.to_le_bytes());
        header[8] = 0; // REQUEST
        header[9] = 0; // no flags

        stream.write_all(&header).await
            .map_err(|e| TtrpcError::Io(e))?;
        stream.write_all(&body).await
            .map_err(|e| TtrpcError::Io(e))?;
        stream.flush().await
            .map_err(|e| TtrpcError::Io(e))?;

        // Read response header
        let mut resp_header = [0u8; 10];
        tokio::time::timeout(self.timeout, stream.read_exact(&mut resp_header))
            .await
            .map_err(|_| TtrpcError::Timeout(self.timeout.as_secs()))?
            .map_err(|e| TtrpcError::Io(e))?;

        let resp_len = u32::from_le_bytes([resp_header[0], resp_header[1], resp_header[2], resp_header[3]]) as usize;
        let msg_type = resp_header[8];

        // Read response body
        let mut resp_body = vec![0u8; resp_len];
        tokio::time::timeout(self.timeout, stream.read_exact(&mut resp_body))
            .await
            .map_err(|_| TtrpcError::Timeout(self.timeout.as_secs()))?
            .map_err(|e| TtrpcError::Io(e))?;

        // Check for error response (type = 2)
        if msg_type == 2 {
            let err_text = String::from_utf8_lossy(&resp_body);
            return Err(TtrpcError::RpcError(format!("shim error: {}", err_text)));
        }

        serde_json::from_slice(&resp_body)
            .map_err(|e| TtrpcError::RpcError(format!("response parse error: {}", e)))
    }

    /// Create a container task
    #[allow(dead_code)]
    pub async fn create(
        &self,
        id: &str,
        bundle: &str,
        stdout: &str,
        stderr: &str,
    ) -> Result<u32, TtrpcError> {
        tracing::debug!(
            "TTRPC create: id={}, bundle={}, stdout={}, stderr={}",
            id, bundle, stdout, stderr
        );

        let request = serde_json::json!({
            "id": id,
            "bundle": bundle,
            "stdout": stdout,
            "stderr": stderr,
            "terminal": false,
        });

        let response = self.send_request("containerd.task.v2.Task", "Create", &request).await?;

        let pid = response.get("pid")
            .and_then(|v| v.as_u64())
            .ok_or_else(|| TtrpcError::RpcError("Response missing pid field".to_string()))?;

        Ok(pid as u32)
    }

    /// Start a created container
    #[allow(dead_code)]
    pub async fn start(&self, id: &str) -> Result<u32, TtrpcError> {
        tracing::debug!("TTRPC start: id={}", id);

        let request = serde_json::json!({ "id": id });
        let response = self.send_request("containerd.task.v2.Task", "Start", &request).await?;

        let pid = response.get("pid")
            .and_then(|v| v.as_u64())
            .ok_or_else(|| TtrpcError::RpcError("Response missing pid field".to_string()))?;

        Ok(pid as u32)
    }

    /// Kill a container process
    #[allow(dead_code)]
    pub async fn kill(&self, id: &str, signal: u32, all: bool) -> Result<(), TtrpcError> {
        tracing::debug!("TTRPC kill: id={}, signal={}, all={}", id, signal, all);

        let request = serde_json::json!({
            "id": id,
            "signal": signal,
            "all": all,
        });

        self.send_request("containerd.task.v2.Task", "Kill", &request).await?;
        Ok(())
    }

    /// Delete a container
    #[allow(dead_code)]
    pub async fn delete(&self, id: &str) -> Result<(u32, u32), TtrpcError> {
        tracing::debug!("TTRPC delete: id={}", id);

        let request = serde_json::json!({ "id": id });
        let response = self.send_request("containerd.task.v2.Task", "Delete", &request).await?;

        let pid = response.get("pid").and_then(|v| v.as_u64()).unwrap_or(0) as u32;
        let exit_status = response.get("exit_status").and_then(|v| v.as_u64()).unwrap_or(0) as u32;

        Ok((pid, exit_status))
    }

    /// Wait for container exit
    #[allow(dead_code)]
    pub async fn wait(&self, id: &str) -> Result<u32, TtrpcError> {
        tracing::debug!("TTRPC wait: id={}", id);

        let request = serde_json::json!({ "id": id });
        let response = self.send_request("containerd.task.v2.Task", "Wait", &request).await?;

        let exit_status = response.get("exit_status")
            .and_then(|v| v.as_u64())
            .ok_or_else(|| TtrpcError::RpcError("Response missing exit_status field".to_string()))?;

        Ok(exit_status as u32)
    }

    /// Get container state
    #[allow(dead_code)]
    pub async fn state(&self, id: &str) -> Result<TaskState, TtrpcError> {
        tracing::debug!("TTRPC state: id={}", id);

        let request = serde_json::json!({ "id": id });
        let response = self.send_request("containerd.task.v2.Task", "State", &request).await?;

        let bundle = response.get("bundle")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();
        let pid = response.get("pid").and_then(|v| v.as_u64()).unwrap_or(0) as u32;
        let status_str = response.get("status")
            .and_then(|v| v.as_str())
            .unwrap_or("unknown");

        Ok(TaskState {
            id: id.to_string(),
            bundle,
            pid,
            status: TaskStatus::from_str(status_str),
        })
    }
}

/// Container task state
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct TaskState {
    pub id: String,
    pub bundle: String,
    pub pid: u32,
    pub status: TaskStatus,
}

/// Task status
#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TaskStatus {
    Unknown,
    Created,
    Running,
    Stopped,
    Paused,
    Pausing,
}

impl TaskStatus {
    #[allow(dead_code)]
    pub fn from_str(s: &str) -> Self {
        match s.to_lowercase().as_str() {
            "created" => TaskStatus::Created,
            "running" => TaskStatus::Running,
            "stopped" => TaskStatus::Stopped,
            "paused" => TaskStatus::Paused,
            "pausing" => TaskStatus::Pausing,
            _ => TaskStatus::Unknown,
        }
    }

    #[allow(dead_code)]
    pub fn as_str(&self) -> &'static str {
        match self {
            TaskStatus::Unknown => "unknown",
            TaskStatus::Created => "created",
            TaskStatus::Running => "running",
            TaskStatus::Stopped => "stopped",
            TaskStatus::Paused => "paused",
            TaskStatus::Pausing => "pausing",
        }
    }
}
