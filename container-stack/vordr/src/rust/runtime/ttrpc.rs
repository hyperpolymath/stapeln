// SPDX-License-Identifier: PMPL-1.0-or-later
//! TTRPC client for shim communication
//!
//! This module provides TTRPC-based communication with container shims,
//! compatible with the containerd shim v2 protocol.

use std::path::Path;
use std::time::Duration;
use thiserror::Error;

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

/// TTRPC client for shim communication
pub struct TtrpcClient {
    socket_path: String,
    timeout: Duration,
}

impl TtrpcClient {
    /// Create a new TTRPC client
    pub fn new(socket_path: impl AsRef<Path>, timeout_secs: u64) -> Self {
        Self {
            socket_path: socket_path.as_ref().to_string_lossy().into_owned(),
            timeout: Duration::from_secs(timeout_secs),
        }
    }

    /// Connect to the shim socket
    pub async fn connect(&self) -> Result<(), TtrpcError> {
        // Check if socket exists
        let path = Path::new(&self.socket_path);
        if !path.exists() {
            return Err(TtrpcError::ConnectionFailed(format!(
                "socket not found: {}",
                self.socket_path
            )));
        }

        // In production, this would establish the TTRPC connection
        // For now, we verify the socket exists
        Ok(())
    }

    /// Create a container task
    pub async fn create(
        &self,
        id: &str,
        bundle: &str,
        stdout: &str,
        stderr: &str,
    ) -> Result<u32, TtrpcError> {
        // TODO: Implement TTRPC create call
        // This would send a CreateTaskRequest and return the PID.
        // Actual implementation requires Protobuf message definitions (generated from .proto files)
        // and proper TTRPC client setup (e.g., using ttrpc::Client).
        // For now, this remains a stub.
        tracing::debug!(
            "TTRPC create: id={}, bundle={}, stdout={}, stderr={}",
            id,
            bundle,
            stdout,
            stderr
        );

        Err(TtrpcError::RpcError("TTRPC create call requires Protobuf message definitions and TTRPC client setup.".to_string()))
    }

    /// Start a created container
    pub async fn start(&self, id: &str) -> Result<u32, TtrpcError> {
        tracing::debug!("TTRPC start: id={}", id);
        Err(TtrpcError::RpcError("TTRPC not yet implemented".to_string()))
    }

    /// Kill a container process
    pub async fn kill(&self, id: &str, signal: u32, all: bool) -> Result<(), TtrpcError> {
        tracing::debug!("TTRPC kill: id={}, signal={}, all={}", id, signal, all);
        Err(TtrpcError::RpcError("TTRPC not yet implemented".to_string()))
    }

    /// Delete a container
    pub async fn delete(&self, id: &str) -> Result<(u32, u32), TtrpcError> {
        tracing::debug!("TTRPC delete: id={}", id);
        Err(TtrpcError::RpcError("TTRPC not yet implemented".to_string()))
    }

    /// Wait for container exit
    pub async fn wait(&self, id: &str) -> Result<u32, TtrpcError> {
        tracing::debug!("TTRPC wait: id={}", id);
        Err(TtrpcError::RpcError("TTRPC not yet implemented".to_string()))
    }

    /// Get container state
    pub async fn state(&self, id: &str) -> Result<TaskState, TtrpcError> {
        tracing::debug!("TTRPC state: id={}", id);
        Err(TtrpcError::RpcError("TTRPC not yet implemented".to_string()))
    }
}

/// Container task state
#[derive(Debug, Clone)]
pub struct TaskState {
    pub id: String,
    pub bundle: String,
    pub pid: u32,
    pub status: TaskStatus,
}

/// Task status
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