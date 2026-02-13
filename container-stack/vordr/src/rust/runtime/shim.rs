// SPDX-License-Identifier: PMPL-1.0-or-later
//! Container runtime shim management

use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use thiserror::Error;
use tracing::{debug, info};
use tokio::task;

#[derive(Error, Debug)]
pub enum ShimError {
    #[error("Failed to spawn shim: {0}")]
    SpawnFailed(String),
    #[error("Shim connection failed: {0}")]
    ConnectionFailed(String),
    #[error("Runtime error: {0}")]
    RuntimeError(String),
    #[error("Timeout waiting for shim")]
    Timeout,
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    #[error("Container not found: {0}")]
    NotFound(String),
}

/// Client for communicating with container runtime shims
pub struct ShimClient {
    runtime: String,
    bundle_path: PathBuf,
    socket_path: Option<PathBuf>,
}

impl ShimClient {
    /// Create a new shim client
    pub fn new(runtime: &str, bundle_path: &str) -> Self {
        Self {
            runtime: runtime.to_string(),
            bundle_path: PathBuf::from(bundle_path),
            socket_path: None,
        }
    }

    /// Create and start a container
    pub async fn create_and_start(&self, container_id: &str) -> Result<u32, ShimError> {
        info!("Creating container {} with {}", container_id, self.runtime);

        // Find the runtime binary
        let runtime_path = self.find_runtime()?;
        debug!("Using runtime: {}", runtime_path.display());

        // Create the container
        let create_output = Command::new(&runtime_path)
            .arg("create")
            .arg("--bundle")
            .arg(&self.bundle_path)
            .arg(container_id)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()?;

        if !create_output.status.success() {
            let stderr = String::from_utf8_lossy(&create_output.stderr);
            return Err(ShimError::RuntimeError(format!(
                "create failed: {}",
                stderr
            )));
        }

        // Start the container
        let start_output = Command::new(&runtime_path)
            .arg("start")
            .arg(container_id)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()?;

        if !start_output.status.success() {
            let stderr = String::from_utf8_lossy(&start_output.stderr);
            return Err(ShimError::RuntimeError(format!(
                "start failed: {}",
                stderr
            )));
        }

        // Get the container state to find the PID
        let state = self.state(container_id).await?;
        Ok(state.pid)
    }

    /// Get container state
    pub async fn state(&self, container_id: &str) -> Result<ContainerState, ShimError> {
        let runtime_path = self.find_runtime()?;

        let output = Command::new(&runtime_path)
            .arg("state")
            .arg(container_id)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            if stderr.contains("does not exist") {
                return Err(ShimError::NotFound(container_id.to_string()));
            }
            return Err(ShimError::RuntimeError(format!("state failed: {}", stderr)));
        }

        let state: serde_json::Value =
            serde_json::from_slice(&output.stdout).map_err(|e| {
                ShimError::RuntimeError(format!("failed to parse state: {}", e))
            })?;

        Ok(ContainerState {
            id: state["id"].as_str().unwrap_or(container_id).to_string(),
            pid: state["pid"].as_u64().unwrap_or(0) as u32,
            status: state["status"].as_str().unwrap_or("unknown").to_string(),
            bundle: state["bundle"].as_str().unwrap_or("").to_string(),
        })
    }

    /// Kill a container
    pub async fn kill(&self, container_id: &str, signal: u32, all: bool) -> Result<(), ShimError> {
        let runtime_path = self.find_runtime()?;

        let mut cmd = Command::new(&runtime_path);
        cmd.arg("kill");

        if all {
            cmd.arg("--all");
        }

        cmd.arg(container_id);
        cmd.arg(signal.to_string());

        let output = cmd.stdout(Stdio::piped()).stderr(Stdio::piped()).output()?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(ShimError::RuntimeError(format!("kill failed: {}", stderr)));
        }

        Ok(())
    }

    /// Delete a container
    pub async fn delete(&self, container_id: &str, force: bool) -> Result<(), ShimError> {
        let runtime_path = self.find_runtime()?;

        let mut cmd = Command::new(&runtime_path);
        cmd.arg("delete");

        if force {
            cmd.arg("--force");
        }

        cmd.arg(container_id);

        let output = cmd.stdout(Stdio::piped()).stderr(Stdio::piped()).output()?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // Ignore "does not exist" errors
            if !stderr.contains("does not exist") {
                return Err(ShimError::RuntimeError(format!(
                    "delete failed: {}",
                    stderr
                )));
            }
        }

        Ok(())
    }

    /// Execute a process in a running container
    pub async fn exec(
        &self,
        container_id: &str,
        process_spec: &str,
        tty: bool,
    ) -> Result<u32, ShimError> {
        let runtime_path = self.find_runtime()?;

        // Write process spec to file
        let exec_spec_path = self.bundle_path.join("exec.json");
        std::fs::write(&exec_spec_path, process_spec)?;

        let mut cmd = Command::new(&runtime_path);
        cmd.arg("exec");

        if tty {
            cmd.arg("--tty");
        }

        cmd.arg("--process").arg(&exec_spec_path);
        cmd.arg(container_id);

        let child = cmd
            .stdin(Stdio::inherit())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .spawn()?;

        Ok(child.id())
    }

    /// Execute a process in a running container and wait for completion.
    pub async fn exec_wait(&self, container_id: &str, process_spec: &str) -> Result<i32, ShimError> {
        let runtime_path = self.find_runtime()?;
        let exec_spec_path = self.bundle_path.join("exec.json");
        std::fs::write(&exec_spec_path, process_spec)?;

        let runtime_path = runtime_path.clone();
        let exec_spec_path = exec_spec_path.clone();
        let container = container_id.to_string();

        let handle = task::spawn_blocking(move || {
            let output = Command::new(&runtime_path)
                .arg("exec")
                .arg("--process")
                .arg(&exec_spec_path)
                .arg(&container)
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .output()?;

            if !output.status.success() {
                let stderr = String::from_utf8_lossy(&output.stderr);
                return Err(ShimError::RuntimeError(format!("exec failed: {}", stderr)));
            }

            Ok(output.status.code().unwrap_or(0))
        });

        let exit_result = handle
            .await
            .map_err(|e| ShimError::RuntimeError(format!("exec task failed: {}", e)))?;

        exit_result
    }

    /// Wait for a container to exit
    pub async fn wait(&self, container_id: &str) -> Result<i32, ShimError> {
        // Poll state until container exits
        loop {
            match self.state(container_id).await {
                Ok(state) => {
                    if state.status == "stopped" {
                        // Get exit code from state file
                        let exit_path = self.bundle_path.join("exit");
                        if exit_path.exists() {
                            if let Ok(code) = std::fs::read_to_string(&exit_path) {
                                return Ok(code.trim().parse().unwrap_or(-1));
                            }
                        }
                        return Ok(0);
                    }
                }
                Err(ShimError::NotFound(_)) => {
                    return Ok(0);
                }
                Err(e) => return Err(e),
            }

            tokio::time::sleep(std::time::Duration::from_millis(100)).await;
        }
    }

    /// Find the runtime binary in PATH
    fn find_runtime(&self) -> Result<PathBuf, ShimError> {
        // Check if runtime is an absolute path
        let path = Path::new(&self.runtime);
        if path.is_absolute() && path.exists() {
            return Ok(path.to_path_buf());
        }

        // Try to find in PATH
        which::which(&self.runtime)
            .map_err(|_| ShimError::SpawnFailed(format!("runtime '{}' not found in PATH", self.runtime)))
    }
}

/// Container state from runtime
#[derive(Debug, Clone)]
pub struct ContainerState {
    pub id: String,
    pub pid: u32,
    pub status: String,
    pub bundle: String,
}

/// Spawn a container with the shim (background process management)
pub struct ShimProcess {
    container_id: String,
    pid: u32,
    socket_path: PathBuf,
}

impl ShimProcess {
    /// Spawn a new shim process
    pub fn spawn(
        _runtime: &str,
        container_id: &str,
        _bundle_path: &Path,
        root_dir: &Path,
    ) -> Result<Self, ShimError> {
        let socket_path = root_dir.join(format!("{}.sock", container_id));

        info!(
            "Spawning shim for container {} at {}",
            container_id,
            socket_path.display()
        );

        // For now, we'll use direct runtime invocation
        // In production, this would spawn conmon-rs or a similar shim

        Ok(Self {
            container_id: container_id.to_string(),
            pid: 0, // Will be set after container starts
            socket_path,
        })
    }

    /// Get the shim socket path
    pub fn socket_path(&self) -> &Path {
        &self.socket_path
    }

    /// Get the container PID
    pub fn pid(&self) -> u32 {
        self.pid
    }
}
