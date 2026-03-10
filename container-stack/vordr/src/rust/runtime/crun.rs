// SPDX-License-Identifier: PMPL-1.0-or-later
//! Direct container runtime execution via crun/youki subprocess
//!
//! Wraps an OCI-compatible low-level runtime (crun or youki) as an async
//! subprocess, following the same invocation pattern that podman uses.
//! This is the primary path for `vordr run` — the TTRPC shim path is the
//! fallback for containerd-compatible shim v2 setups.

use std::path::PathBuf;
use std::process::Output;

use anyhow::{bail, Context, Result};
use tokio::process::Command;
use tracing::{debug, info, warn};

/// Preferred runtimes in search order.
/// crun is preferred (lighter, written in C, faster cold-start) but youki
/// (Rust) is also supported.
const RUNTIME_SEARCH_ORDER: &[&str] = &["crun", "youki"];

/// Direct OCI runtime wrapper.
///
/// All operations shell out to the runtime binary following the OCI Runtime
/// Specification (create / start / state / kill / delete).
pub struct DirectRuntime {
    /// Absolute path to the resolved runtime binary.
    runtime_path: PathBuf,
    /// Name of the runtime for logging (e.g. "crun", "youki").
    runtime_name: String,
}

impl DirectRuntime {
    // ------------------------------------------------------------------
    // Construction
    // ------------------------------------------------------------------

    /// Auto-detect crun or youki on `$PATH`.
    ///
    /// Returns `Ok(Self)` with whichever is found first (crun preferred),
    /// or an error if neither is available.
    pub fn new() -> Result<Self> {
        for name in RUNTIME_SEARCH_ORDER {
            match which::which(name) {
                Ok(path) => {
                    info!(runtime = %name, path = %path.display(), "Found OCI runtime");
                    return Ok(Self {
                        runtime_path: path,
                        runtime_name: name.to_string(),
                    });
                }
                Err(_) => {
                    debug!(runtime = %name, "OCI runtime not found on PATH, trying next");
                }
            }
        }

        bail!(
            "No OCI runtime found on PATH. Install crun or youki.\n\
             Searched for: {}",
            RUNTIME_SEARCH_ORDER.join(", ")
        )
    }

    /// Create a `DirectRuntime` using a specific binary path or name.
    ///
    /// If `runtime` is an absolute path it is used directly; otherwise it is
    /// looked up on `$PATH` via `which`.
    pub fn with_runtime(runtime: &str) -> Result<Self> {
        let path = std::path::Path::new(runtime);
        let resolved = if path.is_absolute() && path.exists() {
            PathBuf::from(runtime)
        } else {
            which::which(runtime)
                .with_context(|| format!("Runtime '{}' not found on PATH", runtime))?
        };

        let name = resolved
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_else(|| runtime.to_string());

        info!(runtime = %name, path = %resolved.display(), "Using OCI runtime");
        Ok(Self {
            runtime_path: resolved,
            runtime_name: name,
        })
    }

    /// Return the name of the resolved runtime (e.g. "crun").
    pub fn name(&self) -> &str {
        &self.runtime_name
    }

    // ------------------------------------------------------------------
    // OCI runtime commands
    // ------------------------------------------------------------------

    /// `crun create --bundle <bundle> <id>`
    ///
    /// Creates an OCI container from the bundle directory (which must contain
    /// a valid `config.json`). The container enters the "created" state and
    /// its init process is started but blocked at the `execve` barrier.
    pub async fn create(&self, id: &str, bundle: &str) -> Result<()> {
        info!(container = %id, bundle = %bundle, "Creating container");
        let output = self
            .run_command(&["create", "--bundle", bundle, id])
            .await?;
        self.check_output("create", &output)?;
        debug!(container = %id, "Container created successfully");
        Ok(())
    }

    /// `crun start <id>`
    ///
    /// Signals the runtime to start the container's init process (unblocks
    /// the `execve` barrier set during `create`).
    pub async fn start(&self, id: &str) -> Result<()> {
        info!(container = %id, "Starting container");
        let output = self.run_command(&["start", id]).await?;
        self.check_output("start", &output)?;
        debug!(container = %id, "Container started successfully");
        Ok(())
    }

    /// `crun kill <id> <signal>`
    ///
    /// Sends a signal (e.g. "SIGTERM", "SIGKILL", "15", "9") to the
    /// container's init process.
    pub async fn kill(&self, id: &str, signal: &str) -> Result<()> {
        info!(container = %id, signal = %signal, "Killing container");
        let output = self.run_command(&["kill", id, signal]).await?;
        self.check_output("kill", &output)?;
        Ok(())
    }

    /// `crun delete <id>`
    ///
    /// Deletes the container's runtime state.  The container must be in the
    /// "stopped" state (or use `delete_force` for forced removal).
    pub async fn delete(&self, id: &str) -> Result<()> {
        info!(container = %id, "Deleting container");
        let output = self.run_command(&["delete", id]).await?;
        // Tolerate "does not exist" — the container may already have been
        // cleaned up by a previous delete or a crash recovery path.
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            if stderr.contains("does not exist") || stderr.contains("not found") {
                warn!(container = %id, "Container already deleted, ignoring");
                return Ok(());
            }
            bail!(
                "{} delete failed (exit {}): {}",
                self.runtime_name,
                output.status.code().unwrap_or(-1),
                stderr.trim()
            );
        }
        Ok(())
    }

    /// `crun delete --force <id>`
    ///
    /// Force-deletes the container, killing it first if necessary.
    pub async fn delete_force(&self, id: &str) -> Result<()> {
        info!(container = %id, "Force-deleting container");
        let output = self.run_command(&["delete", "--force", id]).await?;
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            if stderr.contains("does not exist") || stderr.contains("not found") {
                warn!(container = %id, "Container already deleted, ignoring");
                return Ok(());
            }
            bail!(
                "{} delete --force failed (exit {}): {}",
                self.runtime_name,
                output.status.code().unwrap_or(-1),
                stderr.trim()
            );
        }
        Ok(())
    }

    /// `crun state <id>`
    ///
    /// Returns the OCI runtime state as a raw JSON string.  The JSON
    /// conforms to the OCI Runtime State schema and includes fields like
    /// `id`, `status`, `pid`, `bundle`.
    pub async fn state(&self, id: &str) -> Result<String> {
        debug!(container = %id, "Querying container state");
        let output = self.run_command(&["state", id]).await?;
        self.check_output("state", &output)?;
        let json = String::from_utf8(output.stdout)
            .context("Runtime state output is not valid UTF-8")?;
        Ok(json)
    }

    /// Convenience: query state and extract the PID of the container's init
    /// process.  Returns `0` if the PID field is absent or the container has
    /// already exited.
    pub async fn get_pid(&self, id: &str) -> Result<u32> {
        let json_str = self.state(id).await?;
        let value: serde_json::Value =
            serde_json::from_str(&json_str).context("Failed to parse runtime state JSON")?;
        let pid = value
            .get("pid")
            .and_then(|v| v.as_u64())
            .unwrap_or(0) as u32;
        debug!(container = %id, pid = pid, "Got container PID");
        Ok(pid)
    }

    /// Convenience: query state and extract the status string (e.g.
    /// "created", "running", "stopped").
    pub async fn get_status(&self, id: &str) -> Result<String> {
        let json_str = self.state(id).await?;
        let value: serde_json::Value =
            serde_json::from_str(&json_str).context("Failed to parse runtime state JSON")?;
        let status = value
            .get("status")
            .and_then(|v| v.as_str())
            .unwrap_or("unknown")
            .to_string();
        Ok(status)
    }

    /// High-level helper: create + start in one call, returning the PID.
    ///
    /// This is the primary entry point used by `vordr run` and
    /// `ContainerLifecycle::start`.
    pub async fn create_and_start(&self, id: &str, bundle: &str) -> Result<u32> {
        self.create(id, bundle).await?;
        self.start(id).await?;
        let pid = self.get_pid(id).await?;
        info!(container = %id, pid = pid, "Container running");
        Ok(pid)
    }

    // ------------------------------------------------------------------
    // Internal helpers
    // ------------------------------------------------------------------

    /// Run the runtime binary with the given arguments, capturing stdout and
    /// stderr.  Uses `tokio::process::Command` for proper async I/O.
    async fn run_command(&self, args: &[&str]) -> Result<Output> {
        debug!(
            runtime = %self.runtime_name,
            args = ?args,
            "Running OCI runtime command"
        );

        let output = Command::new(&self.runtime_path)
            .args(args)
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn()
            .with_context(|| {
                format!(
                    "Failed to spawn {} with args {:?}",
                    self.runtime_path.display(),
                    args
                )
            })?
            .wait_with_output()
            .await
            .with_context(|| {
                format!(
                    "Failed to wait for {} with args {:?}",
                    self.runtime_path.display(),
                    args
                )
            })?;

        debug!(
            runtime = %self.runtime_name,
            exit_code = output.status.code().unwrap_or(-1),
            stdout_len = output.stdout.len(),
            stderr_len = output.stderr.len(),
            "Runtime command completed"
        );

        Ok(output)
    }

    /// Check command output and return an error with context on failure.
    fn check_output(&self, operation: &str, output: &Output) -> Result<()> {
        if output.status.success() {
            return Ok(());
        }

        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);

        let mut msg = format!(
            "{} {} failed (exit {})",
            self.runtime_name,
            operation,
            output.status.code().unwrap_or(-1),
        );

        if !stderr.trim().is_empty() {
            msg.push_str(&format!(": {}", stderr.trim()));
        } else if !stdout.trim().is_empty() {
            msg.push_str(&format!(": {}", stdout.trim()));
        }

        bail!("{}", msg)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_runtime_search_order() {
        // Verify the search-order constant is non-empty and contains the
        // expected entries.
        assert!(RUNTIME_SEARCH_ORDER.contains(&"crun"));
        assert!(RUNTIME_SEARCH_ORDER.contains(&"youki"));
        assert_eq!(RUNTIME_SEARCH_ORDER[0], "crun", "crun should be preferred");
    }

    #[test]
    fn test_with_runtime_nonexistent() {
        // Trying to resolve a non-existent runtime should fail gracefully.
        let result = DirectRuntime::with_runtime("definitely_not_a_real_runtime_binary_xyz");
        assert!(result.is_err());
    }
}
