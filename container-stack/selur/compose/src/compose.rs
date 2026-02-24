// SPDX-License-Identifier: PMPL-1.0-or-later
//! Compose file parsing and validation

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Compose file format (TOML-based)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComposeFile {
    /// Format version (currently "1.0")
    pub version: String,

    /// Service definitions
    #[serde(default)]
    pub services: HashMap<String, Service>,

    /// Volume definitions
    #[serde(default)]
    pub volumes: HashMap<String, Volume>,

    /// Network definitions
    #[serde(default)]
    pub networks: HashMap<String, Network>,

    /// Secret definitions
    #[serde(default)]
    pub secrets: HashMap<String, Secret>,
}

/// Service definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Service {
    /// .ctp bundle image reference (required)
    pub image: String,

    /// Override container command
    #[serde(skip_serializing_if = "Option::is_none")]
    pub command: Option<Vec<String>>,

    /// Environment variables
    #[serde(default)]
    pub environment: HashMap<String, EnvValue>,

    /// Port mappings (host:container)
    #[serde(default)]
    pub ports: Vec<String>,

    /// Service dependencies
    #[serde(default)]
    pub depends_on: Vec<String>,

    /// Volume mounts
    #[serde(default)]
    pub volumes: Vec<String>,

    /// Networks to join
    #[serde(default)]
    pub networks: Vec<String>,

    /// Secrets to mount
    #[serde(default)]
    pub secrets: Vec<String>,

    /// Restart policy
    #[serde(default = "default_restart")]
    pub restart: String,

    /// Health check configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub healthcheck: Option<HealthCheck>,

    /// Deployment configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub deploy: Option<Deploy>,

    /// Build configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub build: Option<Build>,
}

/// Environment variable value (string or secret reference)
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum EnvValue {
    String(String),
    Secret { secret: String },
}

/// Health check configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthCheck {
    /// Health check command
    pub test: String,

    /// Check interval (e.g., "30s")
    #[serde(default = "default_interval")]
    pub interval: String,

    /// Timeout for each check
    #[serde(default = "default_timeout")]
    pub timeout: String,

    /// Number of retries before unhealthy
    #[serde(default = "default_retries")]
    pub retries: u32,

    /// Start period before checking
    #[serde(default)]
    pub start_period: Option<String>,
}

/// Deployment configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Deploy {
    /// Number of replicas
    #[serde(default = "default_replicas")]
    pub replicas: u32,

    /// Update strategy
    #[serde(default = "default_strategy")]
    pub strategy: String,
}

/// Build configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Build {
    /// Build context directory
    pub context: String,

    /// Dockerfile path (relative to context)
    #[serde(default = "default_dockerfile")]
    pub dockerfile: Option<String>,

    /// Build arguments
    #[serde(default)]
    pub args: HashMap<String, String>,

    /// Target stage for multi-stage builds
    #[serde(skip_serializing_if = "Option::is_none")]
    pub target: Option<String>,
}

/// Volume definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Volume {
    /// Volume driver
    #[serde(default = "default_driver")]
    pub driver: String,

    /// Driver options
    #[serde(default)]
    pub driver_opts: HashMap<String, String>,

    /// Volume labels
    #[serde(default)]
    pub labels: HashMap<String, String>,
}

/// Network definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Network {
    /// Network driver (selur = zero-copy, bridge = traditional)
    #[serde(default = "default_network_driver")]
    pub driver: String,

    /// Driver options
    #[serde(default)]
    pub driver_opts: HashMap<String, String>,

    /// IPAM configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ipam: Option<IpamConfig>,
}

/// IPAM (IP Address Management) configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IpamConfig {
    /// Subnet CIDR
    pub subnet: String,

    /// Gateway IP
    #[serde(skip_serializing_if = "Option::is_none")]
    pub gateway: Option<String>,
}

/// Secret definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Secret {
    /// Read secret from file
    #[serde(skip_serializing_if = "Option::is_none")]
    pub file: Option<PathBuf>,

    /// Use external secret (from vault)
    #[serde(default)]
    pub external: bool,
}

// Default values

fn default_restart() -> String {
    "no".to_string()
}

fn default_interval() -> String {
    "30s".to_string()
}

fn default_timeout() -> String {
    "30s".to_string()
}

fn default_retries() -> u32 {
    3
}

fn default_replicas() -> u32 {
    1
}

fn default_strategy() -> String {
    "rolling".to_string()
}

fn default_dockerfile() -> Option<String> {
    Some("Dockerfile".to_string())
}

fn default_driver() -> String {
    "local".to_string()
}

fn default_network_driver() -> String {
    "selur".to_string()
}

impl ComposeFile {
    /// Load compose file from path
    pub fn load(path: impl AsRef<Path>) -> Result<Self> {
        let path = path.as_ref();
        let contents = std::fs::read_to_string(path)
            .with_context(|| format!("Failed to read {}", path.display()))?;

        let compose: ComposeFile = toml::from_str(&contents)
            .with_context(|| format!("Failed to parse {}", path.display()))?;

        compose.validate()?;

        Ok(compose)
    }

    /// Validate compose file
    fn validate(&self) -> Result<()> {
        // Check version
        if self.version != "1.0" {
            anyhow::bail!("Unsupported version: {} (expected 1.0)", self.version);
        }

        // Check all services have images
        for (name, service) in &self.services {
            if service.image.is_empty() {
                anyhow::bail!("Service '{}' missing required field 'image'", name);
            }

            // Check dependencies exist
            for dep in &service.depends_on {
                if !self.services.contains_key(dep) {
                    anyhow::bail!("Service '{}' depends on unknown service '{}'", name, dep);
                }
            }

            // Check networks exist
            for network in &service.networks {
                if !self.networks.contains_key(network) {
                    anyhow::bail!(
                        "Service '{}' references unknown network '{}'",
                        name,
                        network
                    );
                }
            }

            // Check volumes exist
            for volume_ref in &service.volumes {
                // Handle both named volumes and bind mounts
                if !volume_ref.starts_with("./") && !volume_ref.starts_with("/") {
                    let volume_name = volume_ref.split(':').next().unwrap_or(volume_ref);
                    if !self.volumes.contains_key(volume_name) {
                        anyhow::bail!(
                            "Service '{}' references unknown volume '{}'",
                            name,
                            volume_name
                        );
                    }
                }
            }

            // Check secrets exist
            for secret in &service.secrets {
                if !self.secrets.contains_key(secret) {
                    anyhow::bail!(
                        "Service '{}' references unknown secret '{}'",
                        name,
                        secret
                    );
                }
            }
        }

        Ok(())
    }

    /// Get default project name (current directory name)
    pub fn default_project_name(&self) -> String {
        std::env::current_dir()
            .ok()
            .and_then(|p| p.file_name().map(|n| n.to_string_lossy().to_string()))
            .unwrap_or_else(|| "selur-project".to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_basic_compose() {
        let toml = r#"
            version = "1.0"

            [services.web]
            image = "nginx:latest.ctp"
            ports = ["8080:80"]
        "#;

        let compose: ComposeFile = toml::from_str(toml).unwrap();
        assert_eq!(compose.version, "1.0");
        assert_eq!(compose.services.len(), 1);
        assert_eq!(compose.services["web"].image, "nginx:latest.ctp");
    }

    #[test]
    fn test_validate_missing_dependency() {
        let toml = r#"
            version = "1.0"

            [services.web]
            image = "nginx:latest.ctp"
            depends_on = ["api"]
        "#;

        let compose: ComposeFile = toml::from_str(toml).unwrap();
        assert!(compose.validate().is_err());
    }
}
