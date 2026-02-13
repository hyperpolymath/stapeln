// SPDX-License-Identifier: PMPL-1.0-or-later
//! Netavark integration for container networking

use serde::{Deserialize, Serialize};
use std::path::Path;
use std::process::{Command, Stdio};
use thiserror::Error;
use tracing::{debug, info};

#[derive(Error, Debug)]
pub enum NetworkError {
    #[error("Netavark not found in PATH")]
    NotFound,
    #[error("Netavark execution failed: {0}")]
    ExecutionFailed(String),
    #[error("Invalid network configuration: {0}")]
    InvalidConfig(String),
    #[error("Network already exists: {0}")]
    AlreadyExists(String),
    #[error("Network not found: {0}")]
    NetworkNotFound(String),
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),
}

/// Network configuration for a container
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkConfig {
    pub container_id: String,
    pub container_name: String,
    pub networks: Vec<NetworkAttachment>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub port_mappings: Option<Vec<PortMapping>>,
}

/// Network attachment specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkAttachment {
    pub network_name: String,
    pub interface_name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub static_ips: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub aliases: Option<Vec<String>>,
}

/// Port mapping specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PortMapping {
    pub host_ip: Option<String>,
    pub container_port: u16,
    pub host_port: u16,
    pub protocol: String,
}

/// Result of network setup
#[derive(Debug, Clone, Deserialize)]
pub struct NetworkResult {
    pub interfaces: Vec<InterfaceResult>,
}

/// Result for a single interface
#[derive(Debug, Clone, Deserialize)]
pub struct InterfaceResult {
    pub name: String,
    pub mac_address: String,
    pub subnets: Vec<SubnetResult>,
}

/// Result for a subnet
#[derive(Debug, Clone, Deserialize)]
pub struct SubnetResult {
    pub ipnet: String,
    pub gateway: Option<String>,
}

/// Network definition for creating new networks
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkDefinition {
    pub name: String,
    pub id: String,
    pub driver: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub network_interface: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub subnets: Option<Vec<Subnet>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ipv6_enabled: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub internal: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dns_enabled: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub options: Option<std::collections::HashMap<String, String>>,
}

/// Subnet definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Subnet {
    pub subnet: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub gateway: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lease_range: Option<LeaseRange>,
}

/// DHCP lease range
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LeaseRange {
    pub start_ip: String,
    pub end_ip: String,
}

/// Network manager using Netavark
pub struct NetworkManager {
    netavark_path: String,
    config_dir: String,
    run_dir: String,
}

impl NetworkManager {
    /// Create a new network manager
    pub fn new(config_dir: impl Into<String>, run_dir: impl Into<String>) -> Result<Self, NetworkError> {
        let netavark_path = which::which("netavark")
            .map_err(|_| NetworkError::NotFound)?
            .to_string_lossy()
            .into_owned();

        Ok(Self {
            netavark_path,
            config_dir: config_dir.into(),
            run_dir: run_dir.into(),
        })
    }

    /// Create a new network manager with a custom netavark path
    pub fn with_path(
        netavark_path: impl Into<String>,
        config_dir: impl Into<String>,
        run_dir: impl Into<String>,
    ) -> Self {
        Self {
            netavark_path: netavark_path.into(),
            config_dir: config_dir.into(),
            run_dir: run_dir.into(),
        }
    }

    /// Set up networking for a container
    pub fn setup(&self, config: &NetworkConfig, netns_path: &str) -> Result<NetworkResult, NetworkError> {
        info!(
            "Setting up network for container {} at {}",
            config.container_id, netns_path
        );

        let config_json = serde_json::to_string(config)?;
        debug!("Network config: {}", config_json);

        let output = Command::new(&self.netavark_path)
            .args(["setup", netns_path])
            .env("NETAVARK_CONFIG", &self.config_dir)
            .env("NETAVARK_TMPDIR", &self.run_dir)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .and_then(|mut child| {
                use std::io::Write;
                if let Some(stdin) = child.stdin.as_mut() {
                    stdin.write_all(config_json.as_bytes())?;
                }
                child.wait_with_output()
            })?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(NetworkError::ExecutionFailed(stderr.into_owned()));
        }

        serde_json::from_slice(&output.stdout).map_err(|e| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            NetworkError::InvalidConfig(format!("failed to parse result: {} (output: {})", e, stdout))
        })
    }

    /// Tear down networking for a container
    pub fn teardown(&self, config: &NetworkConfig, netns_path: &str) -> Result<(), NetworkError> {
        info!(
            "Tearing down network for container {}",
            config.container_id
        );

        let config_json = serde_json::to_string(config)?;

        let output = Command::new(&self.netavark_path)
            .args(["teardown", netns_path])
            .env("NETAVARK_CONFIG", &self.config_dir)
            .env("NETAVARK_TMPDIR", &self.run_dir)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .and_then(|mut child| {
                use std::io::Write;
                if let Some(stdin) = child.stdin.as_mut() {
                    stdin.write_all(config_json.as_bytes())?;
                }
                child.wait_with_output()
            })?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(NetworkError::ExecutionFailed(stderr.into_owned()));
        }

        Ok(())
    }

    /// Create a new network
    pub fn create_network(&self, definition: &NetworkDefinition) -> Result<(), NetworkError> {
        info!("Creating network: {}", definition.name);

        // Write network definition to config directory
        let network_file = Path::new(&self.config_dir).join(format!("{}.json", definition.name));
        let json = serde_json::to_string_pretty(definition)?;
        std::fs::write(&network_file, json)?;

        Ok(())
    }

    /// Delete a network
    pub fn delete_network(&self, name: &str) -> Result<(), NetworkError> {
        info!("Deleting network: {}", name);

        let network_file = Path::new(&self.config_dir).join(format!("{}.json", name));
        if network_file.exists() {
            std::fs::remove_file(&network_file)?;
            Ok(())
        } else {
            Err(NetworkError::NetworkNotFound(name.to_string()))
        }
    }

    /// List networks
    pub fn list_networks(&self) -> Result<Vec<NetworkDefinition>, NetworkError> {
        let config_path = Path::new(&self.config_dir);
        let mut networks = Vec::new();

        if !config_path.exists() {
            return Ok(networks);
        }

        for entry in std::fs::read_dir(config_path)? {
            let entry = entry?;
            let path = entry.path();

            if path.extension().map_or(false, |ext| ext == "json") {
                if let Ok(content) = std::fs::read_to_string(&path) {
                    if let Ok(def) = serde_json::from_str(&content) {
                        networks.push(def);
                    }
                }
            }
        }

        Ok(networks)
    }

    /// Get network configuration directory
    pub fn config_dir(&self) -> &str {
        &self.config_dir
    }

    /// Create default bridge network if it doesn't exist
    pub fn ensure_default_network(&self) -> Result<(), NetworkError> {
        let default_file = Path::new(&self.config_dir).join("vordr.json");

        if default_file.exists() {
            return Ok(());
        }

        // Create config directory
        std::fs::create_dir_all(&self.config_dir)?;

        let default_network = NetworkDefinition {
            name: "vordr".to_string(),
            id: uuid::Uuid::new_v4().to_string(),
            driver: "bridge".to_string(),
            network_interface: Some("vordr0".to_string()),
            subnets: Some(vec![Subnet {
                subnet: "10.89.0.0/24".to_string(),
                gateway: Some("10.89.0.1".to_string()),
                lease_range: None,
            }]),
            ipv6_enabled: Some(false),
            internal: Some(false),
            dns_enabled: Some(true),
            options: None,
        };

        self.create_network(&default_network)?;
        info!("Created default bridge network 'vordr'");

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_network_config_serialization() {
        let config = NetworkConfig {
            container_id: "test123".to_string(),
            container_name: "test-container".to_string(),
            networks: vec![NetworkAttachment {
                network_name: "vordr".to_string(),
                interface_name: "eth0".to_string(),
                static_ips: None,
                aliases: Some(vec!["web".to_string()]),
            }],
            port_mappings: Some(vec![PortMapping {
                host_ip: None,
                container_port: 80,
                host_port: 8080,
                protocol: "tcp".to_string(),
            }]),
        };

        let json = serde_json::to_string(&config).unwrap();
        assert!(json.contains("test123"));
        assert!(json.contains("vordr"));
    }
}
