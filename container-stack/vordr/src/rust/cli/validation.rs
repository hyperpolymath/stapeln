// SPDX-License-Identifier: PMPL-1.0-or-later
//! Input validation utilities for CLI arguments

use anyhow::{bail, Result};

/// Validate a container ID or name to prevent path traversal and injection attacks
///
/// Container IDs must be:
/// - 1-64 characters long
/// - Alphanumeric characters, hyphens, and underscores only
/// - No path traversal sequences (../, /, etc.)
///
/// # Arguments
/// * `id` - The container ID or name to validate
///
/// # Returns
/// * `Ok(())` if valid
/// * `Err` with descriptive message if invalid
pub fn validate_container_id(id: &str) -> Result<()> {
    // Check length
    if id.is_empty() {
        bail!("Container ID cannot be empty");
    }

    if id.len() > 64 {
        bail!("Container ID too long (max 64 characters)");
    }

    // Check for path traversal
    if id.contains("..") || id.contains('/') || id.contains('\\') {
        bail!("Container ID contains invalid characters (path traversal attempt?)");
    }

    // Check characters are alphanumeric, hyphen, or underscore
    if !id.chars().all(|c| c.is_alphanumeric() || c == '-' || c == '_') {
        bail!("Container ID must contain only alphanumeric characters, hyphens, and underscores");
    }

    Ok(())
}

/// Validate a network name to prevent path traversal
///
/// Network names must be:
/// - 1-253 characters (DNS label limit)
/// - Lowercase alphanumeric, hyphens, and underscores only
/// - No path traversal sequences
#[allow(dead_code)]
pub fn validate_network_name(name: &str) -> Result<()> {
    if name.is_empty() {
        bail!("Network name cannot be empty");
    }

    if name.len() > 253 {
        bail!("Network name too long (max 253 characters)");
    }

    if name.contains("..") || name.contains('/') || name.contains('\\') {
        bail!("Network name contains invalid characters");
    }

    if !name.chars().all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-' || c == '_') {
        bail!("Network name must be lowercase alphanumeric with hyphens/underscores only");
    }

    Ok(())
}

/// Validate a volume name or path
#[allow(dead_code)]
pub fn validate_volume_spec(spec: &str) -> Result<()> {
    // Split on : to get source and destination
    let parts: Vec<&str> = spec.split(':').collect();

    if parts.is_empty() || parts.len() > 3 {
        bail!("Invalid volume specification format");
    }

    // Basic check - don't allow .. in paths
    for part in &parts {
        if part.contains("..") {
            bail!("Volume specification contains path traversal");
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_container_ids() {
        assert!(validate_container_id("abc123").is_ok());
        assert!(validate_container_id("my-container").is_ok());
        assert!(validate_container_id("test_container_1").is_ok());
        assert!(validate_container_id("a1b2c3d4").is_ok());
    }

    #[test]
    fn test_invalid_container_ids() {
        assert!(validate_container_id("").is_err());
        assert!(validate_container_id("../etc/passwd").is_err());
        assert!(validate_container_id("test/path").is_err());
        assert!(validate_container_id("test\\path").is_err());
        assert!(validate_container_id("test..path").is_err());
        assert!(validate_container_id("test with spaces").is_err());
        assert!(validate_container_id("test@container").is_err());
    }

    #[test]
    fn test_valid_network_names() {
        assert!(validate_network_name("bridge").is_ok());
        assert!(validate_network_name("my-network").is_ok());
        assert!(validate_network_name("network_1").is_ok());
    }

    #[test]
    fn test_invalid_network_names() {
        assert!(validate_network_name("").is_err());
        assert!(validate_network_name("My-Network").is_err()); // uppercase
        assert!(validate_network_name("../network").is_err());
        assert!(validate_network_name("net/work").is_err());
    }
}
