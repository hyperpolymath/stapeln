// SPDX-License-Identifier: PMPL-1.0-or-later
//! FFI bindings to the Ada/SPARK Gatekeeper
//!
//! This module provides safe Rust wrappers around the formally verified
//! Ada/SPARK security policy validation code.

use std::ffi::{CStr, CString};
use std::os::raw::{c_char, c_int};
use thiserror::Error;

/// Errors that can occur during gatekeeper validation
#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum GatekeeperError {
    #[error("SYS_ADMIN capability requires privileged mode")]
    InvalidCapabilities,

    #[error("Root UID (0) requires user namespace to be enabled")]
    InvalidUserNamespace,

    #[error("NET_ADMIN capability requires Restricted or Admin network mode")]
    InvalidNetworkMode,

    #[error("Potential privilege escalation: set no_new_privileges or enable user namespace")]
    InvalidPrivilegeEscape,

    #[error("Failed to parse OCI configuration JSON")]
    ParseError,

    #[error("Internal error in Gatekeeper")]
    InternalError,

    #[error("Null byte found in JSON string")]
    NullByte,

    #[allow(dead_code)]
    #[error("Gatekeeper not initialized")]
    NotInitialized,
}

impl GatekeeperError {
    /// Convert from FFI error code
    fn from_code(code: c_int) -> Result<(), Self> {
        match code {
            0 => Ok(()),
            1 => Err(GatekeeperError::InvalidCapabilities),
            2 => Err(GatekeeperError::InvalidUserNamespace),
            3 => Err(GatekeeperError::InvalidNetworkMode),
            4 => Err(GatekeeperError::InvalidPrivilegeEscape),
            5 => Err(GatekeeperError::ParseError),
            _ => Err(GatekeeperError::InternalError),
        }
    }
}

// FFI declarations
extern "C" {
    fn verify_json_config(json: *const c_char) -> c_int;
    fn get_error_message(code: c_int) -> *const c_char;
    fn sanitise_config(json: *const c_char, output: *mut c_char, size: c_int) -> c_int;
    fn gatekeeper_version() -> *const c_char;
    fn gatekeeper_init() -> c_int;
}

/// Global initialization state
static INIT: std::sync::Once = std::sync::Once::new();
static mut INIT_RESULT: c_int = -1;

/// Initialize the gatekeeper. Must be called before any validation.
/// This is safe to call multiple times - subsequent calls are no-ops.
pub fn init() -> Result<(), GatekeeperError> {
    INIT.call_once(|| {
        // Safety: This is only called once due to std::sync::Once
        unsafe {
            INIT_RESULT = gatekeeper_init();
        }
    });

    // Safety: INIT_RESULT is only written to once in call_once
    if unsafe { INIT_RESULT } == 0 {
        Ok(())
    } else {
        Err(GatekeeperError::InternalError)
    }
}

/// Validate an OCI runtime specification through the SPARK Gatekeeper.
///
/// # Arguments
/// * `json_content` - The OCI runtime configuration JSON string
///
/// # Returns
/// * `Ok(())` if the configuration passes all security checks
/// * `Err(GatekeeperError)` describing why validation failed
///
/// # Safety
/// This function calls into Ada code via FFI. The Ada code is formally
/// verified to be free of runtime errors for all inputs.
///
/// # Example
/// ```ignore
/// use vordr::ffi::gatekeeper;
///
/// gatekeeper::init().unwrap();
/// let config = r#"{"process": {"user": {"uid": 1000}}}"#;
/// match gatekeeper::validate_oci_config(config) {
///     Ok(()) => println!("Configuration is secure"),
///     Err(e) => eprintln!("Validation failed: {}", e),
/// }
/// ```
pub fn validate_oci_config(json_content: &str) -> Result<(), GatekeeperError> {
    let c_str = CString::new(json_content).map_err(|_| GatekeeperError::NullByte)?;

    // Safety: c_str is a valid null-terminated string,
    // and verify_json_config is proven to handle all inputs safely
    let result = unsafe { verify_json_config(c_str.as_ptr()) };

    GatekeeperError::from_code(result)
}

/// Get a human-readable description of an error code.
///
/// # Arguments
/// * `code` - The error code returned by validation
///
/// # Returns
/// A string describing the error
#[allow(dead_code)]
pub fn get_error_description(code: i32) -> String {
    // Safety: get_error_message returns a pointer to a static string
    unsafe {
        let ptr = get_error_message(code as c_int);
        if ptr.is_null() {
            return "Unknown error".to_string();
        }
        CStr::from_ptr(ptr).to_string_lossy().into_owned()
    }
}

/// Sanitise an OCI configuration by applying security defaults.
///
/// # Arguments
/// * `json_content` - The OCI runtime configuration JSON string
///
/// # Returns
/// * `Ok(String)` containing the sanitised configuration
/// * `Err(GatekeeperError)` if sanitisation failed
#[allow(dead_code)]
pub fn sanitise_oci_config(json_content: &str) -> Result<String, GatekeeperError> {
    let c_str = CString::new(json_content).map_err(|_| GatekeeperError::NullByte)?;

    // Allocate output buffer
    const BUFFER_SIZE: usize = 65536;
    let mut buffer: Vec<u8> = vec![0; BUFFER_SIZE];

    // Safety: buffer is properly sized and initialized
    let result = unsafe {
        sanitise_config(
            c_str.as_ptr(),
            buffer.as_mut_ptr() as *mut c_char,
            BUFFER_SIZE as c_int,
        )
    };

    if result < 0 {
        return GatekeeperError::from_code(-result).map(|_| String::new());
    }

    // Truncate buffer to actual length
    buffer.truncate(result as usize);

    String::from_utf8(buffer).map_err(|_| GatekeeperError::ParseError)
}

/// Get the gatekeeper library version.
pub fn version() -> String {
    // Safety: gatekeeper_version returns a pointer to a static string
    unsafe {
        let ptr = gatekeeper_version();
        if ptr.is_null() {
            return "unknown".to_string();
        }
        CStr::from_ptr(ptr).to_string_lossy().into_owned()
    }
}

/// Builder for constructing validated container configurations.
#[derive(Debug, Clone)]
pub struct ConfigValidator {
    privileged: bool,
    user_namespace: bool,
    user_id: u32,
    network_mode: NetworkMode,
    capabilities: Vec<String>,
    no_new_privileges: bool,
    readonly_rootfs: bool,
}

/// Network privilege mode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum NetworkMode {
    #[default]
    Unprivileged,
    Restricted,
    Admin,
}

impl Default for ConfigValidator {
    fn default() -> Self {
        Self::new()
    }
}

impl ConfigValidator {
    /// Create a new configuration validator with secure defaults.
    pub fn new() -> Self {
        Self {
            privileged: false,
            user_namespace: true,
            user_id: 1000,
            network_mode: NetworkMode::Unprivileged,
            capabilities: Vec::new(),
            no_new_privileges: true,
            readonly_rootfs: true,
        }
    }

    /// Set privileged mode (bypasses security checks).
    pub fn privileged(mut self, privileged: bool) -> Self {
        self.privileged = privileged;
        self
    }

    /// Enable or disable user namespace.
    pub fn user_namespace(mut self, enabled: bool) -> Self {
        self.user_namespace = enabled;
        self
    }

    /// Set the user ID to run as.
    pub fn user_id(mut self, uid: u32) -> Self {
        self.user_id = uid;
        self
    }

    /// Set network mode.
    pub fn network_mode(mut self, mode: NetworkMode) -> Self {
        self.network_mode = mode;
        self
    }

    /// Add a capability.
    pub fn add_capability(mut self, cap: impl Into<String>) -> Self {
        self.capabilities.push(cap.into());
        self
    }

    /// Set no_new_privileges flag.
    #[allow(dead_code)]
    pub fn no_new_privileges(mut self, enabled: bool) -> Self {
        self.no_new_privileges = enabled;
        self
    }

    /// Set readonly root filesystem.
    pub fn readonly_rootfs(mut self, readonly: bool) -> Self {
        self.readonly_rootfs = readonly;
        self
    }

    /// Build and validate the configuration.
    pub fn validate(self) -> Result<ValidatedConfig, GatekeeperError> {
        // Build a minimal OCI config JSON for validation
        let config_json = self.to_json();

        // Validate through the gatekeeper
        validate_oci_config(&config_json)?;

        Ok(ValidatedConfig {
            privileged: self.privileged,
            user_namespace: self.user_namespace,
            user_id: self.user_id,
            network_mode: self.network_mode,
            capabilities: self.capabilities,
            no_new_privileges: self.no_new_privileges,
            readonly_rootfs: self.readonly_rootfs,
        })
    }

    fn to_json(&self) -> String {
        let network_mode = match self.network_mode {
            NetworkMode::Unprivileged => "unprivileged",
            NetworkMode::Restricted => "restricted",
            NetworkMode::Admin => "admin",
        };

        format!(
            r#"{{
                "process": {{
                    "user": {{ "uid": {} }},
                    "noNewPrivileges": {}
                }},
                "root": {{ "readonly": {} }},
                "linux": {{
                    "namespaces": [{}],
                    "network_mode": "{}"
                }}
            }}"#,
            self.user_id,
            self.no_new_privileges,
            self.readonly_rootfs,
            if self.user_namespace {
                r#"{"type": "user"}"#
            } else {
                ""
            },
            network_mode
        )
    }
}

/// A container configuration that has passed security validation.
#[derive(Debug, Clone)]
pub struct ValidatedConfig {
    pub privileged: bool,
    pub user_namespace: bool,
    pub user_id: u32,
    #[allow(dead_code)]
    pub network_mode: NetworkMode,
    pub capabilities: Vec<String>,
    pub no_new_privileges: bool,
    pub readonly_rootfs: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn setup() {
        let _ = init();
    }

    #[test]
    fn test_version() {
        setup();
        let ver = version();
        assert!(!ver.is_empty());
    }

    #[test]
    fn test_validate_secure_config() {
        setup();
        let config = r#"{"process": {"user": {"uid": 1000}}}"#;
        let result = validate_oci_config(config);
        assert!(result.is_ok());
    }

    #[test]
    fn test_empty_config_fails() {
        setup();
        let result = validate_oci_config("");
        assert!(result.is_err());
    }

    #[test]
    fn test_config_builder_defaults() {
        setup();
        let result = ConfigValidator::new().validate();
        assert!(result.is_ok());
    }

    #[test]
    fn test_config_builder_with_user_id() {
        setup();
        let result = ConfigValidator::new().user_id(0).user_namespace(true).validate();
        assert!(result.is_ok());
    }

    #[test]
    fn test_error_description() {
        setup();
        let desc = get_error_description(1);
        assert!(!desc.is_empty());
        assert!(desc.contains("SYS_ADMIN") || desc.contains("capability"));
    }
}
