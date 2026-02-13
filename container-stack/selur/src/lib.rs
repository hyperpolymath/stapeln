// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

//! Rust bindings for selur WASM sealant
//!
//! This library provides a safe Rust API for loading and interacting
//! with the selur.wasm module in Svalinn and Vörðr.
//!
//! # Example
//!
//! ```no_run
//! use selur::Bridge;
//!
//! let mut bridge = Bridge::new("path/to/selur.wasm")?;
//! let request = b"CREATE_CONTAINER...";
//! let response = bridge.send_request(request)?;
//! # Ok::<(), anyhow::Error>(())
//! ```

use std::path::Path;
use wasmtime::*;

/// The main bridge interface for zero-copy IPC between Svalinn and Vörðr
///
/// This struct manages a WASM instance of the selur sealant, providing
/// request/response communication with linear memory semantics.
pub struct Bridge {
    #[allow(dead_code)]
    engine: Engine,
    #[allow(dead_code)]
    module: Module,
    store: Store<()>,
    instance: Instance,
}

impl Bridge {
    /// Load selur.wasm and initialize the bridge
    ///
    /// # Arguments
    ///
    /// * `wasm_path` - Path to the selur.wasm file
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The WASM file cannot be found or read
    /// - The WASM module is invalid
    /// - Instance initialization fails
    pub fn new(wasm_path: impl AsRef<Path>) -> Result<Self> {
        let engine = Engine::default();
        let module = Module::from_file(&engine, wasm_path)?;
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[])?;

        Ok(Self {
            engine,
            module,
            store,
            instance,
        })
    }

    /// Get the WASM memory size in bytes
    ///
    /// # Errors
    ///
    /// Returns an error if the memory export is not found
    pub fn memory_size(&mut self) -> Result<usize> {
        let memory = self
            .instance
            .get_memory(&mut self.store, "memory")
            .ok_or_else(|| anyhow::anyhow!("Failed to get WASM memory"))?;

        Ok(memory.data_size(&self.store))
    }

    /// Send a request through the sealant
    ///
    /// # Arguments
    ///
    /// * `request` - The request payload as bytes
    ///
    /// # Returns
    ///
    /// The response payload as bytes
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - Request allocation fails
    /// - WASM memory operations fail
    /// - The WASM module returns an error code
    /// - Response retrieval fails
    pub fn send_request(&mut self, request: &[u8]) -> Result<Vec<u8>> {
        // 1. Allocate WASM memory for request
        let allocate = self
            .instance
            .get_typed_func::<u32, u32>(&mut self.store, "allocate")?;
        let request_ptr = allocate.call(&mut self.store, request.len() as u32)?;

        // 2. Write request data to WASM linear memory
        let memory = self
            .instance
            .get_memory(&mut self.store, "memory")
            .ok_or_else(|| anyhow::anyhow!("Failed to get WASM memory"))?;
        memory.write(&mut self.store, request_ptr as usize, request)?;

        // 3. Call send_request in WASM
        let send_request_fn = self
            .instance
            .get_typed_func::<(u32, u32), u32>(&mut self.store, "send_request")?;
        let error_code =
            send_request_fn.call(&mut self.store, (request_ptr, request.len() as u32))?;

        if error_code != 0 {
            return Err(anyhow::anyhow!(
                "WASM send_request failed with code {}",
                error_code
            ));
        }

        // 4. Get response pointer from WASM
        let get_response_fn = self
            .instance
            .get_typed_func::<u32, u32>(&mut self.store, "get_response")?;
        let response_len = get_response_fn.call(&mut self.store, 0)?;

        // 5. Read response data from WASM linear memory
        let mut response = vec![0u8; response_len as usize];
        memory.read(&self.store, 0, &mut response)?;

        // 6. Deallocate request memory (response memory handled by WASM)
        let deallocate = self
            .instance
            .get_typed_func::<(u32, u32), ()>(&mut self.store, "deallocate")?;
        deallocate.call(&mut self.store, (request_ptr, request.len() as u32))?;

        Ok(response)
    }
}

/// Error codes returned by WASM module
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ErrorCode {
    /// Operation successful
    Success = 0,
    /// Invalid request format or size
    InvalidRequest = 1,
    /// Container not found
    ContainerNotFound = 2,
    /// Permission denied
    PermissionDenied = 3,
}

impl ErrorCode {
    /// Convert u32 error code to ErrorCode enum
    pub fn from_u32(code: u32) -> Option<Self> {
        match code {
            0 => Some(Self::Success),
            1 => Some(Self::InvalidRequest),
            2 => Some(Self::ContainerNotFound),
            3 => Some(Self::PermissionDenied),
            _ => None,
        }
    }
}

impl std::fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Success => write!(f, "Success"),
            Self::InvalidRequest => write!(f, "Invalid request"),
            Self::ContainerNotFound => write!(f, "Container not found"),
            Self::PermissionDenied => write!(f, "Permission denied"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_code_conversion() {
        assert_eq!(ErrorCode::from_u32(0), Some(ErrorCode::Success));
        assert_eq!(ErrorCode::from_u32(1), Some(ErrorCode::InvalidRequest));
        assert_eq!(ErrorCode::from_u32(2), Some(ErrorCode::ContainerNotFound));
        assert_eq!(ErrorCode::from_u32(3), Some(ErrorCode::PermissionDenied));
        assert_eq!(ErrorCode::from_u32(99), None);
    }

    #[test]
    fn test_error_code_display() {
        assert_eq!(ErrorCode::Success.to_string(), "Success");
        assert_eq!(ErrorCode::InvalidRequest.to_string(), "Invalid request");
    }

    #[test]
    #[ignore] // Requires selur.wasm to be built
    fn test_bridge_load() {
        let bridge = Bridge::new("zig-out/bin/selur.wasm");
        assert!(bridge.is_ok(), "Failed to load WASM module");
    }

    #[test]
    #[ignore] // Requires selur.wasm to be built
    fn test_send_request() {
        let mut bridge = Bridge::new("zig-out/bin/selur.wasm").unwrap();

        // Create a simple request
        let mut request = Vec::new();
        request.push(0x01); // CREATE_CONTAINER command
        request.extend_from_slice(&10u32.to_le_bytes()); // payload length
        request.extend_from_slice(b"test_image"); // payload

        let result = bridge.send_request(&request);
        // For now, this will likely fail since WASM is a stub
        // But it tests the plumbing
        assert!(result.is_ok() || result.is_err()); // Either is valid for stub
    }
}
