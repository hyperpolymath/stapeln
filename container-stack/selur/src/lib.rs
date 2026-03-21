// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
//
// selur — Ephapax-linear WASM sealant bridge between Svalinn and Vörðr.
//
// Architecture:
//   1. Svalinn writes a binary request into WASM linear memory
//   2. WASM send_request() validates and enqueues, returns a handle
//   3. Rust host polls pending handles, reads command + payload
//   4. Rust host dispatches to Vörðr via HTTP REST API
//   5. Rust host writes Vörðr's response into WASM memory, calls fulfill_request()
//   6. Svalinn calls get_response() to retrieve the fulfilled result
//
// The WASM module is the zero-copy shared memory region. The Rust host is the
// bridge that translates between WASM queue handles and Vörðr HTTP calls.

#![forbid(unsafe_code)]

use std::path::Path;
use wasmtime::*;

/// Binary protocol constants (must match zig/runtime.zig).
const RESPONSE_HEADER_SIZE: usize = 5;

/// Container command types (must match zig/runtime.zig Command enum).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Command {
    CreateContainer = 1,
    StartContainer = 2,
    StopContainer = 3,
    InspectContainer = 4,
    DeleteContainer = 5,
    ListContainers = 6,
}

impl Command {
    pub fn from_u8(v: u8) -> Option<Self> {
        match v {
            1 => Some(Self::CreateContainer),
            2 => Some(Self::StartContainer),
            3 => Some(Self::StopContainer),
            4 => Some(Self::InspectContainer),
            5 => Some(Self::DeleteContainer),
            6 => Some(Self::ListContainers),
            _ => None,
        }
    }

    /// Map command to Vörðr REST API method and path.
    /// Returns (HTTP method, path template).
    pub fn vordr_endpoint(&self) -> (&'static str, &'static str) {
        match self {
            Self::CreateContainer => ("POST", "/api/v1/containers"),
            Self::StartContainer => ("POST", "/api/v1/containers/{id}/start"),
            Self::StopContainer => ("POST", "/api/v1/containers/{id}/stop"),
            Self::InspectContainer => ("GET", "/api/v1/containers/{id}"),
            Self::DeleteContainer => ("DELETE", "/api/v1/containers/{id}"),
            Self::ListContainers => ("GET", "/api/v1/containers"),
        }
    }
}

/// Error codes returned by the WASM module.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum StatusCode {
    Success = 0,
    InvalidRequest = 1,
    ContainerNotFound = 2,
    ContainerAlreadyExists = 3,
    ResourceExhausted = 4,
    PermissionDenied = 5,
    InternalError = 6,
}

impl StatusCode {
    pub fn from_u8(v: u8) -> Option<Self> {
        match v {
            0 => Some(Self::Success),
            1 => Some(Self::InvalidRequest),
            2 => Some(Self::ContainerNotFound),
            3 => Some(Self::ContainerAlreadyExists),
            4 => Some(Self::ResourceExhausted),
            5 => Some(Self::PermissionDenied),
            6 => Some(Self::InternalError),
            _ => None,
        }
    }
}

impl std::fmt::Display for StatusCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Success => write!(f, "Success"),
            Self::InvalidRequest => write!(f, "Invalid request"),
            Self::ContainerNotFound => write!(f, "Container not found"),
            Self::ContainerAlreadyExists => write!(f, "Container already exists"),
            Self::ResourceExhausted => write!(f, "Resource exhausted"),
            Self::PermissionDenied => write!(f, "Permission denied"),
            Self::InternalError => write!(f, "Internal error"),
        }
    }
}

/// Vörðr HTTP client configuration.
#[derive(Debug, Clone)]
pub struct VordrConfig {
    /// Base URL for the Vörðr REST API (e.g. "http://127.0.0.1:4000").
    pub base_url: String,
    /// Request timeout in seconds.
    pub timeout_secs: u64,
}

impl Default for VordrConfig {
    fn default() -> Self {
        Self {
            base_url: std::env::var("VORDR_ENDPOINT")
                .unwrap_or_else(|_| "http://127.0.0.1:4000".to_string()),
            timeout_secs: 10,
        }
    }
}

/// The main bridge interface for zero-copy IPC between Svalinn and Vörðr.
///
/// Manages a WASM instance of the selur sealant and dispatches queued
/// requests to Vörðr's HTTP REST API.
pub struct Bridge {
    #[allow(dead_code)]
    engine: Engine,
    #[allow(dead_code)]
    module: Module,
    store: Store<()>,
    instance: Instance,
    vordr: VordrConfig,
}

impl Bridge {
    /// Load selur.wasm and initialize the bridge with default Vörðr config.
    pub fn new(wasm_path: impl AsRef<Path>) -> Result<Self> {
        Self::with_config(wasm_path, VordrConfig::default())
    }

    /// Load selur.wasm with explicit Vörðr configuration.
    pub fn with_config(wasm_path: impl AsRef<Path>, vordr: VordrConfig) -> Result<Self> {
        let engine = Engine::default();
        let module = Module::from_file(&engine, wasm_path)?;
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[])?;

        Ok(Self {
            engine,
            module,
            store,
            instance,
            vordr,
        })
    }

    /// Get the WASM memory export.
    fn memory(&mut self) -> Result<Memory> {
        self.instance
            .get_memory(&mut self.store, "memory")
            .ok_or_else(|| anyhow::anyhow!("Failed to get WASM memory export"))
    }

    /// Get the WASM linear memory size in bytes.
    pub fn memory_size(&mut self) -> Result<usize> {
        Ok(self.memory()?.data_size(&self.store))
    }

    /// Send a request through the bridge and get the response.
    ///
    /// This is the main entry point. It:
    /// 1. Writes the request into WASM memory
    /// 2. Calls WASM send_request() to enqueue it
    /// 3. Dispatches the pending request to Vörðr via HTTP
    /// 4. Writes Vörðr's response back into WASM memory
    /// 5. Calls fulfill_request() to mark it complete
    /// 6. Calls get_response() to read the result
    ///
    /// The request must be in binary protocol format:
    ///   [command:1B][payload_len:4B LE][payload:NB]
    pub fn send_request(&mut self, request: &[u8]) -> Result<Vec<u8>> {
        if request.len() < 5 {
            return Err(anyhow::anyhow!("Request too short (need >= 5 bytes)"));
        }

        // --- Step 1: Allocate WASM memory and write request ---
        let allocate = self
            .instance
            .get_typed_func::<u32, u32>(&mut self.store, "allocate")?;
        let request_ptr = allocate.call(&mut self.store, request.len() as u32)?;
        if request_ptr == 0 {
            return Err(anyhow::anyhow!("WASM memory allocation failed"));
        }

        let memory = self.memory()?;
        memory.write(&mut self.store, request_ptr as usize, request)?;

        // --- Step 2: Enqueue the request in WASM ---
        let send_request_fn = self
            .instance
            .get_typed_func::<(u32, u32), u32>(&mut self.store, "send_request")?;
        let handle = send_request_fn.call(&mut self.store, (request_ptr, request.len() as u32))?;

        if handle == 0 {
            // Deallocate and report error
            let deallocate = self
                .instance
                .get_typed_func::<(u32, u32), ()>(&mut self.store, "deallocate")?;
            deallocate.call(&mut self.store, (request_ptr, request.len() as u32))?;
            return Err(anyhow::anyhow!("WASM send_request validation failed"));
        }

        // --- Step 3: Read the pending request and dispatch to Vörðr ---
        let get_cmd = self
            .instance
            .get_typed_func::<u32, u32>(&mut self.store, "get_request_command")?;
        let get_payload_ptr = self
            .instance
            .get_typed_func::<u32, u32>(&mut self.store, "get_request_payload_ptr")?;
        let get_payload_len = self
            .instance
            .get_typed_func::<u32, u32>(&mut self.store, "get_request_payload_len")?;

        let cmd_byte = get_cmd.call(&mut self.store, handle)? as u8;
        let payload_ptr = get_payload_ptr.call(&mut self.store, handle)?;
        let payload_len = get_payload_len.call(&mut self.store, handle)?;

        // Read payload from WASM memory
        let mut payload = vec![0u8; payload_len as usize];
        if payload_len > 0 {
            let mem = self.memory()?;
            mem.read(&self.store, payload_ptr as usize, &mut payload)?;
        }

        let command = Command::from_u8(cmd_byte)
            .ok_or_else(|| anyhow::anyhow!("Unknown command: {cmd_byte}"))?;

        // Call Vörðr
        let vordr_response = self.dispatch_to_vordr(command, &payload)?;

        // --- Step 4: Write response into WASM memory ---
        let response_alloc = self
            .instance
            .get_typed_func::<u32, u32>(&mut self.store, "allocate")?;
        let response_ptr = response_alloc.call(&mut self.store, vordr_response.len() as u32)?;
        if response_ptr == 0 {
            return Err(anyhow::anyhow!(
                "WASM allocation failed for response ({} bytes)",
                vordr_response.len()
            ));
        }

        let mem = self.memory()?;
        mem.write(
            &mut self.store,
            response_ptr as usize,
            &vordr_response,
        )?;

        // --- Step 5: Fulfill the request ---
        let fulfill = self
            .instance
            .get_typed_func::<(u32, u32, u32), u32>(&mut self.store, "fulfill_request")?;
        let fulfill_result = fulfill.call(
            &mut self.store,
            (handle, response_ptr, vordr_response.len() as u32),
        )?;

        if fulfill_result != 0 {
            return Err(anyhow::anyhow!(
                "fulfill_request failed with code {fulfill_result}"
            ));
        }

        // --- Step 6: Read the response via get_response ---
        // Allocate a buffer for the Response struct (32 bytes: 4+4+4+8 = 20, padded)
        let resp_struct_size = 32u32;
        let resp_buf_ptr = allocate.call(&mut self.store, resp_struct_size)?;

        let get_response_fn = self
            .instance
            .get_typed_func::<u32, u32>(&mut self.store, "get_response")?;
        let status = get_response_fn.call(&mut self.store, resp_buf_ptr)?;

        // Extract response data: skip the status byte, read data portion
        // The actual data is in the WASM memory at the response_ptr we wrote
        // Parse the binary protocol: [status:1B][data_len:4B LE][data:NB]
        if vordr_response.len() >= RESPONSE_HEADER_SIZE {
            let data_len = u32::from_le_bytes([
                vordr_response[1],
                vordr_response[2],
                vordr_response[3],
                vordr_response[4],
            ]) as usize;
            let data = vordr_response
                .get(RESPONSE_HEADER_SIZE..RESPONSE_HEADER_SIZE + data_len)
                .unwrap_or(&[])
                .to_vec();

            // Cleanup: deallocate request and response buffers
            let deallocate = self
                .instance
                .get_typed_func::<(u32, u32), ()>(&mut self.store, "deallocate")?;
            deallocate.call(&mut self.store, (request_ptr, request.len() as u32))?;
            deallocate.call(&mut self.store, (response_ptr, vordr_response.len() as u32))?;
            deallocate.call(&mut self.store, (resp_buf_ptr, resp_struct_size))?;

            if status != 0 {
                let code = StatusCode::from_u8(status as u8)
                    .unwrap_or(StatusCode::InternalError);
                return Err(anyhow::anyhow!("Vörðr returned error: {code}"));
            }

            Ok(data)
        } else {
            Err(anyhow::anyhow!("Malformed response from Vörðr"))
        }
    }

    /// Dispatch a command to Vörðr's REST API and return a binary response.
    ///
    /// The response is in binary protocol format: [status:1B][data_len:4B LE][data:NB]
    fn dispatch_to_vordr(&self, command: Command, payload: &[u8]) -> Result<Vec<u8>> {
        let (method, path_template) = command.vordr_endpoint();

        // Extract container ID from payload for commands that need it
        let payload_str = std::str::from_utf8(payload).unwrap_or("");
        let path = if path_template.contains("{id}") {
            // Payload is the container name/ID for single-container commands
            let id = payload_str.trim();
            if id.is_empty() {
                return Ok(make_error_response(StatusCode::InvalidRequest, "missing container ID"));
            }
            path_template.replace("{id}", id)
        } else {
            path_template.to_string()
        };

        let url = format!("{}{}", self.vordr.base_url, path);

        let agent = ureq::AgentBuilder::new()
            .timeout(std::time::Duration::from_secs(self.vordr.timeout_secs))
            .build();

        let result = match method {
            "GET" => agent.get(&url).call(),
            "DELETE" => agent.delete(&url).call(),
            "POST" => {
                if payload_str.is_empty() || !payload_str.starts_with('{') {
                    // No JSON body or not JSON — send empty POST
                    agent.post(&url).call()
                } else {
                    agent
                        .post(&url)
                        .set("Content-Type", "application/json")
                        .send_string(payload_str)
                }
            }
            _ => return Ok(make_error_response(StatusCode::InternalError, "unsupported HTTP method")),
        };

        match result {
            Ok(resp) => {
                let body = resp.into_string().unwrap_or_default();
                Ok(make_success_response(&body))
            }
            Err(ureq::Error::Status(404, _)) => {
                Ok(make_error_response(StatusCode::ContainerNotFound, "not found"))
            }
            Err(ureq::Error::Status(403, _)) => {
                Ok(make_error_response(StatusCode::PermissionDenied, "forbidden"))
            }
            Err(ureq::Error::Status(409, _)) => {
                Ok(make_error_response(StatusCode::ContainerAlreadyExists, "conflict"))
            }
            Err(ureq::Error::Status(code, resp)) => {
                let body = resp.into_string().unwrap_or_default();
                Ok(make_error_response(
                    StatusCode::InternalError,
                    &format!("HTTP {code}: {body}"),
                ))
            }
            Err(ureq::Error::Transport(e)) => {
                Ok(make_error_response(
                    StatusCode::InternalError,
                    &format!("transport error: {e}"),
                ))
            }
        }
    }
}

/// Build a binary protocol success response: [0x00][data_len:4B LE][data]
fn make_success_response(data: &str) -> Vec<u8> {
    let data_bytes = data.as_bytes();
    let mut resp = Vec::with_capacity(RESPONSE_HEADER_SIZE + data_bytes.len());
    resp.push(StatusCode::Success as u8);
    resp.extend_from_slice(&(data_bytes.len() as u32).to_le_bytes());
    resp.extend_from_slice(data_bytes);
    resp
}

/// Build a binary protocol error response: [status][msg_len:4B LE][msg]
fn make_error_response(status: StatusCode, msg: &str) -> Vec<u8> {
    let msg_bytes = msg.as_bytes();
    let mut resp = Vec::with_capacity(RESPONSE_HEADER_SIZE + msg_bytes.len());
    resp.push(status as u8);
    resp.extend_from_slice(&(msg_bytes.len() as u32).to_le_bytes());
    resp.extend_from_slice(msg_bytes);
    resp
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_status_code_conversion() {
        assert_eq!(StatusCode::from_u8(0), Some(StatusCode::Success));
        assert_eq!(StatusCode::from_u8(1), Some(StatusCode::InvalidRequest));
        assert_eq!(StatusCode::from_u8(6), Some(StatusCode::InternalError));
        assert_eq!(StatusCode::from_u8(99), None);
    }

    #[test]
    fn test_status_code_display() {
        assert_eq!(StatusCode::Success.to_string(), "Success");
        assert_eq!(StatusCode::ContainerNotFound.to_string(), "Container not found");
    }

    #[test]
    fn test_command_from_u8() {
        assert_eq!(Command::from_u8(1), Some(Command::CreateContainer));
        assert_eq!(Command::from_u8(6), Some(Command::ListContainers));
        assert_eq!(Command::from_u8(0), None);
        assert_eq!(Command::from_u8(7), None);
    }

    #[test]
    fn test_command_endpoints() {
        assert_eq!(
            Command::CreateContainer.vordr_endpoint(),
            ("POST", "/api/v1/containers")
        );
        assert_eq!(
            Command::InspectContainer.vordr_endpoint(),
            ("GET", "/api/v1/containers/{id}")
        );
        assert_eq!(
            Command::ListContainers.vordr_endpoint(),
            ("GET", "/api/v1/containers")
        );
    }

    #[test]
    fn test_make_success_response() {
        let resp = make_success_response("hello");
        assert_eq!(resp[0], 0x00); // Success
        let len = u32::from_le_bytes([resp[1], resp[2], resp[3], resp[4]]);
        assert_eq!(len, 5);
        assert_eq!(&resp[5..], b"hello");
    }

    #[test]
    fn test_make_error_response() {
        let resp = make_error_response(StatusCode::ContainerNotFound, "not found");
        assert_eq!(resp[0], 0x02); // ContainerNotFound
        let len = u32::from_le_bytes([resp[1], resp[2], resp[3], resp[4]]);
        assert_eq!(len, 9);
        assert_eq!(&resp[5..], b"not found");
    }

    #[test]
    fn test_vordr_config_default() {
        let config = VordrConfig::default();
        assert_eq!(config.timeout_secs, 10);
        // base_url comes from env or defaults to localhost:4000
    }

    #[test]
    #[ignore] // Requires selur.wasm to be built
    fn test_bridge_load() {
        let bridge = Bridge::new("zig/zig-out/bin/selur.wasm");
        assert!(bridge.is_ok(), "Failed to load WASM module");
    }

    #[test]
    #[ignore] // Requires selur.wasm + Vörðr running
    fn test_full_roundtrip() {
        let mut bridge = Bridge::new("zig/zig-out/bin/selur.wasm").unwrap();

        // List containers request: [cmd=6][payload_len=0][no payload]
        let mut request = Vec::new();
        request.push(Command::ListContainers as u8);
        request.extend_from_slice(&0u32.to_le_bytes());

        let result = bridge.send_request(&request);
        match result {
            Ok(data) => {
                let body = std::str::from_utf8(&data).unwrap_or("<binary>");
                println!("Response: {body}");
            }
            Err(e) => {
                // Acceptable in test — Vörðr may not be running
                println!("Expected error (Vörðr not running): {e}");
            }
        }
    }
}
