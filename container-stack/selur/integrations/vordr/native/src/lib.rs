// SPDX-License-Identifier: PMPL-1.0-or-later
// Elixir NIF bindings for selur WASM bridge
// Enables Vörðr (Elixir orchestrator) to use zero-copy IPC

use rustler::{Encoder, Env, Error, ResourceArc, Term};
use selur::{Bridge, ErrorCode};
use std::sync::Mutex;

mod atoms {
    rustler::atoms! {
        ok,
        error,

        // Error atoms
        invalid_request,
        container_not_found,
        container_already_exists,
        network_error,
        internal_error,
        unknown_error,
    }
}

// Resource wrapper for Bridge (allows Elixir to hold Rust state)
struct BridgeResource {
    bridge: Mutex<Option<Bridge>>,
}

impl BridgeResource {
    fn new(bridge: Bridge) -> Self {
        Self {
            bridge: Mutex::new(Some(bridge)),
        }
    }

    fn take(&self) -> Option<Bridge> {
        self.bridge.lock().ok()?.take()
    }

    fn with_bridge<F, R>(&self, f: F) -> Result<R, Error>
    where
        F: FnOnce(&mut Bridge) -> Result<R, Error>,
    {
        let mut guard = self.bridge.lock().map_err(|_| Error::Term(Box::new("lock poisoned")))?;
        let bridge = guard.as_mut().ok_or_else(|| Error::Term(Box::new("bridge already freed")))?;
        f(bridge)
    }
}

// Register resource type with Rustler
fn load(env: Env, _info: Term) -> bool {
    rustler::resource!(BridgeResource, env);
    true
}

rustler::init!(
    "Elixir.Vordr.Selur.Bridge",
    [
        bridge_new,
        bridge_free,
        bridge_memory_size,
        create_container,
        start_container,
        stop_container,
        inspect_container,
        delete_container,
        list_containers,
    ],
    load = load
);

// Convert ErrorCode to Elixir atom
fn error_code_to_atom(env: Env, code: ErrorCode) -> Term {
    match code {
        ErrorCode::InvalidRequest => atoms::invalid_request().encode(env),
        ErrorCode::ContainerNotFound => atoms::container_not_found().encode(env),
        ErrorCode::ContainerAlreadyExists => atoms::container_already_exists().encode(env),
        ErrorCode::NetworkError => atoms::network_error().encode(env),
        ErrorCode::InternalError => atoms::internal_error().encode(env),
        _ => atoms::unknown_error().encode(env),
    }
}

/// Create new bridge from WASM module path
/// Returns: {:ok, bridge_resource} | {:error, atom}
#[rustler::nif]
fn bridge_new(env: Env, wasm_path: String) -> Term {
    match Bridge::new(wasm_path) {
        Ok(bridge) => {
            let resource = ResourceArc::new(BridgeResource::new(bridge));
            (atoms::ok(), resource).encode(env)
        }
        Err(e) => {
            let error_code = ErrorCode::from(e);
            (atoms::error(), error_code_to_atom(env, error_code)).encode(env)
        }
    }
}

/// Free bridge resources
/// Returns: :ok
#[rustler::nif]
fn bridge_free(resource: ResourceArc<BridgeResource>) -> rustler::Atom {
    let _ = resource.take();
    atoms::ok()
}

/// Get WASM memory size
/// Returns: {:ok, size} | {:error, atom}
#[rustler::nif]
fn bridge_memory_size(env: Env, resource: ResourceArc<BridgeResource>) -> Term {
    match resource.with_bridge(|bridge| {
        bridge.memory_size()
            .map_err(|_| Error::Term(Box::new("memory_size failed")))
    }) {
        Ok(size) => (atoms::ok(), size).encode(env),
        Err(_) => (atoms::error(), atoms::internal_error()).encode(env),
    }
}

/// Create container from image reference
/// Returns: {:ok, container_id} | {:error, atom}
#[rustler::nif]
fn create_container(env: Env, resource: ResourceArc<BridgeResource>, image: String) -> Term {
    match resource.with_bridge(|bridge| {
        let mut request = Vec::new();
        request.push(0x01); // CREATE_CONTAINER

        let image_bytes = image.as_bytes();
        request.extend_from_slice(&(image_bytes.len() as u32).to_le_bytes());
        request.extend_from_slice(image_bytes);

        bridge.send_request(&request)
            .map_err(|e| Error::Term(Box::new(format!("send_request failed: {:?}", e))))
    }) {
        Ok(response) => {
            if response.is_empty() {
                return (atoms::error(), atoms::internal_error()).encode(env);
            }

            let status_code = response[0];
            let error_code = ErrorCode::from(status_code);

            match error_code {
                ErrorCode::Success => {
                    // Parse container ID from response (skip status byte)
                    let container_id = String::from_utf8_lossy(&response[1..]).to_string();
                    (atoms::ok(), container_id).encode(env)
                }
                code => (atoms::error(), error_code_to_atom(env, code)).encode(env),
            }
        }
        Err(_) => (atoms::error(), atoms::internal_error()).encode(env),
    }
}

/// Start container by ID
/// Returns: :ok | {:error, atom}
#[rustler::nif]
fn start_container(env: Env, resource: ResourceArc<BridgeResource>, container_id: String) -> Term {
    match resource.with_bridge(|bridge| {
        let mut request = Vec::new();
        request.push(0x02); // START_CONTAINER

        let id_bytes = container_id.as_bytes();
        request.extend_from_slice(&(id_bytes.len() as u32).to_le_bytes());
        request.extend_from_slice(id_bytes);

        bridge.send_request(&request)
            .map_err(|e| Error::Term(Box::new(format!("send_request failed: {:?}", e))))
    }) {
        Ok(response) => {
            if response.is_empty() {
                return (atoms::error(), atoms::internal_error()).encode(env);
            }

            let status_code = response[0];
            let error_code = ErrorCode::from(status_code);

            match error_code {
                ErrorCode::Success => atoms::ok().encode(env),
                code => (atoms::error(), error_code_to_atom(env, code)).encode(env),
            }
        }
        Err(_) => (atoms::error(), atoms::internal_error()).encode(env),
    }
}

/// Stop container by ID
/// Returns: :ok | {:error, atom}
#[rustler::nif]
fn stop_container(env: Env, resource: ResourceArc<BridgeResource>, container_id: String) -> Term {
    match resource.with_bridge(|bridge| {
        let mut request = Vec::new();
        request.push(0x03); // STOP_CONTAINER

        let id_bytes = container_id.as_bytes();
        request.extend_from_slice(&(id_bytes.len() as u32).to_le_bytes());
        request.extend_from_slice(id_bytes);

        bridge.send_request(&request)
            .map_err(|e| Error::Term(Box::new(format!("send_request failed: {:?}", e))))
    }) {
        Ok(response) => {
            if response.is_empty() {
                return (atoms::error(), atoms::internal_error()).encode(env);
            }

            let status_code = response[0];
            let error_code = ErrorCode::from(status_code);

            match error_code {
                ErrorCode::Success => atoms::ok().encode(env),
                code => (atoms::error(), error_code_to_atom(env, code)).encode(env),
            }
        }
        Err(_) => (atoms::error(), atoms::internal_error()).encode(env),
    }
}

/// Inspect container by ID
/// Returns: {:ok, info_json} | {:error, atom}
#[rustler::nif]
fn inspect_container(env: Env, resource: ResourceArc<BridgeResource>, container_id: String) -> Term {
    match resource.with_bridge(|bridge| {
        let mut request = Vec::new();
        request.push(0x04); // INSPECT_CONTAINER

        let id_bytes = container_id.as_bytes();
        request.extend_from_slice(&(id_bytes.len() as u32).to_le_bytes());
        request.extend_from_slice(id_bytes);

        bridge.send_request(&request)
            .map_err(|e| Error::Term(Box::new(format!("send_request failed: {:?}", e))))
    }) {
        Ok(response) => {
            if response.is_empty() {
                return (atoms::error(), atoms::internal_error()).encode(env);
            }

            let status_code = response[0];
            let error_code = ErrorCode::from(status_code);

            match error_code {
                ErrorCode::Success => {
                    let info_json = String::from_utf8_lossy(&response[1..]).to_string();
                    (atoms::ok(), info_json).encode(env)
                }
                code => (atoms::error(), error_code_to_atom(env, code)).encode(env),
            }
        }
        Err(_) => (atoms::error(), atoms::internal_error()).encode(env),
    }
}

/// Delete container by ID
/// Returns: :ok | {:error, atom}
#[rustler::nif]
fn delete_container(env: Env, resource: ResourceArc<BridgeResource>, container_id: String) -> Term {
    match resource.with_bridge(|bridge| {
        let mut request = Vec::new();
        request.push(0x05); // DELETE_CONTAINER

        let id_bytes = container_id.as_bytes();
        request.extend_from_slice(&(id_bytes.len() as u32).to_le_bytes());
        request.extend_from_slice(id_bytes);

        bridge.send_request(&request)
            .map_err(|e| Error::Term(Box::new(format!("send_request failed: {:?}", e))))
    }) {
        Ok(response) => {
            if response.is_empty() {
                return (atoms::error(), atoms::internal_error()).encode(env);
            }

            let status_code = response[0];
            let error_code = ErrorCode::from(status_code);

            match error_code {
                ErrorCode::Success => atoms::ok().encode(env),
                code => (atoms::error(), error_code_to_atom(env, code)).encode(env),
            }
        }
        Err(_) => (atoms::error(), atoms::internal_error()).encode(env),
    }
}

/// List all containers
/// Returns: {:ok, [container_ids]} | {:error, atom}
#[rustler::nif]
fn list_containers(env: Env, resource: ResourceArc<BridgeResource>) -> Term {
    match resource.with_bridge(|bridge| {
        let mut request = Vec::new();
        request.push(0x06); // LIST_CONTAINERS
        request.extend_from_slice(&0u32.to_le_bytes()); // Empty payload

        bridge.send_request(&request)
            .map_err(|e| Error::Term(Box::new(format!("send_request failed: {:?}", e))))
    }) {
        Ok(response) => {
            if response.is_empty() {
                return (atoms::error(), atoms::internal_error()).encode(env);
            }

            let status_code = response[0];
            let error_code = ErrorCode::from(status_code);

            match error_code {
                ErrorCode::Success => {
                    let ids_str = String::from_utf8_lossy(&response[1..]);
                    let ids: Vec<String> = ids_str
                        .split('\n')
                        .filter(|s| !s.is_empty())
                        .map(|s| s.to_string())
                        .collect();
                    (atoms::ok(), ids).encode(env)
                }
                code => (atoms::error(), error_code_to_atom(env, code)).encode(env),
            }
        }
        Err(_) => (atoms::error(), atoms::internal_error()).encode(env),
    }
}
