// SPDX-License-Identifier: PMPL-1.0-or-later
//! MCP HTTP Server for Vörðr
//!
//! Handles JSON-RPC 2.0 requests from Svalinn and other MCP clients.

use axum::{
    extract::State,
    routing::{get, post},
    Json, Router,
};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::collections::HashMap;
use std::net::SocketAddr;
use std::path::Path;
use std::sync::Arc;
use std::time::Duration;
use tracing::{info, warn};
use tokio::time::{sleep, timeout};

use crate::engine::config::MountSpec;
use crate::engine::lifecycle::{HealthCheckSpec, RuntimeConfig};
use crate::engine::{ContainerLifecycle, ContainerState, StateManager};
use crate::ffi::ConfigValidator;
use crate::network::netavark::{
    NetworkAttachment, NetworkConfig, NetworkDefinition, NetworkManager, PortMapping, Subnet,
};
use crate::runtime::ShimClient;

/// MCP Server configuration
#[derive(Clone)]
pub struct McpServerConfig {
    pub host: String,
    pub port: u16,
    pub db_path: String,
    pub root_dir: String,
    pub runtime: String,
}

impl Default for McpServerConfig {
    fn default() -> Self {
        Self {
            host: "0.0.0.0".into(),
            port: 8080,
            db_path: "/var/lib/vordr/vordr.db".into(),
            root_dir: "/var/lib/vordr".into(),
            runtime: "youki".into(),
        }
    }
}

/// Shared server state - stores configuration only
/// ContainerLifecycle is created on demand per-request to avoid Send/Sync issues
#[derive(Clone)]
struct AppState {
    config: McpServerConfig,
}

impl AppState {
    fn new(config: McpServerConfig) -> Self {
        Self { config }
    }

    /// Create a new ContainerLifecycle instance for a request
    fn get_lifecycle(&self) -> Result<ContainerLifecycle, String> {
        ContainerLifecycle::new(
            Path::new(&self.config.db_path),
            Path::new(&self.config.root_dir),
            &self.config.runtime,
        )
        .map_err(|e| format!("Failed to get lifecycle: {}", e))
    }
}

/// JSON-RPC 2.0 Request
#[derive(Debug, Deserialize)]
struct JsonRpcRequest {
    jsonrpc: String,
    method: String,
    params: Option<Value>,
    id: Option<Value>,
}

/// JSON-RPC 2.0 Response
#[derive(Debug, Serialize)]
struct JsonRpcResponse {
    jsonrpc: String,
    result: Option<Value>,
    error: Option<JsonRpcError>,
    id: Value,
}

#[derive(Debug, Serialize)]
struct JsonRpcError {
    code: i32,
    message: String,
    data: Option<Value>,
}

impl JsonRpcResponse {
    fn success(id: Value, result: Value) -> Self {
        Self {
            jsonrpc: "2.0".into(),
            result: Some(result),
            error: None,
            id,
        }
    }

    fn error(id: Value, code: i32, message: impl Into<String>) -> Self {
        Self {
            jsonrpc: "2.0".into(),
            result: None,
            error: Some(JsonRpcError {
                code,
                message: message.into(),
                data: None,
            }),
            id,
        }
    }
}

/// Start the MCP server
pub async fn start_server(config: McpServerConfig) -> Result<(), Box<dyn std::error::Error>> {
    let addr: SocketAddr = format!("{}:{}", config.host, config.port).parse()?;
    let state = Arc::new(AppState::new(config));

    let app = Router::new()
        .route("/", post(handle_jsonrpc))
        .route("/health", get(handle_health))
        .route("/tools", get(handle_list_tools))
        .with_state(state);

    info!("Starting MCP server on {}", addr);
    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app).await?;
    Ok(())
}

/// Health check endpoint
async fn handle_health() -> Json<Value> {
    Json(json!({
        "status": "ok",
        "version": env!("CARGO_PKG_VERSION"),
        "service": "vordr-mcp"
    }))
}

/// List available tools
async fn handle_list_tools() -> Json<Value> {
    let tools = super::get_tool_definitions();
    Json(json!({
        "tools": tools
    }))
}

/// Handle JSON-RPC requests
#[axum::debug_handler]
async fn handle_jsonrpc(
    State(state): State<Arc<AppState>>,
    Json(request): Json<JsonRpcRequest>,
) -> Json<JsonRpcResponse> {
    let id = request.id.clone().unwrap_or(Value::Null);

    // Validate JSON-RPC version
    if request.jsonrpc != "2.0" {
        return Json(JsonRpcResponse::error(id, -32600, "Invalid JSON-RPC version"));
    }

    // Dispatch based on method
    let response = match request.method.as_str() {
        "tools/list" => {
            let tools = super::get_tool_definitions();
            JsonRpcResponse::success(id, json!({ "tools": tools }))
        }
        "tools/call" => {
            let params = request.params.unwrap_or(Value::Null);
            match handle_tool_call(&state, params).await {
                Ok(result) => JsonRpcResponse::success(id, result),
                Err(e) => JsonRpcResponse::error(id, -32000, e),
            }
        }
        _ => JsonRpcResponse::error(id, -32601, "Method not found"),
    };

    Json(response)
}

/// Handle tool/call requests
async fn handle_tool_call(state: &AppState, params: Value) -> Result<Value, String> {
    let tool_name = params
        .get("name")
        .and_then(|v| v.as_str())
        .ok_or("Missing tool name")?;

    let arguments = params.get("arguments").cloned().unwrap_or(Value::Null);

    info!("Tool call: {} with args: {}", tool_name, arguments);

    match tool_name {
        "vordr_container_create" | "vordr_run" => handle_run(state, arguments).await,
        "vordr_container_start" => handle_start(state, arguments).await,
        "vordr_container_stop" | "vordr_stop" => handle_stop(state, arguments).await,
        "vordr_container_remove" | "vordr_rm" => handle_remove(state, arguments).await,
        "vordr_container_list" | "vordr_ps" => handle_list(state, arguments).await,
        "vordr_container_inspect" | "vordr_inspect" => handle_inspect(state, arguments).await,
        "vordr_exec" => handle_exec(state, arguments).await,
        "vordr_image_list" | "vordr_images" => handle_images(state, arguments).await,
        "vordr_verify_image" => handle_verify(state, arguments).await,
        "vordr_network_ls" => handle_network_list(state, arguments).await,
        "vordr_network_create" => handle_network_create(state, arguments).await,
        "vordr_network_rm" => handle_network_remove(state, arguments).await,
        "vordr_volume_ls" => handle_volume_list(state, arguments).await,
        "vordr_volume_create" => handle_volume_create(state, arguments).await,
        "vordr_volume_rm" => handle_volume_remove(state, arguments).await,
        _ => Err(format!("Unknown tool: {}", tool_name)),
    }
}

/// Handle vordr_run / vordr_container_create
async fn handle_run(state: &AppState, args: Value) -> Result<Value, String> {
    use crate::runtime::ShimClient;

    let image = args
        .get("image")
        .and_then(|v| v.as_str())
        .ok_or("Missing image")?;

    let name = args.get("name").and_then(|v| v.as_str());
    let detach = args.get("detach").and_then(|v| v.as_bool()).unwrap_or(true);

    // Build config via gatekeeper
    let config = args.get("config").cloned().unwrap_or(json!({}));
    let privileged = config.get("privileged").and_then(|v| v.as_bool()).unwrap_or(false);
    let read_only = config.get("readOnlyRoot").and_then(|v| v.as_bool()).unwrap_or(true);

    let validator = ConfigValidator::new()
        .privileged(privileged)
        .readonly_rootfs(read_only)
        .user_namespace(true);

    let validated = validator.validate().map_err(|e| format!("Validation failed: {}", e))?;

    // Generate ID and name
    let container_id = generate_container_id();
    let container_name = name.map(|s| s.to_string()).unwrap_or_else(generate_name);
    let image_id = format!("sha256:{}", &container_id[..12]);

    let command = args
        .get("command")
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str())
                .map(String::from)
                .collect()
        });

    let env = parse_env(config.get("env"));
    let volumes_root = format!("{}/volumes", state.config.root_dir);
    let mounts = parse_volumes(config.get("volumes"), &volumes_root)?;
    let restart = parse_restart(config.get("restart"));
    let healthcheck = parse_healthcheck(config.get("healthcheck"));
    let runtime_config = RuntimeConfig {
        env,
        mounts,
        restart,
        healthcheck,
    };
    let runtime_for_creation = if runtime_config.is_empty() {
        None
    } else {
        Some(runtime_config.clone())
    };

    let port_mappings = parse_ports(config.get("ports"))?;
    let network_attachments = parse_networks(config.get("networks"));
    let mut network_config = if network_attachments.is_empty() && port_mappings.is_empty() {
        None
    } else {
        Some(NetworkConfig {
            container_id: container_id.clone(),
            container_name: container_name.clone(),
            networks: if network_attachments.is_empty() {
                vec![NetworkAttachment {
                    network_name: "vordr".to_string(),
                    interface_name: "eth0".to_string(),
                    static_ips: None,
                    aliases: None,
                }]
            } else {
                network_attachments
            },
            port_mappings: if port_mappings.is_empty() {
                None
            } else {
                Some(port_mappings)
            },
        })
    };

    // Bundle path for container state
    let bundle_path = format!("{}/containers/{}/bundle", state.config.root_dir, container_id);

    // SYNC PHASE: Create container record in database (no await, so SQLite is fine)
    {
        let lifecycle = state.get_lifecycle()?;
        lifecycle
            .create(
                &container_id,
                &container_name,
                &image_id,
                &validated,
                command,
                runtime_for_creation.clone(),
            )
            .map_err(|e| format!("Create failed: {}", e))?;
        // lifecycle dropped here - SQLite connection released before any await
    }

    // ASYNC PHASE: Start container via runtime shim (no SQLite held)
    if detach {
        let shim = ShimClient::new(&state.config.runtime, &bundle_path);
        match shim.create_and_start(&container_id).await {
            Ok(pid) => {
                info!("Container {} started with PID {}", container_id, pid);
                if let Some(net_config) = network_config.as_mut() {
                    let net_config_dir = format!("{}/networks", state.config.root_dir);
                    let net_run_dir = format!("{}/run/netavark", state.config.root_dir);
                    let net_mgr = NetworkManager::new(&net_config_dir, &net_run_dir)
                        .map_err(|e| format!("Network manager init failed: {}", e))?;
                    net_mgr
                        .ensure_default_network()
                        .map_err(|e| format!("Default network failed: {}", e))?;
                    ensure_networks(&net_mgr, &net_config.networks)?;
                    let netns_path = format!("/proc/{}/ns/net", pid);
                    if let Err(e) = net_mgr.setup(net_config, &netns_path) {
                        let _ = shim.kill(&container_id, 9, true).await;
                        let _ = shim.delete(&container_id, true).await;
                        return Err(format!("Network setup failed: {}", e));
                    }
                }
                // Update state in a new scope (short-lived)
                let state_mgr = StateManager::open(Path::new(&state.config.db_path))
                    .map_err(|e| format!("Failed to open state: {}", e))?;
                state_mgr
                    .set_container_state(&container_id, ContainerState::Running, Some(pid as i32))
                    .map_err(|e| format!("Failed to update state: {}", e))?;
                if runtime_config.healthcheck.is_some() || runtime_config.restart.is_some() {
                    let monitor_state = state.clone();
                    let container_id_clone = container_id.clone();
                    let bundle_path_clone = bundle_path.clone();
                    let monitor_config = runtime_config.clone();
                    tokio::spawn(async move {
                        if let Err(e) = monitor_container(
                            monitor_state,
                            container_id_clone,
                            bundle_path_clone,
                            monitor_config,
                        )
                        .await
                        {
                            warn!("Health monitor for {} terminated: {}", container_id, e);
                        }
                    });
                }
            }
            Err(e) => {
                return Err(format!("Start failed: {}", e));
            }
        }
    }

    Ok(json!({
        "containerId": container_id,
        "name": container_name,
        "image": image,
        "status": if detach { "running" } else { "created" }
    }))
}

/// Handle vordr_container_start
async fn handle_start(state: &AppState, args: Value) -> Result<Value, String> {
    use crate::runtime::ShimClient;

    let container_id = args
        .get("containerId")
        .or_else(|| args.get("container"))
        .and_then(|v| v.as_str())
        .ok_or("Missing containerId")?
        .to_string();

    // SYNC PHASE: Get container info from database
    let bundle_path = {
        let state_mgr = StateManager::open(Path::new(&state.config.db_path))
            .map_err(|e| format!("Failed to open state: {}", e))?;
        let container = state_mgr
            .get_container(&container_id)
            .map_err(|e| format!("Container not found: {}", e))?;

        if container.state != ContainerState::Created {
            return Err(format!(
                "Container not in Created state (state: {:?})",
                container.state
            ));
        }
        container.bundle_path.clone()
    }; // state_mgr dropped here

    // ASYNC PHASE: Start via runtime shim
    let shim = ShimClient::new(&state.config.runtime, &bundle_path);
    let pid = shim
        .create_and_start(&container_id)
        .await
        .map_err(|e| format!("Start failed: {}", e))?;

    // SYNC PHASE: Update state
    {
        let state_mgr = StateManager::open(Path::new(&state.config.db_path))
            .map_err(|e| format!("Failed to open state: {}", e))?;
        state_mgr
            .set_container_state(&container_id, ContainerState::Running, Some(pid as i32))
            .map_err(|e| format!("Failed to update state: {}", e))?;
    }

    Ok(json!({
        "containerId": container_id,
        "pid": pid,
        "status": "running"
    }))
}

/// Handle vordr_container_stop / vordr_stop
async fn handle_stop(state: &AppState, args: Value) -> Result<Value, String> {
    use crate::runtime::ShimClient;

    let container_id = args
        .get("containerId")
        .or_else(|| args.get("container"))
        .and_then(|v| v.as_str())
        .ok_or("Missing containerId")?
        .to_string();

    let timeout = args
        .get("timeout")
        .and_then(|v| v.as_u64())
        .unwrap_or(10) as u32;

    // SYNC PHASE: Get container info
    let bundle_path = {
        let state_mgr = StateManager::open(Path::new(&state.config.db_path))
            .map_err(|e| format!("Failed to open state: {}", e))?;
        let container = state_mgr
            .get_container(&container_id)
            .map_err(|e| format!("Container not found: {}", e))?;

        if container.state != ContainerState::Running {
            return Err(format!(
                "Container not running (state: {:?})",
                container.state
            ));
        }
        container.bundle_path.clone()
    }; // state_mgr dropped here

    // ASYNC PHASE: Stop via runtime shim
    let shim = ShimClient::new(&state.config.runtime, &bundle_path);

    // Send SIGTERM (signal 15)
    shim.kill(&container_id, 15, false)
        .await
        .map_err(|e| format!("Failed to send SIGTERM: {}", e))?;

    // Wait for graceful shutdown
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(timeout as u64);
    loop {
        match shim.state(&container_id).await {
            Ok(state) if state.status == "stopped" => break,
            Ok(_) => {
                if std::time::Instant::now() >= deadline {
                    // Send SIGKILL (signal 9)
                    let _ = shim.kill(&container_id, 9, true).await;
                    break;
                }
                tokio::time::sleep(std::time::Duration::from_millis(100)).await;
            }
            Err(_) => break, // Container already gone
        }
    }

    // SYNC PHASE: Update state
    {
        let state_mgr = StateManager::open(Path::new(&state.config.db_path))
            .map_err(|e| format!("Failed to open state: {}", e))?;
        state_mgr
            .set_container_state(&container_id, ContainerState::Stopped, None)
            .map_err(|e| format!("Failed to update state: {}", e))?;
    }

    Ok(json!({
        "containerId": container_id,
        "status": "stopped"
    }))
}

/// Handle vordr_container_remove / vordr_rm
async fn handle_remove(state: &AppState, args: Value) -> Result<Value, String> {
    let container_id = args
        .get("containerId")
        .or_else(|| args.get("container"))
        .and_then(|v| v.as_str())
        .ok_or("Missing containerId")?;

    let force = args.get("force").and_then(|v| v.as_bool()).unwrap_or(false);

    let lifecycle = state.get_lifecycle()?;
    lifecycle
        .delete(container_id, force)
        .map_err(|e| format!("Remove failed: {}", e))?;

    Ok(json!({
        "containerId": container_id,
        "status": "removed"
    }))
}

/// Handle vordr_container_list / vordr_ps
async fn handle_list(state: &AppState, args: Value) -> Result<Value, String> {
    let all = args.get("all").and_then(|v| v.as_bool()).unwrap_or(false);

    let state_filter = if all {
        None
    } else {
        Some(ContainerState::Running)
    };

    let lifecycle = state.get_lifecycle()?;
    let containers = lifecycle
        .list(state_filter)
        .map_err(|e| format!("List failed: {}", e))?;

    let container_list: Vec<Value> = containers
        .iter()
        .map(|c| {
            json!({
                "id": c.id,
                "name": c.name,
                "image": c.image_id,
                "state": format!("{:?}", c.state),
                "created": c.created_at
            })
        })
        .collect();

    Ok(json!({ "containers": container_list }))
}

/// Handle vordr_container_inspect / vordr_inspect
async fn handle_inspect(state: &AppState, args: Value) -> Result<Value, String> {
    let container_id = args
        .get("containerId")
        .or_else(|| args.get("container"))
        .and_then(|v| v.as_str())
        .ok_or("Missing containerId")?;

    let lifecycle = state.get_lifecycle()?;
    let info = lifecycle
        .get(container_id)
        .map_err(|e| format!("Inspect failed: {}", e))?;

    Ok(json!({
        "id": info.id,
        "name": info.name,
        "image": info.image_id,
        "state": format!("{:?}", info.state),
        "pid": info.pid,
        "bundlePath": info.bundle_path,
        "created": info.created_at,
        "config": info.config
    }))
}

/// Handle vordr_exec
async fn handle_exec(state: &AppState, args: Value) -> Result<Value, String> {
    use crate::runtime::ShimClient;

    let container_id = args
        .get("containerId")
        .or_else(|| args.get("container"))
        .and_then(|v| v.as_str())
        .ok_or("Missing containerId")?
        .to_string();

    let command: Vec<String> = args
        .get("command")
        .and_then(|v| v.as_array())
        .ok_or("Missing command")?
        .iter()
        .filter_map(|v| v.as_str().map(String::from))
        .collect();

    if command.is_empty() {
        return Err("Empty command".into());
    }

    // SYNC PHASE: Get container info to find bundle path
    let bundle_path = {
        let state_mgr = StateManager::open(Path::new(&state.config.db_path))
            .map_err(|e| format!("Failed to open state: {}", e))?;
        let container = state_mgr
            .get_container(&container_id)
            .map_err(|e| format!("Container not found: {}", e))?;

        if container.state != ContainerState::Running {
            return Err(format!(
                "Container is not running (state: {:?})",
                container.state
            ));
        }
        container.bundle_path.clone()
    }; // state_mgr dropped here

    // ASYNC PHASE: Execute via shim
    let shim = ShimClient::new(&state.config.runtime, &bundle_path);

    let process_spec = json!({
        "terminal": false,
        "user": { "uid": 0, "gid": 0 },
        "args": command,
        "cwd": "/"
    });

    let exec_pid = shim
        .exec(&container_id, &process_spec.to_string(), false)
        .await
        .map_err(|e| format!("Exec failed: {}", e))?;

    Ok(json!({
        "containerId": container_id,
        "execPid": exec_pid
    }))
}

/// Handle vordr_image_list / vordr_images
async fn handle_images(state: &AppState, _args: Value) -> Result<Value, String> {
    let state_mgr = StateManager::open(Path::new(&state.config.db_path))
        .map_err(|e| format!("Failed to open state: {}", e))?;

    let images = state_mgr
        .list_images()
        .map_err(|e| format!("List images failed: {}", e))?;

    let image_list: Vec<Value> = images
        .iter()
        .map(|img| {
            json!({
                "id": img.id,
                "digest": img.digest,
                "repository": img.repository,
                "tags": img.tags,
                "size": img.size
            })
        })
        .collect();

    Ok(json!({ "images": image_list }))
}

/// Handle vordr_verify_image
async fn handle_verify(_state: &AppState, args: Value) -> Result<Value, String> {
    let image = args
        .get("image")
        .and_then(|v| v.as_str())
        .ok_or("Missing image")?;

    // TODO: Implement actual image verification via Idris2 core
    // For now, return a placeholder
    info!("Verifying image: {}", image);

    Ok(json!({
        "image": image,
        "verified": true,
        "attestations": [],
        "warnings": []
    }))
}

async fn handle_network_list(state: &AppState, _args: Value) -> Result<Value, String> {
    let net_config_dir = format!("{}/networks", state.config.root_dir);
    let net_run_dir = format!("{}/run/netavark", state.config.root_dir);
    let net_mgr = NetworkManager::new(&net_config_dir, &net_run_dir)
        .map_err(|e| format!("Network manager init failed: {}", e))?;
    let networks = net_mgr
        .list_networks()
        .map_err(|e| format!("Network list failed: {}", e))?;

    let items: Vec<Value> = networks
        .into_iter()
        .map(|net| {
            json!({
                "name": net.name,
                "id": net.id,
                "driver": net.driver,
                "subnets": net.subnets
            })
        })
        .collect();

    Ok(json!({ "networks": items }))
}

async fn handle_network_create(state: &AppState, args: Value) -> Result<Value, String> {
    let name = args
        .get("name")
        .and_then(|v| v.as_str())
        .ok_or("Missing name")?;
    let driver = args
        .get("driver")
        .and_then(|v| v.as_str())
        .unwrap_or("bridge");
    let subnet = args.get("subnet").and_then(|v| v.as_str());

    let net_config_dir = format!("{}/networks", state.config.root_dir);
    let net_run_dir = format!("{}/run/netavark", state.config.root_dir);
    let net_mgr = NetworkManager::new(&net_config_dir, &net_run_dir)
        .map_err(|e| format!("Network manager init failed: {}", e))?;
    net_mgr
        .ensure_default_network()
        .map_err(|e| format!("Default network failed: {}", e))?;

    let definition = NetworkDefinition {
        name: name.to_string(),
        id: uuid::Uuid::new_v4().to_string(),
        driver: driver.to_string(),
        network_interface: None,
        subnets: subnet.map(|s| {
            vec![Subnet {
                subnet: s.to_string(),
                gateway: None,
                lease_range: None,
            }]
        }),
        ipv6_enabled: Some(false),
        internal: Some(false),
        dns_enabled: Some(true),
        options: None,
    };

    net_mgr
        .create_network(&definition)
        .map_err(|e| format!("Network create failed: {}", e))?;

    Ok(json!({
        "name": definition.name,
        "id": definition.id,
        "driver": definition.driver
    }))
}

async fn handle_network_remove(state: &AppState, args: Value) -> Result<Value, String> {
    let name = args
        .get("name")
        .and_then(|v| v.as_str())
        .ok_or("Missing name")?;

    let net_config_dir = format!("{}/networks", state.config.root_dir);
    let net_run_dir = format!("{}/run/netavark", state.config.root_dir);
    let net_mgr = NetworkManager::new(&net_config_dir, &net_run_dir)
        .map_err(|e| format!("Network manager init failed: {}", e))?;

    net_mgr
        .delete_network(name)
        .map_err(|e| format!("Network delete failed: {}", e))?;

    Ok(json!({
        "name": name,
        "status": "removed"
    }))
}

async fn handle_volume_list(state: &AppState, _args: Value) -> Result<Value, String> {
    let volumes_dir = Path::new(&state.config.root_dir).join("volumes");
    if !volumes_dir.exists() {
        return Ok(json!({ "volumes": [] }));
    }

    let mut volumes = Vec::new();
    for entry in std::fs::read_dir(&volumes_dir).map_err(|e| e.to_string())? {
        let entry = entry.map_err(|e| e.to_string())?;
        if entry.file_type().map_err(|e| e.to_string())?.is_dir() {
            if let Some(name) = entry.file_name().to_str() {
                volumes.push(json!({
                    "name": name,
                    "path": entry.path().to_string_lossy()
                }));
            }
        }
    }

    Ok(json!({ "volumes": volumes }))
}

async fn handle_volume_create(state: &AppState, args: Value) -> Result<Value, String> {
    let name = args
        .get("name")
        .and_then(|v| v.as_str())
        .ok_or("Missing name")?;
    let driver = args
        .get("driver")
        .and_then(|v| v.as_str())
        .unwrap_or("local");
    if driver != "local" {
        return Err(format!("Unsupported driver: {}", driver));
    }

    let volumes_dir = Path::new(&state.config.root_dir).join("volumes");
    std::fs::create_dir_all(&volumes_dir).map_err(|e| e.to_string())?;
    let volume_path = volumes_dir.join(name);
    std::fs::create_dir_all(&volume_path).map_err(|e| e.to_string())?;

    Ok(json!({
        "name": name,
        "path": volume_path.to_string_lossy(),
        "driver": driver
    }))
}

async fn handle_volume_remove(state: &AppState, args: Value) -> Result<Value, String> {
    let id = args
        .get("id")
        .and_then(|v| v.as_str())
        .ok_or("Missing id")?;

    let volumes_dir = Path::new(&state.config.root_dir).join("volumes");
    let volume_path = volumes_dir.join(id);
    if volume_path.exists() {
        std::fs::remove_dir_all(&volume_path).map_err(|e| e.to_string())?;
    }

    Ok(json!({
        "id": id,
        "status": "removed"
    }))
}

fn parse_restart(value: Option<&Value>) -> Option<String> {
    value
        .and_then(|v| v.as_str())
        .map(|s| s.trim().to_ascii_lowercase())
        .filter(|policy| policy != "no" && !policy.is_empty())
}

fn parse_healthcheck(value: Option<&Value>) -> Option<HealthCheckSpec> {
    let obj = value.and_then(|v| v.as_object())?;
    let test = obj.get("test")?.as_str()?.to_string();

    let interval = parse_duration_value(obj.get("interval"), 30);
    let timeout = parse_duration_value(obj.get("timeout"), 5);
    let retries = obj
        .get("retries")
        .and_then(|v| v.as_u64())
        .unwrap_or(3) as u32;
    let start_period = parse_duration_value(
        obj.get("start_period").or_else(|| obj.get("startPeriod")),
        0,
    );

    Some(HealthCheckSpec {
        test,
        interval,
        timeout,
        retries,
        start_period,
    })
}

fn parse_duration_value(value: Option<&Value>, default_secs: u64) -> Duration {
    value
        .and_then(|v| v.as_str())
        .and_then(|s| parse_duration_str(s))
        .unwrap_or_else(|| Duration::from_secs(default_secs))
}

fn parse_duration_str(text: &str) -> Option<Duration> {
    let text = text.trim();
    if let Some(num) = text.strip_suffix('s') {
        num.parse::<u64>().ok().map(Duration::from_secs)
    } else if let Some(num) = text.strip_suffix('m') {
        num.parse::<u64>().ok().map(|v| Duration::from_secs(v * 60))
    } else if let Some(num) = text.strip_suffix('h') {
        num.parse::<u64>().ok().map(|v| Duration::from_secs(v * 3600))
    } else {
        text.parse::<u64>().ok().map(Duration::from_secs)
    }
}

async fn monitor_container(
    state: AppState,
    container_id: String,
    bundle_path: String,
    runtime_config: RuntimeConfig,
) -> Result<(), String> {
    let healthcheck = runtime_config.healthcheck.clone();
    let restart_policy = runtime_config.restart.clone();
    let shim_runtime = state.config.runtime.clone();
    let monitor_interval = healthcheck
        .as_ref()
        .map(|hc| hc.interval)
        .unwrap_or_else(|| Duration::from_secs(5));

    if let Some(ref hc) = healthcheck {
        if hc.start_period > Duration::ZERO {
            sleep(hc.start_period).await;
        }
    }

    let shim = ShimClient::new(&shim_runtime, &bundle_path);
    loop {
        let running = match shim.state(&container_id).await {
            Ok(status) => status.status == "running",
            Err(err) => {
                warn!("Failed to query state for {}: {}", container_id, err);
                false
            }
        };

        if !running {
            warn!("Container {} is not running", container_id);
            if should_restart(restart_policy.as_deref()) {
                restart_container(&state, &container_id).await?;
                continue;
            }
            let lifecycle = state.get_lifecycle()?;
            let _ = lifecycle.stop(&container_id, 5).await;
            break;
        }

        if let Some(ref hc) = healthcheck {
            if !run_healthcheck(&shim, &container_id, hc).await {
                warn!("Health check for {} failed", container_id);
                if should_restart(restart_policy.as_deref()) {
                    restart_container(&state, &container_id).await?;
                    continue;
                }
                let lifecycle = state.get_lifecycle()?;
                let _ = lifecycle.stop(&container_id, 5).await;
                break;
            }
            sleep(hc.interval).await;
        } else {
            sleep(monitor_interval).await;
        }
    }

    Ok(())
}

async fn restart_container(state: &AppState, container_id: &str) -> Result<(), String> {
    let lifecycle = state.get_lifecycle()?;
    let _ = lifecycle.stop(container_id, 5).await;

    let state_mgr = StateManager::open(Path::new(&state.config.db_path))
        .map_err(|e| format!("Failed to open state: {}", e))?;
    state_mgr
        .set_container_state(container_id, ContainerState::Created, None)
        .map_err(|e| format!("Failed to reset state: {}", e))?;

    lifecycle
        .start(container_id)
        .await
        .map_err(|e| format!("Failed to restart container: {}", e))?;

    Ok(())
}

async fn run_healthcheck(shim: &ShimClient, container_id: &str, spec: &HealthCheckSpec) -> bool {
    let process_spec = json!({
        "terminal": false,
        "user": { "uid": 0, "gid": 0 },
        "cwd": "/",
        "args": ["/bin/sh", "-c", spec.test.clone()]
    })
    .to_string();

    match timeout(spec.timeout, shim.exec_wait(container_id, &process_spec)).await {
        Ok(Ok(code)) => code == 0,
        Ok(Err(err)) => {
            warn!("Health check exec failed for {}: {}", container_id, err);
            false
        }
        Err(_) => {
            warn!("Health check timed out for {}", container_id);
            false
        }
    }
}

fn should_restart(policy: Option<&str>) -> bool {
    matches!(
        policy.unwrap_or("no"),
        "always" | "on-failure" | "unless-stopped"
    )
}

fn parse_env(value: Option<&Value>) -> Vec<String> {
    let mut env = Vec::new();

    match value {
        Some(Value::Object(map)) => {
            for (key, val) in map {
                env.push(format!("{}={}", key, normalize_env_value(val)));
            }
        }
        Some(Value::Array(items)) => {
            for item in items {
                match item {
                    Value::String(s) => env.push(s.clone()),
                    Value::Object(obj) => {
                        if let Some(name) = obj.get("name").and_then(|v| v.as_str()) {
                            let val = obj.get("value").map(normalize_env_value).unwrap_or_default();
                            env.push(format!("{}={}", name, val));
                        }
                    }
                    _ => {}
                }
            }
        }
        _ => {}
    }

    env
}

fn normalize_env_value(value: &Value) -> String {
    match value {
        Value::String(s) => s.clone(),
        Value::Number(n) => n.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Null => String::new(),
        other => other.to_string(),
    }
}

fn parse_ports(value: Option<&Value>) -> Result<Vec<PortMapping>, String> {
    let mut ports = Vec::new();
    let Some(value) = value else {
        return Ok(ports);
    };

    match value {
        Value::Array(items) => {
            for item in items {
                match item {
                    Value::String(s) => {
                        let mapping = parse_port_string(s)?;
                        ports.push(mapping);
                    }
                    Value::Object(obj) => {
                        let container_port = obj
                            .get("containerPort")
                            .and_then(|v| v.as_u64())
                            .ok_or("port entry missing containerPort")? as u16;
                        let host_port = obj
                            .get("hostPort")
                            .and_then(|v| v.as_u64())
                            .ok_or("port entry missing hostPort")? as u16;
                        let protocol = obj
                            .get("protocol")
                            .and_then(|v| v.as_str())
                            .unwrap_or("tcp")
                            .to_string();
                        let host_ip = obj.get("hostIp").and_then(|v| v.as_str()).map(|s| s.to_string());
                        ports.push(PortMapping {
                            host_ip,
                            container_port,
                            host_port,
                            protocol,
                        });
                    }
                    _ => return Err("invalid port entry".to_string()),
                }
            }
        }
        _ => return Err("ports must be an array".to_string()),
    }

    Ok(ports)
}

fn parse_port_string(s: &str) -> Result<PortMapping, String> {
    let (raw, protocol) = if let Some((left, proto)) = s.split_once('/') {
        (left, proto)
    } else {
        (s, "tcp")
    };

    let parts: Vec<&str> = raw.split(':').collect();
    match parts.len() {
        2 => Ok(PortMapping {
            host_ip: None,
            host_port: parts[0].parse().map_err(|_| "invalid host port")?,
            container_port: parts[1].parse().map_err(|_| "invalid container port")?,
            protocol: protocol.to_string(),
        }),
        3 => Ok(PortMapping {
            host_ip: Some(parts[0].to_string()),
            host_port: parts[1].parse().map_err(|_| "invalid host port")?,
            container_port: parts[2].parse().map_err(|_| "invalid container port")?,
            protocol: protocol.to_string(),
        }),
        _ => Err("invalid port mapping".to_string()),
    }
}

fn parse_networks(value: Option<&Value>) -> Vec<NetworkAttachment> {
    let mut attachments = Vec::new();

    match value {
        Some(Value::Array(items)) => {
            for (idx, item) in items.iter().enumerate() {
                match item {
                    Value::String(name) => attachments.push(NetworkAttachment {
                        network_name: name.clone(),
                        interface_name: format!("eth{}", idx),
                        static_ips: None,
                        aliases: None,
                    }),
                    Value::Object(obj) => {
                        let name = obj
                            .get("name")
                            .or_else(|| obj.get("network"))
                            .and_then(|v| v.as_str())
                            .unwrap_or("vordr");
                        let aliases = obj.get("aliases").and_then(|v| v.as_array()).map(|arr| {
                            arr.iter()
                                .filter_map(|v| v.as_str().map(String::from))
                                .collect::<Vec<String>>()
                        });
                        attachments.push(NetworkAttachment {
                            network_name: name.to_string(),
                            interface_name: format!("eth{}", idx),
                            static_ips: None,
                            aliases,
                        });
                    }
                    _ => {}
                }
            }
        }
        Some(Value::Object(map)) => {
            for (idx, (name, _)) in map.iter().enumerate() {
                attachments.push(NetworkAttachment {
                    network_name: name.clone(),
                    interface_name: format!("eth{}", idx),
                    static_ips: None,
                    aliases: None,
                });
            }
        }
        _ => {}
    }

    attachments
}

fn parse_volumes(value: Option<&Value>, volumes_root: &str) -> Result<Vec<MountSpec>, String> {
    let mut mounts = Vec::new();
    let Some(value) = value else {
        return Ok(mounts);
    };

    let volumes_root_path = Path::new(volumes_root);
    if let Value::Array(items) = value {
        for item in items {
            match item {
                Value::String(entry) => {
                    let mount = parse_volume_string(entry, volumes_root_path)?;
                    mounts.push(mount);
                }
                Value::Object(obj) => {
                    let source = obj
                        .get("source")
                        .or_else(|| obj.get("src"))
                        .or_else(|| obj.get("name"))
                        .and_then(|v| v.as_str())
                        .ok_or("volume entry missing source")?;
                    let target = obj
                        .get("target")
                        .or_else(|| obj.get("dst"))
                        .or_else(|| obj.get("destination"))
                        .and_then(|v| v.as_str())
                        .ok_or("volume entry missing target")?;
                    let read_only = obj
                        .get("readOnly")
                        .or_else(|| obj.get("readonly"))
                        .or_else(|| obj.get("ro"))
                        .and_then(|v| v.as_bool())
                        .unwrap_or(false);
                    let mount = build_mount_spec(source, target, read_only, volumes_root_path)?;
                    mounts.push(mount);
                }
                _ => return Err("invalid volume entry".to_string()),
            }
        }
    } else {
        return Err("volumes must be an array".to_string());
    }

    Ok(mounts)
}

fn parse_volume_string(entry: &str, volumes_root: &Path) -> Result<MountSpec, String> {
    let parts: Vec<&str> = entry.split(':').collect();
    if parts.len() < 2 || parts.len() > 3 {
        return Err(format!("invalid volume mapping: {}", entry));
    }
    let source = parts[0];
    let target = parts[1];
    let read_only = parts
        .get(2)
        .map(|mode| mode.eq_ignore_ascii_case("ro"))
        .unwrap_or(false);

    build_mount_spec(source, target, read_only, volumes_root)
}

fn build_mount_spec(
    source: &str,
    target: &str,
    read_only: bool,
    volumes_root: &Path,
) -> Result<MountSpec, String> {
    let source_path = if source.starts_with('/') {
        source.to_string()
    } else if source.starts_with("./") || source.starts_with("../") {
        return Err("relative bind mounts require an absolute host path".to_string());
    } else {
        std::fs::create_dir_all(volumes_root).map_err(|e| e.to_string())?;
        let volume_path = volumes_root.join(source);
        std::fs::create_dir_all(&volume_path).map_err(|e| e.to_string())?;
        volume_path.to_string_lossy().to_string()
    };

    let mut options = vec!["rbind".to_string()];
    if read_only {
        options.push("ro".to_string());
    } else {
        options.push("rw".to_string());
    }

    Ok(MountSpec {
        source: source_path,
        destination: target.to_string(),
        mount_type: "bind".to_string(),
        options,
    })
}

fn ensure_networks(net_mgr: &NetworkManager, attachments: &[NetworkAttachment]) -> Result<(), String> {
    let existing = net_mgr
        .list_networks()
        .map_err(|e| format!("Network list failed: {}", e))?;
    let mut known: HashMap<String, String> = HashMap::new();
    for net in existing {
        known.insert(net.name, net.id);
    }

    for attachment in attachments {
        if known.contains_key(&attachment.network_name) {
            continue;
        }
        let definition = NetworkDefinition {
            name: attachment.network_name.clone(),
            id: uuid::Uuid::new_v4().to_string(),
            driver: "bridge".to_string(),
            network_interface: None,
            subnets: None,
            ipv6_enabled: Some(false),
            internal: Some(false),
            dns_enabled: Some(true),
            options: None,
        };
        net_mgr
            .create_network(&definition)
            .map_err(|e| format!("Network create failed: {}", e))?;
    }

    Ok(())
}

/// Generate a unique container ID
fn generate_container_id() -> String {
    use sha2::{Digest, Sha256};

    let mut hasher = Sha256::new();
    hasher.update(uuid::Uuid::new_v4().as_bytes());

    // Use monotonic time if system time fails (e.g., clock set before epoch)
    let timestamp = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or_else(|_| {
            // Fallback to monotonic clock
            use std::time::Instant;
            static START: std::sync::OnceLock<Instant> = std::sync::OnceLock::new();
            let start = START.get_or_init(Instant::now);
            start.elapsed().as_nanos()
        });

    hasher.update(timestamp.to_le_bytes());

    hex::encode(hasher.finalize())
}

/// Generate a random container name
fn generate_name() -> String {
    let adjectives = ["brave", "calm", "eager", "gentle", "happy", "jolly", "kind"];
    let nouns = ["bear", "crane", "deer", "eagle", "falcon", "goose", "heron"];

    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let mut hasher = DefaultHasher::new();
    std::time::SystemTime::now().hash(&mut hasher);
    let hash = hasher.finish();

    let adj = adjectives[(hash % adjectives.len() as u64) as usize];
    let noun = nouns[((hash >> 8) % nouns.len() as u64) as usize];

    format!("{}_{}", adj, noun)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_container_id_generation() {
        let id1 = generate_container_id();
        let id2 = generate_container_id();
        assert_ne!(id1, id2);
        assert_eq!(id1.len(), 64);
    }
}
