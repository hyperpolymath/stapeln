// SPDX-License-Identifier: PMPL-1.0-or-later
//! MCP server for AI-assisted container management
//!
//! This module provides Model Context Protocol tool definitions
//! and HTTP server for integration with AI assistants and Svalinn gateway.

pub mod server;

use serde::{Deserialize, Serialize};
use serde_json::json;

pub use server::{start_server, McpServerConfig};

/// MCP tool definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpToolDefinition {
    pub name: String,
    pub description: String,
    pub input_schema: serde_json::Value,
}

/// Generate MCP tool definitions for Vordr
pub fn get_tool_definitions() -> Vec<McpToolDefinition> {
    vec![
        McpToolDefinition {
            name: "vordr_run".into(),
            description: "Create and start a container from an image. The image will be pulled if not present locally.".into(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "image": {
                        "type": "string",
                        "description": "Container image reference (e.g., alpine:latest, ghcr.io/owner/repo:tag)"
                    },
                    "name": {
                        "type": "string",
                        "description": "Optional name for the container"
                    },
                    "command": {
                        "type": "array",
                        "items": { "type": "string" },
                        "description": "Command to run in the container"
                    },
                    "env": {
                        "type": "array",
                        "items": { "type": "string" },
                        "description": "Environment variables in KEY=VALUE format"
                    },
                    "volumes": {
                        "type": "array",
                        "items": { "type": "string" },
                        "description": "Volume mounts in host:container format"
                    },
                    "ports": {
                        "type": "array",
                        "items": { "type": "string" },
                        "description": "Port mappings in host:container format"
                    },
                    "detach": {
                        "type": "boolean",
                        "description": "Run container in background"
                    },
                    "user": {
                        "type": "string",
                        "description": "User ID to run as"
                    },
                    "workdir": {
                        "type": "string",
                        "description": "Working directory inside the container"
                    }
                },
                "required": ["image"]
            }),
        },
        McpToolDefinition {
            name: "vordr_ps".into(),
            description: "List containers. Shows running containers by default, use 'all' to show all containers.".into(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "all": {
                        "type": "boolean",
                        "description": "Show all containers (default shows running only)"
                    },
                    "filter": {
                        "type": "string",
                        "description": "Filter by state: created, running, paused, stopped"
                    }
                }
            }),
        },
        McpToolDefinition {
            name: "vordr_stop".into(),
            description: "Stop a running container gracefully. Sends SIGTERM first, then SIGKILL after timeout.".into(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "container": {
                        "type": "string",
                        "description": "Container ID or name"
                    },
                    "timeout": {
                        "type": "integer",
                        "description": "Seconds to wait before killing (default: 10)"
                    }
                },
                "required": ["container"]
            }),
        },
        McpToolDefinition {
            name: "vordr_rm".into(),
            description: "Remove a container. Container must be stopped unless force is used.".into(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "container": {
                        "type": "string",
                        "description": "Container ID or name"
                    },
                    "force": {
                        "type": "boolean",
                        "description": "Force remove running container"
                    }
                },
                "required": ["container"]
            }),
        },
        McpToolDefinition {
            name: "vordr_exec".into(),
            description: "Execute a command in a running container.".into(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "container": {
                        "type": "string",
                        "description": "Container ID or name"
                    },
                    "command": {
                        "type": "array",
                        "items": { "type": "string" },
                        "description": "Command to execute"
                    },
                    "env": {
                        "type": "array",
                        "items": { "type": "string" },
                        "description": "Additional environment variables"
                    },
                    "workdir": {
                        "type": "string",
                        "description": "Working directory for the command"
                    },
                    "user": {
                        "type": "string",
                        "description": "User to run as"
                    }
                },
                "required": ["container", "command"]
            }),
        },
        McpToolDefinition {
            name: "vordr_logs".into(),
            description: "Fetch container logs.".into(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "container": {
                        "type": "string",
                        "description": "Container ID or name"
                    },
                    "tail": {
                        "type": "integer",
                        "description": "Number of lines from the end"
                    },
                    "follow": {
                        "type": "boolean",
                        "description": "Follow log output"
                    }
                },
                "required": ["container"]
            }),
        },
        McpToolDefinition {
            name: "vordr_inspect".into(),
            description: "Display detailed information about a container.".into(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "container": {
                        "type": "string",
                        "description": "Container ID or name"
                    }
                },
                "required": ["container"]
            }),
        },
        McpToolDefinition {
            name: "vordr_images".into(),
            description: "List available container images.".into(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "all": {
                        "type": "boolean",
                        "description": "Show all images including intermediate layers"
                    }
                }
            }),
        },
        McpToolDefinition {
            name: "vordr_pull".into(),
            description: "Pull a container image from a registry.".into(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "image": {
                        "type": "string",
                        "description": "Image reference to pull"
                    }
                },
                "required": ["image"]
            }),
        },
        McpToolDefinition {
            name: "vordr_network_ls".into(),
            description: "List container networks.".into(),
            input_schema: json!({
                "type": "object",
                "properties": {}
            }),
        },
        McpToolDefinition {
            name: "vordr_network_create".into(),
            description: "Create a new container network.".into(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "name": {
                        "type": "string",
                        "description": "Network name"
                    },
                    "driver": {
                        "type": "string",
                        "description": "Network driver (default: bridge)"
                    },
                    "subnet": {
                        "type": "string",
                        "description": "Subnet in CIDR format"
                    }
                },
                "required": ["name"]
            }),
        },
        McpToolDefinition {
            name: "vordr_network_rm".into(),
            description: "Remove a container network by name.".into(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "name": {
                        "type": "string",
                        "description": "Network name"
                    }
                },
                "required": ["name"]
            }),
        },
        McpToolDefinition {
            name: "vordr_volume_ls".into(),
            description: "List volumes.".into(),
            input_schema: json!({
                "type": "object",
                "properties": {}
            }),
        },
        McpToolDefinition {
            name: "vordr_volume_create".into(),
            description: "Create a new volume.".into(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "name": {
                        "type": "string",
                        "description": "Volume name"
                    },
                    "driver": {
                        "type": "string",
                        "description": "Volume driver (default: local)"
                    }
                },
                "required": ["name"]
            }),
        },
        McpToolDefinition {
            name: "vordr_volume_rm".into(),
            description: "Remove a volume by name or id.".into(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "id": {
                        "type": "string",
                        "description": "Volume id or name"
                    }
                },
                "required": ["id"]
            }),
        },
    ]
}

/// MCP tool call result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpToolResult {
    pub success: bool,
    pub output: String,
    pub error: Option<String>,
}

impl McpToolResult {
    pub fn success(output: impl Into<String>) -> Self {
        Self {
            success: true,
            output: output.into(),
            error: None,
        }
    }

    pub fn error(message: impl Into<String>) -> Self {
        Self {
            success: false,
            output: String::new(),
            error: Some(message.into()),
        }
    }
}
