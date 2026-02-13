// SPDX-License-Identifier: PMPL-1.0-or-later
// Svalinn MCP tool definitions

open McpTypes

// Tool: svalinn_run
// Validate and delegate container run to Vörðr
let svalinnRun: tool = {
  name: "svalinn_run",
  description: "Run a container with edge validation. Validates request against verified-container-spec, checks edge policy, then delegates to Vörðr.",
  inputSchema: {
    type_: "object",
    properties: Obj.magic({
      "image": {
        "type": "string",
        "description": "Container image reference",
      },
      "name": {
        "type": "string",
        "description": "Optional container name",
      },
      "command": {
        "type": "array",
        "items": {"type": "string"},
        "description": "Command to run",
      },
      "detach": {
        "type": "boolean",
        "description": "Run in background",
      },
      "removeOnExit": {
        "type": "boolean",
        "description": "Remove container when it exits",
      },
    }),
    required: ["image"],
  },
}

// Tool: svalinn_ps
// List containers via Vörðr
let svalinnPs: tool = {
  name: "svalinn_ps",
  description: "List containers managed by Vörðr",
  inputSchema: {
    type_: "object",
    properties: Obj.magic({
      "all": {
        "type": "boolean",
        "description": "Show all containers (default shows only running)",
      },
      "filter": {
        "type": "string",
        "description": "Filter containers by name or image",
      },
    }),
    required: [],
  },
}

// Tool: svalinn_stop
// Stop container via Vörðr
let svalinnStop: tool = {
  name: "svalinn_stop",
  description: "Stop a running container",
  inputSchema: {
    type_: "object",
    properties: Obj.magic({
      "containerId": {
        "type": "string",
        "description": "Container ID to stop",
      },
      "timeout": {
        "type": "integer",
        "description": "Timeout in seconds before force kill",
      },
    }),
    required: ["containerId"],
  },
}

// Tool: svalinn_verify
// Verify image attestation via Vörðr
let svalinnVerify: tool = {
  name: "svalinn_verify",
  description: "Verify container image signature and attestation via Vörðr",
  inputSchema: {
    type_: "object",
    properties: Obj.magic({
      "image": {
        "type": "string",
        "description": "Image reference to verify",
      },
      "checkSbom": {
        "type": "boolean",
        "description": "Check SBOM attestation",
      },
      "checkSignature": {
        "type": "boolean",
        "description": "Check image signature",
      },
    }),
    required: ["image"],
  },
}

// Tool: svalinn_policy
// Edge policy management
let svalinnPolicy: tool = {
  name: "svalinn_policy",
  description: "Manage edge security policies",
  inputSchema: {
    type_: "object",
    properties: Obj.magic({
      "action": {
        "type": "string",
        "enum": ["get", "set", "validate"],
        "description": "Policy action to perform",
      },
      "policy": {
        "type": "object",
        "description": "Policy configuration (for set action)",
      },
    }),
    required: ["action"],
  },
}

// Tool: svalinn_logs
// Get container logs
let svalinnLogs: tool = {
  name: "svalinn_logs",
  description: "Get container logs",
  inputSchema: {
    type_: "object",
    properties: Obj.magic({
      "containerId": {
        "type": "string",
        "description": "Container ID",
      },
      "tail": {
        "type": "integer",
        "description": "Number of lines to show from end",
      },
      "since": {
        "type": "string",
        "description": "Show logs since timestamp",
      },
    }),
    required: ["containerId"],
  },
}

// Tool: svalinn_exec
// Execute command in container
let svalinnExec: tool = {
  name: "svalinn_exec",
  description: "Execute a command in a running container",
  inputSchema: {
    type_: "object",
    properties: Obj.magic({
      "containerId": {
        "type": "string",
        "description": "Container ID",
      },
      "command": {
        "type": "array",
        "items": {"type": "string"},
        "description": "Command to execute",
      },
    }),
    required: ["containerId", "command"],
  },
}

// Tool: svalinn_rm
// Remove container
let svalinnRm: tool = {
  name: "svalinn_rm",
  description: "Remove a stopped container",
  inputSchema: {
    type_: "object",
    properties: Obj.magic({
      "containerId": {
        "type": "string",
        "description": "Container ID to remove",
      },
      "force": {
        "type": "boolean",
        "description": "Force removal even if running",
      },
    }),
    required: ["containerId"],
  },
}

// All tools
let allTools: array<tool> = [
  svalinnRun,
  svalinnPs,
  svalinnStop,
  svalinnVerify,
  svalinnPolicy,
  svalinnLogs,
  svalinnExec,
  svalinnRm,
]
