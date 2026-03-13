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
    properties: Js.Json.object_(Js.Dict.fromArray([
      ("image", Js.Json.object_(Js.Dict.fromArray([
        ("type", Js.Json.string("string")),
        ("description", Js.Json.string("Container image reference")),
      ]))),
      ("name", Js.Json.object_(Js.Dict.fromArray([
        ("type", Js.Json.string("string")),
        ("description", Js.Json.string("Optional container name")),
      ]))),
      ("command", Js.Json.object_(Js.Dict.fromArray([
        ("type", Js.Json.string("array")),
        ("items", Js.Json.object_(Js.Dict.fromArray([("type", Js.Json.string("string"))]))),
        ("description", Js.Json.string("Command to run")),
      ]))),
      ("detach", Js.Json.object_(Js.Dict.fromArray([
        ("type", Js.Json.string("boolean")),
        ("description", Js.Json.string("Run in background")),
      ]))),
      ("removeOnExit", Js.Json.object_(Js.Dict.fromArray([
        ("type", Js.Json.string("boolean")),
        ("description", Js.Json.string("Remove container when it exits")),
      ]))),
    ])),
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
    properties: Js.Json.object_(Js.Dict.fromArray([
      ("all", Js.Json.object_(Js.Dict.fromArray([
        ("type", Js.Json.string("boolean")),
        ("description", Js.Json.string("Show all containers (default shows only running)")),
      ]))),
      ("filter", Js.Json.object_(Js.Dict.fromArray([
        ("type", Js.Json.string("string")),
        ("description", Js.Json.string("Filter containers by name or image")),
      ]))),
    ])),
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
    properties: Js.Json.object_(Js.Dict.fromArray([
      ("containerId", Js.Json.object_(Js.Dict.fromArray([
        ("type", Js.Json.string("string")),
        ("description", Js.Json.string("Container ID to stop")),
      ]))),
      ("timeout", Js.Json.object_(Js.Dict.fromArray([
        ("type", Js.Json.string("integer")),
        ("description", Js.Json.string("Timeout in seconds before force kill")),
      ]))),
    ])),
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
    properties: Js.Json.object_(Js.Dict.fromArray([
      ("image", Js.Json.object_(Js.Dict.fromArray([
        ("type", Js.Json.string("string")),
        ("description", Js.Json.string("Image reference to verify")),
      ]))),
      ("checkSbom", Js.Json.object_(Js.Dict.fromArray([
        ("type", Js.Json.string("boolean")),
        ("description", Js.Json.string("Check SBOM attestation")),
      ]))),
      ("checkSignature", Js.Json.object_(Js.Dict.fromArray([
        ("type", Js.Json.string("boolean")),
        ("description", Js.Json.string("Check image signature")),
      ]))),
    ])),
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
    properties: Js.Json.object_(Js.Dict.fromArray([
      ("action", Js.Json.object_(Js.Dict.fromArray([
        ("type", Js.Json.string("string")),
        ("enum", Js.Json.array([Js.Json.string("get"), Js.Json.string("set"), Js.Json.string("validate")])),
        ("description", Js.Json.string("Policy action to perform")),
      ]))),
      ("policy", Js.Json.object_(Js.Dict.fromArray([
        ("type", Js.Json.string("object")),
        ("description", Js.Json.string("Policy configuration (for set action)")),
      ]))),
    ])),
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
    properties: Js.Json.object_(Js.Dict.fromArray([
      ("containerId", Js.Json.object_(Js.Dict.fromArray([
        ("type", Js.Json.string("string")),
        ("description", Js.Json.string("Container ID")),
      ]))),
      ("tail", Js.Json.object_(Js.Dict.fromArray([
        ("type", Js.Json.string("integer")),
        ("description", Js.Json.string("Number of lines to show from end")),
      ]))),
      ("since", Js.Json.object_(Js.Dict.fromArray([
        ("type", Js.Json.string("string")),
        ("description", Js.Json.string("Show logs since timestamp")),
      ]))),
    ])),
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
    properties: Js.Json.object_(Js.Dict.fromArray([
      ("containerId", Js.Json.object_(Js.Dict.fromArray([
        ("type", Js.Json.string("string")),
        ("description", Js.Json.string("Container ID")),
      ]))),
      ("command", Js.Json.object_(Js.Dict.fromArray([
        ("type", Js.Json.string("array")),
        ("items", Js.Json.object_(Js.Dict.fromArray([("type", Js.Json.string("string"))]))),
        ("description", Js.Json.string("Command to execute")),
      ]))),
    ])),
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
    properties: Js.Json.object_(Js.Dict.fromArray([
      ("containerId", Js.Json.object_(Js.Dict.fromArray([
        ("type", Js.Json.string("string")),
        ("description", Js.Json.string("Container ID to remove")),
      ]))),
      ("force", Js.Json.object_(Js.Dict.fromArray([
        ("type", Js.Json.string("boolean")),
        ("description", Js.Json.string("Force removal even if running")),
      ]))),
    ])),
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
