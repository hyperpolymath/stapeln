// SPDX-License-Identifier: PMPL-1.0-or-later
// Vörðr MCP Adapter - Tool implementations

open Types
open Protocol

// Tool definitions for MCP server
let toolDefinitions: array<toolDefinition> = [
  // Container lifecycle tools
  {
    name: "vordr_container_create",
    description: "Create a new container with verified configuration",
    inputSchema: %raw(`{
      "type": "object",
      "properties": {
        "image": { "type": "string", "description": "Container image reference" },
        "name": { "type": "string", "description": "Container name (optional)" },
        "config": { "type": "object", "description": "Container configuration" }
      },
      "required": ["image"]
    }`),
  },
  {
    name: "vordr_container_start",
    description: "Start a created container after verification",
    inputSchema: %raw(`{
      "type": "object",
      "properties": {
        "containerId": { "type": "string", "description": "Container ID to start" }
      },
      "required": ["containerId"]
    }`),
  },
  {
    name: "vordr_container_stop",
    description: "Stop a running container",
    inputSchema: %raw(`{
      "type": "object",
      "properties": {
        "containerId": { "type": "string", "description": "Container ID to stop" },
        "timeout": { "type": "integer", "description": "Stop timeout in seconds" }
      },
      "required": ["containerId"]
    }`),
  },
  {
    name: "vordr_container_remove",
    description: "Remove a stopped container",
    inputSchema: %raw(`{
      "type": "object",
      "properties": {
        "containerId": { "type": "string", "description": "Container ID to remove" },
        "force": { "type": "boolean", "description": "Force remove even if running" }
      },
      "required": ["containerId"]
    }`),
  },
  // Verification tools
  {
    name: "vordr_verify_image",
    description: "Verify a container image signature and attestation",
    inputSchema: %raw(`{
      "type": "object",
      "properties": {
        "image": { "type": "string", "description": "Image reference to verify" },
        "checkSbom": { "type": "boolean", "description": "Check SBOM attestation" },
        "checkSignature": { "type": "boolean", "description": "Check image signature" }
      },
      "required": ["image"]
    }`),
  },
  {
    name: "vordr_verify_config",
    description: "Verify container configuration against security policy",
    inputSchema: %raw(`{
      "type": "object",
      "properties": {
        "config": { "type": "object", "description": "Container configuration to verify" }
      },
      "required": ["config"]
    }`),
  },
  // Authorization tools
  {
    name: "vordr_request_authorization",
    description: "Request threshold authorization for privileged operation",
    inputSchema: %raw(`{
      "type": "object",
      "properties": {
        "operation": { "type": "string", "description": "Operation requiring authorization" },
        "threshold": { "type": "integer", "description": "Required signature threshold" },
        "signers": { "type": "integer", "description": "Total authorized signers" }
      },
      "required": ["operation", "threshold", "signers"]
    }`),
  },
  {
    name: "vordr_submit_signature",
    description: "Submit a signature share for authorization",
    inputSchema: %raw(`{
      "type": "object",
      "properties": {
        "requestId": { "type": "string", "description": "Authorization request ID" },
        "signature": { "type": "string", "description": "Signature share (base64)" },
        "signerId": { "type": "string", "description": "Signer identifier" }
      },
      "required": ["requestId", "signature", "signerId"]
    }`),
  },
  // Monitoring tools
  {
    name: "vordr_monitor_start",
    description: "Start eBPF monitoring for a container",
    inputSchema: %raw(`{
      "type": "object",
      "properties": {
        "containerId": { "type": "string", "description": "Container ID to monitor" },
        "syscalls": { "type": "boolean", "description": "Monitor syscalls" },
        "network": { "type": "boolean", "description": "Monitor network activity" },
        "filesystem": { "type": "boolean", "description": "Monitor filesystem access" }
      },
      "required": ["containerId"]
    }`),
  },
  {
    name: "vordr_monitor_stop",
    description: "Stop eBPF monitoring for a container",
    inputSchema: %raw(`{
      "type": "object",
      "properties": {
        "containerId": { "type": "string", "description": "Container ID to stop monitoring" }
      },
      "required": ["containerId"]
    }`),
  },
  {
    name: "vordr_get_anomalies",
    description: "Get detected anomalies for a container",
    inputSchema: %raw(`{
      "type": "object",
      "properties": {
        "containerId": { "type": "string", "description": "Container ID" },
        "severity": { "type": "string", "enum": ["low", "medium", "high", "critical"] }
      },
      "required": ["containerId"]
    }`),
  },
  // Rollback tools
  {
    name: "vordr_rollback",
    description: "Rollback to previous container state (Bennett reversibility)",
    inputSchema: %raw(`{
      "type": "object",
      "properties": {
        "containerId": { "type": "string", "description": "Container ID" },
        "steps": { "type": "integer", "description": "Number of steps to rollback" }
      },
      "required": ["containerId"]
    }`),
  },
  {
    name: "vordr_preview_rollback",
    description: "Preview what rollback would do without executing",
    inputSchema: %raw(`{
      "type": "object",
      "properties": {
        "containerId": { "type": "string", "description": "Container ID" },
        "steps": { "type": "integer", "description": "Number of steps to preview" }
      },
      "required": ["containerId"]
    }`),
  },
]

// Get all tool definitions
let getTools = (): array<toolDefinition> => toolDefinitions

// Tool handler stub (implementations call into Elixir/Rust backend)
let handleTool = (name: string, params: option<Js.Json.t>): toolResponse => {
  // Stub implementation - actual calls go to Elixir GenServer via FFI
  {
    success: true,
    message: Some(`Tool ${name} invoked`),
    data: params,
  }
}
