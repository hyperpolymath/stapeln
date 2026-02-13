// SPDX-License-Identifier: PMPL-1.0-or-later
// Types for Vörðr MCP client

// MCP JSON-RPC types
type mcpRequest = {
  jsonrpc: string,
  method: string,
  params: Js.Json.t,
  id: int,
}

type mcpError = {
  code: int,
  message: string,
  data: option<Js.Json.t>,
}

type mcpResponse = {
  jsonrpc: string,
  result: option<Js.Json.t>,
  error: option<mcpError>,
  id: int,
}

// Vörðr-specific types
type containerConfig = {
  privileged: bool,
  readOnlyRoot: bool,
  networkMode: option<string>,
  memory: option<int>,
  cpus: option<float>,
}

type createContainerParams = {
  image: string,
  name: option<string>,
  config: containerConfig,
}

type verifyImageParams = {
  image: string,
  checkSbom: bool,
  checkSignature: bool,
}

type authorizationRequest = {
  operation: string,
  threshold: int,
  signers: int,
}

type signatureShare = {
  requestId: string,
  signature: string,
  signerId: string,
}

type monitorConfig = {
  containerId: string,
  syscalls: bool,
  network: bool,
  filesystem: bool,
}

// Gateway-compatible types (stubs for now)
module Gateway = {
  module Types = {
    type containerState = Running | Stopped | Created

    type containerInfo = {
      id: string,
      name: string,
      image: string,
      imageDigest: string,
      state: containerState,
      policyVerdict: string,
      createdAt: option<string>,
      startedAt: option<string>,
    }

    type imageInfo = {
      id: string,
      tags: array<string>,
      digest: string,
      size: int,
    }

    type runRequest = {
      imageName: string,
      imageDigest: string,
      name: option<string>,
      containerConfig: option<Js.Json.t>,
    }

    type verificationResult = {
      verified: bool,
      signatures: array<string>,
      sbom: option<Js.Json.t>,
    }
  }
}

// Tool names (matching Vörðr MCP adapter)
let toolContainerCreate = "vordr_container_create"
let toolContainerStart = "vordr_container_start"
let toolContainerStop = "vordr_container_stop"
let toolContainerRemove = "vordr_container_remove"
let toolVerifyImage = "vordr_verify_image"
let toolVerifyConfig = "vordr_verify_config"
let toolRequestAuth = "vordr_request_authorization"
let toolSubmitSignature = "vordr_submit_signature"
let toolMonitorStart = "vordr_monitor_start"
let toolMonitorStop = "vordr_monitor_stop"
let toolGetAnomalies = "vordr_get_anomalies"
let toolRollback = "vordr_rollback"
let toolPreviewRollback = "vordr_preview_rollback"
