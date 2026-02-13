// SPDX-License-Identifier: PMPL-1.0-or-later
// Vörðr MCP Adapter - Protocol definitions

// MCP JSON-RPC request/response types
type jsonRpcRequest = {
  jsonrpc: string,
  id: option<Js.Json.t>,
  method: string,
  params: option<Js.Json.t>,
}

type jsonRpcError = {
  code: int,
  message: string,
  data: option<Js.Json.t>,
}

type jsonRpcResponse = {
  jsonrpc: string,
  id: option<Js.Json.t>,
  result: option<Js.Json.t>,
  error: option<jsonRpcError>,
}

// MCP capability types
type toolCapability = {available: bool}
type resourceCapability = {available: bool}
type promptCapability = {available: bool}
type rootsCapability = {listChanged: bool}
type samplingCapability = {}

type serverCapabilities = {
  tools: option<toolCapability>,
  resources: option<resourceCapability>,
  prompts: option<promptCapability>,
}

type clientCapabilities = {
  roots: option<rootsCapability>,
  sampling: option<samplingCapability>,
}

// MCP tool definition
type toolParameter = {
  name: string,
  description: string,
  required: bool,
  schema: Js.Json.t,
}

type toolDefinition = {
  name: string,
  description: string,
  inputSchema: Js.Json.t,
}

// Standard MCP error codes
module ErrorCode = {
  let parseError = -32700
  let invalidRequest = -32600
  let methodNotFound = -32601
  let invalidParams = -32602
  let internalError = -32603
}

// Create a success response
let successResponse = (id: option<Js.Json.t>, result: Js.Json.t): jsonRpcResponse => {
  {
    jsonrpc: "2.0",
    id,
    result: Some(result),
    error: None,
  }
}

// Create an error response
let errorResponse = (
  id: option<Js.Json.t>,
  code: int,
  message: string,
  data: option<Js.Json.t>,
): jsonRpcResponse => {
  {
    jsonrpc: "2.0",
    id,
    result: None,
    error: Some({
      code,
      message,
      data,
    }),
  }
}
