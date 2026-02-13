// SPDX-License-Identifier: PMPL-1.0-or-later
// MCP types for Svalinn edge tools

// MCP protocol types
type toolInput = {
  name: string,
  description: option<string>,
  type_: string,
  required: bool,
}

type inputSchema = {
  type_: string,
  properties: Js.Json.t,
  required: array<string>,
}

type tool = {
  name: string,
  description: string,
  inputSchema: inputSchema,
}

type textContent = {
  type_: string,
  text: string,
}

type toolResult = {
  content: array<textContent>,
  isError: option<bool>,
}

type listToolsResult = {
  tools: array<tool>,
}

// JSON-RPC types
type jsonRpcRequest = {
  jsonrpc: string,
  method: string,
  params: option<Js.Json.t>,
  id: option<int>,
}

type jsonRpcError = {
  code: int,
  message: string,
}

type jsonRpcResponse = {
  jsonrpc: string,
  result: option<Js.Json.t>,
  error: option<jsonRpcError>,
  id: option<int>,
}

// Method names
let methodInitialize = "initialize"
let methodListTools = "tools/list"
let methodCallTool = "tools/call"
let methodNotification = "notifications/message"
