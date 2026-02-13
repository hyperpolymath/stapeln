// SPDX-License-Identifier: PMPL-1.0-or-later
// Vörðr MCP Adapter - MCP Server implementation

open Protocol
open Tools

// Server state
type serverState = {
  mutable initialized: bool,
  mutable clientCapabilities: option<clientCapabilities>,
}

let state: serverState = {
  initialized: false,
  clientCapabilities: None,
}

// Server capabilities
let serverCapabilities: serverCapabilities = {
  tools: Some({available: true}),
  resources: Some({available: true}),
  prompts: None,
}

// Initialize the server
let handleInitialize = (params: option<Js.Json.t>): jsonRpcResponse => {
  state.initialized = true

  // Parse client capabilities if provided
  switch params {
  | Some(_) => state.clientCapabilities = None  // Parse in real impl
  | None => ()
  }

  let result: Js.Json.t = %raw(`{
    "protocolVersion": "2024-11-05",
    "capabilities": {
      "tools": { "listChanged": false },
      "resources": { "listChanged": false }
    },
    "serverInfo": {
      "name": "vordr-mcp-adapter",
      "version": "0.1.0"
    }
  }`)

  successResponse(None, result)
}

// List available tools
let handleToolsList = (_params: option<Js.Json.t>): jsonRpcResponse => {
  let tools = getTools()
  let toolsJson = tools->Array.map(tool => {
    let obj = Dict.make()
    Dict.set(obj, "name", JSON.Encode.string(tool.name))
    Dict.set(obj, "description", JSON.Encode.string(tool.description))
    Dict.set(obj, "inputSchema", tool.inputSchema)
    JSON.Encode.object(obj)
  })

  let result = Dict.make()
  Dict.set(result, "tools", JSON.Encode.array(toolsJson))

  successResponse(None, JSON.Encode.object(result))
}

// Call a tool
let handleToolsCall = (params: option<Js.Json.t>): jsonRpcResponse => {
  switch params {
  | None =>
    errorResponse(None, ErrorCode.invalidParams, "Missing params", None)
  | Some(paramsJson) =>
    // Extract tool name from params
    let name = switch JSON.Decode.object(paramsJson) {
    | Some(obj) =>
      switch Dict.get(obj, "name") {
      | Some(nameJson) => JSON.Decode.string(nameJson)
      | None => None
      }
    | None => None
    }

    switch name {
    | None =>
      errorResponse(None, ErrorCode.invalidParams, "Missing tool name", None)
    | Some(toolName) =>
      let toolResult = handleTool(toolName, params)
      let resultObj = Dict.make()
      Dict.set(resultObj, "content", %raw(`[{
        "type": "text",
        "text": "Tool executed successfully"
      }]`))
      successResponse(None, JSON.Encode.object(resultObj))
    }
  }
}

// Main request handler
let handleRequest = (request: jsonRpcRequest): jsonRpcResponse => {
  switch request.method {
  | "initialize" => handleInitialize(request.params)
  | "initialized" => successResponse(request.id, JSON.Encode.object(Dict.make()))
  | "tools/list" => handleToolsList(request.params)
  | "tools/call" => handleToolsCall(request.params)
  | "ping" =>
    let result = Dict.make()
    successResponse(request.id, JSON.Encode.object(result))
  | method =>
    errorResponse(
      request.id,
      ErrorCode.methodNotFound,
      `Method not found: ${method}`,
      None
    )
  }
}
