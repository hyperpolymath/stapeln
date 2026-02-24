// SPDX-License-Identifier: PMPL-1.0-or-later
// Vörðr MCP Adapter - MCP Server implementation

open Protocol
open Tools

// Server state
type serverState = {
  mutable initialized: bool,
}

let state: serverState = {
  initialized: false,
}

// Initialize the server
let handleInitialize = (params: option<Js.Json.t>): jsonRpcResponse => {
  state.initialized = true
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
  let toolsJson = tools->Js.Array2.map(tool => {
    let obj = Js.Dict.empty()
    Js.Dict.set(obj, "name", Js.Json.string(tool.name))
    Js.Dict.set(obj, "description", Js.Json.string(tool.description))
    Js.Dict.set(obj, "inputSchema", tool.inputSchema)
    Js.Json.object_(obj)
  })
  let result = Js.Dict.empty()
  Js.Dict.set(result, "tools", Js.Json.array(toolsJson))
  successResponse(None, Js.Json.object_(result))
}

// Direct tool calls
let handleDirectToolCall = (method: string, params: option<Js.Json.t>, id: option<Js.Json.t>): jsonRpcResponse => {
  Js.Console.log2("  ⚙️  Direct tool call:", method)
  let result = Js.Dict.empty()
  Js.Dict.set(result, "success", Js.Json.boolean(true))
  Js.Dict.set(result, "tool", Js.Json.string(method))
  Js.Dict.set(result, "params", params->Belt.Option.getWithDefault(Js.Json.null))
  successResponse(id, Js.Json.object_(result))
}

// Call a tool
let handleToolsCall = (params: option<Js.Json.t>): jsonRpcResponse => {
  switch params {
  | None => errorResponse(None, -32602, "Missing params", None)
  | Some(paramsJson) =>
    let obj = Js.Json.decodeObject(paramsJson)
    let name = switch obj {
    | Some(o) => Js.Dict.get(o, "name")->Belt.Option.flatMap(Js.Json.decodeString)
    | None => None
    }
    switch name {
    | None => errorResponse(None, -32602, "Missing tool name", None)
    | Some(toolName) => handleDirectToolCall(toolName, params, None)
    }
  }
}

// Main request handler
let handleRequest = (request: jsonRpcRequest): jsonRpcResponse => {
  switch request.method {
  | "initialize" => handleInitialize(request.params)
  | "initialized" => successResponse(request.id, Js.Json.object_(Js.Dict.empty()))
  | "tools/list" => handleToolsList(request.params)
  | "tools/call" => handleToolsCall(request.params)
  | "ping" => successResponse(request.id, Js.Json.object_(Js.Dict.fromArray([("pong", Js.Json.boolean(true))])))
  | "vordr_network_create"
  | "vordr_network_rm"
  | "vordr_volume_create"
  | "vordr_volume_rm"
  | "containers/list"
  | "containers/create"
  | "containers/start"
  | "containers/stop" => handleDirectToolCall(request.method, request.params, request.id)
  | method => errorResponse(request.id, -32601, `Method not found: ${method}`, None)
  }
}
