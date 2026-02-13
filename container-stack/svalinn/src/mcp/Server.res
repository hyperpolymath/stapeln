// SPDX-License-Identifier: PMPL-1.0-or-later
// Svalinn MCP server implementation

open McpTypes

// Server info
let serverName = "svalinn"
let serverVersion = "0.1.0"
let protocolVersion = "2024-11-05"

// Handle initialize request
let handleInitialize = (_params: option<Js.Json.t>): Js.Json.t => {
  Obj.magic({
    "protocolVersion": protocolVersion,
    "capabilities": {
      "tools": %raw(`{}`),
    },
    "serverInfo": {
      "name": serverName,
      "version": serverVersion,
    },
  })
}

// Handle list tools request
let handleListTools = (): Js.Json.t => {
  let result: listToolsResult = {
    tools: Tools.allTools,
  }
  Obj.magic(result)
}

// Handle tool call
let rec handleCallTool = async (params: Js.Json.t): Js.Json.t => {
  let name: string = Obj.magic(params)["name"]
  let arguments: Js.Json.t = Obj.magic(params)["arguments"]

  let result = switch name {
  | "svalinn_run" => await handleRun(arguments)
  | "svalinn_ps" => await handlePs(arguments)
  | "svalinn_stop" => await handleStop(arguments)
  | "svalinn_verify" => await handleVerify(arguments)
  | "svalinn_policy" => handlePolicy(arguments)
  | "svalinn_logs" => await handleLogs(arguments)
  | "svalinn_exec" => await handleExec(arguments)
  | "svalinn_rm" => await handleRm(arguments)
  | _ =>
    makeError(`Unknown tool: ${name}`)
  }

  result
}

// Tool handlers
and handleRun = async (_args: Js.Json.t): Js.Json.t => {
  // TODO: Implement container run via Vörðr MCP
  makeError("svalinn_run not yet implemented")
}

and handlePs = async (_args: Js.Json.t): Js.Json.t => {
  // TODO: Implement container list via Vörðr MCP
  makeError("svalinn_ps not yet implemented")
}

and handleStop = async (_args: Js.Json.t): Js.Json.t => {
  // TODO: Implement container stop via Vörðr MCP
  makeError("svalinn_stop not yet implemented")
}

and handleVerify = async (_args: Js.Json.t): Js.Json.t => {
  // TODO: Implement image verification via Vörðr MCP
  makeError("svalinn_verify not yet implemented")
}

and handlePolicy = (args: Js.Json.t): Js.Json.t => {
  let action: string = Obj.magic(args)["action"]

  switch action {
  | "get" =>
    let policy = Validation.defaultPolicy
    makeSuccess(Js.Json.stringify(Obj.magic(policy)))
  | "validate" =>
    makeSuccess("Policy valid")
  | _ =>
    makeError(`Unknown policy action: ${action}`)
  }
}

and handleLogs = async (args: Js.Json.t): Js.Json.t => {
  let _containerId: string = Obj.magic(args)["containerId"]
  // Would call Vörðr for logs - not implemented yet
  makeSuccess("Logs not yet implemented")
}

and handleExec = async (args: Js.Json.t): Js.Json.t => {
  let _containerId: string = Obj.magic(args)["containerId"]
  let _command: array<string> = Obj.magic(args)["command"]
  // Would call Vörðr for exec - not implemented yet
  makeSuccess("Exec not yet implemented")
}

and handleRm = async (_args: Js.Json.t): Js.Json.t => {
  // TODO: Implement container remove via Vörðr MCP
  makeError("svalinn_rm not yet implemented")
}

// Helper functions
and makeSuccess = (text: string): Js.Json.t => {
  let result: toolResult = {
    content: [{type_: "text", text}],
    isError: None,
  }
  Obj.magic(result)
}

and makeError = (text: string): Js.Json.t => {
  let result: toolResult = {
    content: [{type_: "text", text}],
    isError: Some(true),
  }
  Obj.magic(result)
}

// Main request dispatcher
let handleRequest = async (request: jsonRpcRequest): jsonRpcResponse => {
  let result = switch request.method {
  | "initialize" =>
    handleInitialize(request.params)
  | "tools/list" =>
    handleListTools()
  | "tools/call" =>
    switch request.params {
    | Some(p) => await handleCallTool(p)
    | None => makeError("Missing params for tools/call")
    }
  | method =>
    makeError(`Unknown method: ${method}`)
  }

  {
    jsonrpc: "2.0",
    result: Some(result),
    error: None,
    id: request.id,
  }
}
