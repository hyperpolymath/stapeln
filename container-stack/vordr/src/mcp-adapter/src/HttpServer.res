// SPDX-License-Identifier: PMPL-1.0-or-later
// HTTP transport for MCP protocol (LSP-style architecture)

// JSON-RPC 2.0 types
type jsonRpcRequest = {
  jsonrpc: string,
  method: string,
  params: option<Js.Json.t>,
  id: option<Js.Json.t>,
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

// Parse JSON-RPC request from JSON
let parseRequest = (json: Js.Json.t): option<jsonRpcRequest> => {
  json->Js.Json.decodeObject->Belt.Option.map(obj => {
    let jsonrpc = obj
      ->Js.Dict.get("jsonrpc")
      ->Belt.Option.flatMap(Js.Json.decodeString)
      ->Belt.Option.getWithDefault("2.0")

    let method = obj
      ->Js.Dict.get("method")
      ->Belt.Option.flatMap(Js.Json.decodeString)
      ->Belt.Option.getWithDefault("")

    let params = obj->Js.Dict.get("params")
    let id = obj->Js.Dict.get("id")

    {jsonrpc, method, params, id}
  })
}

// MCP Protocol Handler (pure logic, no transport concerns)
module Protocol = {
  type t = {mutable initialized: bool}

  let make = () => {initialized: false}

  let success = (id: option<Js.Json.t>, result: Js.Json.t): jsonRpcResponse => {
    {
      jsonrpc: "2.0",
      id,
      result: Some(result),
      error: None,
    }
  }

  let error = (id: option<Js.Json.t>, code: int, message: string): jsonRpcResponse => {
    {
      jsonrpc: "2.0",
      id,
      result: None,
      error: Some({code, message, data: None}),
    }
  }

  let handleInitialize = (self: t, id: option<Js.Json.t>): jsonRpcResponse => {
    self.initialized = true
    let result = Js.Dict.fromArray([
      ("protocolVersion", Js.Json.string("2024-11-05")),
      (
        "capabilities",
        Js.Json.object_(
          Js.Dict.fromArray([
            ("tools", Js.Json.object_(Js.Dict.fromArray([("listChanged", Js.Json.boolean(false))]))),
            (
              "resources",
              Js.Json.object_(Js.Dict.fromArray([("listChanged", Js.Json.boolean(false))])),
            ),
          ]),
        ),
      ),
      (
        "serverInfo",
        Js.Json.object_(
          Js.Dict.fromArray([
            ("name", Js.Json.string("vordr-mcp-adapter")),
            ("version", Js.Json.string("0.1.0")),
          ]),
        ),
      ),
    ])

    success(id, Js.Json.object_(result))
  }

  let handleToolsList = (id: option<Js.Json.t>): jsonRpcResponse => {
    let tools = [
      Js.Json.object_(
        Js.Dict.fromArray([
          ("name", Js.Json.string("containers/list")),
          ("description", Js.Json.string("List all containers")),
          (
            "inputSchema",
            Js.Json.object_(Js.Dict.fromArray([("type", Js.Json.string("object"))])),
          ),
        ]),
      ),
      Js.Json.object_(
        Js.Dict.fromArray([
          ("name", Js.Json.string("containers/create")),
          ("description", Js.Json.string("Create a container")),
          (
            "inputSchema",
            Js.Json.object_(
              Js.Dict.fromArray([
                ("type", Js.Json.string("object")),
                (
                  "properties",
                  Js.Json.object_(
                    Js.Dict.fromArray([
                      (
                        "image",
                        Js.Json.object_(Js.Dict.fromArray([("type", Js.Json.string("string"))])),
                      ),
                      (
                        "name",
                        Js.Json.object_(Js.Dict.fromArray([("type", Js.Json.string("string"))])),
                      ),
                    ]),
                  ),
                ),
                ("required", Js.Json.array([Js.Json.string("image")])),
              ]),
            ),
          ),
        ]),
      ),
      Js.Json.object_(
        Js.Dict.fromArray([
          ("name", Js.Json.string("containers/start")),
          ("description", Js.Json.string("Start a container")),
          (
            "inputSchema",
            Js.Json.object_(
              Js.Dict.fromArray([
                ("type", Js.Json.string("object")),
                (
                  "properties",
                  Js.Json.object_(
                    Js.Dict.fromArray([
                      (
                        "id",
                        Js.Json.object_(Js.Dict.fromArray([("type", Js.Json.string("string"))])),
                      ),
                    ]),
                  ),
                ),
                ("required", Js.Json.array([Js.Json.string("id")])),
              ]),
            ),
          ),
        ]),
      ),
      Js.Json.object_(
        Js.Dict.fromArray([
          ("name", Js.Json.string("containers/stop")),
          ("description", Js.Json.string("Stop a container")),
          (
            "inputSchema",
            Js.Json.object_(
              Js.Dict.fromArray([
                ("type", Js.Json.string("object")),
                (
                  "properties",
                  Js.Json.object_(
                    Js.Dict.fromArray([
                      (
                        "id",
                        Js.Json.object_(Js.Dict.fromArray([("type", Js.Json.string("string"))])),
                      ),
                    ]),
                  ),
                ),
                ("required", Js.Json.array([Js.Json.string("id")])),
              ]),
            ),
          ),
        ]),
      ),
      Js.Json.object_(
        Js.Dict.fromArray([
          ("name", Js.Json.string("images/verify")),
          ("description", Js.Json.string("Verify a container image (.ctp bundle)")),
          (
            "inputSchema",
            Js.Json.object_(
              Js.Dict.fromArray([
                ("type", Js.Json.string("object")),
                (
                  "properties",
                  Js.Json.object_(
                    Js.Dict.fromArray([
                      (
                        "digest",
                        Js.Json.object_(Js.Dict.fromArray([("type", Js.Json.string("string"))])),
                      ),
                      (
                        "policy",
                        Js.Json.object_(Js.Dict.fromArray([("type", Js.Json.string("object"))])),
                      ),
                    ]),
                  ),
                ),
                ("required", Js.Json.array([Js.Json.string("digest")])),
              ]),
            ),
          ),
        ]),
      ),
    ]

    let result = Js.Dict.fromArray([("tools", Js.Json.array(tools))])
    success(id, Js.Json.object_(result))
  }

  let handleDirectToolCall = (method: string, params: option<Js.Json.t>, id: option<Js.Json.t>): jsonRpcResponse => {
    Js.Console.log2("  ⚙️  Direct tool call:", method)

    let result = Js.Dict.fromArray([
      ("success", Js.Json.boolean(true)),
      ("tool", Js.Json.string(method)),
      ("params", params->Belt.Option.getWithDefault(Js.Json.null)),
      ("message", Js.Json.string(`Tool ${method} executed successfully (mock response)`)),
    ])

    success(id, Js.Json.object_(result))
  }

  let handleRequest = (self: t, request: jsonRpcRequest): jsonRpcResponse => {
    Js.Console.log2("→", request.method)
    if request.params->Belt.Option.isSome {
      Js.Console.log(request.params->Belt.Option.getExn)
    }

    switch request.method {
    | "initialize" => handleInitialize(self, request.id)
    | "initialized" => success(request.id, Js.Json.object_(Js.Dict.empty()))
    | "tools/list" => handleToolsList(request.id)
    | "tools/call" => {
        // Standard MCP tool call - params should have {name, arguments}
        let result = Js.Dict.fromArray([
          (
            "content",
            Js.Json.array([
              Js.Json.object_(
                Js.Dict.fromArray([
                  ("type", Js.Json.string("text")),
                  ("text", Js.Json.string("Tool executed successfully (mock)")),
                ]),
              ),
            ]),
          ),
        ])
        success(request.id, Js.Json.object_(result))
      }
    | "ping" => success(request.id, Js.Json.object_(Js.Dict.fromArray([("pong", Js.Json.boolean(true))])))

    // Direct tool calls (Svalinn compatibility)
    | "containers/list"
    | "containers/create"
    | "containers/start"
    | "containers/stop"
    | "containers/get"
    | "containers/remove"
    | "containers/logs"
    | "containers/exec"
    | "images/list"
    | "images/pull"
    | "images/remove"
    | "images/verify"
    | "health"
    | "version" =>
      handleDirectToolCall(request.method, request.params, request.id)

    | _ => error(request.id, -32601, `Method not found: ${request.method}`)
    }
  }
}

// Encode JSON-RPC response to JSON
let encodeResponse = (response: jsonRpcResponse): Js.Json.t => {
  let dict = Js.Dict.empty()
  Js.Dict.set(dict, "jsonrpc", Js.Json.string(response.jsonrpc))

  switch response.id {
  | Some(id) => Js.Dict.set(dict, "id", id)
  | None => ()
  }

  switch response.result {
  | Some(result) => Js.Dict.set(dict, "result", result)
  | None => ()
  }

  switch response.error {
  | Some(err) => {
      let errorDict = Js.Dict.fromArray([
        ("code", Js.Json.number(Belt.Int.toFloat(err.code))),
        ("message", Js.Json.string(err.message)),
      ])
      switch err.data {
      | Some(data) => Js.Dict.set(errorDict, "data", data)
      | None => ()
      }
      Js.Dict.set(dict, "error", Js.Json.object_(errorDict))
    }
  | None => ()
  }

  Js.Json.object_(dict)
}

// Deno bindings
module Deno = {
  type serveOptions = {
    port: int,
    hostname: string,
  }

  type request
  type response

  @scope("Deno") @val
  external serve: ((request) => Js.Promise.t<response>, serveOptions) => unit = "serve"

  @new external makeResponse: (string, {"headers": Js.Dict.t<string>}) => response = "Response"

  @get external getMethod: request => string = "method"
  @send external json: request => Js.Promise.t<Js.Json.t> = "json"
}

// HTTP Server
let port = 8080
let host = "0.0.0.0"

let protocol = Protocol.make()

let corsHeaders = Js.Dict.fromArray([
    // SECURITY FIX: Replaced CORS wildcard with environment-based origin
  ("Access-Control-Allow-Origin", Node.process->Node.Process.env->Js.Dict.get("ALLOWED_ORIGINS")->Belt.Option.getWithDefault("http://localhost:3000")),
  ("Access-Control-Allow-Methods", "POST, OPTIONS"),
  ("Access-Control-Allow-Headers", "Content-Type"),
  ("Content-Type", "application/json"),
])

let handler = (req: Deno.request): Js.Promise.t<Deno.response> => {
  // CORS preflight
  if Deno.getMethod(req) == "OPTIONS" {
    Js.Promise.resolve(Deno.makeResponse("", {"headers": corsHeaders}))
  } else if Deno.getMethod(req) == "POST" {
    Deno.json(req)
    ->Js.Promise.then_(json => {
      let request = parseRequest(json)

      let response = switch request {
      | Some(req) => {
          let response = Protocol.handleRequest(protocol, req)
          let responseJson = encodeResponse(response)
          let responseText = Js.Json.stringify(responseJson)
          Deno.makeResponse(responseText, {"headers": corsHeaders})
        }
      | None => {
          let errorResponse = Protocol.error(None, -32700, "Parse error")
          let responseJson = encodeResponse(errorResponse)
          let responseText = Js.Json.stringify(responseJson)
          Deno.makeResponse(responseText, {"headers": corsHeaders})
        }
      }
      Js.Promise.resolve(response)
    }, _)
    ->Js.Promise.catch(_err => {
      let errorResponse = Protocol.error(None, -32603, "Internal error")
      let responseJson = encodeResponse(errorResponse)
      let responseText = Js.Json.stringify(responseJson)
      Js.Promise.resolve(Deno.makeResponse(responseText, {"headers": corsHeaders}))
    }, _)
  } else {
    Js.Promise.resolve(Deno.makeResponse("Method not allowed", {"headers": corsHeaders}))
  }
}

// Print banner
Js.Console.log("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
Js.Console.log("  Vörðr MCP Adapter v0.1.0")
Js.Console.log("  Transport: HTTP (LSP-style)")
Js.Console.log(`  Listening: http://0.0.0.0:${Belt.Int.toString(port)}`)
Js.Console.log("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")

// Start server
Deno.serve(handler, {port, hostname: host})
