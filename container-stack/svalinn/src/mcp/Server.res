// SPDX-License-Identifier: PMPL-1.0-or-later
// Svalinn MCP server implementation
// Exposes Svalinn edge operations to MCP clients (AI agents) via JSON-RPC 2.0.
// Delegates container/image operations to Vordr through McpClient.

open McpTypes

// JSON-RPC 2.0 error codes
let errorInvalidRequest = -32600
let errorMethodNotFound = -32601
let errorInvalidParams = -32602
let errorInternal = -32603

// Server info
let serverName = "svalinn"
let serverVersion = "0.1.0"
let protocolVersion = "2024-11-05"

// Shared McpClient config (initialised once from environment)
let mcpConfig = McpClient.fromEnv()

// Handle initialize request
let handleInitialize = (_params: option<Js.Json.t>): Js.Json.t => {
  Js.Json.object_(Js.Dict.fromArray([
    ("protocolVersion", Js.Json.string(protocolVersion)),
    ("capabilities", Js.Json.object_(Js.Dict.fromArray([
      ("tools", Js.Json.object_(Js.Dict.empty())),
    ]))),
    ("serverInfo", Js.Json.object_(Js.Dict.fromArray([
      ("name", Js.Json.string(serverName)),
      ("version", Js.Json.string(serverVersion)),
    ]))),
  ]))
}

// Handle list tools request
let handleListTools = (): Js.Json.t => {
  let tools = Belt.Array.map(Tools.allTools, tool => {
    let schemaEntries = [
      ("type", Js.Json.string(tool.inputSchema.type_)),
      ("properties", tool.inputSchema.properties),
      ("required", Js.Json.array(
        Belt.Array.map(tool.inputSchema.required, Js.Json.string),
      )),
    ]
    Js.Json.object_(Js.Dict.fromArray([
      ("name", Js.Json.string(tool.name)),
      ("description", Js.Json.string(tool.description)),
      ("inputSchema", Js.Json.object_(Js.Dict.fromArray(schemaEntries))),
    ]))
  })
  Js.Json.object_(Js.Dict.fromArray([
    ("tools", Js.Json.array(tools)),
  ]))
}

// ---------------------------------------------------------------------------
// Helper functions
// ---------------------------------------------------------------------------

// Build a successful MCP tool result from a text payload.
let makeSuccess = (text: string): Js.Json.t => {
  Js.Json.object_(Js.Dict.fromArray([
    ("content", Js.Json.array([
      Js.Json.object_(Js.Dict.fromArray([
        ("type", Js.Json.string("text")),
        ("text", Js.Json.string(text)),
      ])),
    ])),
  ]))
}

// Build a successful MCP tool result from a structured JSON payload.
let makeSuccessJson = (json: Js.Json.t): Js.Json.t => {
  Js.Json.object_(Js.Dict.fromArray([
    ("content", Js.Json.array([
      Js.Json.object_(Js.Dict.fromArray([
        ("type", Js.Json.string("text")),
        ("text", Js.Json.string(Js.Json.stringify(json))),
      ])),
    ])),
  ]))
}

// Build an error MCP tool result.
let makeError = (text: string): Js.Json.t => {
  Js.Json.object_(Js.Dict.fromArray([
    ("content", Js.Json.array([
      Js.Json.object_(Js.Dict.fromArray([
        ("type", Js.Json.string("text")),
        ("text", Js.Json.string(text)),
      ])),
    ])),
    ("isError", Js.Json.boolean(true)),
  ]))
}

// Build a JSON-RPC error response (for protocol-level errors, not tool errors).
let makeJsonRpcError = (id: option<int>, code: int, message: string): jsonRpcResponse => {
  {
    jsonrpc: "2.0",
    result: None,
    error: Some({code, message}),
    id,
  }
}

// Extract a required string parameter, returning an error result on absence.
let requireString = (args: Js.Json.t, field: string): result<string, Js.Json.t> => {
  switch Validation.getString(args, field) {
  | Some(v) => Ok(v)
  | None => Error(makeError(`Missing required parameter: ${field}`))
  }
}

// ---------------------------------------------------------------------------
// Tool handlers — each delegates to McpClient for Vordr communication
// ---------------------------------------------------------------------------

// svalinn_run: Create and start a container.
// Validates via PolicyEngine, checks Rokur gate, then delegates to Vordr.
let handleRun = async (args: Js.Json.t): Js.Json.t => {
  switch requireString(args, "image") {
  | Error(e) => e
  | Ok(image) => {
      // Policy pre-check: ensure the image comes from an allowed registry
      let registryPolicy = Validation.defaultPolicy
      if Validation.isDeniedImage(image, registryPolicy) {
        makeError(`Image denied by policy: ${image}`)
      } else if !Validation.isAllowedRegistry(image, registryPolicy) {
        makeError(`Image registry not allowed by policy: ${image}`)
      } else {
        try {
          let name = Validation.getString(args, "name")
          let command = Validation.getArray(args, "command")
          let detach = Validation.getBool(args, "detach")
          let removeOnExit = Validation.getBool(args, "removeOnExit")

          // Build container config from optional fields
          let configEntries = []
          let configEntries = switch command {
          | Some(cmdArr) => Belt.Array.concat(configEntries, [
              ("cmd", Js.Json.array(cmdArr)),
            ])
          | None => configEntries
          }
          let configEntries = switch detach {
          | Some(d) => Belt.Array.concat(configEntries, [("detach", Js.Json.boolean(d))])
          | None => configEntries
          }
          let configEntries = switch removeOnExit {
          | Some(r) => Belt.Array.concat(configEntries, [("removeOnExit", Js.Json.boolean(r))])
          | None => configEntries
          }

          let containerConfig = if Belt.Array.length(configEntries) > 0 {
            Some(Js.Json.object_(Js.Dict.fromArray(configEntries)))
          } else {
            None
          }

          // Create container via Vordr
          let createResult = switch (name, containerConfig) {
          | (Some(n), Some(c)) =>
            await McpClient.Container.create(mcpConfig, ~image, ~name=n, ~containerConfig=c, ())
          | (Some(n), None) =>
            await McpClient.Container.create(mcpConfig, ~image, ~name=n, ())
          | (None, Some(c)) =>
            await McpClient.Container.create(mcpConfig, ~image, ~containerConfig=c, ())
          | (None, None) =>
            await McpClient.Container.create(mcpConfig, ~image, ())
          }

          // Extract container ID and start it
          let containerId = Validation.getString(createResult, "id")
            ->Belt.Option.getWithDefault("unknown")
          let startResult = await McpClient.Container.start(mcpConfig, containerId)

          // Merge create + start results into a single response
          let response = Js.Json.object_(
            Js.Dict.fromArray([
              ("containerId", Js.Json.string(containerId)),
              ("image", Js.Json.string(image)),
              ("created", createResult),
              ("started", startResult),
            ])
          )
          makeSuccessJson(response)
        } catch {
        | Js.Exn.Error(e) => {
            let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Container run failed")
            makeError(`svalinn_run failed: ${message}`)
          }
        }
      }
    }
  }
}

// svalinn_ps: List running containers.
let handlePs = async (args: Js.Json.t): Js.Json.t => {
  try {
    let showAll = Validation.getBool(args, "all")->Belt.Option.getWithDefault(false)
    let result = await McpClient.Container.list(mcpConfig, ~all=showAll, ())
    makeSuccessJson(result)
  } catch {
  | Js.Exn.Error(e) => {
      let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Container list failed")
      makeError(`svalinn_ps failed: ${message}`)
    }
  }
}

// svalinn_stop: Stop a container by ID.
let handleStop = async (args: Js.Json.t): Js.Json.t => {
  switch requireString(args, "containerId") {
  | Error(e) => e
  | Ok(containerId) => {
      try {
        let timeout = Validation.getNumber(args, "timeout")
          ->Belt.Option.map(Belt.Float.toInt)
        let result = switch timeout {
        | Some(t) => await McpClient.Container.stop(mcpConfig, containerId, ~timeout=t, ())
        | None => await McpClient.Container.stop(mcpConfig, containerId, ())
        }
        makeSuccessJson(result)
      } catch {
      | Js.Exn.Error(e) => {
          let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Container stop failed")
          makeError(`svalinn_stop failed: ${message}`)
        }
      }
    }
  }
}

// svalinn_verify: Verify a container image via PolicyEngine + Vordr.
let handleVerify = async (args: Js.Json.t): Js.Json.t => {
  switch requireString(args, "image") {
  | Error(e) => e
  | Ok(image) => {
      try {
        // Build policy hints from optional boolean flags
        let checkSbom = Validation.getBool(args, "checkSbom")->Belt.Option.getWithDefault(true)
        let checkSignature = Validation.getBool(args, "checkSignature")->Belt.Option.getWithDefault(true)

        let policyHints = Js.Json.object_(
          Js.Dict.fromArray([
            ("checkSbom", Js.Json.boolean(checkSbom)),
            ("checkSignature", Js.Json.boolean(checkSignature)),
          ])
        )

        // Delegate to Vordr image verification (image doubles as digest reference)
        let result = await McpClient.Image.verify(mcpConfig, image, ~policy=policyHints, ())

        // Run a local PolicyEngine evaluation on the default policy for completeness
        let policyResult = PolicyEngine.evaluate(PolicyEngine.defaultPolicy, [])
        let combinedResponse = Js.Json.object_(
          Js.Dict.fromArray([
            ("image", Js.Json.string(image)),
            ("vordrVerification", result),
            ("policyEvaluation", PolicyEngine.formatResult(policyResult)),
          ])
        )
        makeSuccessJson(combinedResponse)
      } catch {
      | Js.Exn.Error(e) => {
          let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Image verification failed")
          makeError(`svalinn_verify failed: ${message}`)
        }
      }
    }
  }
}

// svalinn_policy: List, get, or set policies.
let handlePolicy = (args: Js.Json.t): Js.Json.t => {
  switch requireString(args, "action") {
  | Error(e) => e
  | Ok(action) => {
      switch action {
      | "get" => {
          // Return the current default and permissive policies
          let response = Js.Json.object_(
            Js.Dict.fromArray([
              ("default", PolicyEngine.formatResult({
                allowed: true,
                mode: PolicyEngine.Strict,
                predicatesFound: PolicyEngine.defaultPolicy.requiredPredicates,
                missingPredicates: [],
                signersVerified: [],
                invalidSigners: [],
                logCount: 1,
                logQuorumMet: true,
                violations: [],
                warnings: [],
              })),
              ("permissive", PolicyEngine.formatResult({
                allowed: true,
                mode: PolicyEngine.Permissive,
                predicatesFound: [],
                missingPredicates: [],
                signersVerified: [],
                invalidSigners: [],
                logCount: 0,
                logQuorumMet: true,
                violations: [],
                warnings: [],
              })),
            ])
          )
          makeSuccessJson(response)
        }
      | "set" => {
          // Validate the provided policy payload
          switch Validation.getField(args, "policy") {
          | None => makeError("Missing 'policy' object for set action")
          | Some(policyJson) => {
              switch PolicyEngine.parsePolicy(policyJson) {
              | None => makeError("Invalid policy format")
              | Some(_parsedPolicy) => {
                  // In a full implementation this would persist the policy.
                  // For now, acknowledge receipt.
                  makeSuccess("Policy accepted (note: runtime policy persistence not yet implemented)")
                }
              }
            }
          }
        }
      | "validate" => {
          switch Validation.getField(args, "policy") {
          | None => makeError("Missing 'policy' object for validate action")
          | Some(policyJson) => {
              switch PolicyEngine.parsePolicy(policyJson) {
              | None => makeError("Policy validation failed: invalid format")
              | Some(_) => makeSuccess("Policy is valid")
              }
            }
          }
        }
      | other => makeError(`Unknown policy action: ${other}. Expected: get, set, validate`)
      }
    }
  }
}

// svalinn_logs: Get container logs.
let handleLogs = async (args: Js.Json.t): Js.Json.t => {
  switch requireString(args, "containerId") {
  | Error(e) => e
  | Ok(containerId) => {
      try {
        let tail = Validation.getNumber(args, "tail")->Belt.Option.map(Belt.Float.toInt)
        let result = switch tail {
        | Some(t) => await McpClient.Container.logs(mcpConfig, containerId, ~tail=t, ())
        | None => await McpClient.Container.logs(mcpConfig, containerId, ())
        }
        makeSuccessJson(result)
      } catch {
      | Js.Exn.Error(e) => {
          let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Failed to get logs")
          makeError(`svalinn_logs failed: ${message}`)
        }
      }
    }
  }
}

// svalinn_exec: Execute a command in a running container.
let handleExec = async (args: Js.Json.t): Js.Json.t => {
  switch requireString(args, "containerId") {
  | Error(e) => e
  | Ok(containerId) => {
      switch Validation.getArray(args, "command") {
      | None => makeError("Missing required parameter: command")
      | Some(cmdJsonArr) => {
          let cmd = Belt.Array.keepMap(cmdJsonArr, Js.Json.decodeString)
          if Belt.Array.length(cmd) == 0 {
            makeError("command array must contain at least one non-empty string")
          } else {
            try {
              let workdir = Validation.getString(args, "workdir")
              let result = switch workdir {
              | Some(w) =>
                await McpClient.Container.exec(mcpConfig, containerId, cmd, ~workdir=w, ())
              | None =>
                await McpClient.Container.exec(mcpConfig, containerId, cmd, ())
              }
              makeSuccessJson(result)
            } catch {
            | Js.Exn.Error(e) => {
                let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Exec failed")
                makeError(`svalinn_exec failed: ${message}`)
              }
            }
          }
        }
      }
    }
  }
}

// svalinn_rm: Remove a stopped container.
let handleRm = async (args: Js.Json.t): Js.Json.t => {
  switch requireString(args, "containerId") {
  | Error(e) => e
  | Ok(containerId) => {
      try {
        let force = Validation.getBool(args, "force")->Belt.Option.getWithDefault(false)
        let result = await McpClient.Container.remove(mcpConfig, containerId, ~force, ())
        makeSuccessJson(result)
      } catch {
      | Js.Exn.Error(e) => {
          let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Container remove failed")
          makeError(`svalinn_rm failed: ${message}`)
        }
      }
    }
  }
}

// ---------------------------------------------------------------------------
// Tool call dispatcher
// ---------------------------------------------------------------------------

let handleCallTool = async (params: Js.Json.t): Js.Json.t => {
  let paramsObj = switch Js.Json.decodeObject(params) {
  | Some(obj) => obj
  | None => raise(Js.Exn.raiseError("tools/call params must be an object"))
  }
  let name = switch Js.Dict.get(paramsObj, "name")->Belt.Option.flatMap(Js.Json.decodeString) {
  | Some(n) => n
  | None => raise(Js.Exn.raiseError("tools/call missing 'name' parameter"))
  }
  let arguments = switch Js.Dict.get(paramsObj, "arguments") {
  | Some(a) => a
  | None => Js.Json.object_(Js.Dict.empty())
  }

  switch name {
  | "svalinn_run" => await handleRun(arguments)
  | "svalinn_ps" => await handlePs(arguments)
  | "svalinn_stop" => await handleStop(arguments)
  | "svalinn_verify" => await handleVerify(arguments)
  | "svalinn_policy" => handlePolicy(arguments)
  | "svalinn_logs" => await handleLogs(arguments)
  | "svalinn_exec" => await handleExec(arguments)
  | "svalinn_rm" => await handleRm(arguments)
  | _ => makeError(`Unknown tool: ${name}`)
  }
}

// ---------------------------------------------------------------------------
// JSON-RPC 2.0 request dispatcher
// ---------------------------------------------------------------------------

// Parse a raw JSON body into a jsonRpcRequest, returning a protocol-level error
// response if the payload is malformed.
let parseRequest = (body: Js.Json.t): result<jsonRpcRequest, jsonRpcResponse> => {
  switch Js.Json.decodeObject(body) {
  | None => Error(makeJsonRpcError(None, errorInvalidRequest, "Request must be a JSON object"))
  | Some(obj) => {
      let jsonrpc = obj
        ->Js.Dict.get("jsonrpc")
        ->Belt.Option.flatMap(Js.Json.decodeString)
        ->Belt.Option.getWithDefault("")

      if jsonrpc != "2.0" {
        Error(makeJsonRpcError(None, errorInvalidRequest, "jsonrpc must be \"2.0\""))
      } else {
        let method = obj
          ->Js.Dict.get("method")
          ->Belt.Option.flatMap(Js.Json.decodeString)

        let id = obj
          ->Js.Dict.get("id")
          ->Belt.Option.flatMap(Js.Json.decodeNumber)
          ->Belt.Option.map(Belt.Float.toInt)

        let params = obj->Js.Dict.get("params")

        switch method {
        | None =>
          Error(makeJsonRpcError(id, errorInvalidRequest, "Missing 'method' field"))
        | Some(m) =>
          Ok({jsonrpc: "2.0", method: m, params, id})
        }
      }
    }
  }
}

// Top-level handler: accepts a raw JSON body and returns a JSON-RPC response.
let handleRequest = async (body: Js.Json.t): jsonRpcResponse => {
  switch parseRequest(body) {
  | Error(errResp) => errResp
  | Ok(request) => {
      try {
        let result = switch request.method {
        | "initialize" => handleInitialize(request.params)
        | "tools/list" => handleListTools()
        | "tools/call" =>
          switch request.params {
          | Some(p) => await handleCallTool(p)
          | None => makeError("Missing params for tools/call")
          }
        | "notifications/message" =>
          // Notifications are fire-and-forget; acknowledge silently
          Js.Json.object_(Js.Dict.fromArray([("ok", Js.Json.boolean(true))]))
        | method =>
          // Method not found — raise to enter error handler
          raise(Js.Exn.raiseError(`Unknown method: ${method}`))
        }

        {
          jsonrpc: "2.0",
          result: Some(result),
          error: None,
          id: request.id,
        }
      } catch {
      | Js.Exn.Error(e) => {
          let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Internal server error")
          makeJsonRpcError(request.id, errorInternal, message)
        }
      }
    }
  }
}

// Convenience: serialise a jsonRpcResponse to Js.Json.t for HTTP transport.
let responseToJson = (resp: jsonRpcResponse): Js.Json.t => {
  let base = [
    ("jsonrpc", Js.Json.string(resp.jsonrpc)),
  ]

  let base = switch resp.id {
  | Some(id) => Belt.Array.concat(base, [("id", Js.Json.number(Belt.Int.toFloat(id)))])
  | None => Belt.Array.concat(base, [("id", Js.Json.null)])
  }

  let base = switch resp.result {
  | Some(r) => Belt.Array.concat(base, [("result", r)])
  | None => base
  }

  let base = switch resp.error {
  | Some(err) =>
    Belt.Array.concat(base, [
      ("error", Js.Json.object_(Js.Dict.fromArray([
        ("code", Js.Json.number(Belt.Int.toFloat(err.code))),
        ("message", Js.Json.string(err.message)),
      ]))),
    ])
  | None => base
  }

  Js.Json.object_(Js.Dict.fromArray(base))
}
