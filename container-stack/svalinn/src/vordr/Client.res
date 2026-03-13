// SPDX-License-Identifier: PMPL-1.0-or-later
// Vörðr MCP client for Svalinn

open VordrTypes

// Client configuration
type clientConfig = {
  endpoint: string,
  timeout: int,
}

// Client instance
type t = {
  config: clientConfig,
  mutable requestId: int,
}

// Create client
let make = (config: clientConfig): t => {
  {config, requestId: 0}
}

// Default client configuration
let defaultConfig: clientConfig = {
  endpoint: "http://localhost:8080",
  timeout: 30000,
}

// Create from environment
let fromEnv = (): t => {
  let endpoint = switch Js.Dict.get(%raw(`Deno.env.toObject()`), "VORDR_ENDPOINT") {
  | Some(e) => e
  | None => defaultConfig.endpoint
  }
  make({...defaultConfig, endpoint})
}

// Generate next request ID
let nextId = (client: t): int => {
  client.requestId = client.requestId + 1
  client.requestId
}

// Make MCP request
let callTool = async (client: t, toolName: string, args: Js.Json.t): Js.Json.t => {
  // Build params object safely
  let paramsDict = Js.Dict.empty()
  Js.Dict.set(paramsDict, "name", Js.Json.string(toolName))
  Js.Dict.set(paramsDict, "arguments", args)

  let requestId = nextId(client)
  let requestBody = Js.Json.object_(Js.Dict.fromArray([
    ("jsonrpc", Js.Json.string("2.0")),
    ("method", Js.Json.string("tools/call")),
    ("params", Js.Json.object_(paramsDict)),
    ("id", Js.Json.number(Belt.Int.toFloat(requestId))),
  ]))

  // Make HTTP request to Vörðr
  let response = await Fetch.fetch(
    client.config.endpoint,
    {
      "method": "POST",
      "headers": {"Content-Type": "application/json"},
      "body": Js.Json.stringify(requestBody),
    },
  )

  let json = await Fetch.Response.json(response)
  // Validate response structure before casting
  let mcpResp: mcpResponse = switch json->Js.Json.decodeObject {
  | Some(obj) => {
      // Check for required jsonrpc field
      let jsonrpc = obj->Js.Dict.get("jsonrpc")->Belt.Option.flatMap(Js.Json.decodeString)
      let id = obj->Js.Dict.get("id")->Belt.Option.flatMap(Js.Json.decodeNumber)
      let result = obj->Js.Dict.get("result")
      let error = obj->Js.Dict.get("error")->Belt.Option.flatMap(Js.Json.decodeObject)->Belt.Option.map(errObj => {
        code: errObj->Js.Dict.get("code")->Belt.Option.flatMap(Js.Json.decodeNumber)->Belt.Option.map(Belt.Float.toInt)->Belt.Option.getWithDefault(-1),
        message: errObj->Js.Dict.get("message")->Belt.Option.flatMap(Js.Json.decodeString)->Belt.Option.getWithDefault("Unknown error"),
        data: errObj->Js.Dict.get("data"),
      })

      {
        jsonrpc: jsonrpc->Belt.Option.getWithDefault("2.0"),
        id: id->Belt.Option.map(Belt.Float.toInt)->Belt.Option.getWithDefault(0),
        result,
        error,
      }
    }
  | None => raise(Js.Exn.raiseError("Invalid MCP response: expected JSON object"))
  }

  switch mcpResp.error {
  | Some(err) => Js.Exn.raiseError(err.message)
  | None =>
    switch mcpResp.result {
    | Some(r) => r
    | None => Js.Json.null
    }
  }
}

// Ping Vörðr to check connectivity
let ping = async (client: t): bool => {
  try {
    let _ = await Fetch.fetch(
      `${client.config.endpoint}/health`,
      %raw(`{method: "GET"}`),
    )
    true
  } catch {
  | _ => false
  }
}

// Container operations
let listContainers = async (_client: t): array<Gateway.Types.containerInfo> => {
  // Vörðr doesn't have a list tool, we'd need to track locally
  // For now return empty
  []
}

let listImages = async (_client: t): array<Gateway.Types.imageInfo> => {
  // Same as above - would need local tracking
  []
}

let runContainer = async (
  client: t,
  request: Gateway.Types.runRequest,
): Gateway.Types.containerInfo => {
  // First create
  let nameJson = switch request.name {
  | Some(n) => Js.Json.string(n)
  | None => Js.Json.null
  }
  let createArgs = Js.Json.object_(Js.Dict.fromArray([
    ("image", Js.Json.string(request.imageName)),
    ("name", nameJson),
    ("config", Js.Json.object_(Js.Dict.fromArray([
      ("privileged", Js.Json.boolean(false)),
      ("readOnlyRoot", Js.Json.boolean(true)),
    ]))),
  ]))
  let createResult = await callTool(client, toolContainerCreate, createArgs)

  // Then start
  let containerId = switch Js.Json.decodeObject(createResult) {
  | Some(obj) => switch Js.Dict.get(obj, "containerId") {
    | Some(v) => switch Js.Json.decodeString(v) {
      | Some(s) => s
      | None => raise(Js.Exn.raiseError("containerId is not a string"))
      }
    | None => raise(Js.Exn.raiseError("Response missing containerId"))
    }
  | None => raise(Js.Exn.raiseError("Invalid response format"))
  }
  let _ = await callTool(client, toolContainerStart, Js.Json.object_(Js.Dict.fromArray([("containerId", Js.Json.string(containerId))])))

  {
    id: containerId,
    name: request.name->Belt.Option.getWithDefault(containerId),
    image: request.imageName,
    imageDigest: request.imageDigest,
    state: Gateway.Types.Running,
    policyVerdict: "allowed",
    createdAt: Some(Js.Date.now()->Belt.Float.toString),
    startedAt: Some(Js.Date.now()->Belt.Float.toString),
  }
}

let verifyImage = async (
  client: t,
  imageRef: string,
  _digest: string,
): Gateway.Types.verificationResult => {
  let args = Js.Json.object_(Js.Dict.fromArray([
    ("image", Js.Json.string(imageRef)),
    ("checkSbom", Js.Json.boolean(true)),
    ("checkSignature", Js.Json.boolean(true)),
  ]))
  let result = await callTool(client, toolVerifyImage, args)
  // NOTE: MCP JSON-RPC result cast — future work: decode with pattern matching
  Obj.magic(result)
}

let stopContainer = async (client: t, containerId: string): unit => {
  let _ = await callTool(client, toolContainerStop, Js.Json.object_(Js.Dict.fromArray([("containerId", Js.Json.string(containerId))])))
}

let removeContainer = async (client: t, containerId: string): unit => {
  let _ = await callTool(client, toolContainerRemove, Js.Json.object_(Js.Dict.fromArray([("containerId", Js.Json.string(containerId))])))
}

let inspectContainer = async (_client: t, containerId: string): Gateway.Types.containerInfo => {
  // Placeholder - would call Vörðr's inspect if available
  {
    id: containerId,
    name: containerId,
    image: "unknown",
    imageDigest: "",
    state: Gateway.Types.Running,
    policyVerdict: "unknown",
    createdAt: None,
    startedAt: None,
  }
}

// Authorization operations
let requestAuthorization = async (
  client: t,
  operation: string,
  threshold: int,
  signers: int,
): Js.Json.t => {
  let args = Js.Json.object_(Js.Dict.fromArray([
    ("operation", Js.Json.string(operation)),
    ("threshold", Js.Json.number(Belt.Int.toFloat(threshold))),
    ("signers", Js.Json.number(Belt.Int.toFloat(signers))),
  ]))
  await callTool(client, toolRequestAuth, args)
}

let submitSignature = async (
  client: t,
  share: signatureShare,
): Js.Json.t => {
  let args = Js.Json.object_(Js.Dict.fromArray([
    ("requestId", Js.Json.string(share.requestId)),
    ("signature", Js.Json.string(share.signature)),
    ("signerId", Js.Json.string(share.signerId)),
  ]))
  await callTool(client, toolSubmitSignature, args)
}

// Monitoring operations
let startMonitor = async (client: t, config: monitorConfig): Js.Json.t => {
  let args = Js.Json.object_(Js.Dict.fromArray([
    ("containerId", Js.Json.string(config.containerId)),
    ("syscalls", Js.Json.boolean(config.syscalls)),
    ("network", Js.Json.boolean(config.network)),
    ("filesystem", Js.Json.boolean(config.filesystem)),
  ]))
  await callTool(client, toolMonitorStart, args)
}

let stopMonitor = async (client: t, containerId: string): Js.Json.t => {
  await callTool(client, toolMonitorStop, Js.Json.object_(Js.Dict.fromArray([("containerId", Js.Json.string(containerId))])))
}

let getAnomalies = async (
  client: t,
  containerId: string,
  severity: string,
): Js.Json.t => {
  let args = Js.Json.object_(Js.Dict.fromArray([
    ("containerId", Js.Json.string(containerId)),
    ("severity", Js.Json.string(severity)),
  ]))
  await callTool(client, toolGetAnomalies, args)
}

// Reversibility operations
let rollback = async (client: t, containerId: string, steps: int): Js.Json.t => {
  let args = Js.Json.object_(Js.Dict.fromArray([
    ("containerId", Js.Json.string(containerId)),
    ("steps", Js.Json.number(Belt.Int.toFloat(steps))),
  ]))
  await callTool(client, toolRollback, args)
}

let previewRollback = async (client: t, containerId: string): Js.Json.t => {
  let args = Js.Json.object_(Js.Dict.fromArray([("containerId", Js.Json.string(containerId))]))
  await callTool(client, toolPreviewRollback, args)
}

// Export default client instance
let client = fromEnv()
