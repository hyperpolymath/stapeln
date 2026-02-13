// SPDX-License-Identifier: PMPL-1.0-or-later
// MCP (Model Context Protocol) client for Vörðr integration

// MCP request/response types
type mcpRequest = {
  jsonrpc: string,
  method: string,
  params: Js.Json.t,
  id: float,
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
  id: float,
}

// Client configuration
type config = {
  endpoint: string,
  timeout: int, // milliseconds
  retries: int,
}

// Default configuration
let defaultConfig: config = {
  endpoint: "http://localhost:8080",
  timeout: 30000, // 30 seconds
  retries: 3,
}

// Create config from environment
@scope(("Deno", "env")) @val external getEnv: string => option<string> = "get"

let fromEnv = (): config => {
  {
    endpoint: getEnv("VORDR_ENDPOINT")->Belt.Option.getWithDefault(defaultConfig.endpoint),
    timeout: getEnv("VORDR_TIMEOUT")
      ->Belt.Option.flatMap(Belt.Int.fromString)
      ->Belt.Option.getWithDefault(defaultConfig.timeout),
    retries: getEnv("VORDR_RETRIES")
      ->Belt.Option.flatMap(Belt.Int.fromString)
      ->Belt.Option.getWithDefault(defaultConfig.retries),
  }
}

// Call MCP method with retries
let rec callWithRetry = async (
  config: config,
  method: string,
  params: Js.Json.t,
  attempt: int
): Js.Json.t => {
  let requestId = Js.Date.now()

  let request: mcpRequest = {
    jsonrpc: "2.0",
    method,
    params,
    id: requestId,
  }

  let requestBody = Js.Json.object_(
    Js.Dict.fromArray([
      ("jsonrpc", Js.Json.string(request.jsonrpc)),
      ("method", Js.Json.string(request.method)),
      ("params", request.params),
      ("id", Js.Json.number(request.id)),
    ])
  )

  try {
    let controller = %raw(`new AbortController()`)
    let timeoutId = %raw(`setTimeout(() => controller.abort(), config.timeout)`)

    let response = await Fetch.fetch(
      config.endpoint,
      {
        "method": "POST",
        "headers": {"Content-Type": "application/json"},
        "body": Js.Json.stringify(requestBody),
        "signal": %raw(`controller.signal`),
      }
    )

    %raw(`clearTimeout(timeoutId)`)

    if !Fetch.Response.ok(response) {
      let status = Fetch.Response.status(response)
      raise(Js.Exn.raiseError(`HTTP ${Belt.Int.toString(status)}`))
    }

    let json = await Fetch.Response.json(response)
    let obj = json->Js.Json.decodeObject->Belt.Option.getExn

    // Check for MCP error
    switch obj->Js.Dict.get("error") {
    | Some(errorJson) => {
        let errorObj = errorJson->Js.Json.decodeObject->Belt.Option.getExn
        let code = errorObj
          ->Js.Dict.get("code")
          ->Belt.Option.flatMap(Js.Json.decodeNumber)
          ->Belt.Option.map(Belt.Float.toInt)
          ->Belt.Option.getWithDefault(-1)
        let message = errorObj
          ->Js.Dict.get("message")
          ->Belt.Option.flatMap(Js.Json.decodeString)
          ->Belt.Option.getWithDefault("Unknown error")

        raise(Js.Exn.raiseError(`MCP error ${Belt.Int.toString(code)}: ${message}`))
      }
    | None => {
        // Return result
        obj->Js.Dict.get("result")->Belt.Option.getWithDefault(Js.Json.null)
      }
    }
  } catch {
  | Js.Exn.Error(e) if attempt < config.retries => {
      let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Unknown error")
      Js.Console.warn(`MCP call failed (attempt ${Belt.Int.toString(attempt + 1)}): ${message}`)

      // Exponential backoff: 100ms, 200ms, 400ms, etc.
      let delayMs = 100.0 *. Js.Math.pow_float(~base=2.0, ~exp=Belt.Int.toFloat(attempt))
      // Sleep using Promise and setTimeout
      let _ = await %raw(`(ms) => new Promise(resolve => setTimeout(resolve, ms))`)(delayMs)

      await callWithRetry(config, method, params, attempt + 1)
    }
  | Js.Exn.Error(e) => {
      let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Unknown error")
      raise(Js.Exn.raiseError(`MCP call failed after ${Belt.Int.toString(config.retries)} retries: ${message}`))
    }
  }
}

// Call MCP method
let call = async (config: config, method: string, params: Js.Json.t): Js.Json.t => {
  await callWithRetry(config, method, params, 0)
}

// Convenience methods for Vörðr operations

// Container operations
module Container = {
  // List containers
  let list = async (config: config, ~all: bool=false, ()): Js.Json.t => {
    let params = Js.Json.object_(Js.Dict.fromArray([("all", Js.Json.boolean(all))]))
    await call(config, "containers/list", params)
  }

  // Get container by ID
  let get = async (config: config, id: string): Js.Json.t => {
    let params = Js.Json.object_(Js.Dict.fromArray([("id", Js.Json.string(id))]))
    await call(config, "containers/get", params)
  }

  // Create container
  let create = async (
    config: config,
    ~image: string,
    ~name: option<string>=?,
    ~containerConfig: option<Js.Json.t>=?,
    ()
  ): Js.Json.t => {
    let paramsDict = [("image", Js.Json.string(image))]

    let paramsDict = switch name {
    | Some(n) => Belt.Array.concat(paramsDict, [("name", Js.Json.string(n))])
    | None => paramsDict
    }

    let paramsDict = switch containerConfig {
    | Some(c) => Belt.Array.concat(paramsDict, [("config", c)])
    | None => paramsDict
    }

    let params = Js.Json.object_(Js.Dict.fromArray(paramsDict))
    await call(config, "containers/create", params)
  }

  // Start container
  let start = async (config: config, id: string): Js.Json.t => {
    let params = Js.Json.object_(Js.Dict.fromArray([("id", Js.Json.string(id))]))
    await call(config, "containers/start", params)
  }

  // Stop container
  let stop = async (config: config, id: string, ~timeout: option<int>=?, ()): Js.Json.t => {
    let paramsDict = [("id", Js.Json.string(id))]

    let paramsDict = switch timeout {
    | Some(t) => Belt.Array.concat(paramsDict, [("timeout", Js.Json.number(Belt.Int.toFloat(t)))])
    | None => paramsDict
    }

    let params = Js.Json.object_(Js.Dict.fromArray(paramsDict))
    await call(config, "containers/stop", params)
  }

  // Remove container
  let remove = async (config: config, id: string, ~force: bool=false, ()): Js.Json.t => {
    let params = Js.Json.object_(
      Js.Dict.fromArray([("id", Js.Json.string(id)), ("force", Js.Json.boolean(force))])
    )
    await call(config, "containers/remove", params)
  }

  // Get container logs
  let logs = async (
    config: config,
    id: string,
    ~follow: bool=false,
    ~tail: option<int>=?,
    ()
  ): Js.Json.t => {
    let paramsDict = [("id", Js.Json.string(id)), ("follow", Js.Json.boolean(follow))]

    let paramsDict = switch tail {
    | Some(t) => Belt.Array.concat(paramsDict, [("tail", Js.Json.number(Belt.Int.toFloat(t)))])
    | None => paramsDict
    }

    let params = Js.Json.object_(Js.Dict.fromArray(paramsDict))
    await call(config, "containers/logs", params)
  }

  // Execute command in container
  let exec = async (
    config: config,
    id: string,
    cmd: array<string>,
    ~workdir: option<string>=?,
    ()
  ): Js.Json.t => {
    let paramsDict = [
      ("id", Js.Json.string(id)),
      ("cmd", Js.Json.array(Belt.Array.map(cmd, Js.Json.string))),
    ]

    let paramsDict = switch workdir {
    | Some(w) => Belt.Array.concat(paramsDict, [("workdir", Js.Json.string(w))])
    | None => paramsDict
    }

    let params = Js.Json.object_(Js.Dict.fromArray(paramsDict))
    await call(config, "containers/exec", params)
  }
}

// Image operations
module Image = {
  // List images
  let list = async (config: config): Js.Json.t => {
    let params = Js.Json.object_(Js.Dict.empty())
    await call(config, "images/list", params)
  }

  // Pull image
  let pull = async (config: config, image: string): Js.Json.t => {
    let params = Js.Json.object_(Js.Dict.fromArray([("image", Js.Json.string(image))]))
    await call(config, "images/pull", params)
  }

  // Remove image
  let remove = async (config: config, image: string, ~force: bool=false, ()): Js.Json.t => {
    let params = Js.Json.object_(
      Js.Dict.fromArray([("image", Js.Json.string(image)), ("force", Js.Json.boolean(force))])
    )
    await call(config, "images/remove", params)
  }

  // Verify image (using Cerro Torre)
  let verify = async (config: config, digest: string, ~policy: option<Js.Json.t>=?, ()): Js.Json.t => {
    let paramsDict = [("digest", Js.Json.string(digest))]

    let paramsDict = switch policy {
    | Some(p) => Belt.Array.concat(paramsDict, [("policy", p)])
    | None => paramsDict
    }

    let params = Js.Json.object_(Js.Dict.fromArray(paramsDict))
    await call(config, "images/verify", params)
  }
}

// Health check
let health = async (config: config): bool => {
  try {
    let _ = await call(config, "health", Js.Json.object_(Js.Dict.empty()))
    true
  } catch {
  | _ => false
  }
}

// Get Vörðr version info
let version = async (config: config): Js.Json.t => {
  await call(config, "version", Js.Json.object_(Js.Dict.empty()))
}
