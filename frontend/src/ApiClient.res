// SPDX-License-Identifier: PMPL-1.0-or-later
// ApiClient.res - Centralized API client for stapeln backend
//
// Uses the existing WebAPI.fetch bindings from WebAPI.res for all HTTP
// communication with the stapeln backend API.

let baseUrl = "/api"

// ---------------------------------------------------------------------------
// Auth token management (localStorage)
// ---------------------------------------------------------------------------

let getToken = (): option<string> => {
  WebAPI.getItem("stapeln_auth_token")->Nullable.toOption
}

let setToken = (token: string): unit => {
  WebAPI.setItem("stapeln_auth_token", token)
}

let clearToken = (): unit => {
  WebAPI.removeItem("stapeln_auth_token")
}

// Build an authorization-aware headers dict.
let authHeaders = (): dict<string> => {
  let headers = Dict.fromArray([("content-type", "application/json")])
  switch getToken() {
  | Some(token) => Dict.set(headers, "authorization", "Bearer " ++ token)
  | None => ()
  }
  headers
}

// ---------------------------------------------------------------------------
// Generic helpers
// ---------------------------------------------------------------------------

// Perform a fetch returning the response text on success or an error string.
let fetchText = (url: string, init: WebAPI.fetchInit): promise<Result.t<string, string>> => {
  WebAPI.fetch(url, init)
  ->Promise.then(res => {
    if WebAPI.fetchOk(res) {
      WebAPI.text(res)->Promise.then(text => Promise.resolve(Ok(text)))
    } else {
      let status = WebAPI.fetchStatus(res)
      Promise.resolve(Error("HTTP " ++ Int.toString(status)))
    }
  })
  ->Promise.catch(exn => {
    let msg = switch exn {
    | JsExn(e) =>
      Belt.Option.getWithDefault(JsExn.message(e), "Network error")
    | _ => "Network error"
    }
    Promise.resolve(Error(msg))
  })
}

// Perform a fetch returning parsed JSON on success or an error string.
let fetchJson = (url: string, init: WebAPI.fetchInit): promise<Result.t<JSON.t, string>> => {
  WebAPI.fetch(url, init)
  ->Promise.then(res => {
    if WebAPI.fetchOk(res) {
      WebAPI.json(res)->Promise.then(json => Promise.resolve(Ok(json)))
    } else {
      let status = WebAPI.fetchStatus(res)
      Promise.resolve(Error("HTTP " ++ Int.toString(status)))
    }
  })
  ->Promise.catch(exn => {
    let msg = switch exn {
    | JsExn(e) =>
      Belt.Option.getWithDefault(JsExn.message(e), "Network error")
    | _ => "Network error"
    }
    Promise.resolve(Error(msg))
  })
}

// ---------------------------------------------------------------------------
// Stack operations
// ---------------------------------------------------------------------------

// Save a stack definition. `body` is the JSON-encoded string to POST.
// Returns the stack ID on success.
let saveStack = (body: string): promise<Result.t<string, string>> => {
  fetchText(
    baseUrl ++ "/stacks",
    {
      method: "POST",
      headers: authHeaders(),
      body,
    },
  )
}

// Load a stack definition by ID. Returns the raw JSON text on success.
let loadStack = (id: string): promise<Result.t<string, string>> => {
  fetchText(
    baseUrl ++ "/stacks/" ++ id,
    {
      method: "GET",
      headers: authHeaders(),
    },
  )
}

// ---------------------------------------------------------------------------
// Security scan
// ---------------------------------------------------------------------------

// Calls POST /api/stacks/:id/security-scan
// Returns SecurityInspector.state compatible JSON data.
let runSecurityScan = (stackId: int): promise<Result.t<JSON.t, string>> => {
  fetchJson(
    baseUrl ++ "/stacks/" ++ Int.toString(stackId) ++ "/security-scan",
    {
      method: "POST",
      headers: authHeaders(),
    },
  )
}

// ---------------------------------------------------------------------------
// Gap analysis
// ---------------------------------------------------------------------------

// Calls POST /api/stacks/:id/gap-analysis
// Returns GapAnalysis.state compatible JSON data.
let runGapAnalysis = (stackId: int): promise<Result.t<JSON.t, string>> => {
  fetchJson(
    baseUrl ++ "/stacks/" ++ Int.toString(stackId) ++ "/gap-analysis",
    {
      method: "POST",
      headers: authHeaders(),
    },
  )
}

// ---------------------------------------------------------------------------
// Settings
// ---------------------------------------------------------------------------

// Load settings from the backend.
let loadSettings = (): promise<Result.t<JSON.t, string>> => {
  fetchJson(
    baseUrl ++ "/settings",
    {
      method: "GET",
      headers: authHeaders(),
    },
  )
}

// Save settings to the backend.
let saveSettings = (settings: JSON.t): promise<Result.t<unit, string>> => {
  let body = JSON.stringify(settings)
  WebAPI.fetch(
    baseUrl ++ "/settings",
    {
      method: "PUT",
      headers: authHeaders(),
      body,
    },
  )
  ->Promise.then(res => {
    if WebAPI.fetchOk(res) {
      Promise.resolve(Ok())
    } else {
      let status = WebAPI.fetchStatus(res)
      Promise.resolve(Error("HTTP " ++ Int.toString(status)))
    }
  })
  ->Promise.catch(exn => {
    let msg = switch exn {
    | JsExn(e) =>
      Belt.Option.getWithDefault(JsExn.message(e), "Network error")
    | _ => "Network error"
    }
    Promise.resolve(Error(msg))
  })
}

// ---------------------------------------------------------------------------
// Firewall pinholes
// ---------------------------------------------------------------------------

// Create an ephemeral pinhole. Returns the created pinhole JSON on success.
let createPinhole = (
  ~source: string,
  ~destination: string,
  ~port: int,
  ~ttlSeconds: int,
  ~reason: string,
  ~protocol: string="tcp",
): promise<Result.t<JSON.t, string>> => {
  let body = JSON.stringify(
    JSON.Encode.object(
      Dict.fromArray([
        ("source", JSON.Encode.string(source)),
        ("destination", JSON.Encode.string(destination)),
        ("port", JSON.Encode.int(port)),
        ("ttl_seconds", JSON.Encode.int(ttlSeconds)),
        ("reason", JSON.Encode.string(reason)),
        ("protocol", JSON.Encode.string(protocol)),
      ]),
    ),
  )
  fetchJson(
    baseUrl ++ "/firewall/pinholes",
    {
      method: "POST",
      headers: authHeaders(),
      body,
    },
  )
}

// List all active pinholes.
let listPinholes = (): promise<Result.t<JSON.t, string>> => {
  fetchJson(
    baseUrl ++ "/firewall/pinholes",
    {
      method: "GET",
      headers: authHeaders(),
    },
  )
}

// Revoke a pinhole by ID.
let revokePinhole = (id: string): promise<Result.t<JSON.t, string>> => {
  fetchJson(
    baseUrl ++ "/firewall/pinholes/" ++ id,
    {
      method: "DELETE",
      headers: authHeaders(),
    },
  )
}

// Check if traffic is allowed by any active pinhole.
let checkFirewall = (
  ~source: string,
  ~destination: string,
  ~port: int,
): promise<Result.t<JSON.t, string>> => {
  let body = JSON.stringify(
    JSON.Encode.object(
      Dict.fromArray([
        ("source", JSON.Encode.string(source)),
        ("destination", JSON.Encode.string(destination)),
        ("port", JSON.Encode.int(port)),
      ]),
    ),
  )
  fetchJson(
    baseUrl ++ "/firewall/check",
    {
      method: "POST",
      headers: authHeaders(),
      body,
    },
  )
}

// ---------------------------------------------------------------------------
// Auth
// ---------------------------------------------------------------------------

// Login with email and password. Returns an auth token on success.
let login = (email: string, password: string): promise<Result.t<string, string>> => {
  let body =
    JSON.stringify(
      JSON.Encode.object(
        Dict.fromArray([
          ("email", JSON.Encode.string(email)),
          ("password", JSON.Encode.string(password)),
        ]),
      ),
    )
  fetchText(
    baseUrl ++ "/auth/login",
    {
      method: "POST",
      headers: Dict.fromArray([("content-type", "application/json")]),
      body,
    },
  )
}

// Register a new account. Returns an auth token on success.
let register = (email: string, password: string): promise<Result.t<string, string>> => {
  let body =
    JSON.stringify(
      JSON.Encode.object(
        Dict.fromArray([
          ("email", JSON.Encode.string(email)),
          ("password", JSON.Encode.string(password)),
        ]),
      ),
    )
  fetchText(
    baseUrl ++ "/auth/register",
    {
      method: "POST",
      headers: Dict.fromArray([("content-type", "application/json")]),
      body,
    },
  )
}
