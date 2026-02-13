// SPDX-License-Identifier: PMPL-1.0-or-later
// Vörðr MCP Adapter - HTTP transport entry point
// Similar to how LSP servers support multiple transports

open Server
open Protocol

// HTTP server configuration
let port = 8080
let host = "0.0.0.0"

// CORS headers for cross-origin requests
let corsHeaders = {
    // SECURITY FIX: Replaced CORS wildcard with environment-based origin
  "Access-Control-Allow-Origin": Node.process->Node.Process.env->Js.Dict.get("ALLOWED_ORIGINS")->Belt.Option.getWithDefault("http://localhost:3000"),
  "Access-Control-Allow-Methods": "POST, OPTIONS",
  "Access-Control-Allow-Headers": "Content-Type",
  "Content-Type": "application/json",
}

// Handle HTTP request
let handleHttp = async (request: 'request): 'response => {
  let method = %raw(`request.method`)

  // Handle CORS preflight
  if method === "OPTIONS" {
    %raw(`new Response("", { status: 200, headers: corsHeaders })`)
  } else if method === "POST" {
    try {
      // Read JSON-RPC request body
      let body = await %raw(`request.text()`)
      let json = JSON.parseExn(body)

      // Parse JSON-RPC request
      let mcpRequest = switch JSON.Decode.object(json) {
      | Some(obj) => {
          jsonrpc: switch Dict.get(obj, "jsonrpc") {
          | Some(v) => JSON.Decode.string(v)->Belt.Option.getWithDefault("2.0")
          | None => "2.0"
          },
          id: Dict.get(obj, "id"),
          method: switch Dict.get(obj, "method") {
          | Some(v) => JSON.Decode.string(v)->Belt.Option.getWithDefault("")
          | None => ""
          },
          params: Dict.get(obj, "params"),
        }
      | None => {
          jsonrpc: "2.0",
          id: None,
          method: "",
          params: None,
        }
      }

      // Use Server.res protocol handler (shared with stdio transport)
      let mcpResponse = handleRequest(mcpRequest)

      // Serialize JSON-RPC response
      let responseObj = Dict.make()
      Dict.set(responseObj, "jsonrpc", JSON.Encode.string(mcpResponse.jsonrpc))

      switch mcpResponse.id {
      | Some(id) => Dict.set(responseObj, "id", id)
      | None => ()
      }

      switch mcpResponse.result {
      | Some(result) => Dict.set(responseObj, "result", result)
      | None => ()
      }

      switch mcpResponse.error {
      | Some(err) => {
          let errObj = Dict.make()
          Dict.set(errObj, "code", JSON.Encode.int(err.code))
          Dict.set(errObj, "message", JSON.Encode.string(err.message))
          switch err.data {
          | Some(data) => Dict.set(errObj, "data", data)
          | None => ()
          }
          Dict.set(responseObj, "error", JSON.Encode.object(errObj))
        }
      | None => ()
      }

      let responseJson = JSON.stringify(JSON.Encode.object(responseObj))
      %raw(`new Response(responseJson, { status: 200, headers: corsHeaders })`)

    } catch {
    | Js.Exn.Error(e) => {
        let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Parse error")
        let errorObj = Dict.make()
        Dict.set(errorObj, "jsonrpc", JSON.Encode.string("2.0"))
        Dict.set(errorObj, "id", JSON.Encode.null)

        let err = Dict.make()
        Dict.set(err, "code", JSON.Encode.int(-32700))
        Dict.set(err, "message", JSON.Encode.string(message))
        Dict.set(errorObj, "error", JSON.Encode.object(err))

        let errorJson = JSON.stringify(JSON.Encode.object(errorObj))
        %raw(`new Response(errorJson, { status: 500, headers: corsHeaders })`)
      }
    }
  } else {
    %raw(`new Response("Method not allowed", { status: 405, headers: corsHeaders })`)
  }
}

// Start HTTP server (Deno.serve)
let start = () => {
  Console.log("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  Console.log("  Vörðr MCP Adapter v0.1.0")
  Console.log("  Transport: HTTP (LSP-style)")
  Console.log("  Listening: http://" ++ host ++ ":" ++ Belt.Int.toString(port))
  Console.log("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")

  %raw(`Deno.serve({ port: port, hostname: host }, handleHttp)`)
}

// Start server
start()
