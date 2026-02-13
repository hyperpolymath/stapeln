// SPDX-License-Identifier: PMPL-1.0-or-later
// Vörðr MCP Adapter - Main entry point

open Server
open Protocol

// Bindings to Deno stdin/stdout for stdio transport
module Deno = {
  @module("@anthropic-ai/sdk") @scope("default")
  external stdin: 'a = "stdin"

  @val external console: 'a = "console"
}

// Parse JSON-RPC request
let parseRequest = (line: string): result<jsonRpcRequest, string> => {
  try {
    let json = JSON.parseExn(line)
    switch JSON.Decode.object(json) {
    | None => Error("Invalid JSON-RPC request: not an object")
    | Some(obj) =>
      let jsonrpc = switch Dict.get(obj, "jsonrpc") {
      | Some(v) => JSON.Decode.string(v)->Option.getOr("")
      | None => ""
      }

      if jsonrpc != "2.0" {
        Error("Invalid JSON-RPC version")
      } else {
        let method = switch Dict.get(obj, "method") {
        | Some(v) => JSON.Decode.string(v)
        | None => None
        }

        switch method {
        | None => Error("Missing method")
        | Some(m) =>
          Ok({
            jsonrpc,
            id: Dict.get(obj, "id"),
            method: m,
            params: Dict.get(obj, "params"),
          })
        }
      }
    }
  } catch {
  | _ => Error("JSON parse error")
  }
}

// Serialize response to JSON
let serializeResponse = (response: jsonRpcResponse): string => {
  let obj = Dict.make()
  Dict.set(obj, "jsonrpc", JSON.Encode.string(response.jsonrpc))

  switch response.id {
  | Some(id) => Dict.set(obj, "id", id)
  | None => ()
  }

  switch response.result {
  | Some(result) => Dict.set(obj, "result", result)
  | None => ()
  }

  switch response.error {
  | Some(err) =>
    let errObj = Dict.make()
    Dict.set(errObj, "code", JSON.Encode.int(err.code))
    Dict.set(errObj, "message", JSON.Encode.string(err.message))
    switch err.data {
    | Some(data) => Dict.set(errObj, "data", data)
    | None => ()
    }
    Dict.set(obj, "error", JSON.Encode.object(errObj))
  | None => ()
  }

  JSON.stringify(JSON.Encode.object(obj))
}

// Process a single line of input
let processLine = (line: string): string => {
  switch parseRequest(line) {
  | Ok(request) =>
    let response = handleRequest(request)
    serializeResponse(response)
  | Error(msg) =>
    let response = errorResponse(None, ErrorCode.parseError, msg, None)
    serializeResponse(response)
  }
}

// Export for use
let run = () => {
  Console.log("Vörðr MCP Adapter v0.1.0 ready")
  Console.log("Listening for JSON-RPC requests on stdin...")
}
