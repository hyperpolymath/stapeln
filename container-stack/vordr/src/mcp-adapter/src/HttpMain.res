// SPDX-License-Identifier: PMPL-1.0-or-later
// Vörðr MCP Adapter - Deno HTTP transport

open Types
open Server

// Deno bindings
module Deno = {
  type serveOptions = {
    port: int,
    hostname: string,
    handler: (request) => Js.Promise.t<response>,
  }
  type request
  type response
  @scope("Deno") @val external serve: (serveOptions) => unit = "serve"
  @new external makeResponse: (string, {"headers": Js.Dict.t<string>}) => response = "Response"
  @get external getMethod: request => string = "method"
  @send external json: request => Js.Promise.t<Js.Json.t> = "json"
}

let port = 8000
let host = "0.0.0.0"

let corsHeaders = Js.Dict.fromArray([
  ("Access-Control-Allow-Origin", "*"),
  ("Access-Control-Allow-Methods", "POST, OPTIONS"),
  ("Access-Control-Allow-Headers", "Content-Type"),
  ("Content-Type", "application/json"),
])

let handler = (req: Deno.request): Js.Promise.t<Deno.response> => {
  if Deno.getMethod(req) == "OPTIONS" {
    Js.Promise.resolve(Deno.makeResponse("", {"headers": corsHeaders}))
  } else if Deno.getMethod(req) == "POST" {
    Deno.json(req)
    ->Js.Promise.then_(json => {
      // Re-use logic from parseRequest in Main or Protocol
      let requestObj = Js.Json.decodeObject(json)
      let response = switch requestObj {
      | Some(obj) => {
          let jsonrpc = obj->Js.Dict.get("jsonrpc")->Belt.Option.flatMap(Js.Json.decodeString)->Belt.Option.getWithDefault("2.0")
          let method = obj->Js.Dict.get("method")->Belt.Option.flatMap(Js.Json.decodeString)->Belt.Option.getWithDefault("")
          let params = obj->Js.Dict.get("params")
          let id = obj->Js.Dict.get("id")
          handleRequest({jsonrpc, method, params, id})
        }
      | None => {
          jsonrpc: "2.0",
          id: None,
          result: None,
          error: Some({code: -32700, message: "Parse error", data: None})
        }
      }
      
      let responseText = Js.Json.stringify(Protocol.encodeResponse(response))
      Js.Promise.resolve(Deno.makeResponse(responseText, {"headers": corsHeaders}))
    }, _)
  } else {
    Js.Promise.resolve(Deno.makeResponse("Method not allowed", {"headers": corsHeaders}))
  }
}

Js.Console.log(`Vörðr HTTP Server listening on http://[::]:${Belt.Int.toString(port)}`)
Deno.serve({port, hostname: host, handler})


