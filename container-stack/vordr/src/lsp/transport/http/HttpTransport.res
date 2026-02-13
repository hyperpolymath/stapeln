// SPDX-License-Identifier: PMPL-1.0-or-later
// HTTP Transport for LSP (for web-based clients and Svalinn)

// Deno HTTP bindings
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
  @send external text: request => Js.Promise.t<string> = "text"
}

// CORS headers
let corsHeaders = Js.Dict.fromArray([
    // SECURITY FIX: Replaced CORS wildcard with environment-based origin
  ("Access-Control-Allow-Origin", Node.process->Node.Process.env->Js.Dict.get("ALLOWED_ORIGINS")->Belt.Option.getWithDefault("http://localhost:3000")),
  ("Access-Control-Allow-Methods", "POST, OPTIONS"),
  ("Access-Control-Allow-Headers", "Content-Type"),
  ("Content-Type", "application/json"),
])

// Start HTTP transport
let start = (~port: int, ~host: string, ~onMessage: string => Js.Promise.t<option<string>>): unit => {
  Js.Console.log("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  Js.Console.log("  Vörðr LSP Server v0.1.0")
  Js.Console.log("  Transport: HTTP")
  Js.Console.log(`  Listening: http://${host}:${Belt.Int.toString(port)}`)
  Js.Console.log("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")

  let handler = (req: Deno.request): Js.Promise.t<Deno.response> => {
    if Deno.getMethod(req) == "OPTIONS" {
      Js.Promise.resolve(Deno.makeResponse("", {"headers": corsHeaders}))
    } else if Deno.getMethod(req) == "POST" {
      Deno.text(req)
      ->Js.Promise.then_(content => {
        onMessage(content)
        ->Js.Promise.then_(responseOpt => {
          let responseText = responseOpt->Belt.Option.getWithDefault("{}")
          Js.Promise.resolve(Deno.makeResponse(responseText, {"headers": corsHeaders}))
        }, _)
      }, _)
      ->Js.Promise.catch(_err => {
        let errorResponse = `{"jsonrpc":"2.0","error":{"code":-32603,"message":"Internal error"}}`
        Js.Promise.resolve(Deno.makeResponse(errorResponse, {"headers": corsHeaders}))
      }, _)
    } else {
      Js.Promise.resolve(Deno.makeResponse("Method not allowed", {"headers": corsHeaders}))
    }
  }

  Deno.serve(handler, {port, hostname: host})
}
