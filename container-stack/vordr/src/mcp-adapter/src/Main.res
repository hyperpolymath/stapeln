// SPDX-License-Identifier: PMPL-1.0-or-later
// Vörðr MCP Adapter - Stdio transport

open Types
open Protocol
open Server

// Stdio transport implementation
let parseRequest = (line: string): result<jsonRpcRequest, string> => {
  try {
    let json = Js.Json.parseExn(line)
    switch Js.Json.decodeObject(json) {
    | None => Error("Invalid JSON-RPC request: not an object")
    | Some(obj) => {
        let jsonrpc = obj->Js.Dict.get("jsonrpc")->Belt.Option.flatMap(Js.Json.decodeString)->Belt.Option.getWithDefault("2.0")
        let method = obj->Js.Dict.get("method")->Belt.Option.flatMap(Js.Json.decodeString)->Belt.Option.getWithDefault("")
        let params = obj->Js.Dict.get("params")
        let id = obj->Js.Dict.get("id")
        Ok({jsonrpc, method, params, id})
      }
    }
  } catch {
  | _ => Error("Failed to parse JSON")
  }
}

let main = () => {
  Js.Console.log("Vörðr MCP Adapter starting (Stdio transport)")
  // Implementation for stdio loop would go here
}

main()
