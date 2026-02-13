// SPDX-License-Identifier: PMPL-1.0-or-later
// Vörðr LSP Server Main Entry Point

// Import protocol and transport
module Server = Protocol.Server
module StdioTransport = Transport.Stdio.StdioTransport

// Parse JSON-RPC request
let parseRequest = (json: Js.Json.t): option<Server.jsonRpcRequest> => {
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

// Encode JSON-RPC response
let encodeResponse = (response: Server.jsonRpcResponse): Js.Json.t => {
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

// Main LSP server
let main = () => {
  // Log startup to stderr
  let _ = StdioTransport.logError("Vörðr LSP Server v0.1.0 starting...")

  // Create server state
  let serverState = Server.make()

  // Handle incoming messages
  let onMessage = (content: string) => {
    try {
      // Parse JSON
      let json = Js.Json.parseExn(content)
      let request = parseRequest(json)

      switch request {
      | Some(req) => {
          // Handle request
          let response = Server.handleRequest(serverState, req)

          // Send response if not a notification
          switch response {
          | Some(resp) => {
              let responseJson = encodeResponse(resp)
              let responseText = Js.Json.stringify(responseJson)
              let _ = StdioTransport.write(responseText)
            }
          | None => () // Notification, no response needed
          }
        }
      | None => {
          let _ = StdioTransport.logError("Failed to parse request")
        }
      }
    } catch {
    | Js.Exn.Error(e) => {
        let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Unknown error")
        let _ = StdioTransport.logError(`Error processing message: ${message}`)
      }
    }
  }

  let onError = (message: string) => {
    let _ = StdioTransport.logError(message)
  }

  // Start transport
  StdioTransport.start(~onMessage, ~onError)

  let _ = StdioTransport.logError("Vörðr LSP Server ready")
}

// Run the server
main()
