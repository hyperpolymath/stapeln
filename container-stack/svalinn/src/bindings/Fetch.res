// SPDX-License-Identifier: PMPL-1.0-or-later
// Fetch API bindings for ReScript

// Headers
module Headers = {
  type t

  @new external make: unit => t = "Headers"

  external fromObject: {..} => t = "%identity"

  @send external get: (t, string) => option<string> = "get"
  @send external set: (t, string, string) => unit = "set"
  @send external has: (t, string) => bool = "has"
  @send external delete: (t, string) => unit = "delete"
}

// Body
module Body = {
  type t

  external string: string => t = "%identity"
  external json: Js.Json.t => t = "%identity"
}

// Request
module Request = {
  type t

  @new external make: (string, {..}) => t = "Request"

  @get external method_: t => string = "method"
  @get external url: t => string = "url"
  @get external headers: t => Headers.t = "headers"

  @send external json: t => promise<Js.Json.t> = "json"
  @send external text: t => promise<string> = "text"
  @send external clone: t => t = "clone"
}

// Response
module Response = {
  type t

  @new external make: (string, {..}) => t = "Response"

  @scope("Response") @val
  external json_: (Js.Json.t, {..}) => t = "json"

  let json = (data: Js.Json.t, ~status: int=200, ()): t => {
    json_(data, {"status": status, "headers": {"Content-Type": "application/json"}})
  }

  @scope("Response") @val
  external error: unit => t = "error"

  @scope("Response") @val
  external redirect: (string, int) => t = "redirect"

  @get external ok: t => bool = "ok"
  @get external status: t => int = "status"
  @get external headers: t => Headers.t = "headers"

  @send external json: t => promise<Js.Json.t> = "json"
  @send external text: t => promise<string> = "text"
}

// Fetch options
type method_ = [#GET | #POST | #PUT | #DELETE | #PATCH | #HEAD | #OPTIONS]

type fetchOptions = {
  method: method_,
  headers: option<Headers.t>,
  body: option<Body.t>,
}

// Fetch function
@val external fetch: (string, {..}) => promise<Response.t> = "fetch"
