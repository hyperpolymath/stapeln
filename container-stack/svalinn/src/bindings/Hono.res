// SPDX-License-Identifier: PMPL-1.0-or-later
// Hono HTTP framework bindings for ReScript

// Context variable map type
type contextVariableMap

// Request wrapper
module Request = {
  type t

  @get external method_: t => string = "method"
  @get external url: t => string = "url"
  @get external headers: t => Fetch.Headers.t = "headers"

  @send external header: (t, string) => option<string> = "header"
  @send external query: (t, string) => option<string> = "query"
  @send external param: (t, string) => option<string> = "param"

  @send external json: t => promise<Js.Json.t> = "json"
  @send external text: t => promise<string> = "text"
}

// Response wrapper
module Response = {
  type t

  @get external status: t => int = "status"
  @get external headers: t => Fetch.Headers.t = "headers"
  @get external ok: t => bool = "ok"

  @send external json: t => promise<Js.Json.t> = "json"
  @send external text: t => promise<string> = "text"
}

// Hono context
module Context = {
  type t<'env, 'path>

  // Request access
  @get external req: t<'env, 'path> => Request.t = "req"

  // Response helpers
  @send external json: (t<'env, 'path>, Js.Json.t, ~status: int=?, unit) => Response.t = "json"
  @send external text: (t<'env, 'path>, string, ~status: int=?, unit) => Response.t = "text"
  @send external html: (t<'env, 'path>, string, ~status: int=?, unit) => Response.t = "html"

  // Variable storage (for user context, auth result, etc.)
  @send external set: (t<'env, 'path>, string, 'value) => unit = "set"
  @send external get: (t<'env, 'path>, string) => option<'value> = "get"

  // Headers
  @send external header: (t<'env, 'path>, string, string) => unit = "header"

  // Status
  @send external status: (t<'env, 'path>, int) => t<'env, 'path> = "status"
}

// Middleware next function
type next = unit => promise<unit>

// Handler function types
type handler<'env, 'path> = Context.t<'env, 'path> => promise<Response.t>
type middleware<'env, 'path> = (Context.t<'env, 'path>, next) => promise<unit>

// Hono app
type t<'env>

// Constructor
@module("hono") @new
external make: unit => t<'env> = "Hono"

// Routing
@send external get: (t<'env>, string, handler<'env, 'path>) => t<'env> = "get"
@send external post: (t<'env>, string, handler<'env, 'path>) => t<'env> = "post"
@send external put: (t<'env>, string, handler<'env, 'path>) => t<'env> = "put"
@send external delete: (t<'env>, string, handler<'env, 'path>) => t<'env> = "delete"
@send external patch: (t<'env>, string, handler<'env, 'path>) => t<'env> = "patch"
@send external head: (t<'env>, string, handler<'env, 'path>) => t<'env> = "head"
@send external options: (t<'env>, string, handler<'env, 'path>) => t<'env> = "options"
@send external all: (t<'env>, string, handler<'env, 'path>) => t<'env> = "all"

// Middleware registration
@send external use: (t<'env>, middleware<'env, 'path>) => t<'env> = "use"
@send external useWithPath: (t<'env>, string, middleware<'env, 'path>) => t<'env> = "use"

// Server
@send
external serve: (t<'env>, {..}) => {..} = "serve"

// Export for Deno
@send
external fetch: (t<'env>, Fetch.Request.t, 'env) => promise<Fetch.Response.t> = "fetch"
