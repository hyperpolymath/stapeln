// SPDX-License-Identifier: PMPL-1.0-or-later
// Deno runtime bindings for ReScript

// Environment
module Env = {
  @scope(("Deno", "env")) @val
  external get: string => option<string> = "get"

  @scope(("Deno", "env")) @val
  external set: (string, string) => unit = "set"

  @scope(("Deno", "env")) @val
  external toObject: unit => Js.Dict.t<string> = "toObject"
}

// File system
module Fs = {
  @scope("Deno") @val
  external readTextFile: string => promise<string> = "readTextFile"

  @scope("Deno") @val
  external writeTextFile: (string, string) => promise<unit> = "writeTextFile"

  @scope("Deno") @val
  external remove: string => promise<unit> = "remove"

  @scope("Deno") @val
  external mkdir: (string, {..}) => promise<unit> = "mkdir"

  type fileInfo = {
    isFile: bool,
    isDirectory: bool,
    size: int,
  }

  @scope("Deno") @val
  external stat: string => promise<fileInfo> = "stat"
}

// HTTP server
module Http = {
  type conn<'a> = {
    remoteAddr: Js.t<'a>,
  }

  type request = {
    method: string,
    url: string,
    headers: Fetch.Headers.t,
    body: option<Fetch.Body.t>,
  }

  type serveOptions<'signal> = {
    port: int,
    hostname: option<string>,
    signal: option<'signal>,
  }

  @scope("Deno") @val
  external serve: (
    (Fetch.Request.t) => promise<Fetch.Response.t>,
    serveOptions<'a>,
  ) => {..} = "serve"
}

// Standard I/O
module Io = {
  @scope("Deno") @val
  external stdin: {..} = "stdin"

  @scope("Deno") @val
  external stdout: {..} = "stdout"

  @scope("Deno") @val
  external stderr: {..} = "stderr"
}

// Process
@scope("Deno") @val
external exit: int => unit = "exit"

@scope("Deno") @val
external args: array<string> = "args"

// AbortController binding
module AbortController = {
  type t
  type signal

  @new external make: unit => t = "AbortController"

  @get external signal: t => signal = "signal"
  @send external abort: t => unit = "abort"
}
