// SPDX-License-Identifier: PMPL-1.0-or-later
// Stdio Transport for LSP (Language Server Protocol standard)

// Deno stdio bindings
module Deno = {
  module Stdin = {
    type t
    @scope("Deno") @val external stdin: t = "stdin"
    @send external read: (t, Js.TypedArray2.Uint8Array.t) => Js.Promise.t<option<int>> = "read"
  }

  module Stdout = {
    type t
    @scope("Deno") @val external stdout: t = "stdout"
    @send
    external write: (t, Js.TypedArray2.Uint8Array.t) => Js.Promise.t<int> = "write"
  }

  module Stderr = {
    type t
    @scope("Deno") @val external stderr: t = "stderr"
    @send
    external write: (t, Js.TypedArray2.Uint8Array.t) => Js.Promise.t<int> = "write"
  }
}

// Text encoder/decoder
@new external makeTextEncoder: unit => {..} = "TextEncoder"
@new external makeTextDecoder: unit => {..} = "TextDecoder"

let textEncoder = makeTextEncoder()
let textDecoder = makeTextDecoder()

// LSP message format: "Content-Length: N\r\n\r\n{JSON}"
type message = {
  contentLength: int,
  content: string,
}

// Parse LSP message from buffer
let parseMessage = (buffer: string): option<(message, string)> => {
  // Look for "\r\n\r\n" separator
  let separatorIndex = %raw(`buffer.indexOf("\r\n\r\n")`)

  if separatorIndex == -1 {
    None
  } else {
    // Parse headers
    let headerSection = %raw(`buffer.substring(0, separatorIndex)`)
    let contentLengthMatch = %raw(`headerSection.match(/Content-Length: (\d+)/i)`)

    switch contentLengthMatch {
    | Some(matches) => {
        let contentLength = %raw(`parseInt(matches[1], 10)`)
        let messageStart = separatorIndex + 4 // Skip "\r\n\r\n"
        let messageEnd = messageStart + contentLength

        if %raw(`buffer.length`) >= messageEnd {
          let content = %raw(`buffer.substring(messageStart, messageEnd)`)
          let remaining = %raw(`buffer.substring(messageEnd)`)
          Some({contentLength, content}, remaining)
        } else {
          None // Not enough data yet
        }
      }
    | None => None
    }
  }
}

// Format message for LSP
let formatMessage = (content: string): string => {
  let contentLength = %raw(`new TextEncoder().encode(content).length`)
  `Content-Length: ${Belt.Int.toString(contentLength)}\r\n\r\n${content}`
}

// Stdio transport state
type state = {
  mutable buffer: string,
  onMessage: string => unit,
  onError: string => unit,
}

// Read from stdin in a loop
let rec readLoop = async (state: state): unit => {
  let chunk = Js.TypedArray2.Uint8Array.fromLength(8192)

  try {
    let bytesRead = await Deno.Stdin.read(Deno.Stdin.stdin, chunk)

    switch bytesRead {
    | Some(n) if n > 0 => {
        // Decode chunk
        let decoded = %raw(`textDecoder.decode(chunk.subarray(0, n))`)
        state.buffer = state.buffer ++ decoded

        // Try to parse messages
        let rec processBuffer = () => {
          switch parseMessage(state.buffer) {
          | Some((message, remaining)) => {
              state.buffer = remaining
              state.onMessage(message.content)
              processBuffer() // Process next message if any
            }
          | None => () // Wait for more data
          }
        }
        processBuffer()

        // Continue reading
        await readLoop(state)
      }
    | _ => () // EOF or error, stop reading
    }
  } catch {
  | Js.Exn.Error(e) => {
      let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Unknown error")
      state.onError(`Read error: ${message}`)
    }
  }
}

// Write to stdout
let write = async (content: string): unit => {
  try {
    let formatted = formatMessage(content)
    let encoded = %raw(`textEncoder.encode(formatted)`)
    let _ = await Deno.Stdout.write(Deno.Stdout.stdout, encoded)
  } catch {
  | Js.Exn.Error(e) => {
      let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Unknown error")
      // Log to stderr
      let errorMsg = `Write error: ${message}\n`
      let encoded = %raw(`textEncoder.encode(errorMsg)`)
      let _ = await Deno.Stderr.write(Deno.Stderr.stderr, encoded)
    }
  }
}

// Log to stderr (LSP diagnostic channel)
let logError = async (message: string): unit => {
  try {
    let formatted = `[LSP Error] ${message}\n`
    let encoded = %raw(`textEncoder.encode(formatted)`)
    let _ = await Deno.Stderr.write(Deno.Stderr.stderr, encoded)
  } catch {
  | _ => () // Can't do anything if stderr fails
  }
}

// Start stdio transport
let start = (~onMessage: string => unit, ~onError: string => unit): unit => {
  let state = {
    buffer: "",
    onMessage,
    onError,
  }

  // Start reading from stdin
  let _ = readLoop(state)
}
