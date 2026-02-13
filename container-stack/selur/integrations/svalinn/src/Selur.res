// SPDX-License-Identifier: PMPL-1.0-or-later
// ReScript bindings for selur WASM bridge
// Type-safe zero-copy IPC for Svalinn <-> Vörðr communication

// Command codes for container operations
module Command = {
  type t =
    | CreateContainer
    | StartContainer
    | StopContainer
    | InspectContainer
    | DeleteContainer
    | ListContainers

  let toU8 = (cmd: t): int =>
    switch cmd {
    | CreateContainer => 0x01
    | StartContainer => 0x02
    | StopContainer => 0x03
    | InspectContainer => 0x04
    | DeleteContainer => 0x05
    | ListContainers => 0x06
    }
}

// Error codes from selur responses
module ErrorCode = {
  type t =
    | Success
    | InvalidRequest
    | ContainerNotFound
    | ContainerAlreadyExists
    | NetworkError
    | InternalError
    | Unknown(int)

  let fromU8 = (code: int): t =>
    switch code {
    | 0x00 => Success
    | 0x01 => InvalidRequest
    | 0x02 => ContainerNotFound
    | 0x03 => ContainerAlreadyExists
    | 0x04 => NetworkError
    | 0x05 => InternalError
    | n => Unknown(n)
    }

  let toString = (error: t): string =>
    switch error {
    | Success => "Success"
    | InvalidRequest => "Invalid request format"
    | ContainerNotFound => "Container not found"
    | ContainerAlreadyExists => "Container already exists"
    | NetworkError => "Network communication error"
    | InternalError => "Internal WASM runtime error"
    | Unknown(n) => `Unknown error code: ${Int.toString(n)}`
    }
}

// Result type for bridge operations
type result<'a> = Result<'a, ErrorCode.t>

// FFI bindings to Deno FFI wrapper
@module("./ffi.js") external loadLibrary: option<string> => unit = "loadLibrary"
@module("./ffi.js") external closeLibrary: unit => unit = "closeLibrary"
@module("./ffi.js") external createBridge: string => Js.Nullable.t<'a> = "createBridge"
@module("./ffi.js") external destroyBridge: 'a => unit = "destroyBridge"
@module("./ffi.js") external sendRequest: ('a, Js.TypedArray2.Uint8Array.t) => Js.Nullable.t<'b> = "sendRequest"
@module("./ffi.js") external getResponseData: 'b => Js.TypedArray2.Uint8Array.t = "getResponseData"
@module("./ffi.js") external freeResponse: 'b => unit = "freeResponse"
@module("./ffi.js") external getMemorySize: 'a => float = "getMemorySize"

// Bridge handle type
type bridge

// Response type
type response

// Container reference
type containerRef = string

// Image reference
type imageRef = string

// Bridge initialization
let init = (~libPath: option<string>=?, ()): unit => {
  loadLibrary(libPath)
}

let cleanup = (): unit => {
  closeLibrary()
}

// Create new bridge instance
let newBridge = (wasmPath: string): result<bridge> => {
  let bridgePtr = createBridge(wasmPath)

  switch Js.Nullable.toOption(bridgePtr) {
  | Some(ptr) => Ok(ptr)
  | None => Error(ErrorCode.InternalError)
  }
}

// Free bridge resources
let freeBridge = (bridge: bridge): unit => {
  destroyBridge(bridge)
}

// Get WASM memory size
let memorySize = (bridge: bridge): int => {
  getMemorySize(bridge)->Float.toInt
}

// Build request buffer
let buildRequest = (cmd: Command.t, payload: Js.TypedArray2.Uint8Array.t): Js.TypedArray2.Uint8Array.t => {
  let payloadLen = Js.TypedArray2.Uint8Array.length(payload)
  let buffer = Js.TypedArray2.Uint8Array.make(5 + payloadLen)

  // Command byte
  Js.TypedArray2.Uint8Array.unsafe_set(buffer, 0, Command.toU8(cmd))

  // Payload length (4 bytes, little-endian)
  let len = payloadLen
  Js.TypedArray2.Uint8Array.unsafe_set(buffer, 1, Int.land(len, 0xFF))
  Js.TypedArray2.Uint8Array.unsafe_set(buffer, 2, Int.land(Int.asr(len, 8), 0xFF))
  Js.TypedArray2.Uint8Array.unsafe_set(buffer, 3, Int.land(Int.asr(len, 16), 0xFF))
  Js.TypedArray2.Uint8Array.unsafe_set(buffer, 4, Int.land(Int.asr(len, 24), 0xFF))

  // Payload
  for i in 0 to payloadLen - 1 {
    let value = Js.TypedArray2.Uint8Array.unsafe_get(payload, i)
    Js.TypedArray2.Uint8Array.unsafe_set(buffer, 5 + i, value)
  }

  buffer
}

// Send request and get response
let sendRequestRaw = (bridge: bridge, request: Js.TypedArray2.Uint8Array.t): result<Js.TypedArray2.Uint8Array.t> => {
  let responsePtr = sendRequest(bridge, request)

  switch Js.Nullable.toOption(responsePtr) {
  | Some(ptr) => {
      let data = getResponseData(ptr)
      freeResponse(ptr)

      // Check status code (first byte)
      let statusCode = Js.TypedArray2.Uint8Array.unsafe_get(data, 0)
      let errorCode = ErrorCode.fromU8(statusCode)

      switch errorCode {
      | ErrorCode.Success => Ok(data)
      | err => Error(err)
      }
    }
  | None => Error(ErrorCode.InternalError)
  }
}

// Container operations

// Create container from image
let createContainer = (bridge: bridge, image: imageRef): result<containerRef> => {
  let encoder = Js.String.make("")->Js.String.constructor
  let imageBytes = encoder.encode(image)
  let request = buildRequest(Command.CreateContainer, imageBytes)

  switch sendRequestRaw(bridge, request) {
  | Ok(response) => {
      // Skip status byte, read container ID
      let decoder = Js.String.make("")->Js.String.constructor
      let idBytes = Js.TypedArray2.Uint8Array.slice(response, ~start=1, ~end_=Js.TypedArray2.Uint8Array.length(response))
      let containerId = decoder.decode(idBytes)
      Ok(containerId)
    }
  | Error(e) => Error(e)
  }
}

// Start container
let startContainer = (bridge: bridge, container: containerRef): result<unit> => {
  let encoder = Js.String.make("")->Js.String.constructor
  let idBytes = encoder.encode(container)
  let request = buildRequest(Command.StartContainer, idBytes)

  switch sendRequestRaw(bridge, request) {
  | Ok(_) => Ok()
  | Error(e) => Error(e)
  }
}

// Stop container
let stopContainer = (bridge: bridge, container: containerRef): result<unit> => {
  let encoder = Js.String.make("")->Js.String.constructor
  let idBytes = encoder.encode(container)
  let request = buildRequest(Command.StopContainer, idBytes)

  switch sendRequestRaw(bridge, request) {
  | Ok(_) => Ok()
  | Error(e) => Error(e)
  }
}

// Delete container
let deleteContainer = (bridge: bridge, container: containerRef): result<unit> => {
  let encoder = Js.String.make("")->Js.String.constructor
  let idBytes = encoder.encode(container)
  let request = buildRequest(Command.DeleteContainer, idBytes)

  switch sendRequestRaw(bridge, request) {
  | Ok(_) => Ok()
  | Error(e) => Error(e)
  }
}

// List containers
let listContainers = (bridge: bridge): result<array<containerRef>> => {
  let empty = Js.TypedArray2.Uint8Array.make(0)
  let request = buildRequest(Command.ListContainers, empty)

  switch sendRequestRaw(bridge, request) {
  | Ok(response) => {
      // Parse response: skip status byte, then read container IDs
      let decoder = Js.String.make("")->Js.String.constructor
      let dataBytes = Js.TypedArray2.Uint8Array.slice(response, ~start=1, ~end_=Js.TypedArray2.Uint8Array.length(response))
      let idsString = decoder.decode(dataBytes)
      let ids = Js.String.split(idsString, "\n")
      Ok(ids)
    }
  | Error(e) => Error(e)
  }
}
