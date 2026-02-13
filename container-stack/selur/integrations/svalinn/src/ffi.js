// SPDX-License-Identifier: PMPL-1.0-or-later
// Deno FFI wrapper for selur Rust bridge
// This is the low-level JS glue - application code should use Selur.res

const LIBNAME = Deno.build.os === "windows"
  ? "selur.dll"
  : Deno.build.os === "darwin"
  ? "libselur.dylib"
  : "libselur.so"

const SYMBOLS = {
  bridge_new: {
    parameters: ["buffer"],
    result: "pointer",
  },
  bridge_free: {
    parameters: ["pointer"],
    result: "void",
  },
  bridge_send_request: {
    parameters: ["pointer", "buffer", "u32"],
    result: "pointer",
  },
  bridge_memory_size: {
    parameters: ["pointer"],
    result: "u64",
  },
  response_get_data: {
    parameters: ["pointer"],
    result: "buffer",
  },
  response_get_length: {
    parameters: ["pointer"],
    result: "u32",
  },
  response_free: {
    parameters: ["pointer"],
    result: "void",
  },
}

let dylib = null

export function loadLibrary(libPath) {
  if (dylib) {
    return dylib
  }

  const actualPath = libPath || `./${LIBNAME}`
  dylib = Deno.dlopen(actualPath, SYMBOLS)
  return dylib
}

export function closeLibrary() {
  if (dylib) {
    dylib.close()
    dylib = null
  }
}

export function createBridge(wasmPath) {
  if (!dylib) {
    throw new Error("Library not loaded. Call loadLibrary() first.")
  }

  const encoder = new TextEncoder()
  const pathBytes = encoder.encode(wasmPath + "\0")
  const ptr = dylib.symbols.bridge_new(pathBytes)

  if (!ptr) {
    throw new Error(`Failed to create bridge from WASM: ${wasmPath}`)
  }

  return ptr
}

export function destroyBridge(bridgePtr) {
  if (!dylib || !bridgePtr) return
  dylib.symbols.bridge_free(bridgePtr)
}

export function sendRequest(bridgePtr, requestData) {
  if (!dylib || !bridgePtr) {
    throw new Error("Invalid bridge pointer")
  }

  const responsePtr = dylib.symbols.bridge_send_request(
    bridgePtr,
    requestData,
    requestData.length
  )

  if (!responsePtr) {
    throw new Error("Failed to send request")
  }

  return responsePtr
}

export function getResponseData(responsePtr) {
  if (!dylib || !responsePtr) {
    throw new Error("Invalid response pointer")
  }

  const length = dylib.symbols.response_get_length(responsePtr)
  const dataPtr = dylib.symbols.response_get_data(responsePtr)

  return new Uint8Array(
    Deno.UnsafePointerView.getArrayBuffer(dataPtr, length)
  )
}

export function freeResponse(responsePtr) {
  if (!dylib || !responsePtr) return
  dylib.symbols.response_free(responsePtr)
}

export function getMemorySize(bridgePtr) {
  if (!dylib || !bridgePtr) {
    throw new Error("Invalid bridge pointer")
  }

  return dylib.symbols.bridge_memory_size(bridgePtr)
}
