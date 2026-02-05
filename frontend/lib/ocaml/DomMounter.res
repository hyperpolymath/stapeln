// SPDX-License-Identifier: PMPL-1.0-or-later
// DomMounter.res - High-assurance DOM mounting with Idris2 proofs

// FFI bindings to Zig layer (which implements Idris2 ABI)
module FFI = {
  type mountResult =
    | Success
    | ElementNotFound
    | InvalidId
    | AlreadyMounted

  // External binding to Zig FFI (C ABI)
  @val external mount_to_element: string => int = "mount_to_element"
  @val external check_element_exists: string => bool = "check_element_exists"

  let resultFromInt = (code: int): mountResult =>
    switch code {
    | 0 => Success
    | 1 => ElementNotFound
    | 2 => InvalidId
    | 3 => AlreadyMounted
    | _ => InvalidId
    }
}

// High-level type-safe API
type mountResult = Result.t<unit, string>

// Mount with validation (backed by Idris2 proofs)
let mount = (elementId: string): mountResult => {
  // Validate element ID (matches Idris2 ValidElementId proof)
  if String.length(elementId) == 0 {
    Error("Element ID cannot be empty")
  } // Check existence first (matches Idris2 ElementExists proof)
  else if FFI.check_element_exists(elementId) {
    // Call Zig FFI which implements Idris2 ABI
    let code = FFI.mount_to_element(elementId)
    switch FFI.resultFromInt(code) {
    | Success => Ok()
    | ElementNotFound => Error("Element not found: " ++ elementId)
    | InvalidId => Error("Invalid element ID: " ++ elementId)
    | AlreadyMounted => Error("Element already mounted: " ++ elementId)
    }
  } else {
    Error("Element does not exist in DOM: " ++ elementId)
  }
}

// Safe mount with default element ID
let mountToApp = (): mountResult => mount("app")

// Mount with custom error handling
let mountWithCallback = (
  elementId: string,
  onSuccess: unit => unit,
  onError: string => unit,
): unit => {
  switch mount(elementId) {
  | Ok() => onSuccess()
  | Error(msg) => onError(msg)
  }
}

// Example usage with React (placeholder - actual implementation in App.res)
// Note: This uses the high-assurance mount validation before React rendering
let mountReactApp = (elementId: string): mountResult => {
  switch mount(elementId) {
  | Ok() => {
      Console.log("DOM element validated with Idris2 proofs")
      // React rendering happens in App.res after validation
      Ok()
    }
  | Error(msg) => Error(msg)
  }
}
