// SPDX-License-Identifier: PMPL-1.0-or-later
// SolidAdapter.res - Solid.js integration (Phase 5)

// Note: This would be compiled to JavaScript and used with Solid.js
// The actual Solid.js reactive primitives would be imported at runtime

open DomMounterEnhanced

// Solid.js createSignal FFI
module SolidFFI = {
  type signal<'a> = (unit => 'a, ('a => 'a) => unit)

  @val external createSignal: 'a => signal<'a> = "createSignal"
  @val external createEffect: (unit => unit) => unit = "createEffect"
  @val external onCleanup: (unit => unit) => unit = "onCleanup"
}

// Create Solid.js primitive for DOM mounting
let createDomMounter = (elementId: string) => {
  let (mounted, setMounted) = SolidFFI.createSignal(false)
  let (error, setError) = SolidFFI.createSignal(None)

  // Create effect for mounting
  SolidFFI.createEffect(() => {
    switch mount(elementId) {
    | Ok() => setMounted(_ => true)
    | Error(msg) => setError(_ => Some(msg))
    }

    // Cleanup on dispose
    SolidFFI.onCleanup(() => {
      setMounted(_ => false)
      setError(_ => None)
    })
  })

  // Return accessors
  {
    "mounted": mounted,
    "error": error,
  }
}
