// SPDX-License-Identifier: PMPL-1.0-or-later
// SSRAdapter.res - Server-Side Rendering support (Phase 5)

open DomMounterEnhanced

// Detect if we're in SSR environment
let isSSR = (): bool => {
  %raw(`typeof window === 'undefined'`)
}

let isClient = (): bool => {
  !isSSR()
}

// SSR-safe mounting
let mountSSR = (elementId: string): Result.t<unit, string> => {
  if isSSR() {
    // In SSR, just validate the element ID
    if String.length(elementId) == 0 {
      Error("Element ID cannot be empty")
    } else {
      // Mark for hydration on client
      Console.log(`SSR: Marking ${elementId} for client-side hydration`)
      Ok()
    }
  } else {
    // Client-side: actually mount
    mount(elementId)
  }
}

// Hydration-aware mounting
let mountWithHydration = (elementId: string): Result.t<unit, string> => {
  if isClient() {
    // Check if element has SSR marker
    let hasSSRMarker = %raw(`
      (id) => {
        const element = document.getElementById(id);
        return element && element.hasAttribute('data-ssr-rendered');
      }
    `)

    if hasSSRMarker(elementId) {
      // Element was server-rendered, hydrate it
      Console.log(`Hydrating ${elementId}`)

      // Remove SSR marker
      %raw(`
        document.getElementById(elementId).removeAttribute('data-ssr-rendered')
      `)

      Ok()
    } else {
      // No SSR marker, regular mount
      mount(elementId)
    }
  } else {
    // SSR mode
    mountSSR(elementId)
  }
}
