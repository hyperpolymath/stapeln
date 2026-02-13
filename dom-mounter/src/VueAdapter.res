// SPDX-License-Identifier: PMPL-1.0-or-later
// VueAdapter.res - Vue 3 composition API integration (Phase 5)

open DomMounterEnhanced

// Vue 3 Composition API FFI
module VueFFI = {
  type ref<'a>

  @val external ref: 'a => ref<'a> = "ref"
  @val external onMounted: (unit => unit) => unit = "onMounted"
  @val external onUnmounted: (unit => unit) => unit = "onUnmounted"
  @set external setValue: (ref<'a>, 'a) => unit = "value"
}

// Vue composable for DOM mounting
let useDomMounterVue = (elementId: string) => {
  let mounted = VueFFI.ref(false)
  let error = VueFFI.ref(None)

  // Mount on component mount
  VueFFI.onMounted(() => {
    switch mount(elementId) {
    | Ok() => VueFFI.setValue(mounted, true)
    | Error(msg) => VueFFI.setValue(error, Some(msg))
    }
  })

  // Cleanup on unmount
  VueFFI.onUnmounted(() => {
    VueFFI.setValue(mounted, false)
    VueFFI.setValue(error, None)
  })

  {
    "mounted": mounted,
    "error": error,
  }
}
