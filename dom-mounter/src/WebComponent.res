// SPDX-License-Identifier: PMPL-1.0-or-later
// WebComponent.res - Web Components integration (Phase 5)

open DomMounterEnhanced

// Web Components API FFI
module WebComponentsFFI = {
  type customElement

  @val external defineCustomElement: (string, customElement) => unit = "customElements.define"

  // HTMLElement class methods
  @val external connectedCallback: unit => unit = "connectedCallback"
  @val external disconnectedCallback: unit => unit = "disconnectedCallback"
  @val external getAttribute: string => option<string> = "getAttribute"
}

// Register mounted component custom element
let registerMountedComponent = (tagName: string) => {
  // Create custom element class
  let customElement = %raw(`
    class MountedComponent extends HTMLElement {
      connectedCallback() {
        const elementId = this.getAttribute('element-id');
        if (elementId) {
          // Mount using DomMounterEnhanced
          this._mountResult = mount(elementId);

          // Add indicator attribute
          if (this._mountResult.tag === 'Ok') {
            this.setAttribute('data-mounted', 'true');
          } else {
            this.setAttribute('data-mount-error', this._mountResult.error);
          }
        }
      }

      disconnectedCallback() {
        const elementId = this.getAttribute('element-id');
        if (elementId) {
          // Unmount (would call unmount function)
          this.removeAttribute('data-mounted');
        }
      }
    }
  `)

  WebComponentsFFI.defineCustomElement(tagName, customElement)
}

// Usage: registerMountedComponent("mounted-component")
// Then in HTML: <mounted-component element-id="app-root"></mounted-component>
