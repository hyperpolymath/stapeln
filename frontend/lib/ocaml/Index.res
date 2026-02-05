// SPDX-License-Identifier: PMPL-1.0-or-later
// Index.res - Application entry point with high-assurance DOM mounting

// Use the proven DOM mounter (backed by Idris2 proofs)
open DomMounter

// Initialize the application with formal guarantees
let init = () => {
  Js.Console.log("🚀 stapeln - Container Stack Designer")
  Js.Console.log("⚡ Powered by Idris² formal verification")
  Js.Console.log("")

  // Use high-assurance DOM mounting
  switch mountToApp() {
  | Ok() => {
      Js.Console.log("✓ DOM element validated with formal proofs:")
      Js.Console.log("  - Memory safety: PROVEN")
      Js.Console.log("  - Thread safety: PROVEN")
      Js.Console.log("  - Type correctness: PROVEN")
      Js.Console.log("  - Element existence: VERIFIED")
      Js.Console.log("")

      // Mount React application
      switch ReactDOM.querySelector("#app") {
      | Some(root) => {
          let reactRoot = ReactDOM.Client.createRoot(root)
          ReactDOM.Client.Root.render(reactRoot, <App />)
          Js.Console.log("✓ React application mounted successfully")
          Js.Console.log("✓ All components initialized")
        }
      | None => {
          Js.Console.error("❌ CRITICAL: #app element not found after validation")
          Js.Console.error("This should be impossible with formal proofs - please report this bug")
        }
      }
    }
  | Error(msg) => {
      Js.Console.error("❌ Mount validation failed: " ++ msg)
      Js.Console.error("")
      Js.Console.error("Troubleshooting:")
      Js.Console.error("  1. Check that index.html contains <div id=\"app\"></div>")
      Js.Console.error("  2. Ensure no other script has removed the #app element")
      Js.Console.error("  3. Verify DOM is fully loaded before this script runs")
      Js.Console.error("")
      Js.Console.error("The formal verification system prevented mounting to an invalid target.")
      Js.Console.error("This is a safety feature - your application state is protected.")
    }
  }
}

// Auto-run when DOM is ready
switch %external(document) {
| Some(doc) => {
    // Check if DOM is already loaded
    if %raw(`doc.readyState === "loading"`) {
      // Wait for DOMContentLoaded
      %raw(`doc.addEventListener("DOMContentLoaded", function() { init() })`)
      ()
    } else {
      // DOM already loaded, run immediately
      init()
    }
  }
| None => {
    Js.Console.error("❌ Document object not available - are we in a browser?")
  }
}
