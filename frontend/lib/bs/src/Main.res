// SPDX-License-Identifier: PMPL-1.0-or-later
// Main.res - Application entry point (simplified, no TEA dependency)

// Mount the App component to the DOM
switch ReactDOM.querySelector("#app") {
| Some(root) => {
    let reactRoot = ReactDOM.Client.createRoot(root)
    ReactDOM.Client.Root.render(reactRoot, <App />)
  }
| None => Console.error("Root element #app not found")
}
