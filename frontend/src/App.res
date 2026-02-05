// SPDX-License-Identifier: PMPL-1.0-or-later
// App.res - Main application with TEA architecture

open Model
open Msg
open Update

type page =
  | NetworkView    // Cisco-style topology (CiscoView.res)
  | StackView      // Paragon-style vertical (View.res)
  | LagoGreyView   // Lago Grey image designer
  | SettingsView   // Settings and preferences

type appState = {
  currentPage: page,
  model: model,           // TEA model for stack designer
  isDark: bool,
}

let initialAppState = {
  currentPage: NetworkView,
  model: initialModel,
  isDark: true,
}

@react.component
let make = () => {
  let (state, setState) = React.useState(() => initialAppState)

  // Dispatch function for TEA messages
  let dispatch = (msg: msg) => {
    let (newModel, _effect) = update(state.model, msg)
    setState(prev => {...prev, model: newModel})
  }

  let switchPage = (page) => {
    setState(prev => {...prev, currentPage: page})
  }

  <div className="app">
    <nav className="nav-tabs">
      <button
        className={state.currentPage == NetworkView ? "tab active" : "tab"}
        onClick={_ => switchPage(NetworkView)}>
        {"🌐 Network View"->React.string}
      </button>
      <button
        className={state.currentPage == StackView ? "tab active" : "tab"}
        onClick={_ => switchPage(StackView)}>
        {"📚 Stack View"->React.string}
      </button>
      <button
        className={state.currentPage == LagoGreyView ? "tab active" : "tab"}
        onClick={_ => switchPage(LagoGreyView)}>
        {"🏔️ Lago Grey Designer"->React.string}
      </button>
      <button
        className={state.currentPage == SettingsView ? "tab active" : "tab"}
        onClick={_ => switchPage(SettingsView)}>
        {"⚙️ Settings"->React.string}
      </button>
    </nav>

    <div className="content">
      {switch state.currentPage {
      | NetworkView => CiscoView.view(state.model, state.isDark, dispatch)
      | StackView => View.view(state.model)
      | LagoGreyView => <LagoGreyImageDesigner />
      | SettingsView =>
          <div className="page settings-page">
            <h1>{"Settings"->React.string}</h1>
            <div className="settings-group">
              <h2>{"Theme"->React.string}</h2>
              <label>
                <input
                  type_="checkbox"
                  checked={state.isDark}
                  onChange={_ => setState(prev => {...prev, isDark: !prev.isDark})}
                />
                {" Dark Mode"->React.string}
              </label>
            </div>
            <div className="settings-group">
              <h2>{"Default Runtime"->React.string}</h2>
              <p>{"Podman / Docker / nerdctl selection coming soon"->React.string}</p>
            </div>
          </div>
      }}
    </div>
  </div>
}
