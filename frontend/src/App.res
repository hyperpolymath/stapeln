// SPDX-License-Identifier: PMPL-1.0-or-later
// App.res - Main application with TEA architecture

open Model
open Msg
open Update

type page =
  | NetworkView       // Cisco-style topology (CiscoView.res)
  | StackView         // Paragon-style vertical (View.res)
  | LagoGreyView      // Lago Grey image designer
  | PortConfigView    // Port configuration with ephemeral pinholes
  | SecurityView      // Security inspector with attack surface analysis
  | GapAnalysisView   // Gap analysis with automated remediation
  | SimulationView    // Packet animation simulation
  | SettingsView      // Settings and preferences

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

  // Dispatch function for messages
  let dispatch = (msg: msg) => {
    let newModel = update(state.model, msg)
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
        {"🌐 Network"->React.string}
      </button>
      <button
        className={state.currentPage == StackView ? "tab active" : "tab"}
        onClick={_ => switchPage(StackView)}>
        {"📚 Stack"->React.string}
      </button>
      <button
        className={state.currentPage == LagoGreyView ? "tab active" : "tab"}
        onClick={_ => switchPage(LagoGreyView)}>
        {"🏔️ Lago Grey"->React.string}
      </button>
      <button
        className={state.currentPage == PortConfigView ? "tab active" : "tab"}
        onClick={_ => switchPage(PortConfigView)}>
        {"🔌 Ports"->React.string}
      </button>
      <button
        className={state.currentPage == SecurityView ? "tab active" : "tab"}
        onClick={_ => switchPage(SecurityView)}>
        {"🛡️ Security"->React.string}
      </button>
      <button
        className={state.currentPage == GapAnalysisView ? "tab active" : "tab"}
        onClick={_ => switchPage(GapAnalysisView)}>
        {"🔍 Gaps"->React.string}
      </button>
      <button
        className={state.currentPage == SimulationView ? "tab active" : "tab"}
        onClick={_ => switchPage(SimulationView)}>
        {"🎮 Simulation"->React.string}
      </button>
      <button
        className={state.currentPage == SettingsView ? "tab active" : "tab"}
        onClick={_ => switchPage(SettingsView)}>
        {"⚙️ Settings"->React.string}
      </button>

      <div className="nav-actions">
        <button
          className="action-btn"
          onClick={_ => dispatch(TriggerImportDesign)}
          title="Import design from JSON file">
          {"📂 Import"->React.string}
        </button>
        <button
          className="action-btn"
          onClick={_ => dispatch(ExportDesignToJson("Stack design"))}
          title="Export design to JSON file">
          {"💾 Export"->React.string}
        </button>
      </div>
    </nav>

    <div className="content">
      {switch state.currentPage {
      | NetworkView => CiscoView.view(state.model, state.isDark, dispatch)
      | StackView => View.view(state.model)
      | LagoGreyView => <LagoGreyImageDesigner />
      | PortConfigView => <PortConfigPanel />
      | SecurityView => <SecurityInspector />
      | GapAnalysisView => <GapAnalysis />
      | SimulationView => <SimulationMode />
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

    <div
      style={ReactDOM.Style.make(
        ~position="fixed",
        ~bottom="16px",
        ~right="16px",
        ~zIndex="1000",
        (),
      )}>
      <IdrisBadge style=Compact />
    </div>
  </div>
}
