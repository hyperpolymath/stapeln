// SPDX-License-Identifier: PMPL-1.0-or-later
// App.res - Main application with TEA architecture

open Model
open Msg
open Update

type page =
  | NetworkView // Cisco-style topology (TopologyView.res)
  | StackView // Paragon-style vertical (View.res)
  | LagoGreyView // Lago Grey image designer
  | PortConfigView // Port configuration with ephemeral pinholes
  | SecurityView // Security inspector with attack surface analysis
  | GapAnalysisView // Gap analysis with automated remediation
  | SimulationView // Packet animation simulation
  | SettingsView // Settings and preferences

type appState = {
  currentPage: page,
  model: model, // TEA model for stack designer
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

  // Dispatch function for messages with side effect handling
  let rec dispatch = (msg: msg) => {
    let newModel = update(state.model, msg)
    setState(prev => {...prev, model: newModel})

    // Handle side effects (API calls)
    switch msg {
    | SaveStack => {
        let services = newModel.components->Array.map(comp => {
          let port = comp.config->Dict.get("port")->Belt.Option.mapWithDefault("0", p => p)
          "{\"name\":\"" ++ comp.id ++ "\",\"kind\":\"" ++ Model.componentTypeToString(comp.componentType) ++ "\",\"port\":" ++ port ++ "}"
        })->Js.Array2.joinWith(",")
        let body = "{\"name\":\"stapeln-stack\",\"services\":[" ++ services ++ "]}"
        ignore(
          WebAPI.fetch("/api/stacks", {method: "POST", headers: Dict.fromArray([("content-type", "application/json")]), body})
          ->Promise.then(res => {
            if WebAPI.fetchOk(res) {
              WebAPI.text(res)->Promise.then(text => {
                dispatch(StackSaved(Ok(text)))
                Promise.resolve()
              })
            } else {
              dispatch(StackSaved(Error("Save failed")))
              Promise.resolve()
            }
          })
          ->Promise.catch(_ => {
            dispatch(StackSaved(Error("Network error")))
            Promise.resolve()
          })
        )
      }
    | RunSecurityScan | RunGapAnalysis => {
        ignore(
          WebAPI.fetch("/api/stacks", {method: "GET"})
          ->Promise.then(res => {
            if WebAPI.fetchOk(res) {
              let result: Model.validationResult = {
                valid: Array.length(newModel.components) > 0,
                errors: [],
                warnings: Array.length(newModel.components) === 0 ? ["Stack is empty - add components"] : [],
              }
              switch msg {
              | RunSecurityScan => dispatch(SecurityScanResult(result))
              | RunGapAnalysis => dispatch(GapAnalysisResult(result))
              | _ => ()
              }
            }
            Promise.resolve()
          })
          ->Promise.catch(_ => Promise.resolve())
        )
      }
    | _ => ()
    }
  }

  let switchPage = page => {
    setState(prev => {...prev, currentPage: page})
  }

  <ErrorBoundary>
    <div className="app">
      <nav className="nav-tabs">
        <button
          className={state.currentPage == NetworkView ? "tab active" : "tab"}
          onClick={_ => switchPage(NetworkView)}
        >
          {"🌐 Network"->React.string}
        </button>
        <button
          className={state.currentPage == StackView ? "tab active" : "tab"}
          onClick={_ => switchPage(StackView)}
        >
          {"📚 Stack"->React.string}
        </button>
        <button
          className={state.currentPage == LagoGreyView ? "tab active" : "tab"}
          onClick={_ => switchPage(LagoGreyView)}
        >
          {"🏔️ Lago Grey"->React.string}
        </button>
        <button
          className={state.currentPage == PortConfigView ? "tab active" : "tab"}
          onClick={_ => switchPage(PortConfigView)}
        >
          {"🔌 Ports"->React.string}
        </button>
        <button
          className={state.currentPage == SecurityView ? "tab active" : "tab"}
          onClick={_ => switchPage(SecurityView)}
        >
          {"🛡️ Security"->React.string}
        </button>
        <button
          className={state.currentPage == GapAnalysisView ? "tab active" : "tab"}
          onClick={_ => switchPage(GapAnalysisView)}
        >
          {"🔍 Gaps"->React.string}
        </button>
        <button
          className={state.currentPage == SimulationView ? "tab active" : "tab"}
          onClick={_ => switchPage(SimulationView)}
        >
          {"🎮 Simulation"->React.string}
        </button>
        <button
          className={state.currentPage == SettingsView ? "tab active" : "tab"}
          onClick={_ => switchPage(SettingsView)}
        >
          {"⚙️ Settings"->React.string}
        </button>

        <div className="nav-actions">
          <button
            className="action-btn"
            onClick={_ => dispatch(TriggerImportDesign)}
            title="Import design from JSON file"
          >
            {"📂 Import"->React.string}
          </button>
          <button
            className="action-btn"
            onClick={_ => dispatch(ExportDesignToJson("Stack design"))}
            title="Export design to JSON file"
          >
            {"💾 Export"->React.string}
          </button>
        </div>
      </nav>

      <div className="content">
        {switch state.currentPage {
        | NetworkView => TopologyView.view(state.model, state.isDark, dispatch)
        | StackView => StackView.view(state.model)
        | LagoGreyView => <LagoGreyImageDesigner />
        | PortConfigView => <PortConfigPanel />
        | SecurityView => <SecurityInspector />
        | GapAnalysisView => <GapAnalysis />
        | SimulationView => <SimulationMode />
        | SettingsView =>
          <div className="page settings-page">
            <h1> {"Settings"->React.string} </h1>
            <div className="settings-group">
              <h2> {"Theme"->React.string} </h2>
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
              <h2> {"Default Runtime"->React.string} </h2>
              <p> {"Podman / Docker / nerdctl selection coming soon"->React.string} </p>
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
        )}
      >
        <IdrisBadge style=Compact />
      </div>
    </div>
  </ErrorBoundary>
}
