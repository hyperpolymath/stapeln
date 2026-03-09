// SPDX-License-Identifier: PMPL-1.0-or-later
// App.res - Main application with TEA architecture

open Model
open Msg
open Update

// Direct JS binding for Array.join (avoids deprecated Js.Array2.joinWith)
@send external joinWith: (array<string>, string) => string = "join"

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

// Serialise the current stack model to a JSON string suitable for the API.
let serializeStack = (model: model): string => {
  let services =
    model.components
    ->Array.map(comp => {
      let port =
        comp.config->Dict.get("port")->Belt.Option.mapWithDefault("0", p => p)
      "{\"name\":\"" ++
      comp.id ++
      "\",\"kind\":\"" ++
      Model.componentTypeToString(comp.componentType) ++
      "\",\"port\":" ++
      port ++
      "}"
    })
    ->joinWith(",")
  "{\"name\":\"stapeln-stack\",\"services\":[" ++ services ++ "]}"
}

// ---------------------------------------------------------------------------
// Module-level WebSocket connection (created once, survives re-renders)
// ---------------------------------------------------------------------------

let socketConn = Socket.make()
let stackChannel: ref<option<Socket.channel>> = ref(None)

@react.component
let make = () => {
  let (state, setState) = React.useState(() => initialAppState)

  // Dispatch function for messages with side effect handling
  let rec dispatch = (msg: msg) => {
    let newModel = update(state.model, msg)
    setState(prev => {...prev, model: newModel})

    // Handle side effects (API calls + WebSocket)
    switch msg {
    | SaveStack => {
        let body = serializeStack(newModel)
        ignore(
          ApiClient.saveStack(body)
          ->Promise.then(result => {
            dispatch(StackSaved(result))
            Promise.resolve()
          }),
        )
      }

    | RunSecurityScan => {
        // First save the stack, then run the security scan with the ID
        let body = serializeStack(newModel)
        ignore(
          ApiClient.saveStack(body)
          ->Promise.then(saveResult => {
            switch saveResult {
            | Ok(stackIdStr) => {
                let stackId = switch Int.fromString(stackIdStr) {
                | Some(id) => id
                | None => 0
                }
                ApiClient.runSecurityScan(stackId)->Promise.then(scanResult => {
                  dispatch(SecurityScanResult(scanResult))
                  Promise.resolve()
                })
              }
            | Error(err) => {
                dispatch(SecurityScanResult(Error(err)))
                Promise.resolve()
              }
            }
          })
          ->Promise.catch(_ => {
            dispatch(SecurityScanResult(Error("Network error")))
            Promise.resolve()
          }),
        )
      }

    | RunGapAnalysis => {
        // First save the stack, then run gap analysis with the ID
        let body = serializeStack(newModel)
        ignore(
          ApiClient.saveStack(body)
          ->Promise.then(saveResult => {
            switch saveResult {
            | Ok(stackIdStr) => {
                let stackId = switch Int.fromString(stackIdStr) {
                | Some(id) => id
                | None => 0
                }
                ApiClient.runGapAnalysis(stackId)->Promise.then(gapResult => {
                  dispatch(GapAnalysisResult(gapResult))
                  Promise.resolve()
                })
              }
            | Error(err) => {
                dispatch(GapAnalysisResult(Error(err)))
                Promise.resolve()
              }
            }
          })
          ->Promise.catch(_ => {
            dispatch(GapAnalysisResult(Error("Network error")))
            Promise.resolve()
          }),
        )
      }

    | SaveSettings => {
        let settingsJson = JSON.Encode.object(
          Dict.fromArray([
            ("theme", JSON.Encode.string(newModel.settings.theme)),
            ("defaultRuntime", JSON.Encode.string(newModel.settings.defaultRuntime)),
            ("autoSave", JSON.Encode.bool(newModel.settings.autoSave)),
            ("backendUrl", JSON.Encode.string(newModel.settings.backendUrl)),
          ]),
        )
        ignore(
          ApiClient.saveSettings(settingsJson)
          ->Promise.then(result => {
            dispatch(SettingsSaved(result))
            Promise.resolve()
          }),
        )
      }

    | LoadSettings => {
        ignore(
          ApiClient.loadSettings()
          ->Promise.then(result => {
            dispatch(SettingsLoaded(result))
            Promise.resolve()
          }),
        )
      }

    // WebSocket connection management
    | WsConnect => {
        // Register state-change callback
        Socket.onStateChange(socketConn, connState => {
          dispatch(WsConnectionStateChanged(connState))
        })

        // Create and join the lobby channel
        let ch = Socket.channel(socketConn, "stack:lobby")
        stackChannel := Some(ch)

        // Register channel event handlers that dispatch TEA messages
        Socket.on(ch, "validation_result", payload => dispatch(WsValidationResult(payload)))
        Socket.on(ch, "security_result", payload => dispatch(WsSecurityResult(payload)))
        Socket.on(ch, "gap_result", payload => dispatch(WsGapResult(payload)))

        // Connect and auto-join
        Socket.connect(socketConn, Socket.defaultUrl())
        Socket.joinChannel(socketConn, ch)
      }

    | WsDisconnect => {
        switch stackChannel.contents {
        | Some(ch) => Socket.leaveChannel(socketConn, ch)
        | None => ()
        }
        stackChannel := None
        Socket.disconnect(socketConn)
      }

    | WsValidate => {
        switch stackChannel.contents {
        | Some(ch) => {
            let stackJson = serializeStack(newModel)
            let payload = JSON.Encode.object(
              Dict.fromArray([("stack", JSON.Encode.string(stackJson))]),
            )
            Socket.push(socketConn, ch, "validate", payload)
          }
        | None => Console.warn("WebSocket not connected, cannot validate via WS")
        }
      }

    | WsSecurityScan => {
        switch stackChannel.contents {
        | Some(ch) => {
            let stackJson = serializeStack(newModel)
            let payload = JSON.Encode.object(
              Dict.fromArray([("stack", JSON.Encode.string(stackJson))]),
            )
            Socket.push(socketConn, ch, "security_scan", payload)
          }
        | None => Console.warn("WebSocket not connected, cannot security scan via WS")
        }
      }

    | WsGapAnalysis => {
        switch stackChannel.contents {
        | Some(ch) => {
            let stackJson = serializeStack(newModel)
            let payload = JSON.Encode.object(
              Dict.fromArray([("stack", JSON.Encode.string(stackJson))]),
            )
            Socket.push(socketConn, ch, "gap_analysis", payload)
          }
        | None => Console.warn("WebSocket not connected, cannot gap analysis via WS")
        }
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
        | SecurityView =>
          <SecurityInspector initialState=?{state.model.securityState} />
        | GapAnalysisView =>
          <GapAnalysis initialState=?{state.model.gapState} />
        | SimulationView => <SimulationMode />
        | SettingsView =>
          <SettingsPage
            settings={state.model.settings}
            isDark={state.isDark}
            onSave={() => dispatch(SaveSettings)}
            onSettingsChange={newSettings =>
              setState(prev => {
                ...prev,
                model: {...prev.model, settings: newSettings},
                isDark: newSettings.theme === "dark",
              })
            }
          />
        }}
      </div>

      <div
        style={Sx.make(
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
