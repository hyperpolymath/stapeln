// SPDX-License-Identifier: PMPL-1.0-or-later
// Main.res - Application entry point with routing

open Model
open Msg
open Update

type route =
  | ParagonView
  | TopologyView
  | SettingsView

type appModel = {
  route: route,
  stackModel: model,
  isDarkMode: bool,
  settings: Settings.settings,
}

type appMsg =
  | Navigate(route)
  | ToggleTheme
  | StackMsg(msg)

let initialAppModel = {
  route: ParagonView,
  stackModel: initialModel,
  isDarkMode: false, // TODO: Detect from system
  settings: Settings.defaultSettings,
}

let appUpdate = (model: appModel, msg: appMsg): (appModel, Tea.Cmd.t<appMsg>) => {
  switch msg {
  | Navigate(route) => ({...model, route: route}, Tea.Cmd.none)
  | ToggleTheme => ({...model, isDarkMode: !model.isDarkMode}, Tea.Cmd.none)
  | StackMsg(stackMsg) => {
      let (newStackModel, cmd) = update(model.stackModel, stackMsg)
      let newModel = {...model, stackModel: newStackModel}
      let appCmd = Tea.Cmd.map(m => StackMsg(m), cmd)
      (newModel, appCmd)
    }
  }
}

// Navigation bar
let renderNav = (currentRoute: route, isDark: bool, dispatch) => {
  let navButtonStyle = (isActive: bool) => `
    padding: 0.75rem 1.5rem;
    background-color: ${isActive
    ? isDark
      ? "#66B2FF"
      : "#0052CC"
    : "transparent"};
    color: ${isActive ? "white" : isDark ? "#FFFFFF" : "#000000"};
    border: none;
    font-size: 1rem;
    font-weight: 600;
    cursor: pointer;
    transition: background-color 0.2s ease;
  `

  <nav
    role="navigation"
    ariaLabel="Main navigation"
    style={`
      display: flex;
      align-items: center;
      gap: 1rem;
      padding: 1rem 2rem;
      background-color: ${isDark ? "#000000" : "#FFFFFF"};
      border-bottom: 2px solid ${isDark ? "#CCCCCC" : "#333333"};
    `}>
    <h1
      style="margin: 0; font-size: 1.5rem; font-weight: 700; flex: 1;"
      id="app-title">
      {"🏔️ stapeln" -> React.string}
    </h1>

    <button
      onClick={_ => dispatch(Navigate(ParagonView))}
      ariaLabel="Go to Paragon view"
      ariaCurrent={currentRoute === ParagonView ? "page" : "false"}
      style={navButtonStyle(currentRoute === ParagonView)}>
      {"Paragon View" -> React.string}
    </button>

    <button
      onClick={_ => dispatch(Navigate(TopologyView))}
      ariaLabel="Go to Cisco view"
      ariaCurrent={currentRoute === TopologyView ? "page" : "false"}
      style={navButtonStyle(currentRoute === TopologyView)}>
      {"Cisco View" -> React.string}
    </button>

    <button
      onClick={_ => dispatch(Navigate(SettingsView))}
      ariaLabel="Go to Settings"
      ariaCurrent={currentRoute === SettingsView ? "page" : "false"}
      style={navButtonStyle(currentRoute === SettingsView)}>
      {"Settings" -> React.string}
    </button>

    <button
      onClick={_ => dispatch(ToggleTheme)}
      ariaLabel="Toggle dark/light theme"
      role="switch"
      ariaChecked={isDark ? "true" : "false"}
      style={`
        padding: 0.75rem 1.5rem;
        background-color: ${isDark ? "#66B2FF" : "#0052CC"};
        color: white;
        border: none;
        border-radius: 6px;
        font-size: 1rem;
        cursor: pointer;
        font-weight: 600;
      `}>
      {(isDark ? "🌙 Dark" : "☀️ Light") -> React.string}
    </button>
  </nav>
}

// Main app view
let appView = (model: appModel): Tea.Html.t<appMsg> => {
  let dispatch = msg => StackMsg(msg)

  <>
    {renderNav(model.route, model.isDarkMode, m => m)}
    {switch model.route {
    | ParagonView => View.renderParagonStack(model.stackModel, model.isDarkMode)
    | TopologyView =>
      TopologyView.view(model.stackModel, model.isDarkMode, dispatch)
    | SettingsView => Settings.view(model.settings, model.isDarkMode)
    }}
  </>
}

// Initialize the TEA app
let main = Tea.App.program({
  init: () => (initialAppModel, Tea.Cmd.none),
  update: appUpdate,
  view: appView,
  subscriptions: _model => Tea.Sub.none,
})

// Mount to DOM
let () = Tea.App.run(main, ~rootId="app")
