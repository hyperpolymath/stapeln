// SPDX-License-Identifier: PMPL-1.0-or-later
// Minimal working app to demonstrate UI

open Model

type page =
  | NetworkView
  | StackView
  | LagoGreyView
  | SettingsView

type state = {
  currentPage: page,
  components: array<component>,
  isDark: bool,
}

let initialState = {
  currentPage: NetworkView,
  components: [],
  isDark: true,
}

@react.component
let make = () => {
  let (state, setState) = React.useState(() => initialState)

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
      | NetworkView => <div className="page"> {"Network topology coming soon"->React.string} </div>
      | StackView => <div className="page"> {"Vertical stack coming soon"->React.string} </div>
      | LagoGreyView => <LagoGreyImageDesigner />
      | SettingsView => <div className="page"> {"Settings coming soon"->React.string} </div>
      }}
    </div>
  </div>
}
