// SPDX-License-Identifier: PMPL-1.0-or-later
// AppIntegrated.res - Fully integrated application with cross-component state sync

// Unified application state
type state = {
  // Current route
  currentRoute: AppRouter.route,
  // Component states
  portConfig: PortConfigPanel.state,
  securityInspector: SecurityInspector.state,
  gapAnalysis: GapAnalysis.state,
  simulationMode: SimulationMode.state,
  // Toast notifications
  toastState: Toast.state,
  // System health
  systemHealth: int,
}

// Unified message type
type msg =
  // Router messages
  | RouteChanged(AppRouter.route)
  | NavigateTo(AppRouter.route)
  // Port config messages
  | PortConfigMsg(PortConfigPanel.msg)
  // Security inspector messages
  | SecurityInspectorMsg(SecurityInspector.msg)
  // Gap analysis messages
  | GapAnalysisMsg(GapAnalysis.msg)
  // Simulation mode messages
  | SimulationModeMsg(SimulationMode.msg)
  // Toast messages
  | ToastMsg(Toast.msg)
  // Cross-component sync
  | SyncPortsToSecurity
  | SyncSecurityToGaps
  | SyncGapsToSecurity
  | UpdateSystemHealth

// Initialize state
let init = (): state => {
  {
    currentRoute: AppRouter.getCurrentRoute(),
    portConfig: PortConfigPanel.init,
    securityInspector: SecurityInspector.init,
    gapAnalysis: GapAnalysis.init,
    simulationMode: SimulationMode.init,
    toastState: Toast.initialState,
    systemHealth: 85, // Initial health score
  }
}

// Update function with cross-component effects
let update = (msg: msg, state: state): state => {
  switch msg {
  | RouteChanged(route) => {...state, currentRoute: route}

  | NavigateTo(route) => {
      AppRouter.navigateTo(route)
      {...state, currentRoute: route}
    }

  | PortConfigMsg(portMsg) => {
      let newPortConfig = PortConfigPanel.update(portMsg, state.portConfig)

      // Trigger cross-component sync when ports change
      let impact = StateSync.calculatePortSecurityImpact(newPortConfig.ports)
      let newSecurityMetrics = StateSync.updateSecurityMetricsFromPorts(
        state.securityInspector.metrics,
        impact,
      )

      // Generate warnings if critical ports are open
      let warnings = StateSync.generatePortWarnings(newPortConfig.ports)
      let stateWithToasts = if Array.length(warnings) > 0 {
        let firstWarning = Array.get(warnings, 0)
        let toastMsg = switch firstWarning {
        | Some(w) when String.includes(w, "CRITICAL") => Toast.error(w)
        | Some(w) when String.includes(w, "HIGH") => Toast.warning(w)
        | Some(w) => Toast.info(w)
        | None => Toast.info("Port configuration updated")
        }

        let (message, toastType, duration) = toastMsg
        let newToastState = Toast.update(
          Toast.ShowToast(message, toastType, duration),
          state.toastState,
        )
        {...state, toastState: newToastState}
      } else {
        state
      }

      // Calculate new system health
      let gapCount = Array.length(state.gapAnalysis.gaps)
      let systemHealth = StateSync.calculateSystemHealth(
        newSecurityMetrics,
        impact,
        gapCount,
      )

      {
        ...stateWithToasts,
        portConfig: newPortConfig,
        securityInspector: {
          ...state.securityInspector,
          metrics: newSecurityMetrics,
        },
        systemHealth,
      }
    }

  | SecurityInspectorMsg(secMsg) => {
      let newSecurityInspector = SecurityInspector.update(secMsg, state.securityInspector)

      // Sync vulnerabilities to gap analysis
      let gapSync = StateSync.syncSecurityToGaps(newSecurityInspector.vulnerabilities)

      // Show toast for significant security changes
      let stateWithToast = if gapSync.criticalVulns > 0 {
        let message = `Found ${Int.toString(gapSync.criticalVulns)} critical vulnerabilities`
        let newToastState = Toast.update(
          Toast.ShowToast(message, Toast.Error, 5000),
          state.toastState,
        )
        {...state, toastState: newToastState}
      } else {
        state
      }

      {...stateWithToast, securityInspector: newSecurityInspector}
    }

  | GapAnalysisMsg(gapMsg) => {
      let oldGapCount = Array.length(state.gapAnalysis.gaps)
      let newGapAnalysis = GapAnalysis.update(gapMsg, state.gapAnalysis)
      let newGapCount = Array.length(newGapAnalysis.gaps)

      // If gaps were fixed, update security metrics
      let stateWithUpdates = if newGapCount < oldGapCount {
        let gapSync = StateSync.syncGapFixesToSecurity(oldGapCount, newGapCount)
        let newSecurityMetrics = StateSync.updateSecurityMetricsFromGaps(
          state.securityInspector.metrics,
          gapSync,
        )

        // Show success toast
        let message = `Fixed ${Int.toString(gapSync.gapsFixed)} security gaps`
        let newToastState = Toast.update(
          Toast.ShowToast(message, Toast.Success, 4000),
          state.toastState,
        )

        {
          ...state,
          securityInspector: {
            ...state.securityInspector,
            metrics: newSecurityMetrics,
          },
          toastState: newToastState,
        }
      } else {
        state
      }

      // Update system health
      let portImpact = StateSync.calculatePortSecurityImpact(state.portConfig.ports)
      let systemHealth = StateSync.calculateSystemHealth(
        stateWithUpdates.securityInspector.metrics,
        portImpact,
        newGapCount,
      )

      {...stateWithUpdates, gapAnalysis: newGapAnalysis, systemHealth}
    }

  | SimulationModeMsg(simMsg) => {
      let newSimulationMode = SimulationMode.update(simMsg, state.simulationMode)
      {...state, simulationMode: newSimulationMode}
    }

  | ToastMsg(toastMsg) => {
      let newToastState = Toast.update(toastMsg, state.toastState)
      {...state, toastState: newToastState}
    }

  | SyncPortsToSecurity => {
      let impact = StateSync.calculatePortSecurityImpact(state.portConfig.ports)
      let newMetrics = StateSync.updateSecurityMetricsFromPorts(
        state.securityInspector.metrics,
        impact,
      )
      {
        ...state,
        securityInspector: {...state.securityInspector, metrics: newMetrics},
      }
    }

  | SyncSecurityToGaps =>
      // Already handled in SecurityInspectorMsg
      state

  | SyncGapsToSecurity =>
      // Already handled in GapAnalysisMsg
      state

  | UpdateSystemHealth => {
      let portImpact = StateSync.calculatePortSecurityImpact(state.portConfig.ports)
      let gapCount = Array.length(state.gapAnalysis.gaps)
      let systemHealth = StateSync.calculateSystemHealth(
        state.securityInspector.metrics,
        portImpact,
        gapCount,
      )
      {...state, systemHealth}
    }
  }
}

// Main view
@react.component
let make = () => {
  let (state, setState) = React.useState(() => init())

  let dispatch = (msg: msg) => {
    let newState = update(msg, state)
    setState(_ => newState)
  }

  // Subscribe to browser history changes
  React.useEffect0(() => {
    AppRouter.onRouteChange(route => dispatch(RouteChanged(route)))
    None
  })

  <div style={ReactDOM.Style.make(~display="flex", ~height="100vh", ())}>
    <Navigation currentRoute={state.currentRoute} onNavigate={route => dispatch(NavigateTo(route))} />

    <div style={ReactDOM.Style.make(~flex="1", ~display="flex", ~flexDirection="column", ~overflow="hidden", ())}>
      <Breadcrumb currentRoute={state.currentRoute} />

      <div
        style={ReactDOM.Style.make(
          ~padding="16px 20px",
          ~background="rgba(10, 14, 26, 0.95)",
          ~borderBottom="1px solid #2a3142",
          (),
        )}>
        <HealthIndicator health={state.systemHealth} />
      </div>

      <div
        style={ReactDOM.Style.make(
          ~flex="1",
          ~overflowY="auto",
          ~background="#0a0e1a",
          (),
        )}>
        {switch state.currentRoute {
        | NetworkView => <TopologyView />
        | StackView => <StackView />
        | LagoGreyView => <LagoGreyImageDesigner />
        | PortConfigView =>
          <PortConfigPanel
            initialState={Some(state.portConfig)}
            onStateChange={newState => dispatch(PortConfigMsg(PortConfigPanel.SelectPort(0)))}
          />
        | SecurityView =>
          <SecurityInspector
            initialState={Some(state.securityInspector)}
            onStateChange={newState =>
              dispatch(SecurityInspectorMsg(SecurityInspector.UpdateMetrics(newState.metrics)))}
          />
        | GapAnalysisView =>
          <GapAnalysis
            initialState={Some(state.gapAnalysis)}
            onStateChange={newState =>
              dispatch(GapAnalysisMsg(GapAnalysis.SelectGap(0)))}
          />
        | SimulationView =>
          <SimulationMode
            initialState={Some(state.simulationMode)}
            onStateChange={newState =>
              dispatch(SimulationModeMsg(SimulationMode.TogglePlayback))}
          />
        | SettingsView => <Settings />
        | NotFound =>
          <div
            style={ReactDOM.Style.make(
              ~display="flex",
              ~alignItems="center",
              ~justifyContent="center",
              ~height="100%",
              (),
            )}>
            <div style={ReactDOM.Style.make(~textAlign="center", ())}>
              <div style={ReactDOM.Style.make(~fontSize="72px", ~marginBottom="20px", ())}>
                {"404"->React.string}
              </div>
              <div style={ReactDOM.Style.make(~fontSize="24px", ~color="#8892a6", ())}>
                {"Page not found"->React.string}
              </div>
            </div>
          </div>
        }}
      </div>
    </div>

    <Toast toasts={state.toastState.toasts} dispatch={msg => dispatch(ToastMsg(msg))} />
  </div>
}
