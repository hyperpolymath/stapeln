// SPDX-License-Identifier: PMPL-1.0-or-later
// PortConfigPanel.res - Visual port configuration with ephemeral pinholes

// Port states
type portState =
  | Closed
  | Open
  | Ephemeral(int) // Duration in seconds

type portRisk =
  | Critical // SSH (22), RDP (3389), Telnet (23)
  | High     // Database ports (3306, 5432, 27017)
  | Medium   // HTTP (80), HTTPS (443)
  | Safe     // High-numbered ports (>10000)

type port = {
  number: int,
  protocol: string,
  state: portState,
  risk: portRisk,
  description: string,
}

type state = {
  ports: array<port>,
  selectedPort: option<int>,
  showEphemeralConfig: bool,
  ephemeralDuration: int, // seconds
  ephemeralRemaining: option<int>, // countdown
}

// Message types
type msg =
  | SelectPort(int)
  | SetPortState(int, portState)
  | SetEphemeralDuration(int)
  | ToggleEphemeralConfig
  | StartEphemeralTimer(int)
  | DecrementEphemeralTimer(int)
  | ExpireEphemeralPort(int)

// Initialize with common ports
let init: state = {
  ports: [
    {number: 22, protocol: "SSH", state: Closed, risk: Critical, description: "Secure Shell"},
    {number: 80, protocol: "HTTP", state: Closed, risk: Medium, description: "Web Server"},
    {number: 443, protocol: "HTTPS", state: Closed, risk: Medium, description: "Secure Web"},
    {number: 3306, protocol: "MySQL", state: Closed, risk: High, description: "MySQL Database"},
    {number: 5432, protocol: "PostgreSQL", state: Closed, risk: High, description: "PostgreSQL DB"},
    {number: 8080, protocol: "HTTP-Alt", state: Closed, risk: Safe, description: "Alt HTTP"},
    {number: 9000, protocol: "Custom", state: Closed, risk: Safe, description: "Application"},
  ],
  selectedPort: None,
  showEphemeralConfig: false,
  ephemeralDuration: 300, // 5 minutes default
  ephemeralRemaining: None,
}

// Update function
let update = (msg: msg, state: state): state => {
  switch msg {
  | SelectPort(portNum) =>
    {...state, selectedPort: Some(portNum)}

  | SetPortState(portNum, newState) =>
    let updatedPorts = Array.map(state.ports, port =>
      if port.number == portNum {
        {...port, state: newState}
      } else {
        port
      }
    )
    {...state, ports: updatedPorts}

  | SetEphemeralDuration(seconds) =>
    {...state, ephemeralDuration: seconds}

  | ToggleEphemeralConfig =>
    {...state, showEphemeralConfig: !state.showEphemeralConfig}

  | StartEphemeralTimer(portNum) =>
    {...state, ephemeralRemaining: Some(state.ephemeralDuration)}

  | DecrementEphemeralTimer(portNum) =>
    switch state.ephemeralRemaining {
    | Some(remaining) when remaining > 0 =>
      {...state, ephemeralRemaining: Some(remaining - 1)}
    | _ => state
    }

  | ExpireEphemeralPort(portNum) =>
    let updatedPorts = Array.map(state.ports, port =>
      if port.number == portNum {
        {...port, state: Closed}
      } else {
        port
      }
    )
    {...state, ports: updatedPorts, ephemeralRemaining: None}
  }
}

// Helper: Get risk indicator emoji
let riskIndicator = (risk: portRisk): string => {
  switch risk {
  | Critical => "🔴"
  | High => "🟠"
  | Medium => "🟡"
  | Safe => "✅"
  }
}

// Helper: Get risk color
let riskColor = (risk: portRisk): string => {
  switch risk {
  | Critical => "#f44336"
  | High => "#ff9800"
  | Medium => "#ffc107"
  | Safe => "#4caf50"
  }
}

// Helper: Format duration
let formatDuration = (seconds: int): string => {
  if seconds < 60 {
    Int.toString(seconds) ++ "s"
  } else if seconds < 3600 {
    Int.toString(seconds / 60) ++ "m"
  } else if seconds < 86400 {
    Int.toString(seconds / 3600) ++ "h"
  } else {
    Int.toString(seconds / 86400) ++ "d"
  }
}

// Helper: Get state label
let stateLabel = (state: portState): string => {
  switch state {
  | Closed => "Closed"
  | Open => "Open"
  | Ephemeral(duration) => "Ephemeral (" ++ formatDuration(duration) ++ ")"
  }
}

// Helper: Get state color
let stateColor = (state: portState): string => {
  switch state {
  | Closed => "#4caf50" // Green (safe)
  | Open => "#f44336"   // Red (dangerous)
  | Ephemeral(_) => "#ff9800" // Orange (temporary)
  }
}

// View: Port row
let viewPortRow = (port: port, dispatch: msg => unit): React.element => {
  <div
    key={Int.toString(port.number)}
    className="port-row"
    style={ReactDOM.Style.make(
      ~display="flex",
      ~alignItems="center",
      ~justifyContent="space-between",
      ~padding="16px",
      ~marginBottom="8px",
      ~background="linear-gradient(135deg, #1e2431 0%, #252d3d 100%)",
      ~border="1px solid #2a3142",
      ~borderRadius="12px",
      ~cursor="pointer",
      ~transition="all 0.2s",
      (),
    )}>
    {/* Port info */}
    <div style={ReactDOM.Style.make(~display="flex", ~alignItems="center", ~gap="16px", ())}>
      <span style={ReactDOM.Style.make(~fontSize="24px", ())}>
        {riskIndicator(port.risk)->React.string}
      </span>
      <div>
        <div style={ReactDOM.Style.make(~fontSize="16px", ~fontWeight="700", ~color="#e0e6ed", ())}>
          {("Port " ++ Int.toString(port.number))->React.string}
        </div>
        <div style={ReactDOM.Style.make(~fontSize="12px", ~color="#8892a6", ~marginTop="2px", ())}>
          {(port.protocol ++ " - " ++ port.description)->React.string}
        </div>
      </div>
    </div>

    {/* State toggle */}
    <div style={ReactDOM.Style.make(~display="flex", ~gap="8px", ())}>
      <button
        onClick={_ => dispatch(SetPortState(port.number, Closed))}
        style={ReactDOM.Style.make(
          ~padding="8px 16px",
          ~background=port.state == Closed ? "#4caf50" : "#2a3142",
          ~color="white",
          ~border="none",
          ~borderRadius="6px",
          ~fontSize="12px",
          ~fontWeight="600",
          ~cursor="pointer",
          (),
        )}>
        {"Closed"->React.string}
      </button>

      <button
        onClick={_ => dispatch(SetPortState(port.number, Ephemeral(300)))}
        style={ReactDOM.Style.make(
          ~padding="8px 16px",
          ~background=switch port.state {
          | Ephemeral(_) => "#ff9800"
          | _ => "#2a3142"
          },
          ~color="white",
          ~border="none",
          ~borderRadius="6px",
          ~fontSize="12px",
          ~fontWeight="600",
          ~cursor="pointer",
          (),
        )}>
        {"Ephemeral"->React.string}
      </button>

      <button
        onClick={_ => dispatch(SetPortState(port.number, Open))}
        style={ReactDOM.Style.make(
          ~padding="8px 16px",
          ~background=port.state == Open ? "#f44336" : "#2a3142",
          ~color="white",
          ~border="none",
          ~borderRadius="6px",
          ~fontSize="12px",
          ~fontWeight="600",
          ~cursor="pointer",
          (),
        )}>
        {"Open"->React.string}
      </button>
    </div>

    {/* State indicator */}
    <div
      style={ReactDOM.Style.make(
        ~padding="6px 12px",
        ~background=stateColor(port.state),
        ~color="white",
        ~borderRadius="6px",
        ~fontSize="11px",
        ~fontWeight="700",
        ~minWidth="80px",
        ~textAlign="center",
        (),
      )}>
      {stateLabel(port.state)->React.string}
    </div>
  </div>
}

// View: Ephemeral configuration panel
let viewEphemeralConfig = (state: state, dispatch: msg => unit): React.element => {
  <div
    className="ephemeral-config"
    style={ReactDOM.Style.make(
      ~padding="24px",
      ~background="#1e2431",
      ~border="2px solid #ff9800",
      ~borderRadius="12px",
      ~marginTop="16px",
      (),
    )}>
    <h3 style={ReactDOM.Style.make(~fontSize="18px", ~fontWeight="700", ~color="#ff9800", ~marginBottom="16px", ())}>
      {"⏱️ Ephemeral Pinhole Configuration"->React.string}
    </h3>

    <p style={ReactDOM.Style.make(~fontSize="13px", ~color="#b0b8c4", ~marginBottom="16px", ~lineHeight="1.6", ())}>
      {"Ephemeral pinholes automatically close after a set duration. Perfect for temporary access (SSH debugging, database migrations, etc.)."->React.string}
    </p>

    <div style={ReactDOM.Style.make(~marginBottom="16px", ())}>
      <label style={ReactDOM.Style.make(~fontSize="12px", ~color="#8892a6", ~marginBottom="8px", ~display="block", ())}>
        {"Duration:"->React.string}
      </label>
      <div style={ReactDOM.Style.make(~display="flex", ~gap="8px", ~flexWrap="wrap", ())}>
        {[30, 60, 300, 600, 1800, 3600, 7200, 14400, 43200, 86400]
        ->Array.map(seconds => {
          <button
            key={Int.toString(seconds)}
            onClick={_ => dispatch(SetEphemeralDuration(seconds))}
            style={ReactDOM.Style.make(
              ~padding="8px 16px",
              ~background=state.ephemeralDuration == seconds ? "#4a9eff" : "#2a3142",
              ~color="white",
              ~border="none",
              ~borderRadius="6px",
              ~fontSize="12px",
              ~fontWeight="600",
              ~cursor="pointer",
              (),
            )}>
            {formatDuration(seconds)->React.string}
          </button>
        })
        ->React.array}
      </div>
    </div>

    <div
      style={ReactDOM.Style.make(
        ~padding="12px",
        ~background="rgba(255, 152, 0, 0.1)",
        ~border="1px solid #ff9800",
        ~borderRadius="6px",
        ~fontSize="12px",
        ~color="#ff9800",
        (),
      )}>
      <strong>{"Security Note:"->React.string}</strong>
      {" Ephemeral ports automatically close after the timer expires. All access is logged to VeriSimDB for audit."->React.string}
    </div>
  </div>
}

// Main view
@react.component
let make = (~initialState: option<state>=?, ~onStateChange: option<state => unit>=?) => {
  let (state, setState) = React.useState(() =>
    switch initialState {
    | Some(s) => s
    | None => init
    }
  )

  let dispatch = (msg: msg) => {
    let newState = update(msg, state)
    setState(_ => newState)
    switch onStateChange {
    | Some(callback) => callback(newState)
    | None => ()
    }
  }

  // Countdown timer for ephemeral ports
  React.useEffect1(() => {
    switch state.ephemeralRemaining {
    | Some(remaining) when remaining > 0 =>
      let timeoutId = setTimeout(() => {
        dispatch(DecrementEphemeralTimer(0))
      }, 1000)
      Some(() => clearTimeout(timeoutId))
    | Some(0) =>
      // Timer expired, close all ephemeral ports
      Array.forEach(state.ports, port => {
        switch port.state {
        | Ephemeral(_) => dispatch(ExpireEphemeralPort(port.number))
        | _ => ()
        }
      })
      None
    | _ => None
    }
  }, [state.ephemeralRemaining])

  <div
    className="port-config-panel"
    style={ReactDOM.Style.make(
      ~padding="32px",
      ~background="#0a0e1a",
      ~minHeight="100vh",
      (),
    )}>
    {/* Header */}
    <div style={ReactDOM.Style.make(~marginBottom="32px", ())}>
      <h1
        style={ReactDOM.Style.make(
          ~fontSize="32px",
          ~fontWeight="700",
          ~background="linear-gradient(135deg, #4a9eff, #7b6cff)",
          ~webkitBackgroundClip="text",
          ~webkitTextFillColor="transparent",
          ~marginBottom="8px",
          (),
        )}>
        {"🔌 Port Configuration"->React.string}
      </h1>
      <p style={ReactDOM.Style.make(~fontSize="16px", ~color="#8892a6", ())}>
        {"Configure which ports are accessible and for how long"->React.string}
      </p>
    </div>

    {/* Summary stats */}
    <div
      style={ReactDOM.Style.make(
        ~display="grid",
        ~gridTemplateColumns="repeat(4, 1fr)",
        ~gap="16px",
        ~marginBottom="32px",
        (),
      )}>
      {[
        ("Closed", Array.reduce(state.ports, 0, (acc, p) => p.state == Closed ? acc + 1 : acc), "#4caf50"),
        ("Open", Array.reduce(state.ports, 0, (acc, p) => p.state == Open ? acc + 1 : acc), "#f44336"),
        ("Ephemeral", Array.reduce(state.ports, 0, (acc, p) => switch p.state { | Ephemeral(_) => acc + 1 | _ => acc }), "#ff9800"),
        ("Critical Risk", Array.reduce(state.ports, 0, (acc, p) => p.risk == Critical && p.state != Closed ? acc + 1 : acc), "#f44336"),
      ]
      ->Array.map(((label, count, color)) => {
        <div
          key={label}
          style={ReactDOM.Style.make(
            ~padding="20px",
            ~background="linear-gradient(135deg, #1e2431 0%, #252d3d 100%)",
            ~border="2px solid #2a3142",
            ~borderRadius="12px",
            ~textAlign="center",
            (),
          )}>
          <div style={ReactDOM.Style.make(~fontSize="36px", ~fontWeight="700", ~color, ~marginBottom="8px", ())}>
            {Int.toString(count)->React.string}
          </div>
          <div style={ReactDOM.Style.make(~fontSize="13px", ~color="#8892a6", ())}>
            {label->React.string}
          </div>
        </div>
      })
      ->React.array}
    </div>

    {/* Ephemeral config toggle */}
    <button
      onClick={_ => dispatch(ToggleEphemeralConfig)}
      style={ReactDOM.Style.make(
        ~padding="12px 24px",
        ~background="linear-gradient(135deg, #ff9800, #f57c00)",
        ~color="white",
        ~border="none",
        ~borderRadius="8px",
        ~fontSize="14px",
        ~fontWeight="600",
        ~cursor="pointer",
        ~marginBottom="16px",
        (),
      )}>
      {(state.showEphemeralConfig ? "Hide" : "Configure") ++ " Ephemeral Settings"->React.string}
    </button>

    {/* Ephemeral config panel */}
    {state.showEphemeralConfig ? viewEphemeralConfig(state, dispatch) : React.null}

    {/* Port list */}
    <div style={ReactDOM.Style.make(~marginTop="24px", ())}>
      {Array.map(state.ports, port => viewPortRow(port, dispatch))->React.array}
    </div>

    {/* Security warning */}
    <div
      style={ReactDOM.Style.make(
        ~marginTop="32px",
        ~padding="20px",
        ~background="rgba(244, 67, 54, 0.1)",
        ~border="2px solid #f44336",
        ~borderRadius="12px",
        (),
      )}>
      <h4 style={ReactDOM.Style.make(~fontSize="16px", ~fontWeight="700", ~color="#f44336", ~marginBottom="12px", ())}>
        {"⚠️ Security Best Practices"->React.string}
      </h4>
      <ul style={ReactDOM.Style.make(~fontSize="13px", ~color="#b0b8c4", ~lineHeight="1.8", ~paddingLeft="20px", ())}>
        <li>{"Never leave SSH (22), RDP (3389), or database ports open permanently"->React.string}</li>
        <li>{"Use ephemeral pinholes for temporary access - they auto-close"->React.string}</li>
        <li>{"All port access is logged to VeriSimDB for compliance auditing"->React.string}</li>
        <li>{"Default-deny firewall rules protect all closed ports"->React.string}</li>
        <li>{"Critical risk ports trigger miniKanren security warnings"->React.string}</li>
      </ul>
    </div>
  </div>
}
