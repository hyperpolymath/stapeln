// SPDX-License-Identifier: PMPL-1.0-or-later
// PortConfigPanel.res - Visual port configuration with ephemeral pinholes

// Port states
type portState =
  | Closed
  | Open
  | Ephemeral(int) // Duration in seconds

type portRisk =
  | Critical // SSH (22), RDP (3389), Telnet (23)
  | High // Database ports (3306, 5432, 27017)
  | Medium // HTTP (80), HTTPS (443)
  | Safe // High-numbered ports (>10000)

type port = {
  number: int,
  protocol: string,
  state: portState,
  risk: portRisk,
  description: string,
}

// Backend-synced pinhole record
type pinhole = {
  id: string,
  source: string,
  destination: string,
  port: int,
  protocol: string,
  ttlSeconds: int,
  createdAt: string,
  expiresAt: string,
  status: string,
  reason: string,
}

// Pinhole creation form fields
type pinholeForm = {
  source: string,
  destination: string,
  port: string,
  ttlSeconds: int,
  reason: string,
  protocol: string,
}

let emptyPinholeForm: pinholeForm = {
  source: "",
  destination: "",
  port: "",
  ttlSeconds: 300,
  reason: "",
  protocol: "tcp",
}

type state = {
  ports: array<port>,
  selectedPort: option<int>,
  showEphemeralConfig: bool,
  ephemeralDuration: int, // seconds
  ephemeralRemaining: option<int>, // countdown
  // Pinhole management
  showPinholeManager: bool,
  pinholeForm: pinholeForm,
  activePinholes: array<pinhole>,
  pinholeError: option<string>,
  pinholeLoading: bool,
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
  // Pinhole management messages
  | TogglePinholeManager
  | SetPinholeFormSource(string)
  | SetPinholeFormDestination(string)
  | SetPinholeFormPort(string)
  | SetPinholeFormTtl(int)
  | SetPinholeFormReason(string)
  | SetPinholeFormProtocol(string)
  | PinholesLoaded(array<pinhole>)
  | PinholeCreated(pinhole)
  | PinholeRevoked(string)
  | PinholeError(string)
  | ClearPinholeError
  | SetPinholeLoading(bool)
  | DecrementPinholeTimers

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
  showPinholeManager: false,
  pinholeForm: emptyPinholeForm,
  activePinholes: [],
  pinholeError: None,
  pinholeLoading: false,
}

// Update function
let update = (msg: msg, state: state): state => {
  switch msg {
  | SelectPort(portNum) => {...state, selectedPort: Some(portNum)}

  | SetPortState(portNum, newState) =>
    let updatedPorts = Array.map(state.ports, port =>
      if port.number == portNum {
        {...port, state: newState}
      } else {
        port
      }
    )
    {...state, ports: updatedPorts}

  | SetEphemeralDuration(seconds) => {...state, ephemeralDuration: seconds}

  | ToggleEphemeralConfig => {...state, showEphemeralConfig: !state.showEphemeralConfig}

  | StartEphemeralTimer(_portNum) => {...state, ephemeralRemaining: Some(state.ephemeralDuration)}

  | DecrementEphemeralTimer(_portNum) =>
    switch state.ephemeralRemaining {
    | Some(remaining) if remaining > 0 => {...state, ephemeralRemaining: Some(remaining - 1)}
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

  // Pinhole management
  | TogglePinholeManager => {...state, showPinholeManager: !state.showPinholeManager}

  | SetPinholeFormSource(v) => {
      ...state,
      pinholeForm: {...state.pinholeForm, source: v},
    }
  | SetPinholeFormDestination(v) => {
      ...state,
      pinholeForm: {...state.pinholeForm, destination: v},
    }
  | SetPinholeFormPort(v) => {
      ...state,
      pinholeForm: {...state.pinholeForm, port: v},
    }
  | SetPinholeFormTtl(v) => {
      ...state,
      pinholeForm: {...state.pinholeForm, ttlSeconds: v},
    }
  | SetPinholeFormReason(v) => {
      ...state,
      pinholeForm: {...state.pinholeForm, reason: v},
    }
  | SetPinholeFormProtocol(v) => {
      ...state,
      pinholeForm: {...state.pinholeForm, protocol: v},
    }

  | PinholesLoaded(pinholes) => {
      ...state,
      activePinholes: pinholes,
      pinholeLoading: false,
    }

  | PinholeCreated(p) => {
      ...state,
      activePinholes: Array.concat(state.activePinholes, [p]),
      pinholeForm: emptyPinholeForm,
      pinholeLoading: false,
      pinholeError: None,
    }

  | PinholeRevoked(id) => {
      ...state,
      activePinholes: Belt.Array.keep(state.activePinholes, p => p.id != id),
    }

  | PinholeError(err) => {...state, pinholeError: Some(err), pinholeLoading: false}
  | ClearPinholeError => {...state, pinholeError: None}
  | SetPinholeLoading(v) => {...state, pinholeLoading: v}

  | DecrementPinholeTimers => state // Visual only; actual expiry is server-side
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

// Helper: Format duration with minutes and seconds
let formatCountdown = (seconds: int): string => {
  let mins = seconds / 60
  let secs = mod(seconds, 60)
  if mins > 0 {
    Int.toString(mins) ++ "m " ++ Int.toString(secs) ++ "s"
  } else {
    Int.toString(secs) ++ "s"
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
  | Open => "#f44336" // Red (dangerous)
  | Ephemeral(_) => "#ff9800" // Orange (temporary)
  }
}

// Helper: Parse a pinhole from JSON
let parsePinhole = (json: JSON.t): option<pinhole> => {
  switch json {
  | Object(dict) =>
    let getString = key =>
      switch Dict.get(dict, key) {
      | Some(v) =>
        switch v {
        | String(s) => Some(s)
        | _ => None
        }
      | None => None
      }
    let getInt = key =>
      switch Dict.get(dict, key) {
      | Some(v) =>
        switch v {
        | Number(n) => Some(Float.toInt(n))
        | _ => None
        }
      | None => None
      }

    switch (
      getString("id"),
      getString("source"),
      getString("destination"),
      getInt("port"),
      getString("protocol"),
      getInt("ttl_seconds"),
      getString("created_at"),
      getString("expires_at"),
      getString("status"),
      getString("reason"),
    ) {
    | (
        Some(id),
        Some(source),
        Some(destination),
        Some(port),
        Some(protocol),
        Some(ttlSeconds),
        Some(createdAt),
        Some(expiresAt),
        Some(status),
        Some(reason),
      ) =>
      Some({
        id,
        source,
        destination,
        port,
        protocol,
        ttlSeconds,
        createdAt,
        expiresAt,
        status,
        reason,
      })
    | _ => None
    }
  | _ => None
  }
}

// Helper: Parse pinholes array from API response JSON
let parsePinholesResponse = (json: JSON.t): array<pinhole> => {
  switch json {
  | Object(dict) =>
    switch Dict.get(dict, "pinholes") {
    | Some(arr) =>
      switch arr {
      | Array(items) => Array.keepMap(items, parsePinhole)
      | _ => []
      }
    | None => []
    }
  | _ => []
  }
}

// Helper: Parse single pinhole from create response
let parsePinholeResponse = (json: JSON.t): option<pinhole> => {
  switch json {
  | Object(dict) =>
    switch Dict.get(dict, "pinhole") {
    | Some(p) => parsePinhole(p)
    | None => None
    }
  | _ => None
  }
}

// View: Port row
let viewPortRow = (port: port, dispatch: msg => unit): React.element => {
  <div
    key={Int.toString(port.number)}
    className="port-row"
    style={Sx.make(
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
    )}
  >
    <div style={Sx.make(~display="flex", ~alignItems="center", ~gap="16px", ())}>
      <span style={Sx.make(~fontSize="24px", ())}>
        {riskIndicator(port.risk)->React.string}
      </span>
      <div>
        <div style={Sx.make(~fontSize="16px", ~fontWeight="700", ~color="#e0e6ed", ())}>
          {("Port " ++ Int.toString(port.number))->React.string}
        </div>
        <div style={Sx.make(~fontSize="12px", ~color="#8892a6", ~marginTop="2px", ())}>
          {(port.protocol ++ " - " ++ port.description)->React.string}
        </div>
      </div>
    </div>

    <div style={Sx.make(~display="flex", ~gap="8px", ())}>
      <button
        onClick={_ => dispatch(SetPortState(port.number, Closed))}
        style={Sx.make(
          ~padding="8px 16px",
          ~background=port.state == Closed ? "#4caf50" : "#2a3142",
          ~color="white",
          ~border="none",
          ~borderRadius="6px",
          ~fontSize="12px",
          ~fontWeight="600",
          ~cursor="pointer",
          (),
        )}
      >
        {"Closed"->React.string}
      </button>

      <button
        onClick={_ => dispatch(SetPortState(port.number, Ephemeral(300)))}
        style={Sx.make(
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
        )}
      >
        {"Ephemeral"->React.string}
      </button>

      <button
        onClick={_ => dispatch(SetPortState(port.number, Open))}
        style={Sx.make(
          ~padding="8px 16px",
          ~background=port.state == Open ? "#f44336" : "#2a3142",
          ~color="white",
          ~border="none",
          ~borderRadius="6px",
          ~fontSize="12px",
          ~fontWeight="600",
          ~cursor="pointer",
          (),
        )}
      >
        {"Open"->React.string}
      </button>
    </div>

    <div
      style={Sx.make(
        ~padding="6px 12px",
        ~background=stateColor(port.state),
        ~color="white",
        ~borderRadius="6px",
        ~fontSize="11px",
        ~fontWeight="700",
        ~minWidth="80px",
        ~textAlign="center",
        (),
      )}
    >
      {stateLabel(port.state)->React.string}
    </div>
  </div>
}

// View: Ephemeral configuration panel
let viewEphemeralConfig = (state: state, dispatch: msg => unit): React.element => {
  <div
    className="ephemeral-config"
    style={Sx.make(
      ~padding="24px",
      ~background="#1e2431",
      ~border="2px solid #ff9800",
      ~borderRadius="12px",
      ~marginTop="16px",
      (),
    )}
  >
    <h3
      style={Sx.make(
        ~fontSize="18px",
        ~fontWeight="700",
        ~color="#ff9800",
        ~marginBottom="16px",
        (),
      )}
    >
      {"⏱️ Ephemeral Pinhole Configuration"->React.string}
    </h3>

    <p
      style={Sx.make(
        ~fontSize="13px",
        ~color="#b0b8c4",
        ~marginBottom="16px",
        ~lineHeight="1.6",
        (),
      )}
    >
      {"Ephemeral pinholes automatically close after a set duration. Perfect for temporary access (SSH debugging, database migrations, etc.)."->React.string}
    </p>

    <div style={Sx.make(~marginBottom="16px", ())}>
      <label
        style={Sx.make(
          ~fontSize="12px",
          ~color="#8892a6",
          ~marginBottom="8px",
          ~display="block",
          (),
        )}
      >
        {"Duration:"->React.string}
      </label>
      <div style={Sx.make(~display="flex", ~gap="8px", ~flexWrap="wrap", ())}>
        {[30, 60, 300, 600, 1800, 3600, 7200, 14400, 43200, 86400]
        ->Array.map(seconds => {
          <button
            key={Int.toString(seconds)}
            onClick={_ => dispatch(SetEphemeralDuration(seconds))}
            style={Sx.make(
              ~padding="8px 16px",
              ~background=state.ephemeralDuration == seconds ? "#4a9eff" : "#2a3142",
              ~color="white",
              ~border="none",
              ~borderRadius="6px",
              ~fontSize="12px",
              ~fontWeight="600",
              ~cursor="pointer",
              (),
            )}
          >
            {formatDuration(seconds)->React.string}
          </button>
        })
        ->React.array}
      </div>
    </div>

    <div
      style={Sx.make(
        ~padding="12px",
        ~background="rgba(255, 152, 0, 0.1)",
        ~border="1px solid #ff9800",
        ~borderRadius="6px",
        ~fontSize="12px",
        ~color="#ff9800",
        (),
      )}
    >
      <strong> {"Security Note:"->React.string} </strong>
      {" Ephemeral ports automatically close after the timer expires. All access is logged to VeriSimDB for audit."->React.string}
    </div>
  </div>
}

// Shared input style for pinhole form fields
let inputStyle = Sx.make(
  ~padding="10px 14px",
  ~background="#1a1f2e",
  ~color="#e0e6ed",
  ~border="1px solid #2a3142",
  ~borderRadius="6px",
  ~fontSize="13px",
  ~width="100%",
  (),
)

// Shared label style for pinhole form fields
let labelStyle = Sx.make(
  ~fontSize="12px",
  ~color="#8892a6",
  ~marginBottom="6px",
  ~display="block",
  ~fontWeight="600",
  (),
)

// View: Pinhole creation form
let viewPinholeForm = (state: state, dispatch: msg => unit, onSubmit: unit => unit): React.element => {
  let form = state.pinholeForm

  <div
    style={Sx.make(
      ~padding="24px",
      ~background="#1e2431",
      ~border="1px solid #2a3142",
      ~borderRadius="12px",
      ~marginBottom="24px",
      (),
    )}
  >
    <h4
      style={Sx.make(
        ~fontSize="16px",
        ~fontWeight="700",
        ~color="#4a9eff",
        ~marginBottom="20px",
        (),
      )}
    >
      {"Create Ephemeral Pinhole"->React.string}
    </h4>

    <div
      style={Sx.make(
        ~display="grid",
        ~gridTemplateColumns="1fr 1fr",
        ~gap="16px",
        ~marginBottom="16px",
        (),
      )}
    >
      <div>
        <label style={labelStyle}> {"Source Service:"->React.string} </label>
        <input
          type_="text"
          placeholder="e.g. web-app"
          value={form.source}
          onChange={e => dispatch(SetPinholeFormSource(ReactEvent.Form.target(e)["value"]))}
          style={inputStyle}
        />
      </div>
      <div>
        <label style={labelStyle}> {"Destination Service:"->React.string} </label>
        <input
          type_="text"
          placeholder="e.g. postgres-primary"
          value={form.destination}
          onChange={e => dispatch(SetPinholeFormDestination(ReactEvent.Form.target(e)["value"]))}
          style={inputStyle}
        />
      </div>
      <div>
        <label style={labelStyle}> {"Port:"->React.string} </label>
        <input
          type_="number"
          placeholder="e.g. 5432"
          value={form.port}
          onChange={e => dispatch(SetPinholeFormPort(ReactEvent.Form.target(e)["value"]))}
          style={inputStyle}
        />
      </div>
      <div>
        <label style={labelStyle}> {"Protocol:"->React.string} </label>
        <select
          value={form.protocol}
          onChange={e => dispatch(SetPinholeFormProtocol(ReactEvent.Form.target(e)["value"]))}
          style={inputStyle}
        >
          <option value="tcp"> {"TCP"->React.string} </option>
          <option value="udp"> {"UDP"->React.string} </option>
        </select>
      </div>
    </div>

    <div style={Sx.make(~marginBottom="16px", ())}>
      <label style={labelStyle}> {"Reason:"->React.string} </label>
      <input
        type_="text"
        placeholder="e.g. Database migration, SSH debugging session"
        value={form.reason}
        onChange={e => dispatch(SetPinholeFormReason(ReactEvent.Form.target(e)["value"]))}
        style={inputStyle}
      />
    </div>

    <div style={Sx.make(~marginBottom="20px", ())}>
      <label style={labelStyle}> {"Time-to-Live:"->React.string} </label>
      <div style={Sx.make(~display="flex", ~gap="8px", ~flexWrap="wrap", ())}>
        {[60, 300, 600, 1800, 3600, 7200, 14400, 86400]
        ->Array.map(seconds => {
          <button
            key={Int.toString(seconds)}
            onClick={_ => dispatch(SetPinholeFormTtl(seconds))}
            style={Sx.make(
              ~padding="8px 16px",
              ~background=form.ttlSeconds == seconds ? "#4a9eff" : "#2a3142",
              ~color="white",
              ~border="none",
              ~borderRadius="6px",
              ~fontSize="12px",
              ~fontWeight="600",
              ~cursor="pointer",
              (),
            )}
          >
            {formatDuration(seconds)->React.string}
          </button>
        })
        ->React.array}
      </div>
    </div>

    {switch state.pinholeError {
    | Some(err) =>
      <div
        style={Sx.make(
          ~padding="10px 14px",
          ~background="rgba(244, 67, 54, 0.15)",
          ~border="1px solid #f44336",
          ~borderRadius="6px",
          ~fontSize="12px",
          ~color="#f44336",
          ~marginBottom="16px",
          (),
        )}
      >
        {err->React.string}
      </div>
    | None => React.null
    }}

    <button
      onClick={_ => onSubmit()}
      disabled={state.pinholeLoading ||
      form.source == "" ||
      form.destination == "" ||
      form.port == "" ||
      form.reason == ""}
      style={Sx.make(
        ~padding="12px 24px",
        ~background=state.pinholeLoading ? "#555" : "linear-gradient(135deg, #4a9eff, #357abd)",
        ~color="white",
        ~border="none",
        ~borderRadius="8px",
        ~fontSize="14px",
        ~fontWeight="600",
        ~cursor=state.pinholeLoading ? "not-allowed" : "pointer",
        (),
      )}
    >
      {(state.pinholeLoading ? "Creating..." : "Create Pinhole")->React.string}
    </button>
  </div>
}

// View: Active pinhole row
let viewPinholeRow = (p: pinhole, onRevoke: string => unit): React.element => {
  <div
    key={p.id}
    style={Sx.make(
      ~display="flex",
      ~alignItems="center",
      ~justifyContent="space-between",
      ~padding="14px 16px",
      ~marginBottom="8px",
      ~background="linear-gradient(135deg, #1e2431 0%, #252d3d 100%)",
      ~border="1px solid #ff9800",
      ~borderRadius="10px",
      ~transition="all 0.2s",
      (),
    )}
  >
    <div style={Sx.make(~display="flex", ~alignItems="center", ~gap="14px", ())}>
      <span style={Sx.make(~fontSize="20px", ())}> {"🔓"->React.string} </span>
      <div>
        <div style={Sx.make(~fontSize="14px", ~fontWeight="700", ~color="#e0e6ed", ())}>
          {(p.source ++ " -> " ++ p.destination ++ ":" ++ Int.toString(p.port) ++ "/" ++ p.protocol)->React.string}
        </div>
        <div style={Sx.make(~fontSize="11px", ~color="#8892a6", ~marginTop="2px", ())}>
          {p.reason->React.string}
        </div>
      </div>
    </div>

    <div style={Sx.make(~display="flex", ~alignItems="center", ~gap="12px", ())}>
      <div style={Sx.make(~textAlign="right", ())}>
        <div style={Sx.make(~fontSize="12px", ~color="#ff9800", ~fontWeight="600", ())}>
          {"TTL: "->React.string}
          {formatDuration(p.ttlSeconds)->React.string}
        </div>
        <div style={Sx.make(~fontSize="10px", ~color="#8892a6", ~marginTop="2px", ())}>
          {"Expires: "->React.string}
          {p.expiresAt->React.string}
        </div>
      </div>

      <button
        onClick={_ => onRevoke(p.id)}
        style={Sx.make(
          ~padding="8px 14px",
          ~background="rgba(244, 67, 54, 0.2)",
          ~color="#f44336",
          ~border="1px solid #f44336",
          ~borderRadius="6px",
          ~fontSize="11px",
          ~fontWeight="700",
          ~cursor="pointer",
          (),
        )}
      >
        {"Revoke"->React.string}
      </button>
    </div>
  </div>
}

// View: Active pinholes list
let viewActivePinholes = (
  pinholes: array<pinhole>,
  onRevoke: string => unit,
): React.element => {
  <div style={Sx.make(~marginBottom="24px", ())}>
    <h4
      style={Sx.make(
        ~fontSize="16px",
        ~fontWeight="700",
        ~color="#ff9800",
        ~marginBottom="16px",
        (),
      )}
    >
      {"Active Pinholes ("->React.string}
      {Int.toString(Array.length(pinholes))->React.string}
      {")"->React.string}
    </h4>

    {if Array.length(pinholes) == 0 {
      <div
        style={Sx.make(
          ~padding="24px",
          ~textAlign="center",
          ~color="#555e6e",
          ~fontSize="13px",
          ~background="#1e2431",
          ~borderRadius="10px",
          ~border="1px dashed #2a3142",
          (),
        )}
      >
        {"No active pinholes. Create one above to grant temporary network access."->React.string}
      </div>
    } else {
      <div> {Array.map(pinholes, p => viewPinholeRow(p, onRevoke))->React.array} </div>
    }}
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

  // Fetch active pinholes from backend when manager is opened
  React.useEffect1(() => {
    if state.showPinholeManager {
      dispatch(SetPinholeLoading(true))
      let _ =
        ApiClient.listPinholes()
        ->Promise.then(result => {
          switch result {
          | Ok(json) =>
            let pinholes = parsePinholesResponse(json)
            dispatch(PinholesLoaded(pinholes))
          | Error(err) => dispatch(PinholeError(err))
          }
          Promise.resolve()
        })
        ->ignore
    }
    None
  }, [state.showPinholeManager])

  // Countdown timer for ephemeral ports
  React.useEffect1(() => {
    switch state.ephemeralRemaining {
    | Some(remaining) if remaining > 0 =>
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

  // Handle pinhole creation via API
  let handleCreatePinhole = () => {
    let form = state.pinholeForm
    switch Int.fromString(form.port) {
    | Some(portNum) if portNum > 0 && portNum <= 65535 =>
      dispatch(SetPinholeLoading(true))
      let _ =
        ApiClient.createPinhole(
          ~source=form.source,
          ~destination=form.destination,
          ~port=portNum,
          ~ttlSeconds=form.ttlSeconds,
          ~reason=form.reason,
          ~protocol=form.protocol,
        )
        ->Promise.then(result => {
          switch result {
          | Ok(json) =>
            switch parsePinholeResponse(json) {
            | Some(p) => dispatch(PinholeCreated(p))
            | None => dispatch(PinholeError("Failed to parse pinhole response"))
            }
          | Error(err) => dispatch(PinholeError(err))
          }
          Promise.resolve()
        })
        ->ignore
    | _ => dispatch(PinholeError("Port must be a number between 1 and 65535"))
    }
  }

  // Handle pinhole revocation via API
  let handleRevokePinhole = (id: string) => {
    let _ =
      ApiClient.revokePinhole(id)
      ->Promise.then(result => {
        switch result {
        | Ok(_) => dispatch(PinholeRevoked(id))
        | Error(err) => dispatch(PinholeError(err))
        }
        Promise.resolve()
      })
      ->ignore
  }

  <div
    className="port-config-panel"
    style={Sx.make(~padding="32px", ~background="#0a0e1a", ~minHeight="100vh", ())}
  >
    <div style={Sx.make(~marginBottom="32px", ())}>
      <h1
        style={Sx.make(
          ~fontSize="32px",
          ~fontWeight="700",
          ~background="linear-gradient(135deg, #4a9eff, #7b6cff)",
          ~marginBottom="8px",
          (),
        )}
      >
        {"🔌 Port Configuration"->React.string}
      </h1>
      <p style={Sx.make(~fontSize="16px", ~color="#8892a6", ())}>
        {"Configure which ports are accessible and for how long"->React.string}
      </p>
    </div>

    <div
      style={Sx.make(
        ~display="grid",
        ~gridTemplateColumns="repeat(4, 1fr)",
        ~gap="16px",
        ~marginBottom="32px",
        (),
      )}
    >
      {[
        (
          "Closed",
          Array.reduce(state.ports, 0, (acc, p) => p.state == Closed ? acc + 1 : acc),
          "#4caf50",
        ),
        (
          "Open",
          Array.reduce(state.ports, 0, (acc, p) => p.state == Open ? acc + 1 : acc),
          "#f44336",
        ),
        (
          "Ephemeral",
          Array.reduce(state.ports, 0, (acc, p) =>
            switch p.state {
            | Ephemeral(_) => acc + 1
            | _ => acc
            }
          ),
          "#ff9800",
        ),
        (
          "Critical Risk",
          Array.reduce(state.ports, 0, (acc, p) =>
            p.risk == Critical && p.state != Closed ? acc + 1 : acc
          ),
          "#f44336",
        ),
      ]
      ->Array.map(((label, count, color)) => {
        <div
          key={label}
          style={Sx.make(
            ~padding="20px",
            ~background="linear-gradient(135deg, #1e2431 0%, #252d3d 100%)",
            ~border="2px solid #2a3142",
            ~borderRadius="12px",
            ~textAlign="center",
            (),
          )}
        >
          <div
            style={Sx.make(
              ~fontSize="36px",
              ~fontWeight="700",
              ~color,
              ~marginBottom="8px",
              (),
            )}
          >
            {Int.toString(count)->React.string}
          </div>
          <div style={Sx.make(~fontSize="13px", ~color="#8892a6", ())}>
            {label->React.string}
          </div>
        </div>
      })
      ->React.array}
    </div>

    <div style={Sx.make(~display="flex", ~gap="12px", ~marginBottom="16px", ())}>
      <button
        onClick={_ => dispatch(ToggleEphemeralConfig)}
        style={Sx.make(
          ~padding="12px 24px",
          ~background="linear-gradient(135deg, #ff9800, #f57c00)",
          ~color="white",
          ~border="none",
          ~borderRadius="8px",
          ~fontSize="14px",
          ~fontWeight="600",
          ~cursor="pointer",
          (),
        )}
      >
        {((state.showEphemeralConfig ? "Hide" : "Configure") ++ " Ephemeral Settings")->React.string}
      </button>

      <button
        onClick={_ => dispatch(TogglePinholeManager)}
        style={Sx.make(
          ~padding="12px 24px",
          ~background=state.showPinholeManager
            ? "linear-gradient(135deg, #f57c00, #e65100)"
            : "linear-gradient(135deg, #7b6cff, #5a4fcf)",
          ~color="white",
          ~border="none",
          ~borderRadius="8px",
          ~fontSize="14px",
          ~fontWeight="600",
          ~cursor="pointer",
          (),
        )}
      >
        {((state.showPinholeManager ? "Hide" : "Manage") ++ " Pinholes")->React.string}
      </button>
    </div>

    {state.showEphemeralConfig ? viewEphemeralConfig(state, dispatch) : React.null}

    // Ephemeral pinhole manager section
    {state.showPinholeManager
      ? <div
          style={Sx.make(
            ~padding="24px",
            ~background="#151a28",
            ~border="2px solid #7b6cff",
            ~borderRadius="12px",
            ~marginTop="16px",
            ~marginBottom="16px",
            (),
          )}
        >
          <h3
            style={Sx.make(
              ~fontSize="20px",
              ~fontWeight="700",
              ~color="#7b6cff",
              ~marginBottom="20px",
              (),
            )}
          >
            {"🔐 Ephemeral Pinhole Manager"->React.string}
          </h3>
          <p
            style={Sx.make(
              ~fontSize="13px",
              ~color="#b0b8c4",
              ~marginBottom="20px",
              ~lineHeight="1.6",
              (),
            )}
          >
            {"Create temporary, scoped network access rules between services. Pinholes auto-expire after their TTL and are logged for audit."->React.string}
          </p>
          {viewPinholeForm(state, dispatch, handleCreatePinhole)}
          {viewActivePinholes(state.activePinholes, handleRevokePinhole)}
        </div>
      : React.null}

    <div style={Sx.make(~marginTop="24px", ())}>
      {Array.map(state.ports, port => viewPortRow(port, dispatch))->React.array}
    </div>

    <div
      style={Sx.make(
        ~marginTop="32px",
        ~padding="20px",
        ~background="rgba(244, 67, 54, 0.1)",
        ~border="2px solid #f44336",
        ~borderRadius="12px",
        (),
      )}
    >
      <h4
        style={Sx.make(
          ~fontSize="16px",
          ~fontWeight="700",
          ~color="#f44336",
          ~marginBottom="12px",
          (),
        )}
      >
        {"⚠️ Security Best Practices"->React.string}
      </h4>
      <ul
        style={Sx.make(
          ~fontSize="13px",
          ~color="#b0b8c4",
          ~lineHeight="1.8",
          ~paddingLeft="20px",
          (),
        )}
      >
        <li>
          {"Never leave SSH (22), RDP (3389), or database ports open permanently"->React.string}
        </li>
        <li> {"Use ephemeral pinholes for temporary access - they auto-close"->React.string} </li>
        <li> {"All port access is logged to VeriSimDB for compliance auditing"->React.string} </li>
        <li> {"Default-deny firewall rules protect all closed ports"->React.string} </li>
        <li> {"Critical risk ports trigger miniKanren security warnings"->React.string} </li>
      </ul>
    </div>
  </div>
}
