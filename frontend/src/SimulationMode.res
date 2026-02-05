// SPDX-License-Identifier: PMPL-1.0-or-later
// SimulationMode.res - Cisco Packet Tracer-style packet animation

// Packet types
type packetType =
  | HTTP
  | HTTPS
  | TCP
  | UDP
  | ICMP
  | DNS

// Packet state during animation
type packetStatus =
  | InTransit
  | Delivered
  | Dropped
  | Blocked

type packet = {
  id: string,
  packetType: packetType,
  status: packetStatus,
  sourceNode: string,
  targetNode: string,
  payload: string,
  size: int, // bytes
  encrypted: bool,
  timestamp: float,
  progress: float, // 0.0 to 1.0
  position: (float, float), // (x, y) coordinates
}

// Network event types
type networkEvent =
  | PacketSent(string, string, packetType)
  | PacketReceived(string)
  | PacketDropped(string, string) // packet id, reason
  | ConnectionEstablished(string, string)
  | ConnectionClosed(string, string)
  | FirewallBlock(string, string, int) // source, target, port
  | LatencySpike(string, float)

// Simulation speed
type simSpeed =
  | Paused
  | Slow      // 0.5x
  | Normal    // 1.0x
  | Fast      // 2.0x
  | VeryFast  // 4.0x

// Visualization mode
type vizMode =
  | PacketView      // Show individual packets
  | FlowView        // Show aggregated flows
  | HeatmapView     // Show traffic intensity
  | SecurityView    // Highlight security events

// Node position (for animation path calculation)
type nodePosition = {
  id: string,
  name: string,
  x: float,
  y: float,
}

type state = {
  nodes: array<nodePosition>,
  packets: array<packet>,
  events: array<networkEvent>,
  isRunning: bool,
  speed: simSpeed,
  mode: vizMode,
  currentTime: float,
  totalPacketsSent: int,
  totalPacketsDelivered: int,
  totalPacketsDropped: int,
  avgLatency: float,
  showStats: bool,
  selectedPacket: option<string>,
  showEventLog: bool,
}

// Message types
type msg =
  | StartSimulation
  | PauseSimulation
  | StopSimulation
  | SetSpeed(simSpeed)
  | SetMode(vizMode)
  | SendPacket(string, string, packetType)
  | UpdatePacketPositions(float) // delta time
  | PacketArrived(string)
  | DropPacket(string, string)
  | SelectPacket(string)
  | ToggleStats
  | ToggleEventLog
  | ClearEvents

// Initialize with sample nodes
let init: state = {
  nodes: [
    {id: "node-1", name: "API Gateway", x: 150.0, y: 250.0},
    {id: "node-2", name: "Auth Service", x: 450.0, y: 250.0},
    {id: "node-3", name: "PostgreSQL", x: 750.0, y: 250.0},
    {id: "node-4", name: "Redis Cache", x: 450.0, y: 100.0},
    {id: "node-5", name: "Load Balancer", x: 300.0, y: 400.0},
  ],
  packets: [],
  events: [],
  isRunning: false,
  speed: Normal,
  mode: PacketView,
  currentTime: 0.0,
  totalPacketsSent: 0,
  totalPacketsDelivered: 0,
  totalPacketsDropped: 0,
  avgLatency: 0.0,
  showStats: true,
  selectedPacket: None,
  showEventLog: true,
}

// Update function
let update = (msg: msg, state: state): state => {
  switch msg {
  | StartSimulation =>
    {...state, isRunning: true}

  | PauseSimulation =>
    {...state, isRunning: false}

  | StopSimulation =>
    {
      ...state,
      isRunning: false,
      packets: [],
      events: [],
      currentTime: 0.0,
    }

  | SetSpeed(speed) =>
    {...state, speed: speed}

  | SetMode(mode) =>
    {...state, mode: mode}

  | SendPacket(sourceId, targetId, packetType) =>
    let sourceNode = Array.find(state.nodes, n => n.id == sourceId)
    let targetNode = Array.find(state.nodes, n => n.id == targetId)

    switch (sourceNode, targetNode) {
    | (Some(src), Some(tgt)) =>
      let packetId = "packet-" ++ Float.toString(Date.now())
      let newPacket = {
        id: packetId,
        packetType: packetType,
        status: InTransit,
        sourceNode: sourceId,
        targetNode: targetId,
        payload: "Sample payload data",
        size: 1500, // bytes
        encrypted: packetType == HTTPS,
        timestamp: state.currentTime,
        progress: 0.0,
        position: (src.x, src.y),
      }

      let event = PacketSent(sourceId, targetId, packetType)

      {
        ...state,
        packets: Array.concat(state.packets, [newPacket]),
        events: Array.concat(state.events, [event]),
        totalPacketsSent: state.totalPacketsSent + 1,
      }
    | _ => state
    }

  | UpdatePacketPositions(deltaTime) =>
    let speedMultiplier = switch state.speed {
    | Paused => 0.0
    | Slow => 0.5
    | Normal => 1.0
    | Fast => 2.0
    | VeryFast => 4.0
    }

    let updatedPackets = Array.map(state.packets, packet => {
      if packet.status == InTransit {
        let newProgress = packet.progress +. (deltaTime *. speedMultiplier *. 0.001)

        if newProgress >= 1.0 {
          // Packet arrived
          {...packet, progress: 1.0, status: Delivered}
        } else {
          // Calculate interpolated position
          let sourceNode = Array.find(state.nodes, n => n.id == packet.sourceNode)
          let targetNode = Array.find(state.nodes, n => n.id == packet.targetNode)

          switch (sourceNode, targetNode) {
          | (Some(src), Some(tgt)) =>
            let x = src.x +. (tgt.x -. src.x) *. newProgress
            let y = src.y +. (tgt.y -. src.y) *. newProgress
            {...packet, progress: newProgress, position: (x, y)}
          | _ => packet
          }
        }
      } else {
        packet
      }
    })

    // Remove delivered packets after delay
    let filteredPackets = Array.filter(updatedPackets, p =>
      p.status != Delivered || p.progress < 1.2
    )

    {...state, packets: filteredPackets, currentTime: state.currentTime +. deltaTime}

  | PacketArrived(packetId) =>
    let updatedPackets = Array.map(state.packets, packet =>
      if packet.id == packetId {
        {...packet, status: Delivered}
      } else {
        packet
      }
    )

    {
      ...state,
      packets: updatedPackets,
      totalPacketsDelivered: state.totalPacketsDelivered + 1,
    }

  | DropPacket(packetId, reason) =>
    let updatedPackets = Array.map(state.packets, packet =>
      if packet.id == packetId {
        {...packet, status: Dropped}
      } else {
        packet
      }
    )

    let event = PacketDropped(packetId, reason)

    {
      ...state,
      packets: updatedPackets,
      events: Array.concat(state.events, [event]),
      totalPacketsDropped: state.totalPacketsDropped + 1,
    }

  | SelectPacket(packetId) =>
    {...state, selectedPacket: Some(packetId)}

  | ToggleStats =>
    {...state, showStats: !state.showStats}

  | ToggleEventLog =>
    {...state, showEventLog: !state.showEventLog}

  | ClearEvents =>
    {...state, events: []}
  }
}

// Helper: Get packet type color
let packetTypeColor = (packetType: packetType): string => {
  switch packetType {
  | HTTP => "#4a9eff"
  | HTTPS => "#4caf50"
  | TCP => "#9c27b0"
  | UDP => "#ff9800"
  | ICMP => "#f44336"
  | DNS => "#00bcd4"
  }
}

// Helper: Get packet type label
let packetTypeLabel = (packetType: packetType): string => {
  switch packetType {
  | HTTP => "HTTP"
  | HTTPS => "HTTPS"
  | TCP => "TCP"
  | UDP => "UDP"
  | ICMP => "ICMP"
  | DNS => "DNS"
  }
}

// Helper: Get status color
let statusColor = (status: packetStatus): string => {
  switch status {
  | InTransit => "#4a9eff"
  | Delivered => "#4caf50"
  | Dropped => "#f44336"
  | Blocked => "#ff9800"
  }
}

// Helper: Get speed label
let speedLabel = (speed: simSpeed): string => {
  switch speed {
  | Paused => "Paused"
  | Slow => "0.5x"
  | Normal => "1.0x"
  | Fast => "2.0x"
  | VeryFast => "4.0x"
  }
}

// View: Network node
let viewNode = (node: nodePosition): React.element => {
  <div
    key={node.id}
    style={ReactDOM.Style.make(
      ~position="absolute",
      ~left=Float.toString(node.x -. 50.0) ++ "px",
      ~top=Float.toString(node.y -. 40.0) ++ "px",
      ~width="100px",
      ~height="80px",
      ~display="flex",
      ~flexDirection="column",
      ~alignItems="center",
      ~justifyContent="center",
      ~background="linear-gradient(135deg, #1e2431 0%, #252d3d 100%)",
      ~border="2px solid #4a9eff",
      ~borderRadius="12px",
      ~padding="8px",
      ~zIndex="10",
      (),
    )}>
    <div style={ReactDOM.Style.make(~fontSize="24px", ~marginBottom="4px", ())}>
      {"🖥️"->React.string}
    </div>
    <div
      style={ReactDOM.Style.make(
        ~fontSize="11px",
        ~fontWeight="600",
        ~color="#e0e6ed",
        ~textAlign="center",
        ~lineHeight="1.2",
        (),
      )}>
      {node.name->React.string}
    </div>
  </div>
}

// View: Animated packet
let viewPacket = (packet: packet, dispatch: msg => unit): React.element => {
  let (x, y) = packet.position

  <div
    key={packet.id}
    onClick={_ => dispatch(SelectPacket(packet.id))}
    style={ReactDOM.Style.make(
      ~position="absolute",
      ~left=Float.toString(x -. 15.0) ++ "px",
      ~top=Float.toString(y -. 15.0) ++ "px",
      ~width="30px",
      ~height="30px",
      ~background=packetTypeColor(packet.packetType),
      ~border="2px solid white",
      ~borderRadius="50%",
      ~display="flex",
      ~alignItems="center",
      ~justifyContent="center",
      ~fontSize="14px",
      ~cursor="pointer",
      ~zIndex="20",
      ~opacity=switch packet.status {
      | InTransit => "1.0"
      | Delivered => "0.3"
      | Dropped => "0.5"
      | Blocked => "0.5"
      },
      ~transition="all 0.1s",
      ~boxShadow="0 0 10px " ++ packetTypeColor(packet.packetType),
      (),
    )}>
    {(packet.encrypted ? "🔒" : "📦")->React.string}
  </div>
}

// View: Event log entry
let viewEvent = (event: networkEvent, index: int): React.element => {
  let (icon, color, message) = switch event {
  | PacketSent(src, tgt, pType) =>
    ("📤", "#4a9eff", `Packet sent: ${src} → ${tgt} (${packetTypeLabel(pType)})`)
  | PacketReceived(packetId) =>
    ("📥", "#4caf50", `Packet received: ${packetId}`)
  | PacketDropped(packetId, reason) =>
    ("❌", "#f44336", `Packet dropped: ${packetId} - ${reason}`)
  | ConnectionEstablished(src, tgt) =>
    ("🔗", "#4caf50", `Connection: ${src} ↔ ${tgt}`)
  | ConnectionClosed(src, tgt) =>
    ("🔌", "#9e9e9e", `Disconnected: ${src} ↔ ${tgt}`)
  | FirewallBlock(src, tgt, port) =>
    ("🚫", "#ff9800", `Firewall blocked: ${src} → ${tgt}:${Int.toString(port)}`)
  | LatencySpike(node, latency) =>
    ("⚠️", "#ff9800", `Latency spike: ${node} (${Float.toString(latency)}ms)`)
  }

  <div
    key={Int.toString(index)}
    style={ReactDOM.Style.make(
      ~padding="8px 12px",
      ~background="rgba(255, 255, 255, 0.05)",
      ~borderLeft="3px solid " ++ color,
      ~marginBottom="4px",
      ~fontSize="12px",
      ~color="#b0b8c4",
      ~fontFamily="monospace",
      (),
    )}>
    <span style={ReactDOM.Style.make(~marginRight="8px", ())}>
      {icon->React.string}
    </span>
    {message->React.string}
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

  // Animation loop
  React.useEffect1(() => {
    if state.isRunning {
      let animationId = ref(0)
      let lastTime = ref(Date.now())

      let rec animate = () => {
        let currentTime = Date.now()
        let deltaTime = currentTime -. lastTime.contents
        lastTime := currentTime

        dispatch(UpdatePacketPositions(deltaTime))
        animationId := requestAnimationFrame(animate)
      }

      animationId := requestAnimationFrame(animate)
      Some(() => cancelAnimationFrame(animationId.contents))
    } else {
      None
    }
  }, [state.isRunning])

  <div
    className="simulation-mode"
    style={ReactDOM.Style.make(
      ~padding="32px",
      ~background="#0a0e1a",
      ~minHeight="100vh",
      (),
    )}>
    {/* Header */}
    <div style={ReactDOM.Style.make(~marginBottom="24px", ())}>
      <h1
        style={ReactDOM.Style.make(
          ~fontSize="32px",
          ~fontWeight="700",
          ~background="linear-gradient(135deg, #4a9eff, #00bcd4)",
          ~WebkitBackgroundClip="text",
          ~WebkitTextFillColor="transparent",
          ~marginBottom="8px",
          (),
        )}>
        {"🎮 Simulation Mode"->React.string}
      </h1>
      <p style={ReactDOM.Style.make(~fontSize="16px", ~color="#8892a6", ())}>
        {"Cisco Packet Tracer-style network simulation"->React.string}
      </p>
    </div>

    {/* Control panel */}
    <div
      style={ReactDOM.Style.make(
        ~display="flex",
        ~gap="16px",
        ~padding="16px",
        ~background="linear-gradient(135deg, #1e2431 0%, #252d3d 100%)",
        ~border="2px solid #2a3142",
        ~borderRadius="12px",
        ~marginBottom="24px",
        (),
      )}>
      {/* Playback controls */}
      <div style={ReactDOM.Style.make(~display="flex", ~gap="8px", ())}>
        {!state.isRunning
          ? <button
              onClick={_ => dispatch(StartSimulation)}
              style={ReactDOM.Style.make(
                ~padding="10px 20px",
                ~background="linear-gradient(135deg, #4caf50, #66bb6a)",
                ~color="white",
                ~border="none",
                ~borderRadius="8px",
                ~fontSize="14px",
                ~fontWeight="600",
                ~cursor="pointer",
                (),
              )}>
              {"▶️ Start"->React.string}
            </button>
          : <button
              onClick={_ => dispatch(PauseSimulation)}
              style={ReactDOM.Style.make(
                ~padding="10px 20px",
                ~background="linear-gradient(135deg, #ff9800, #ffb74d)",
                ~color="white",
                ~border="none",
                ~borderRadius="8px",
                ~fontSize="14px",
                ~fontWeight="600",
                ~cursor="pointer",
                (),
              )}>
              {"⏸️ Pause"->React.string}
            </button>}

        <button
          onClick={_ => dispatch(StopSimulation)}
          style={ReactDOM.Style.make(
            ~padding="10px 20px",
            ~background="linear-gradient(135deg, #f44336, #e57373)",
            ~color="white",
            ~border="none",
            ~borderRadius="8px",
            ~fontSize="14px",
            ~fontWeight="600",
            ~cursor="pointer",
            (),
          )}>
          {"⏹️ Stop"->React.string}
        </button>
      </div>

      {/* Speed controls */}
      <div style={ReactDOM.Style.make(~display="flex", ~gap="8px", ())}>
        {[Slow, Normal, Fast, VeryFast]
        ->Array.map(speed =>
          <button
            key={speedLabel(speed)}
            onClick={_ => dispatch(SetSpeed(speed))}
            style={ReactDOM.Style.make(
              ~padding="8px 16px",
              ~background=state.speed == speed ? "#4a9eff" : "#2a3142",
              ~color="white",
              ~border="none",
              ~borderRadius="6px",
              ~fontSize="13px",
              ~fontWeight="600",
              ~cursor="pointer",
              (),
            )}>
            {speedLabel(speed)->React.string}
          </button>
        )
        ->React.array}
      </div>

      {/* Packet injection */}
      <button
        onClick={_ => dispatch(SendPacket("node-1", "node-3", HTTPS))}
        style={ReactDOM.Style.make(
          ~padding="10px 20px",
          ~background="linear-gradient(135deg, #4a9eff, #7b6cff)",
          ~color="white",
          ~border="none",
          ~borderRadius="8px",
          ~fontSize="14px",
          ~fontWeight="600",
          ~cursor="pointer",
          (),
        )}>
        {"📤 Send Test Packet"->React.string}
      </button>
    </div>

    {/* Main canvas area */}
    <div style={ReactDOM.Style.make(~display="flex", ~gap="24px", ())}>
      {/* Canvas */}
      <div
        style={ReactDOM.Style.make(
          ~flex="1",
          ~position="relative",
          ~height="600px",
          ~background="linear-gradient(135deg, #0a0e1a 0%, #1e2431 100%)",
          ~border="2px solid #2a3142",
          ~borderRadius="16px",
          ~overflow="hidden",
          (),
        )}>
        {/* Render nodes */}
        {Array.map(state.nodes, node => viewNode(node))->React.array}

        {/* Render packets */}
        {Array.map(state.packets, packet => viewPacket(packet, dispatch))->React.array}

        {/* Connection lines (static) */}
        <svg
          style={ReactDOM.Style.make(
            ~position="absolute",
            ~top="0",
            ~left="0",
            ~width="100%",
            ~height="100%",
            ~pointerEvents="none",
            ~zIndex="5",
            (),
          )}>
          {/* Example: node-1 to node-2 */}
          <line
            x1="150"
            y1="250"
            x2="450"
            y2="250"
            stroke="#2a3142"
            strokeWidth="2"
            strokeDasharray="5,5"
          />
          {/* Example: node-2 to node-3 */}
          <line
            x1="450"
            y1="250"
            x2="750"
            y2="250"
            stroke="#2a3142"
            strokeWidth="2"
            strokeDasharray="5,5"
          />
        </svg>
      </div>

      {/* Side panels */}
      <div style={ReactDOM.Style.make(~width="300px", ~display="flex", ~flexDirection="column", ~gap="16px", ())}>
        {/* Stats panel */}
        {state.showStats
          ? <div
              style={ReactDOM.Style.make(
                ~padding="20px",
                ~background="linear-gradient(135deg, #1e2431 0%, #252d3d 100%)",
                ~border="2px solid #2a3142",
                ~borderRadius="12px",
                (),
              )}>
              <h3
                style={ReactDOM.Style.make(
                  ~fontSize="16px",
                  ~fontWeight="700",
                  ~color="#e0e6ed",
                  ~marginBottom="16px",
                  (),
                )}>
                {"📊 Statistics"->React.string}
              </h3>

              <div style={ReactDOM.Style.make(~display="flex", ~flexDirection="column", ~gap="12px", ())}>
                <div>
                  <div style={ReactDOM.Style.make(~fontSize="24px", ~fontWeight="700", ~color="#4a9eff", ())}>
                    {Int.toString(state.totalPacketsSent)->React.string}
                  </div>
                  <div style={ReactDOM.Style.make(~fontSize="12px", ~color="#8892a6", ())}>
                    {"Packets Sent"->React.string}
                  </div>
                </div>

                <div>
                  <div style={ReactDOM.Style.make(~fontSize="24px", ~fontWeight="700", ~color="#4caf50", ())}>
                    {Int.toString(state.totalPacketsDelivered)->React.string}
                  </div>
                  <div style={ReactDOM.Style.make(~fontSize="12px", ~color="#8892a6", ())}>
                    {"Delivered"->React.string}
                  </div>
                </div>

                <div>
                  <div style={ReactDOM.Style.make(~fontSize="24px", ~fontWeight="700", ~color="#f44336", ())}>
                    {Int.toString(state.totalPacketsDropped)->React.string}
                  </div>
                  <div style={ReactDOM.Style.make(~fontSize="12px", ~color="#8892a6", ())}>
                    {"Dropped"->React.string}
                  </div>
                </div>

                <div>
                  <div style={ReactDOM.Style.make(~fontSize="24px", ~fontWeight="700", ~color="#ff9800", ())}>
                    {Int.toString(Array.length(state.packets))->React.string}
                  </div>
                  <div style={ReactDOM.Style.make(~fontSize="12px", ~color="#8892a6", ())}>
                    {"In Transit"->React.string}
                  </div>
                </div>
              </div>
            </div>
          : React.null}

        {/* Event log panel */}
        {state.showEventLog
          ? <div
              style={ReactDOM.Style.make(
                ~flex="1",
                ~padding="20px",
                ~background="linear-gradient(135deg, #1e2431 0%, #252d3d 100%)",
                ~border="2px solid #2a3142",
                ~borderRadius="12px",
                ~overflow="auto",
                ~maxHeight="400px",
                (),
              )}>
              <div style={ReactDOM.Style.make(~display="flex", ~justifyContent="space-between", ~marginBottom="12px", ())}>
                <h3 style={ReactDOM.Style.make(~fontSize="16px", ~fontWeight="700", ~color="#e0e6ed", ())}>
                  {"📋 Event Log"->React.string}
                </h3>
                <button
                  onClick={_ => dispatch(ClearEvents)}
                  style={ReactDOM.Style.make(
                    ~padding="4px 12px",
                    ~background="#2a3142",
                    ~color="#8892a6",
                    ~border="none",
                    ~borderRadius="4px",
                    ~fontSize="11px",
                    ~cursor="pointer",
                    (),
                  )}>
                  {"Clear"->React.string}
                </button>
              </div>

              {Array.length(state.events) > 0
                ? Array.mapWithIndex(state.events, (event, index) =>
                    viewEvent(event, index)
                  )->React.array
                : <div style={ReactDOM.Style.make(~fontSize="12px", ~color="#8892a6", ~textAlign="center", ~padding="20px", ())}>
                    {"No events yet"->React.string}
                  </div>}
            </div>
          : React.null}
      </div>
    </div>

    {/* Info note */}
    <div
      style={ReactDOM.Style.make(
        ~marginTop="24px",
        ~padding="16px",
        ~background="rgba(74, 158, 255, 0.1)",
        ~border="2px solid #4a9eff",
        ~borderRadius="12px",
        (),
      )}>
      <strong style={ReactDOM.Style.make(~fontSize="13px", ~color="#4a9eff", ())}>
        {"💡 Simulation Tips: "->React.string}
      </strong>
      <span style={ReactDOM.Style.make(~fontSize="13px", ~color="#b0b8c4", ())}>
        {"Click packets for details. Adjust speed to observe traffic patterns. Use test packets to verify connectivity."->React.string}
      </span>
    </div>
  </div>
}
