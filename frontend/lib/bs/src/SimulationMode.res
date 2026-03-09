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
  | CapacityDrop(int) // packets dropped due to simulation cap
  | ConnectionEstablished(string, string)
  | ConnectionClosed(string, string)
  | FirewallBlock(string, string, int) // source, target, port
  | LatencySpike(string, float)

// Simulation speed
type simSpeed =
  | Paused
  | Slow // 0.5x
  | Normal // 1.0x
  | Fast // 2.0x
  | VeryFast // 4.0x

// Visualization mode
type vizMode =
  | PacketView // Show individual packets
  | FlowView // Show aggregated flows
  | HeatmapView // Show traffic intensity
  | SecurityView // Highlight security events

// Node position (for animation path calculation)
type nodePosition = {
  id: string,
  name: string,
  x: float,
  y: float,
}

// Configurable simulation parameters
type simParams = {
  packetRate: float, // packets per second (auto-generation rate)
  latencyMs: float, // base latency in milliseconds
  dropRate: float, // probability 0.0 to 1.0
  jitterMs: float, // random latency variance in ms
}

// Network condition presets
type networkPreset =
  | Ideal
  | Normal
  | Congested
  | Lossy

let presetLabel = (preset: networkPreset): string => {
  switch preset {
  | Ideal => "Ideal"
  | Normal => "Normal"
  | Congested => "Congested"
  | Lossy => "Lossy"
  }
}

let presetParams = (preset: networkPreset): simParams => {
  switch preset {
  | Ideal => {packetRate: 2.0, latencyMs: 5.0, dropRate: 0.0, jitterMs: 0.0}
  | Normal => {packetRate: 4.0, latencyMs: 50.0, dropRate: 0.02, jitterMs: 10.0}
  | Congested => {packetRate: 10.0, latencyMs: 200.0, dropRate: 0.1, jitterMs: 80.0}
  | Lossy => {packetRate: 3.0, latencyMs: 100.0, dropRate: 0.25, jitterMs: 50.0}
  }
}

let defaultParams = presetParams(Normal)

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
  totalLatency: float, // sum of all delivered packet latencies
  throughput: float, // packets delivered per second
  showStats: bool,
  selectedPacket: option<string>,
  showEventLog: bool,
  simParams: simParams,
  activePreset: option<networkPreset>,
  autoGenerate: bool, // whether to auto-generate packets
  lastGenTime: float, // timestamp of last auto-generated packet
}

// Message types
type msg =
  | StartSimulation
  | PauseSimulation
  | StopSimulation
  | StepSimulation // single-step: advance one frame
  | SetSpeed(simSpeed)
  | SetMode(vizMode)
  | SendPacket(string, string, packetType)
  | UpdatePacketPositions(float) // delta time
  | PacketArrived(string)
  | DropPacket(string, string)
  | SelectPacket(string)
  | DeselectPacket
  | ToggleStats
  | ToggleEventLog
  | ClearEvents
  | SetSimParams(simParams)
  | ApplyPreset(networkPreset)
  | ToggleAutoGenerate
  | SetPacketRate(float)
  | SetLatency(float)
  | SetDropRate(float)
  | SetJitter(float)

let maxActivePackets = 2000
let maxEventLogEntries = 1000

let keepLastN = (items: array<'a>, maxCount: int): (array<'a>, int) => {
  let overflow = Array.length(items) - maxCount
  if overflow <= 0 {
    (items, 0)
  } else {
    // Drop the oldest entries and keep a sliding window of recent data.
    (Belt.Array.keepWithIndex(items, (_, idx) => idx >= overflow), overflow)
  }
}

let enforcePacketCap = (packets: array<packet>): (array<packet>, int) =>
  keepLastN(packets, maxActivePackets)

let enforceEventCap = (events: array<networkEvent>): (array<networkEvent>, int) =>
  keepLastN(events, maxEventLogEntries)

@module("./PacketBatchKernel.js")
external stepPacketsBatchExternal: (
  ~packets: array<packet>,
  ~nodesById: dict<nodePosition>,
  ~step: float,
) => array<packet> = "stepPacketsBatch"

@module("./PacketBatchKernel.js")
external isBatchWasmActive: unit => bool = "isBatchWasmActive"

// Packet stepping runs as a batch kernel in JS/WASM to avoid per-packet bridge overhead.
let stepPacketsKernel = (
  ~packets: array<packet>,
  ~nodesById: dict<nodePosition>,
  ~step: float,
): array<packet> => stepPacketsBatchExternal(~packets, ~nodesById, ~step)

// Edge definitions for the network topology (for SVG lines and path-finding)
type edge = {
  fromNode: string,
  toNode: string,
}

let edges: array<edge> = [
  {fromNode: "node-1", toNode: "node-2"},
  {fromNode: "node-2", toNode: "node-3"},
  {fromNode: "node-2", toNode: "node-4"},
  {fromNode: "node-1", toNode: "node-5"},
  {fromNode: "node-5", toNode: "node-2"},
]

// Route pairs for auto-generated packets: (source, target, packetType)
let routePool: array<(string, string, packetType)> = [
  ("node-1", "node-3", HTTPS),
  ("node-1", "node-2", HTTP),
  ("node-5", "node-3", TCP),
  ("node-2", "node-4", TCP),
  ("node-1", "node-4", DNS),
  ("node-5", "node-2", UDP),
  ("node-3", "node-2", TCP),
  ("node-4", "node-2", ICMP),
  ("node-1", "node-5", HTTPS),
  ("node-2", "node-3", HTTP),
]

// Pseudo-random from timestamp to avoid needing a random seed
let pseudoRandom = (seed: float): float => {
  let x = Math.sin(seed *. 9301.0 +. 49297.0) *. 233280.0
  x -. Math.floor(x)
}

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
  totalLatency: 0.0,
  throughput: 0.0,
  showStats: true,
  selectedPacket: None,
  showEventLog: true,
  simParams: defaultParams,
  activePreset: Some(Normal),
  autoGenerate: true,
  lastGenTime: 0.0,
}

// Update function
let rec update = (msg: msg, state: state): state => {
  switch msg {
  | StartSimulation => {...state, isRunning: true, lastGenTime: state.currentTime}

  | PauseSimulation => {...state, isRunning: false}

  | StopSimulation => {
      ...state,
      isRunning: false,
      packets: [],
      events: [],
      currentTime: 0.0,
      totalPacketsSent: 0,
      totalPacketsDelivered: 0,
      totalPacketsDropped: 0,
      avgLatency: 0.0,
      totalLatency: 0.0,
      throughput: 0.0,
      lastGenTime: 0.0,
    }

  | StepSimulation =>
    // Single-step: advance one 16ms frame while paused
    let stepped = update(UpdatePacketPositions(16.67), {...state, speed: Normal})
    {...stepped, speed: state.speed, isRunning: false}

  | SetSpeed(speed) => {...state, speed}

  | SetMode(mode) => {...state, mode}

  | SendPacket(sourceId, targetId, pktType) =>
    let sourceNode = Belt.Array.getBy(state.nodes, n => n.id == sourceId)
    let _targetNode = Belt.Array.getBy(state.nodes, n => n.id == targetId)

    switch (sourceNode, _targetNode) {
    | (Some(src), Some(_tgt)) =>
      let packetId =
        "pkt-" ++ Float.toString(Date.now()) ++ "-" ++ Int.toString(state.totalPacketsSent)
      let newPacket = {
        id: packetId,
        packetType: pktType,
        status: InTransit,
        sourceNode: sourceId,
        targetNode: targetId,
        payload: "Sample payload data",
        size: 1500,
        encrypted: pktType == HTTPS,
        timestamp: state.currentTime,
        progress: 0.0,
        position: (src.x, src.y),
      }

      let nextPackets = Array.concat(state.packets, [newPacket])
      let (cappedPackets, droppedForCap) = enforcePacketCap(nextPackets)
      let nextEvents = if droppedForCap > 0 {
        Array.concat(state.events, [
          PacketSent(sourceId, targetId, pktType),
          CapacityDrop(droppedForCap),
        ])
      } else {
        Array.concat(state.events, [PacketSent(sourceId, targetId, pktType)])
      }
      let (cappedEvents, _) = enforceEventCap(nextEvents)

      {
        ...state,
        packets: cappedPackets,
        events: cappedEvents,
        totalPacketsSent: state.totalPacketsSent + 1,
        totalPacketsDropped: state.totalPacketsDropped + droppedForCap,
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

    let step = deltaTime *. speedMultiplier *. 0.001

    // Build a node lookup once per frame and reuse it across packet updates.
    let nodesById = Dict.make()
    Array.forEach(state.nodes, node => Dict.set(nodesById, node.id, node))

    let updatedPackets = stepPacketsKernel(~packets=state.packets, ~nodesById, ~step)

    // Check for newly arrived packets and apply drop-rate simulation on arrival
    let newDelivered = ref(0)
    let newDropped = ref(0)
    let newLatencySum = ref(0.0)
    let arrivalEvents: ref<array<networkEvent>> = ref([])

    let processedPackets = Array.map(updatedPackets, p => {
      if p.status == InTransit && p.progress >= 1.0 {
        // Decide whether to drop based on simulated drop rate
        let rand = pseudoRandom(
          p.timestamp +. state.currentTime +. Float.fromInt(state.totalPacketsSent),
        )
        if rand < state.simParams.dropRate {
          newDropped := newDropped.contents + 1
          arrivalEvents :=
            Array.concat(arrivalEvents.contents, [PacketDropped(p.id, "simulated drop")])
          {...p, status: Dropped}
        } else {
          let latency =
            state.simParams.latencyMs +.
              pseudoRandom(p.timestamp *. 7.0) *. state.simParams.jitterMs
          newDelivered := newDelivered.contents + 1
          newLatencySum := newLatencySum.contents +. latency
          arrivalEvents := Array.concat(arrivalEvents.contents, [PacketReceived(p.id)])
          {...p, status: Delivered}
        }
      } else {
        p
      }
    })

    // Remove delivered/dropped packets after a visual delay
    let filteredPackets = Belt.Array.keep(processedPackets, p => {
      switch p.status {
      | Delivered => p.progress < 1.3
      | Dropped => p.progress < 1.3
      | InTransit => true
      | Blocked => true
      }
    })

    // Auto-generate packets based on packet rate
    let newTime = state.currentTime +. deltaTime
    let genInterval = if state.simParams.packetRate > 0.0 {
      1000.0 /. state.simParams.packetRate
    } else {
      999999.0
    }
    let timeSinceLastGen = newTime -. state.lastGenTime
    let genPackets: ref<array<packet>> = ref([])
    let genEvents: ref<array<networkEvent>> = ref([])
    let genCount = ref(0)
    let updatedLastGenTime = ref(state.lastGenTime)

    if state.autoGenerate && timeSinceLastGen >= genInterval {
      let numToGen = Float.toInt(timeSinceLastGen /. genInterval)
      let capped = if numToGen > 5 {
        5
      } else {
        numToGen
      }
      for i in 0 to capped - 1 {
        let routeIdx = mod(
          Float.toInt(
            pseudoRandom(newTime +. Float.fromInt(i) *. 1.3) *.
              Float.fromInt(Array.length(routePool)),
          ),
          Array.length(routePool),
        )
        let route = switch Belt.Array.get(routePool, routeIdx) {
        | Some(r) => r
        | None => ("node-1", "node-2", TCP)
        }
        let (srcId, tgtId, pType) = route
        let srcNode = Belt.Array.getBy(state.nodes, n => n.id == srcId)
        switch srcNode {
        | Some(src) =>
          let pid = "auto-" ++ Float.toString(newTime) ++ "-" ++ Int.toString(i)
          genPackets :=
            Array.concat(
              genPackets.contents,
              [
                {
                  id: pid,
                  packetType: pType,
                  status: InTransit,
                  sourceNode: srcId,
                  targetNode: tgtId,
                  payload: "Auto-generated",
                  size: 512 +
                  Float.toInt(pseudoRandom(newTime +. Float.fromInt(i)) *. 1024.0),
                  encrypted: pType == HTTPS,
                  timestamp: newTime,
                  progress: 0.0,
                  position: (src.x, src.y),
                },
              ],
            )
          genEvents := Array.concat(genEvents.contents, [PacketSent(srcId, tgtId, pType)])
        | None => ()
        }
      }
      genCount := capped
      updatedLastGenTime := state.lastGenTime +. Float.fromInt(capped) *. genInterval
    }

    let allPackets = Array.concat(filteredPackets, genPackets.contents)
    let (cappedPackets, droppedForCap) = enforcePacketCap(allPackets)

    let allNewEvents = Array.concat(arrivalEvents.contents, genEvents.contents)
    let capEvents = if droppedForCap > 0 {
      Array.concat(allNewEvents, [CapacityDrop(droppedForCap)])
    } else {
      allNewEvents
    }
    let nextEvents = Array.concat(state.events, capEvents)
    let (cappedEvents, _) = enforceEventCap(nextEvents)

    let totalDel = state.totalPacketsDelivered + newDelivered.contents
    let totalLat = state.totalLatency +. newLatencySum.contents
    let newAvgLatency = if totalDel > 0 {
      totalLat /. Float.fromInt(totalDel)
    } else {
      0.0
    }
    let newThroughput = if newTime > 0.0 {
      Float.fromInt(totalDel) /. (newTime /. 1000.0)
    } else {
      0.0
    }

    {
      ...state,
      packets: cappedPackets,
      events: cappedEvents,
      currentTime: newTime,
      totalPacketsSent: state.totalPacketsSent + genCount.contents,
      totalPacketsDelivered: totalDel,
      totalPacketsDropped: state.totalPacketsDropped + newDropped.contents + droppedForCap,
      avgLatency: newAvgLatency,
      totalLatency: totalLat,
      throughput: newThroughput,
      lastGenTime: updatedLastGenTime.contents,
    }

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

    let nextEvents = Array.concat(state.events, [PacketDropped(packetId, reason)])
    let (cappedEvents, _) = enforceEventCap(nextEvents)

    {
      ...state,
      packets: updatedPackets,
      events: cappedEvents,
      totalPacketsDropped: state.totalPacketsDropped + 1,
    }

  | SelectPacket(packetId) => {...state, selectedPacket: Some(packetId)}

  | DeselectPacket => {...state, selectedPacket: None}

  | ToggleStats => {...state, showStats: !state.showStats}

  | ToggleEventLog => {...state, showEventLog: !state.showEventLog}

  | ClearEvents => {...state, events: []}

  | SetSimParams(params) => {...state, simParams: params, activePreset: None}

  | ApplyPreset(preset) => {
      ...state,
      simParams: presetParams(preset),
      activePreset: Some(preset),
    }

  | ToggleAutoGenerate => {...state, autoGenerate: !state.autoGenerate}

  | SetPacketRate(rate) => {
      ...state,
      simParams: {...state.simParams, packetRate: rate},
      activePreset: None,
    }

  | SetLatency(latency) => {
      ...state,
      simParams: {...state.simParams, latencyMs: latency},
      activePreset: None,
    }

  | SetDropRate(rate) => {
      ...state,
      simParams: {...state.simParams, dropRate: rate},
      activePreset: None,
    }

  | SetJitter(jitter) => {
      ...state,
      simParams: {...state.simParams, jitterMs: jitter},
      activePreset: None,
    }
  }
}

// Helper: Get packet type color
let packetTypeColor = (pt: packetType): string => {
  switch pt {
  | HTTP => "#4a9eff"
  | HTTPS => "#4caf50"
  | TCP => "#9c27b0"
  | UDP => "#ff9800"
  | ICMP => "#f44336"
  | DNS => "#00bcd4"
  }
}

// Helper: Get packet type label
let packetTypeLabel = (pt: packetType): string => {
  switch pt {
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

// Helper: Get status label
let statusLabel = (status: packetStatus): string => {
  switch status {
  | InTransit => "In Transit"
  | Delivered => "Delivered"
  | Dropped => "Dropped"
  | Blocked => "Blocked"
  }
}

// Helper: Format float to 1 decimal place
let formatFloat1 = (v: float): string => {
  let rounded = Math.round(v *. 10.0) /. 10.0
  Float.toString(rounded)
}

// Helper: Format float to 2 decimal places
let formatFloat2 = (v: float): string => {
  let rounded = Math.round(v *. 100.0) /. 100.0
  Float.toString(rounded)
}

// Helper: Format simulation time as mm:ss
let formatSimTime = (ms: float): string => {
  let totalSec = Float.toInt(ms /. 1000.0)
  let mins = totalSec / 60
  let secs = mod(totalSec, 60)
  let mStr = if mins < 10 {
    "0" ++ Int.toString(mins)
  } else {
    Int.toString(mins)
  }
  let sStr = if secs < 10 {
    "0" ++ Int.toString(secs)
  } else {
    Int.toString(secs)
  }
  mStr ++ ":" ++ sStr
}

// Shared button style helper for control bar
let controlBtnStyle = (~bg: string): ReactDOM.style =>
  Sx.make(
    ~padding="10px 20px",
    ~background=bg,
    ~color="white",
    ~border="none",
    ~borderRadius="8px",
    ~fontSize="14px",
    ~fontWeight="600",
    ~cursor="pointer",
    (),
  )

// Small button style for preset/toggle buttons
let smallBtnStyle = (~active: bool): ReactDOM.style =>
  Sx.make(
    ~padding="6px 14px",
    ~background=if active {
      "#4a9eff"
    } else {
      "#2a3142"
    },
    ~color=if active {
      "white"
    } else {
      "#8892a6"
    },
    ~border="none",
    ~borderRadius="6px",
    ~fontSize="12px",
    ~fontWeight="600",
    ~cursor="pointer",
    (),
  )

// Stat row sub-component
let viewStatRow = (~value: string, ~label: string, ~color: string): React.element => {
  <div>
    <div style={Sx.make(~fontSize="22px", ~fontWeight="700", ~color, ())}>
      {value->React.string}
    </div>
    <div style={Sx.make(~fontSize="11px", ~color="#8892a6", ())}>
      {label->React.string}
    </div>
  </div>
}

// Parameter slider row sub-component
let viewParamSlider = (
  ~label: string,
  ~value: float,
  ~min: float,
  ~max: float,
  ~stepVal: float,
  ~unit: string,
  ~onChange: float => unit,
): React.element => {
  <div style={Sx.make(~marginBottom="10px", ())}>
    <div
      style={Sx.make(
        ~display="flex",
        ~justifyContent="space-between",
        ~fontSize="11px",
        ~color="#8892a6",
        ~marginBottom="4px",
        (),
      )}
    >
      <span> {label->React.string} </span>
      <span> {(formatFloat1(value) ++ " " ++ unit)->React.string} </span>
    </div>
    <input
      type_="range"
      min={Float.toString(min)}
      max={Float.toString(max)}
      step={stepVal}
      value={Float.toString(value)}
      onChange={evt => {
        let v = Float.fromString(ReactEvent.Form.target(evt)["value"])
        switch v {
        | Some(f) => onChange(f)
        | None => ()
        }
      }}
      style={Sx.make(~width="100%", ~cursor="pointer", ())}
    />
  </div>
}

// View: Network node
let viewNode = (node: nodePosition): React.element => {
  <div
    key={node.id}
    style={Sx.make(
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
    )}
  >
    <div style={Sx.make(~fontSize="24px", ~marginBottom="4px", ())}>
      {React.string("\xF0\x9F\x96\xA5\xEF\xB8\x8F")}
    </div>
    <div
      style={Sx.make(
        ~fontSize="11px",
        ~fontWeight="600",
        ~color="#e0e6ed",
        ~textAlign="center",
        ~lineHeight="1.2",
        (),
      )}
    >
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
    style={Sx.make(
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
    )}
  >
    {(if packet.encrypted {
      "\xF0\x9F\x94\x92"
    } else {
      "\xF0\x9F\x93\xA6"
    })->React.string}
  </div>
}

// View: Event log entry
let viewEvent = (event: networkEvent, index: int): React.element => {
  let (icon, color, message) = switch event {
  | PacketSent(src, tgt, pType) => (
      "\xF0\x9F\x93\xA4",
      "#4a9eff",
      `Packet sent: ${src} -> ${tgt} (${packetTypeLabel(pType)})`,
    )
  | PacketReceived(packetId) => ("\xF0\x9F\x93\xA5", "#4caf50", `Packet received: ${packetId}`)
  | PacketDropped(packetId, reason) => (
      "\xE2\x9D\x8C",
      "#f44336",
      `Packet dropped: ${packetId} - ${reason}`,
    )
  | CapacityDrop(count) => (
      "\xF0\x9F\x9B\x91",
      "#ff9800",
      `Backpressure: dropped ${Int.toString(count)} packet(s) to stay under cap`,
    )
  | ConnectionEstablished(src, tgt) => (
      "\xF0\x9F\x94\x97",
      "#4caf50",
      `Connection: ${src} <-> ${tgt}`,
    )
  | ConnectionClosed(src, tgt) => (
      "\xF0\x9F\x94\x8C",
      "#9e9e9e",
      `Disconnected: ${src} <-> ${tgt}`,
    )
  | FirewallBlock(src, tgt, port) => (
      "\xF0\x9F\x9A\xAB",
      "#ff9800",
      `Firewall blocked: ${src} -> ${tgt}:${Int.toString(port)}`,
    )
  | LatencySpike(nodeName, latency) => (
      "\xE2\x9A\xA0\xEF\xB8\x8F",
      "#ff9800",
      `Latency spike: ${nodeName} (${Float.toString(latency)}ms)`,
    )
  }

  <div
    key={Int.toString(index)}
    style={Sx.make(
      ~padding="8px 12px",
      ~background="rgba(255, 255, 255, 0.05)",
      ~borderLeft="3px solid " ++ color,
      ~marginBottom="4px",
      ~fontSize="12px",
      ~color="#b0b8c4",
      ~fontFamily="monospace",
      (),
    )}
  >
    <span style={Sx.make(~marginRight="8px", ())}> {icon->React.string} </span>
    {message->React.string}
  </div>
}

// View: Selected packet detail overlay
let viewPacketDetail = (packet: packet, dispatch: msg => unit): React.element => {
  let panelStyle = Sx.make(
    ~padding="16px",
    ~background="linear-gradient(135deg, #1e2431 0%, #252d3d 100%)",
    ~border="2px solid " ++ packetTypeColor(packet.packetType),
    ~borderRadius="12px",
    ~marginBottom="12px",
    (),
  )
  let rowStyle = Sx.make(
    ~display="flex",
    ~justifyContent="space-between",
    ~fontSize="12px",
    ~color="#b0b8c4",
    ~marginBottom="6px",
    (),
  )

  <div style={panelStyle}>
    <div
      style={Sx.make(
        ~display="flex",
        ~justifyContent="space-between",
        ~alignItems="center",
        ~marginBottom="12px",
        (),
      )}
    >
      <h3 style={Sx.make(~fontSize="14px", ~fontWeight="700", ~color="#e0e6ed", ())}>
        {"Packet Detail"->React.string}
      </h3>
      <button
        onClick={_ => dispatch(DeselectPacket)}
        style={Sx.make(
          ~padding="2px 8px",
          ~background="#2a3142",
          ~color="#8892a6",
          ~border="none",
          ~borderRadius="4px",
          ~fontSize="11px",
          ~cursor="pointer",
          (),
        )}
      >
        {"X"->React.string}
      </button>
    </div>
    <div style={rowStyle}>
      <span> {"ID"->React.string} </span>
      <span style={Sx.make(~fontFamily="monospace", ~fontSize="10px", ())}>
        {packet.id->React.string}
      </span>
    </div>
    <div style={rowStyle}>
      <span> {"Type"->React.string} </span>
      <span style={Sx.make(~color=packetTypeColor(packet.packetType), ~fontWeight="600", ())}>
        {packetTypeLabel(packet.packetType)->React.string}
      </span>
    </div>
    <div style={rowStyle}>
      <span> {"Status"->React.string} </span>
      <span style={Sx.make(~color=statusColor(packet.status), ~fontWeight="600", ())}>
        {statusLabel(packet.status)->React.string}
      </span>
    </div>
    <div style={rowStyle}>
      <span> {"Route"->React.string} </span>
      <span> {(packet.sourceNode ++ " -> " ++ packet.targetNode)->React.string} </span>
    </div>
    <div style={rowStyle}>
      <span> {"Size"->React.string} </span>
      <span> {(Int.toString(packet.size) ++ " B")->React.string} </span>
    </div>
    <div style={rowStyle}>
      <span> {"Encrypted"->React.string} </span>
      <span>
        {(if packet.encrypted {
          "Yes"
        } else {
          "No"
        })->React.string}
      </span>
    </div>
    <div style={rowStyle}>
      <span> {"Progress"->React.string} </span>
      <span> {(formatFloat1(packet.progress *. 100.0) ++ "%")->React.string} </span>
    </div>
  </div>
}

// View: Network topology SVG edges (rendered from edge definitions)
let viewEdges = (nodes: array<nodePosition>): React.element => {
  let nodeById = (nid: string): option<nodePosition> =>
    Belt.Array.getBy(nodes, n => n.id == nid)
  Array.mapWithIndex(edges, (idx, edge) => {
    switch (nodeById(edge.fromNode), nodeById(edge.toNode)) {
    | (Some(fromN), Some(toN)) =>
      <line
        key={Int.toString(idx)}
        x1={Float.toString(fromN.x)}
        y1={Float.toString(fromN.y)}
        x2={Float.toString(toN.x)}
        y2={Float.toString(toN.y)}
        stroke="#2a3142"
        strokeWidth="2"
        strokeDasharray="5,5"
      />
    | _ => React.null
    }
  })->React.array
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

      let _animate = () => {
        let currentTime = Date.now()
        let deltaTime = currentTime -. lastTime.contents
        lastTime := currentTime

        dispatch(UpdatePacketPositions(deltaTime))
        animationId := %raw(`requestAnimationFrame(_animate)`)
      }

      animationId := %raw(`requestAnimationFrame(_animate)`)
      Some(() => %raw(`cancelAnimationFrame(animationId.contents)`))
    } else {
      None
    }
  }, [state.isRunning])

  // Look up the selected packet from state
  let selectedPkt = switch state.selectedPacket {
  | Some(sid) => Belt.Array.getBy(state.packets, p => p.id == sid)
  | None => None
  }

  // Panel style shared across sidebar sections
  let sidebarPanelStyle = Sx.make(
    ~padding="16px",
    ~background="linear-gradient(135deg, #1e2431 0%, #252d3d 100%)",
    ~border="2px solid #2a3142",
    ~borderRadius="12px",
    (),
  )

  <div
    className="simulation-mode"
    style={Sx.make(~padding="32px", ~background="#0a0e1a", ~minHeight="100vh", ())}
  >
    // --- Header ---
    <div style={Sx.make(~marginBottom="24px", ())}>
      <h1
        style={Sx.make(
          ~fontSize="32px",
          ~fontWeight="700",
          ~background="linear-gradient(135deg, #4a9eff, #00bcd4)",
          ~marginBottom="8px",
          (),
        )}
      >
        {"Simulation Mode"->React.string}
      </h1>
      <p style={Sx.make(~fontSize="16px", ~color="#8892a6", ())}>
        {"Cisco Packet Tracer-style network simulation"->React.string}
      </p>
    </div>
    // --- Control Bar ---
    <div
      style={Sx.make(
        ~display="flex",
        ~gap="12px",
        ~flexWrap="wrap",
        ~alignItems="center",
        ~padding="16px",
        ~background="linear-gradient(135deg, #1e2431 0%, #252d3d 100%)",
        ~border="2px solid #2a3142",
        ~borderRadius="12px",
        ~marginBottom="24px",
        (),
      )}
    >
      // Play / Pause / Stop / Step
      <div style={Sx.make(~display="flex", ~gap="8px", ())}>
        {if !state.isRunning {
          <button
            onClick={_ => dispatch(StartSimulation)}
            style={controlBtnStyle(
              ~bg="linear-gradient(135deg, #4caf50, #66bb6a)",
            )}
          >
            {"Start"->React.string}
          </button>
        } else {
          <button
            onClick={_ => dispatch(PauseSimulation)}
            style={controlBtnStyle(
              ~bg="linear-gradient(135deg, #ff9800, #ffb74d)",
            )}
          >
            {"Pause"->React.string}
          </button>
        }}
        <button
          onClick={_ => dispatch(StopSimulation)}
          style={controlBtnStyle(
            ~bg="linear-gradient(135deg, #f44336, #e57373)",
          )}
        >
          {"Stop"->React.string}
        </button>
        <button
          onClick={_ => dispatch(StepSimulation)}
          style={controlBtnStyle(
            ~bg=if state.isRunning {
              "#555"
            } else {
              "linear-gradient(135deg, #7b6cff, #9c88ff)"
            },
          )}
        >
          {"Step"->React.string}
        </button>
      </div>
      // Speed buttons
      <div style={Sx.make(~display="flex", ~gap="6px", ~alignItems="center", ())}>
        <span
          style={Sx.make(
            ~fontSize="12px",
            ~color="#8892a6",
            ~marginRight="4px",
            (),
          )}
        >
          {"Speed:"->React.string}
        </span>
        {[Slow, Normal, Fast, VeryFast]
        ->Array.map(spd =>
          <button
            key={speedLabel(spd)}
            onClick={_ => dispatch(SetSpeed(spd))}
            style={smallBtnStyle(~active=state.speed == spd)}
          >
            {speedLabel(spd)->React.string}
          </button>
        )
        ->React.array}
      </div>
      // Network condition presets
      <div style={Sx.make(~display="flex", ~gap="6px", ~alignItems="center", ())}>
        <span
          style={Sx.make(
            ~fontSize="12px",
            ~color="#8892a6",
            ~marginRight="4px",
            (),
          )}
        >
          {"Network:"->React.string}
        </span>
        {[Ideal, Normal, Congested, Lossy]
        ->Array.map(preset =>
          <button
            key={presetLabel(preset)}
            onClick={_ => dispatch(ApplyPreset(preset))}
            style={smallBtnStyle(~active=state.activePreset == Some(preset))}
          >
            {presetLabel(preset)->React.string}
          </button>
        )
        ->React.array}
      </div>
      // Auto-generate toggle
      <button
        onClick={_ => dispatch(ToggleAutoGenerate)}
        style={smallBtnStyle(~active=state.autoGenerate)}
      >
        {(if state.autoGenerate {
          "Auto: ON"
        } else {
          "Auto: OFF"
        })->React.string}
      </button>
      // Manual send test packet
      <button
        onClick={_ => dispatch(SendPacket("node-1", "node-3", HTTPS))}
        style={controlBtnStyle(
          ~bg="linear-gradient(135deg, #4a9eff, #7b6cff)",
        )}
      >
        {"Send Packet"->React.string}
      </button>
      // Simulation clock
      <div
        style={Sx.make(
          ~marginLeft="auto",
          ~fontSize="14px",
          ~color="#00bcd4",
          ~fontFamily="monospace",
          (),
        )}
      >
        {("T " ++ formatSimTime(state.currentTime))->React.string}
      </div>
    </div>
    // --- Main Content: Topology + Sidebar ---
    <div style={Sx.make(~display="flex", ~gap="24px", ())}>
      // --- Topology Canvas ---
      <div
        style={Sx.make(
          ~flex="1",
          ~position="relative",
          ~height="600px",
          ~background="linear-gradient(135deg, #0a0e1a 0%, #1e2431 100%)",
          ~border="2px solid #2a3142",
          ~borderRadius="16px",
          ~overflow="hidden",
          (),
        )}
      >
        // Render nodes
        {Array.map(state.nodes, node => viewNode(node))->React.array}
        // Render packets
        {Array.map(state.packets, packet =>
          viewPacket(packet, dispatch)
        )->React.array}
        // SVG edges
        <svg
          style={Sx.make(
            ~position="absolute",
            ~top="0",
            ~left="0",
            ~width="100%",
            ~height="100%",
            ~pointerEvents="none",
            ~zIndex="5",
            (),
          )}
        >
          {viewEdges(state.nodes)}
        </svg>
        // Legend overlay (bottom-left)
        <div
          style={Sx.make(
            ~position="absolute",
            ~bottom="12px",
            ~left="12px",
            ~display="flex",
            ~gap="10px",
            ~padding="8px 12px",
            ~background="rgba(0, 0, 0, 0.6)",
            ~borderRadius="8px",
            ~zIndex="30",
            (),
          )}
        >
          {[HTTP, HTTPS, TCP, UDP, ICMP, DNS]
          ->Array.map(ptype =>
            <div
              key={packetTypeLabel(ptype)}
              style={Sx.make(
                ~display="flex",
                ~alignItems="center",
                ~gap="4px",
                (),
              )}
            >
              <div
                style={Sx.make(
                  ~width="10px",
                  ~height="10px",
                  ~borderRadius="50%",
                  ~background=packetTypeColor(ptype),
                  (),
                )}
              />
              <span style={Sx.make(~fontSize="10px", ~color="#8892a6", ())}>
                {packetTypeLabel(ptype)->React.string}
              </span>
            </div>
          )
          ->React.array}
        </div>
      </div>
      // --- Right Sidebar ---
      <div
        style={Sx.make(
          ~width="320px",
          ~display="flex",
          ~flexDirection="column",
          ~gap="12px",
          (),
        )}
      >
        // --- Packet Detail (if selected) ---
        {switch selectedPkt {
        | Some(pkt) => viewPacketDetail(pkt, dispatch)
        | None => React.null
        }}
        // --- Statistics Dashboard ---
        {if state.showStats {
          <div style={sidebarPanelStyle}>
            <div
              style={Sx.make(
                ~display="flex",
                ~justifyContent="space-between",
                ~alignItems="center",
                ~marginBottom="12px",
                (),
              )}
            >
              <h3
                style={Sx.make(
                  ~fontSize="14px",
                  ~fontWeight="700",
                  ~color="#e0e6ed",
                  (),
                )}
              >
                {"Statistics"->React.string}
              </h3>
              <button
                onClick={_ => dispatch(ToggleStats)}
                style={Sx.make(
                  ~padding="2px 8px",
                  ~background="#2a3142",
                  ~color="#8892a6",
                  ~border="none",
                  ~borderRadius="4px",
                  ~fontSize="10px",
                  ~cursor="pointer",
                  (),
                )}
              >
                {"Hide"->React.string}
              </button>
            </div>
            <div
              style={Sx.make(
                ~display="flex",
                ~flexDirection="column",
                ~gap="8px",
                (),
              )}
            >
              {viewStatRow(
                ~value=Int.toString(state.totalPacketsSent),
                ~label="Packets Sent",
                ~color="#4a9eff",
              )}
              {viewStatRow(
                ~value=Int.toString(state.totalPacketsDelivered),
                ~label="Delivered",
                ~color="#4caf50",
              )}
              {viewStatRow(
                ~value=Int.toString(state.totalPacketsDropped),
                ~label="Dropped",
                ~color="#f44336",
              )}
              {viewStatRow(
                ~value=Int.toString(Array.length(state.packets)),
                ~label="Active In Transit",
                ~color="#ff9800",
              )}
              // Delivery rate progress bar
              <div>
                <div
                  style={Sx.make(
                    ~display="flex",
                    ~justifyContent="space-between",
                    ~fontSize="11px",
                    ~color="#8892a6",
                    ~marginBottom="4px",
                    (),
                  )}
                >
                  <span> {"Delivery Rate"->React.string} </span>
                  <span>
                    {(if state.totalPacketsSent > 0 {
                      formatFloat1(
                        Float.fromInt(state.totalPacketsDelivered) /.
                          Float.fromInt(state.totalPacketsSent) *.
                          100.0,
                      ) ++ "%"
                    } else {
                      "N/A"
                    })->React.string}
                  </span>
                </div>
                <div
                  style={Sx.make(
                    ~height="6px",
                    ~background="#2a3142",
                    ~borderRadius="3px",
                    ~overflow="hidden",
                    (),
                  )}
                >
                  <div
                    style={Sx.make(
                      ~height="100%",
                      ~width=(if state.totalPacketsSent > 0 {
                        formatFloat1(
                          Float.fromInt(state.totalPacketsDelivered) /.
                            Float.fromInt(state.totalPacketsSent) *.
                            100.0,
                        )
                      } else {
                        "0"
                      }) ++ "%",
                      ~background="linear-gradient(90deg, #4caf50, #66bb6a)",
                      ~borderRadius="3px",
                      ~transition="width 0.3s",
                      (),
                    )}
                  />
                </div>
              </div>
              {viewStatRow(
                ~value=formatFloat1(state.avgLatency) ++ " ms",
                ~label="Avg Latency",
                ~color="#9c27b0",
              )}
              {viewStatRow(
                ~value=formatFloat2(state.throughput) ++ " p/s",
                ~label="Throughput",
                ~color="#00bcd4",
              )}
              {viewStatRow(
                ~value=if isBatchWasmActive() {
                  "WASM"
                } else {
                  "JS"
                },
                ~label="Packet Kernel",
                ~color="#00bcd4",
              )}
            </div>
          </div>
        } else {
          <button
            onClick={_ => dispatch(ToggleStats)}
            style={smallBtnStyle(~active=false)}
          >
            {"Show Stats"->React.string}
          </button>
        }}
        // --- Simulation Parameters ---
        <div style={sidebarPanelStyle}>
          <h3
            style={Sx.make(
              ~fontSize="14px",
              ~fontWeight="700",
              ~color="#e0e6ed",
              ~marginBottom="12px",
              (),
            )}
          >
            {"Parameters"->React.string}
          </h3>
          {viewParamSlider(
            ~label="Packet Rate",
            ~value=state.simParams.packetRate,
            ~min=0.0,
            ~max=20.0,
            ~stepVal=0.5,
            ~unit="p/s",
            ~onChange=v => dispatch(SetPacketRate(v)),
          )}
          {viewParamSlider(
            ~label="Latency",
            ~value=state.simParams.latencyMs,
            ~min=0.0,
            ~max=500.0,
            ~stepVal=5.0,
            ~unit="ms",
            ~onChange=v => dispatch(SetLatency(v)),
          )}
          {viewParamSlider(
            ~label="Drop Rate",
            ~value=state.simParams.dropRate,
            ~min=0.0,
            ~max=1.0,
            ~stepVal=0.01,
            ~unit="",
            ~onChange=v => dispatch(SetDropRate(v)),
          )}
          {viewParamSlider(
            ~label="Jitter",
            ~value=state.simParams.jitterMs,
            ~min=0.0,
            ~max=200.0,
            ~stepVal=5.0,
            ~unit="ms",
            ~onChange=v => dispatch(SetJitter(v)),
          )}
        </div>
        // --- Event Log ---
        {if state.showEventLog {
          <div
            style={Sx.make(
              ~flex="1",
              ~padding="16px",
              ~background="linear-gradient(135deg, #1e2431 0%, #252d3d 100%)",
              ~border="2px solid #2a3142",
              ~borderRadius="12px",
              ~overflow="auto",
              ~maxHeight="300px",
              (),
            )}
          >
            <div
              style={Sx.make(
                ~display="flex",
                ~justifyContent="space-between",
                ~alignItems="center",
                ~marginBottom="10px",
                (),
              )}
            >
              <h3
                style={Sx.make(
                  ~fontSize="14px",
                  ~fontWeight="700",
                  ~color="#e0e6ed",
                  (),
                )}
              >
                {("Event Log (" ++
                Int.toString(Array.length(state.events)) ++
                ")")->React.string}
              </h3>
              <div style={Sx.make(~display="flex", ~gap="6px", ())}>
                <button
                  onClick={_ => dispatch(ClearEvents)}
                  style={Sx.make(
                    ~padding="2px 8px",
                    ~background="#2a3142",
                    ~color="#8892a6",
                    ~border="none",
                    ~borderRadius="4px",
                    ~fontSize="10px",
                    ~cursor="pointer",
                    (),
                  )}
                >
                  {"Clear"->React.string}
                </button>
                <button
                  onClick={_ => dispatch(ToggleEventLog)}
                  style={Sx.make(
                    ~padding="2px 8px",
                    ~background="#2a3142",
                    ~color="#8892a6",
                    ~border="none",
                    ~borderRadius="4px",
                    ~fontSize="10px",
                    ~cursor="pointer",
                    (),
                  )}
                >
                  {"Hide"->React.string}
                </button>
              </div>
            </div>
            {if Array.length(state.events) > 0 {
              // Show most recent events first (reverse copy)
              let reversed = Array.copy(state.events)
              Belt.Array.reverseInPlace(reversed)
              Array.mapWithIndex(reversed, (index, evt) =>
                viewEvent(evt, index)
              )->React.array
            } else {
              <div
                style={Sx.make(
                  ~fontSize="12px",
                  ~color="#8892a6",
                  ~textAlign="center",
                  ~padding="16px",
                  (),
                )}
              >
                {"No events yet. Start the simulation."->React.string}
              </div>
            }}
          </div>
        } else {
          <button
            onClick={_ => dispatch(ToggleEventLog)}
            style={smallBtnStyle(~active=false)}
          >
            {"Show Event Log"->React.string}
          </button>
        }}
      </div>
    </div>
    // --- Footer Tips ---
    <div
      style={Sx.make(
        ~marginTop="24px",
        ~padding="16px",
        ~background="rgba(74, 158, 255, 0.1)",
        ~border="2px solid #4a9eff",
        ~borderRadius="12px",
        (),
      )}
    >
      <strong style={Sx.make(~fontSize="13px", ~color="#4a9eff", ())}>
        {"Simulation Tips: "->React.string}
      </strong>
      <span style={Sx.make(~fontSize="13px", ~color="#b0b8c4", ())}>
        {"Click packets for details. Use presets (Ideal/Normal/Congested/Lossy) to simulate network conditions. Adjust sliders for fine-grained control. Step advances one frame while paused."->React.string}
      </span>
    </div>
  </div>
}
