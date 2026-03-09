// SPDX-License-Identifier: PMPL-1.0-or-later
// Socket.res - Raw WebSocket client for Phoenix channels
//
// Uses the browser WebSocket API directly (no phoenix JS client dependency).
// Implements enough of the Phoenix channel wire protocol to join topics and
// push/receive JSON messages.  The app works perfectly without WebSocket —
// this module is entirely optional and falls back gracefully.
//
// Phoenix channel wire format (v2):
//   [join_ref, ref, topic, event, payload]

// ---------------------------------------------------------------------------
// Raw WebSocket FFI bindings
// ---------------------------------------------------------------------------

type webSocket

@new external makeWebSocket: string => webSocket = "WebSocket"
@send external wsSend: (webSocket, string) => unit = "send"
@send external wsClose: (webSocket, ~code: int=?, ~reason: string=?) => unit = "close"
@get external wsReadyState: webSocket => int = "readyState"

// WebSocket readyState constants
let wsConnecting = 0
let wsOpen = 1
let _wsClosed = 3

// Event handler setters
@set external setOnOpen: (webSocket, unit => unit) => unit = "onopen"
@set external setOnClose: (webSocket, {..} => unit) => unit = "onclose"
@set external setOnError: (webSocket, {..} => unit) => unit = "onerror"
@set external setOnMessage: (webSocket, {"data": string} => unit) => unit = "onmessage"

// ---------------------------------------------------------------------------
// Timer bindings for heartbeat
// ---------------------------------------------------------------------------

type intervalId

@val external setInterval: (unit => unit, int) => intervalId = "setInterval"
@val external clearInterval: intervalId => unit = "clearInterval"

// ---------------------------------------------------------------------------
// Connection state
// ---------------------------------------------------------------------------

type connectionState =
  | Disconnected
  | Connecting
  | Connected
  | Errored

type channelState =
  | Joining
  | Joined
  | Left

type channel = {
  topic: string,
  mutable state: channelState,
  mutable joinRef: string,
  // Registered event handlers: event name -> list of callbacks
  mutable handlers: dict<array<JSON.t => unit>>,
}

type connection = {
  mutable ws: option<webSocket>,
  mutable state: connectionState,
  mutable ref: int,
  mutable channels: array<channel>,
  mutable heartbeatTimer: option<intervalId>,
  mutable onStateChange: option<connectionState => unit>,
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

let nextRef = (conn: connection): string => {
  conn.ref = conn.ref + 1
  Int.toString(conn.ref)
}

// Encode a Phoenix channel message as JSON array
let encodeMsg = (
  joinRef: option<string>,
  ref: string,
  topic: string,
  event: string,
  payload: JSON.t,
): string => {
  let jr = switch joinRef {
  | Some(r) => JSON.Encode.string(r)
  | None => JSON.Encode.null
  }
  JSON.stringify(
    JSON.Encode.array([
      jr,
      JSON.Encode.string(ref),
      JSON.Encode.string(topic),
      JSON.Encode.string(event),
      payload,
    ]),
  )
}

// Decode an incoming Phoenix channel message from JSON array
type incomingMsg = {
  joinRef: option<string>,
  ref: option<string>,
  topic: string,
  event: string,
  payload: JSON.t,
}

let decodeMsg = (raw: string): option<incomingMsg> => {
  switch JSON.parseOrThrow(raw) {
  | exception _ => None
  | json =>
    switch json {
    | Array(arr) if Array.length(arr) >= 5 => {
        let jr = switch arr[0]->Belt.Option.getWithDefault(JSON.Encode.null) {
        | String(s) => Some(s)
        | _ => None
        }
        let r = switch arr[1]->Belt.Option.getWithDefault(JSON.Encode.null) {
        | String(s) => Some(s)
        | _ => None
        }
        let topic = switch arr[2]->Belt.Option.getWithDefault(JSON.Encode.null) {
        | String(s) => s
        | _ => ""
        }
        let event = switch arr[3]->Belt.Option.getWithDefault(JSON.Encode.null) {
        | String(s) => s
        | _ => ""
        }
        let payload = arr[4]->Belt.Option.getWithDefault(JSON.Encode.null)
        Some({joinRef: jr, ref: r, topic, event, payload})
      }
    | _ => None
    }
  }
}

// Find a channel by topic
let findChannel = (conn: connection, topic: string): option<channel> => {
  Belt.Array.getBy(conn.channels, ch => ch.topic == topic)
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

// Create a new connection (does not connect yet).
let make = (): connection => {
  ws: None,
  state: Disconnected,
  ref: 0,
  channels: [],
  heartbeatTimer: None,
  onStateChange: None,
}

// Set a callback invoked whenever connection state changes.
let onStateChange = (conn: connection, cb: connectionState => unit): unit => {
  conn.onStateChange = Some(cb)
}

let notifyState = (conn: connection): unit => {
  switch conn.onStateChange {
  | Some(cb) => cb(conn.state)
  | None => ()
  }
}

// Connect to the Phoenix socket endpoint.
// `url` should be the full WebSocket URL, e.g. "ws://localhost:4000/socket/websocket".
let connect = (conn: connection, url: string): unit => {
  conn.state = Connecting
  notifyState(conn)

  let ws = makeWebSocket(url)
  conn.ws = Some(ws)

  setOnOpen(ws, () => {
    conn.state = Connected
    notifyState(conn)
    Console.log("[Socket] Connected to " ++ url)

    // Start heartbeat every 30 seconds
    let timer = setInterval(() => {
      if wsReadyState(ws) == wsOpen {
        let ref = nextRef(conn)
        let msg = encodeMsg(None, ref, "phoenix", "heartbeat", JSON.Encode.object(Dict.make()))
        wsSend(ws, msg)
      }
    }, 30_000)
    conn.heartbeatTimer = Some(timer)

    // Re-join any channels that were registered before connect
    Array.forEach(conn.channels, ch => {
      let ref = nextRef(conn)
      ch.joinRef = ref
      ch.state = Joining
      let msg = encodeMsg(Some(ref), ref, ch.topic, "phx_join", JSON.Encode.object(Dict.make()))
      wsSend(ws, msg)
    })
  })

  setOnClose(ws, _event => {
    conn.state = Disconnected
    conn.ws = None
    notifyState(conn)
    Console.log("[Socket] Disconnected")
    switch conn.heartbeatTimer {
    | Some(timer) => {
        clearInterval(timer)
        conn.heartbeatTimer = None
      }
    | None => ()
    }
  })

  setOnError(ws, _event => {
    conn.state = Errored
    notifyState(conn)
    Console.error("[Socket] Connection error")
  })

  setOnMessage(ws, event => {
    switch decodeMsg(event["data"]) {
    | None => Console.warn("[Socket] Failed to decode message")
    | Some(msg) =>
      // Handle join reply
      if msg.event == "phx_reply" {
        switch findChannel(conn, msg.topic) {
        | Some(ch) if ch.state == Joining => {
            ch.state = Joined
            Console.log("[Socket] Joined " ++ msg.topic)
          }
        | _ => ()
        }
      } else {
        // Route to channel handlers
        switch findChannel(conn, msg.topic) {
        | Some(ch) =>
          switch Dict.get(ch.handlers, msg.event) {
          | Some(callbacks) => Array.forEach(callbacks, cb => cb(msg.payload))
          | None => ()
          }
        | None => ()
        }
      }
    }
  })
}

// Disconnect the WebSocket.
let disconnect = (conn: connection): unit => {
  switch conn.ws {
  | Some(ws) => wsClose(ws)
  | None => ()
  }
  conn.ws = None
  conn.state = Disconnected
  notifyState(conn)
  switch conn.heartbeatTimer {
  | Some(timer) => {
      clearInterval(timer)
      conn.heartbeatTimer = None
    }
  | None => ()
  }
}

// Create a channel for the given topic.  Does NOT join yet — call `joinChannel`.
let channel = (conn: connection, topic: string): channel => {
  let ch: channel = {
    topic,
    state: Left,
    joinRef: "0",
    handlers: Dict.make(),
  }
  conn.channels = Array.concat(conn.channels, [ch])
  ch
}

// Join a channel.  If the socket is already connected the join is sent
// immediately; otherwise it will be sent when the connection opens.
let joinChannel = (conn: connection, ch: channel): unit => {
  switch conn.ws {
  | Some(ws) if wsReadyState(ws) == wsOpen => {
      let ref = nextRef(conn)
      ch.joinRef = ref
      ch.state = Joining
      let msg = encodeMsg(Some(ref), ref, ch.topic, "phx_join", JSON.Encode.object(Dict.make()))
      wsSend(ws, msg)
    }
  | _ =>
    // Will auto-join when connection opens
    ch.state = Joining
  }
}

// Leave a channel.
let leaveChannel = (conn: connection, ch: channel): unit => {
  switch conn.ws {
  | Some(ws) if wsReadyState(ws) == wsOpen => {
      let ref = nextRef(conn)
      let msg = encodeMsg(Some(ch.joinRef), ref, ch.topic, "phx_leave", JSON.Encode.object(Dict.make()))
      wsSend(ws, msg)
    }
  | _ => ()
  }
  ch.state = Left
}

// Register a handler for an event on this channel.
let on = (ch: channel, event: string, callback: JSON.t => unit): unit => {
  let existing = switch Dict.get(ch.handlers, event) {
  | Some(arr) => arr
  | None => []
  }
  Dict.set(ch.handlers, event, Array.concat(existing, [callback]))
}

// Push a message to the channel.
let push = (conn: connection, ch: channel, event: string, payload: JSON.t): unit => {
  switch conn.ws {
  | Some(ws) if wsReadyState(ws) == wsOpen && ch.state == Joined => {
      let ref = nextRef(conn)
      let msg = encodeMsg(Some(ch.joinRef), ref, ch.topic, event, payload)
      wsSend(ws, msg)
    }
  | _ => Console.warn("[Socket] Cannot push — not connected or channel not joined")
  }
}

// ---------------------------------------------------------------------------
// Convenience: build the default socket URL from the current page location
// ---------------------------------------------------------------------------

@val @scope("window") external locationHost: string = "location.host"
@val @scope("window") external locationProtocol: string = "location.protocol"

// Returns "ws://host/socket/websocket" or "wss://..." depending on current page.
let defaultUrl = (): string => {
  let proto = if locationProtocol == "https:" {
    "wss://"
  } else {
    "ws://"
  }
  proto ++ locationHost ++ "/socket/websocket"
}
