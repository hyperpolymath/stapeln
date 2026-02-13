// SPDX-License-Identifier: PMPL-1.0-or-later
// Vörðr MCP Adapter - Type definitions

// Container lifecycle states matching Idris2 types
type containerState =
  | ImageOnly
  | Created
  | Running
  | Paused
  | Stopped
  | Removed

// Container configuration
type containerConfig = {
  image: string,
  name: option<string>,
  command: option<array<string>>,
  env: option<Js.Dict.t<string>>,
  privileged: bool,
  readOnlyRoot: bool,
  userNamespace: bool,
  seccompEnabled: bool,
}

// Verification attestation
type attestation = {
  containerHash: string,
  signature: string,
  timestamp: float,
  verified: bool,
}

// Authorization request
type authorizationRequest = {
  requestId: string,
  config: containerConfig,
  threshold: int,
  totalSigners: int,
  signatures: array<string>,
  state: [#pending | #authorized | #denied | #expired],
}

// MCP Tool response
type toolResponse = {
  success: bool,
  message: option<string>,
  data: option<Js.Json.t>,
}

// Event types for eBPF monitoring
type syscallEvent = {
  pid: int,
  comm: string,
  syscallNr: int,
  timestamp: float,
}

type networkEvent = {
  pid: int,
  srcAddr: string,
  dstAddr: string,
  srcPort: int,
  dstPort: int,
  protocol: string,
}

type fileEvent = {
  pid: int,
  path: string,
  operation: [#\"open" | #read | #write | #close | #unlink],
  timestamp: float,
}

// Anomaly detection
type anomaly = {
  severity: [#low | #medium | #high | #critical],
  description: string,
  timestamp: float,
  relatedEvents: array<syscallEvent>,
}
