// SPDX-License-Identifier: PMPL-1.0-or-later
// Policy visualization component for Svalinn Web UI
//
// Fetches and displays container signing policies from /api/v1/policies.
// Color-codes policies: green for allowed, red for denied, yellow for warnings.

open Tea

// ============================================================================
// Types
// ============================================================================

type predicate = {
  predicateType: string,
  required: bool,
}

type signer = {
  identity: string,
  issuer: string,
}

type policy = {
  name: string,
  description: string,
  mode: string,
  requiredPredicates: array<predicate>,
  allowedSigners: array<signer>,
  logQuorum: int,
  enforced: bool,
}

type policiesResponse = {
  policies: array<policy>,
}

// ============================================================================
// Decoders
// ============================================================================

let predicateDecoder: Json.decoder<predicate> =
  Json.map2(
    (predicateType, required) => {predicateType, required},
    Json.field("type", Json.string),
    Json.field("required", Json.bool),
  )

let signerDecoder: Json.decoder<signer> =
  Json.map2(
    (identity, issuer) => {identity, issuer},
    Json.field("identity", Json.string),
    Json.field("issuer", Json.string),
  )

let policyDecoder: Json.decoder<policy> =
  Json.map7(
    (name, description, mode, requiredPredicates, allowedSigners, logQuorum, enforced) => {
      name,
      description,
      mode,
      requiredPredicates,
      allowedSigners,
      logQuorum,
      enforced,
    },
    Json.field("name", Json.string),
    Json.field("description", Json.string),
    Json.field("mode", Json.string),
    Json.field("requiredPredicates", Json.array(predicateDecoder)),
    Json.field("allowedSigners", Json.array(signerDecoder)),
    Json.field("logQuorum", Json.int),
    Json.field("enforced", Json.bool),
  )

let policiesDecoder: Json.decoder<policiesResponse> =
  Json.map(
    policies => {policies: policies},
    Json.field("policies", Json.array(policyDecoder)),
  )

// ============================================================================
// API
// ============================================================================

let getPolicies = (
  baseUrl: string,
  toMsg: result<policiesResponse, Tea.Http.httpError> => 'msg,
) => Tea.Http.getJson(`${baseUrl}/v1/policies`, policiesDecoder, toMsg)

// ============================================================================
// View
// ============================================================================

let modeColor = (mode: string): string =>
  switch mode {
  | "enforce" => "badge-denied"
  | "warn" => "badge-warning"
  | "permissive" | "allow" => "badge-verified"
  | _ => "badge"
  }

let predicateRow = (pred: predicate): React.element =>
  <div className="predicate-row" key={pred.predicateType}>
    <span className="muted"> {React.string(pred.predicateType)} </span>
    <span className={pred.required ? "badge-verified" : "badge"}>
      {React.string(pred.required ? "required" : "optional")}
    </span>
  </div>

let signerRow = (signer: signer): React.element =>
  <div className="signer-row" key={signer.identity}>
    <span> {React.string(signer.identity)} </span>
    <span className="muted"> {React.string(signer.issuer)} </span>
  </div>

let policyCard = (policy: policy): React.element =>
  <div className="policy-card" key={policy.name}>
    <div className="policy-header">
      <h3> {React.string(policy.name)} </h3>
      <span className={modeColor(policy.mode)}>
        {React.string(policy.mode)}
      </span>
    </div>
    <p className="muted"> {React.string(policy.description)} </p>
    <div className="policy-section">
      <h4> {React.string("Required Predicates")} </h4>
      {if Belt.Array.length(policy.requiredPredicates) > 0 {
        policy.requiredPredicates->Belt.Array.map(predicateRow)->React.array
      } else {
        <p className="muted"> {React.string("None required.")} </p>
      }}
    </div>
    <div className="policy-section">
      <h4> {React.string("Allowed Signers")} </h4>
      {if Belt.Array.length(policy.allowedSigners) > 0 {
        policy.allowedSigners->Belt.Array.map(signerRow)->React.array
      } else {
        <p className="muted"> {React.string("Any signer accepted.")} </p>
      }}
    </div>
    <div className="policy-footer">
      <span className="muted">
        {React.string(`Log quorum: ${Belt.Int.toString(policy.logQuorum)}`)}
      </span>
      <span className={policy.enforced ? "badge-verified" : "badge-warning"}>
        {React.string(policy.enforced ? "enforced" : "not enforced")}
      </span>
    </div>
  </div>

let view = (
  policies: array<policy>,
  loading: bool,
  error: option<string>,
): React.element =>
  <div>
    <div className="header">
      <h1> {React.string("Policies")} </h1>
      {loading
        ? <div className="loading-spinner" />
        : <span className="badge">
            {React.string(
              Belt.Int.toString(Belt.Array.length(policies)) ++ " policies",
            )}
          </span>}
    </div>
    {switch error {
    | Some(message) =>
      <div className="error-banner">
        <strong> {React.string("Error: ")} </strong>
        {React.string(message)}
      </div>
    | None => React.null
    }}
    <div className="policy-grid">
      {if Belt.Array.length(policies) > 0 {
        policies->Belt.Array.map(policyCard)->React.array
      } else if !loading {
        <div className="card">
          <h3> {React.string("No policies configured")} </h3>
          <p className="muted">
            {React.string("Configure policies in the Svalinn server to see them here.")}
          </p>
        </div>
      } else {
        React.null
      }}
    </div>
  </div>
