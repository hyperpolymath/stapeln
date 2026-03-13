// SPDX-License-Identifier: PMPL-1.0-or-later
// Image verification UI for Svalinn Web UI
//
// Allows users to submit an image digest for attestation verification.
// Displays verification results: predicates, signers, policy verdict,
// SBOM and signature status.

open Tea

// ============================================================================
// Types
// ============================================================================

type predicateResult = {
  predicateType: string,
  found: bool,
}

type signerResult = {
  identity: string,
  verified: bool,
}

type verificationResult = {
  digest: string,
  policyVerdict: string,
  predicates: array<predicateResult>,
  signers: array<signerResult>,
  sbomStatus: string,
  signatureStatus: string,
  timestamp: string,
}

type verifyForm = {
  digest: string,
  submitting: bool,
}

let emptyForm: verifyForm = {
  digest: "",
  submitting: false,
}

// ============================================================================
// Decoders
// ============================================================================

let predicateResultDecoder: Json.decoder<predicateResult> =
  Json.map2(
    (predicateType, found) => {predicateType, found},
    Json.field("type", Json.string),
    Json.field("found", Json.bool),
  )

let signerResultDecoder: Json.decoder<signerResult> =
  Json.map2(
    (identity, verified) => {identity, verified},
    Json.field("identity", Json.string),
    Json.field("verified", Json.bool),
  )

let verificationResultDecoder: Json.decoder<verificationResult> =
  Json.map7(
    (digest, policyVerdict, predicates, signers, sbomStatus, signatureStatus, timestamp) => {
      digest,
      policyVerdict,
      predicates,
      signers,
      sbomStatus,
      signatureStatus,
      timestamp,
    },
    Json.field("digest", Json.string),
    Json.field("policyVerdict", Json.string),
    Json.field("predicates", Json.array(predicateResultDecoder)),
    Json.field("signers", Json.array(signerResultDecoder)),
    Json.field("sbomStatus", Json.string),
    Json.field("signatureStatus", Json.string),
    Json.field("timestamp", Json.string),
  )

// ============================================================================
// API
// ============================================================================

let verifyImage = (
  baseUrl: string,
  digest: string,
  toMsg: result<verificationResult, Tea.Http.httpError> => 'msg,
) => Tea.Http.getJson(`${baseUrl}/v1/verify/${digest}`, verificationResultDecoder, toMsg)

// ============================================================================
// View
// ============================================================================

let verdictBadgeClass = (verdict: string): string =>
  switch verdict {
  | "pass" | "allowed" => "badge-verified"
  | "fail" | "denied" => "badge-denied"
  | "warn" | "warning" => "badge-warning"
  | _ => "badge-unverified"
  }

let statusBadgeClass = (status: string): string =>
  switch status {
  | "present" | "valid" | "verified" => "badge-verified"
  | "missing" | "invalid" | "unsigned" => "badge-denied"
  | "partial" => "badge-warning"
  | _ => "badge-unverified"
  }

type verifyMsg =
  | SetDigest(string)
  | SubmitVerify
  | ClearResult

let formView = (form: verifyForm, dispatch: verifyMsg => unit): React.element =>
  <div className="verify-form">
    <div className="card">
      <h3> {React.string("Verify Image Attestation")} </h3>
      <p className="muted">
        {React.string(
          "Enter an image digest (sha256:...) to check its attestation, " ++
          "signatures, and policy compliance.",
        )}
      </p>
      <div className="form-group">
        <label htmlFor="image-digest"> {React.string("Image Digest")} </label>
        <input
          id="image-digest"
          type_="text"
          className="form-input"
          placeholder="sha256:abc123..."
          value={form.digest}
          onChange={event => {
            let value = ReactEvent.Form.target(event)["value"]
            dispatch(SetDigest(value))
          }}
          onKeyDown={event => {
            if ReactEvent.Keyboard.key(event) == "Enter" {
              dispatch(SubmitVerify)
            }
          }}
          disabled={form.submitting}
        />
      </div>
      <div>
        <button
          className="btn-primary"
          onClick={_ => dispatch(SubmitVerify)}
          disabled={form.submitting || Js.String2.trim(form.digest) == ""}>
          {form.submitting ? React.string("Verifying...") : React.string("Verify")}
        </button>
      </div>
    </div>
  </div>

let resultView = (result: verificationResult, dispatch: verifyMsg => unit): React.element =>
  <div className="verify-result">
    <div className="card">
      <div className="header">
        <h3> {React.string("Verification Result")} </h3>
        <button onClick={_ => dispatch(ClearResult)}> {React.string("Clear")} </button>
      </div>
      <p className="muted"> {React.string(`Digest: ${result.digest}`)} </p>
      <p className="muted"> {React.string(`Verified at: ${result.timestamp}`)} </p>
      <div className="verify-summary">
        <div className="verify-summary-item">
          <span className="muted"> {React.string("Policy Verdict")} </span>
          <span className={verdictBadgeClass(result.policyVerdict)}>
            {React.string(result.policyVerdict)}
          </span>
        </div>
        <div className="verify-summary-item">
          <span className="muted"> {React.string("SBOM")} </span>
          <span className={statusBadgeClass(result.sbomStatus)}>
            {React.string(result.sbomStatus)}
          </span>
        </div>
        <div className="verify-summary-item">
          <span className="muted"> {React.string("Signature")} </span>
          <span className={statusBadgeClass(result.signatureStatus)}>
            {React.string(result.signatureStatus)}
          </span>
        </div>
      </div>
    </div>
    <div className="card">
      <h3> {React.string("Predicates")} </h3>
      {if Belt.Array.length(result.predicates) > 0 {
        <div className="verify-table">
          {result.predicates
          ->Belt.Array.map(pred =>
            <div className="verify-table-row" key={pred.predicateType}>
              <span> {React.string(pred.predicateType)} </span>
              <span className={pred.found ? "badge-verified" : "badge-denied"}>
                {React.string(pred.found ? "found" : "missing")}
              </span>
            </div>
          )
          ->React.array}
        </div>
      } else {
        <p className="muted"> {React.string("No predicates checked.")} </p>
      }}
    </div>
    <div className="card">
      <h3> {React.string("Signers")} </h3>
      {if Belt.Array.length(result.signers) > 0 {
        <div className="verify-table">
          {result.signers
          ->Belt.Array.map(signer =>
            <div className="verify-table-row" key={signer.identity}>
              <span> {React.string(signer.identity)} </span>
              <span className={signer.verified ? "badge-verified" : "badge-denied"}>
                {React.string(signer.verified ? "verified" : "unverified")}
              </span>
            </div>
          )
          ->React.array}
        </div>
      } else {
        <p className="muted"> {React.string("No signers found.")} </p>
      }}
    </div>
  </div>

let view = (
  form: verifyForm,
  result: option<verificationResult>,
  loading: bool,
  error: option<string>,
  dispatch: verifyMsg => unit,
): React.element =>
  <div>
    <div className="header">
      <h1> {React.string("Verify")} </h1>
      {loading
        ? <div className="loading-spinner" />
        : <span className="badge"> {React.string("Attestation")} </span>}
    </div>
    {switch error {
    | Some(message) =>
      <div className="error-banner">
        <strong> {React.string("Error: ")} </strong>
        {React.string(message)}
      </div>
    | None => React.null
    }}
    {formView(form, dispatch)}
    {switch result {
    | Some(r) => resultView(r, dispatch)
    | None => React.null
    }}
  </div>
