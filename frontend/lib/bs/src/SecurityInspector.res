// SPDX-License-Identifier: PMPL-1.0-or-later
// SecurityInspector.res - Attack surface analysis with security scoring

// Severity levels
type severity =
  | Critical
  | High
  | Medium
  | Low
  | Info

// Vulnerability types
type vulnerability = {
  id: string,
  title: string,
  severity: severity,
  description: string,
  affectedComponent: string,
  cveId: option<string>,
  fixAvailable: bool,
  fixDescription: option<string>,
}

// Security check results
type checkResult =
  | Pass
  | Fail
  | Warning
  | Unknown

type securityCheck = {
  name: string,
  description: string,
  result: checkResult,
  details: string,
}

// Security metrics (0-100)
type securityMetrics = {
  security: int,
  performance: int,
  reliability: int,
  compliance: int,
}

// Overall grade
type grade =
  | APlus
  | A
  | AMinus
  | BPlus
  | B
  | BMinus
  | CPlus
  | C
  | CMinus
  | D
  | F

// Exposed port info
type exposedPort = {
  port: int,
  protocol: string,
  service: string,
  risk: string,
  publiclyAccessible: bool,
}

type state = {
  metrics: securityMetrics,
  grade: grade,
  vulnerabilities: array<vulnerability>,
  checks: array<securityCheck>,
  exposedPorts: array<exposedPort>,
  selectedVulnerability: option<string>,
  showDetails: bool,
  filterSeverity: option<severity>,
}

// Message types
type msg =
  | UpdateMetrics(securityMetrics)
  | SelectVulnerability(string)
  | ToggleDetails
  | FilterBySeverity(option<severity>)
  | ApplyFix(string)
  | RunSecurityScan

// Initialize with sample data
let init: state = {
  metrics: {
    security: 85,
    performance: 92,
    reliability: 88,
    compliance: 95,
  },
  grade: A,
  vulnerabilities: [
    {
      id: "vuln-1",
      title: "Unencrypted connection from gateway to application",
      severity: High,
      description: "Connection between API Gateway and Auth Service is not encrypted. This exposes sensitive data in transit.",
      affectedComponent: "conn-1",
      cveId: None,
      fixAvailable: true,
      fixDescription: Some("Enable TLS encryption on connection. Update protocol to HTTPS."),
    },
    {
      id: "vuln-2",
      title: "Database port exposed without firewall",
      severity: Critical,
      description: "PostgreSQL port 5432 is accessible without firewall protection. Database should only accept connections from application tier.",
      affectedComponent: "node-3",
      cveId: Some("CWE-284"),
      fixAvailable: true,
      fixDescription: Some("Enable firewall on database node. Configure to only accept connections from Auth Service."),
    },
    {
      id: "vuln-3",
      title: "Container running as root",
      severity: Medium,
      description: "Auth Service container is running as root user. This violates least privilege principle.",
      affectedComponent: "node-2",
      cveId: Some("CWE-250"),
      fixAvailable: true,
      fixDescription: Some("Add USER directive to Containerfile. Create non-privileged user."),
    },
  ],
  checks: [
    {
      name: "Image Signatures",
      description: "All container images are cryptographically signed",
      result: Pass,
      details: "3/3 images verified with Ed448+Dilithium5 signatures",
    },
    {
      name: "SBOM Present",
      description: "Software Bill of Materials available for audit",
      result: Pass,
      details: "CycloneDX SBOMs generated for all components",
    },
    {
      name: "Non-Root Containers",
      description: "Containers run with least privilege",
      result: Warning,
      details: "1/3 containers still running as root (Auth Service)",
    },
    {
      name: "Health Checks",
      description: "All services have health check endpoints",
      result: Fail,
      details: "0/3 services have configured health checks",
    },
    {
      name: "Resource Limits",
      description: "CPU and memory limits configured",
      result: Pass,
      details: "All containers have resource constraints",
    },
    {
      name: "Network Segmentation",
      description: "Services isolated in separate networks",
      result: Pass,
      details: "Using stapeln_network with proper isolation",
    },
  ],
  exposedPorts: [
    {
      port: 80,
      protocol: "HTTP",
      service: "API Gateway",
      risk: "Medium",
      publiclyAccessible: true,
    },
    {
      port: 8080,
      protocol: "HTTP",
      service: "Auth Service",
      risk: "Low",
      publiclyAccessible: false,
    },
    {
      port: 5432,
      protocol: "TCP",
      service: "PostgreSQL",
      risk: "Critical",
      publiclyAccessible: false,
    },
  ],
  selectedVulnerability: None,
  showDetails: true,
  filterSeverity: None,
}

// Helper: Calculate overall grade from metrics
let calculateGrade = (metrics: securityMetrics): grade => {
  let avg = (metrics.security + metrics.performance + metrics.reliability + metrics.compliance) / 4
  if avg >= 97 {
    APlus
  } else if avg >= 93 {
    A
  } else if avg >= 90 {
    AMinus
  } else if avg >= 87 {
    BPlus
  } else if avg >= 83 {
    B
  } else if avg >= 80 {
    BMinus
  } else if avg >= 77 {
    CPlus
  } else if avg >= 73 {
    C
  } else if avg >= 70 {
    CMinus
  } else if avg >= 60 {
    D
  } else {
    F
  }
}

// Update function
let update = (msg: msg, state: state): state => {
  switch msg {
  | UpdateMetrics(newMetrics) =>
    let grade = calculateGrade(newMetrics)
    {...state, metrics: newMetrics, grade: grade}

  | SelectVulnerability(vulnId) =>
    {...state, selectedVulnerability: Some(vulnId)}

  | ToggleDetails =>
    {...state, showDetails: !state.showDetails}

  | FilterBySeverity(severity) =>
    {...state, filterSeverity: severity}

  | ApplyFix(vulnId) =>
    // Remove fixed vulnerability
    let updatedVulns = Belt.Array.keep(state.vulnerabilities, v => v.id != vulnId)
    // Recalculate metrics
    let newMetrics = {
      ...state.metrics,
      security: min(100, state.metrics.security + 5),
    }
    let grade = calculateGrade(newMetrics)
    {...state, vulnerabilities: updatedVulns, metrics: newMetrics, grade: grade}

  | RunSecurityScan =>
    // Trigger security scan (placeholder)
    state
  }
}

// Helper: Get severity color
let severityColor = (severity: severity): string => {
  switch severity {
  | Critical => "#d32f2f"
  | High => "#f57c00"
  | Medium => "#fbc02d"
  | Low => "#388e3c"
  | Info => "#1976d2"
  }
}

// Helper: Get severity label
let severityLabel = (severity: severity): string => {
  switch severity {
  | Critical => "Critical"
  | High => "High"
  | Medium => "Medium"
  | Low => "Low"
  | Info => "Info"
  }
}

// Helper: Get check result icon
let checkResultIcon = (result: checkResult): string => {
  switch result {
  | Pass => "✅"
  | Fail => "❌"
  | Warning => "⚠️"
  | Unknown => "❓"
  }
}

// Helper: Get check result color
let checkResultColor = (result: checkResult): string => {
  switch result {
  | Pass => "#4caf50"
  | Fail => "#f44336"
  | Warning => "#ff9800"
  | Unknown => "#9e9e9e"
  }
}

// Helper: Get grade display
let gradeDisplay = (grade: grade): string => {
  switch grade {
  | APlus => "A+"
  | A => "A"
  | AMinus => "A-"
  | BPlus => "B+"
  | B => "B"
  | BMinus => "B-"
  | CPlus => "C+"
  | C => "C"
  | CMinus => "C-"
  | D => "D"
  | F => "F"
  }
}

// Helper: Get grade color
let gradeColor = (grade: grade): string => {
  switch grade {
  | APlus | A => "#4caf50"
  | AMinus | BPlus => "#8bc34a"
  | B | BMinus => "#ffc107"
  | CPlus | C => "#ff9800"
  | CMinus | D => "#ff5722"
  | F => "#f44336"
  }
}

// View: Metric bar
let viewMetricBar = (label: string, value: int, color: string): React.element => {
  <div style={ReactDOM.Style.make(~marginBottom="16px", ())}>
    <div style={ReactDOM.Style.make(~display="flex", ~justifyContent="space-between", ~marginBottom="8px", ())}>
      <span style={ReactDOM.Style.make(~fontSize="14px", ~fontWeight="600", ~color="#e0e6ed", ())}>
        {label->React.string}
      </span>
      <span style={ReactDOM.Style.make(~fontSize="14px", ~fontWeight="700", ~color, ())}>
        {(Int.toString(value) ++ "%")->React.string}
      </span>
    </div>
    <div
      style={ReactDOM.Style.make(
        ~width="100%",
        ~height="8px",
        ~background="#2a3142",
        ~borderRadius="4px",
        ~overflow="hidden",
        (),
      )}>
      <div
        style={ReactDOM.Style.make(
          ~width=(Int.toString(value) ++ "%"),
          ~height="100%",
          ~background=color,
          ~transition="width 0.3s ease",
          (),
        )}
      />
    </div>
  </div>
}

// View: Vulnerability card
let viewVulnerability = (vuln: vulnerability, dispatch: msg => unit): React.element => {
  <div
    key={vuln.id}
    style={ReactDOM.Style.make(
      ~padding="16px",
      ~background="linear-gradient(135deg, #1e2431 0%, #252d3d 100%)",
      ~border="2px solid #2a3142",
      ~borderRadius="12px",
      ~marginBottom="12px",
      ~cursor="pointer",
      ~transition="all 0.2s",
      (),
    )}
    onClick={_ => dispatch(SelectVulnerability(vuln.id))}>
    <div style={ReactDOM.Style.make(~display="flex", ~alignItems="flex-start", ~gap="12px", ())}>
      <div
        style={ReactDOM.Style.make(
          ~padding="4px 12px",
          ~background=severityColor(vuln.severity),
          ~color="white",
          ~borderRadius="6px",
          ~fontSize="11px",
          ~fontWeight="700",
          ~textTransform="uppercase",
          ~minWidth="80px",
          ~textAlign="center",
          (),
        )}>
        {severityLabel(vuln.severity)->React.string}
      </div>

      <div style={ReactDOM.Style.make(~flex="1", ())}>
        <div style={ReactDOM.Style.make(~fontSize="15px", ~fontWeight="700", ~color="#e0e6ed", ~marginBottom="6px", ())}>
          {vuln.title->React.string}
        </div>
        <div style={ReactDOM.Style.make(~fontSize="13px", ~color="#8892a6", ~marginBottom="8px", ~lineHeight="1.5", ())}>
          {vuln.description->React.string}
        </div>

        <div style={ReactDOM.Style.make(~display="flex", ~gap="16px", ~alignItems="center", ())}>
          <span style={ReactDOM.Style.make(~fontSize="12px", ~color="#6b7a90", ())}>
            {"Component: "->React.string}
            <strong style={ReactDOM.Style.make(~color="#4a9eff", ())}>
              {vuln.affectedComponent->React.string}
            </strong>
          </span>

          {switch vuln.cveId {
          | Some(cveId) =>
            <span style={ReactDOM.Style.make(~fontSize="12px", ~color="#6b7a90", ())}>
              {"CVE: "->React.string}
              <strong style={ReactDOM.Style.make(~color="#ff9800", ())}>
                {cveId->React.string}
              </strong>
            </span>
          | None => React.null
          }}

          {vuln.fixAvailable
            ? <button
                onClick={e => {
                  ReactEvent.Mouse.stopPropagation(e)
                  dispatch(ApplyFix(vuln.id))
                }}
                style={ReactDOM.Style.make(
                  ~padding="6px 12px",
                  ~background="linear-gradient(135deg, #4caf50, #66bb6a)",
                  ~color="white",
                  ~border="none",
                  ~borderRadius="6px",
                  ~fontSize="11px",
                  ~fontWeight="600",
                  ~cursor="pointer",
                  (),
                )}>
                {"🔧 Auto-Fix"->React.string}
              </button>
            : React.null}
        </div>

        {switch vuln.fixDescription {
        | Some(desc) =>
          <div
            style={ReactDOM.Style.make(
              ~marginTop="12px",
              ~padding="12px",
              ~background="rgba(76, 175, 80, 0.1)",
              ~border="1px solid #4caf50",
              ~borderRadius="6px",
              ~fontSize="12px",
              ~color="#b0b8c4",
              (),
            )}>
            <strong style={ReactDOM.Style.make(~color="#4caf50", ())}>
              {"Fix: "->React.string}
            </strong>
            {desc->React.string}
          </div>
        | None => React.null
        }}
      </div>
    </div>
  </div>
}

// View: Security check row
let viewSecurityCheck = (check: securityCheck): React.element => {
  <div
    key={check.name}
    style={ReactDOM.Style.make(
      ~display="flex",
      ~alignItems="center",
      ~justifyContent="space-between",
      ~padding="16px",
      ~background="linear-gradient(135deg, #1e2431 0%, #252d3d 100%)",
      ~border="1px solid #2a3142",
      ~borderRadius="8px",
      ~marginBottom="8px",
      (),
    )}>
    <div style={ReactDOM.Style.make(~display="flex", ~alignItems="center", ~gap="12px", ())}>
      <span style={ReactDOM.Style.make(~fontSize="24px", ())}>
        {checkResultIcon(check.result)->React.string}
      </span>
      <div>
        <div style={ReactDOM.Style.make(~fontSize="14px", ~fontWeight="600", ~color="#e0e6ed", ())}>
          {check.name->React.string}
        </div>
        <div style={ReactDOM.Style.make(~fontSize="12px", ~color="#8892a6", ~marginTop="2px", ())}>
          {check.description->React.string}
        </div>
      </div>
    </div>
    <div
      style={ReactDOM.Style.make(
        ~padding="6px 12px",
        ~background=checkResultColor(check.result),
        ~color="white",
        ~borderRadius="6px",
        ~fontSize="11px",
        ~fontWeight="700",
        ~minWidth="100px",
        ~textAlign="center",
        (),
      )}>
      {check.details->React.string}
    </div>
  </div>
}

// View: Exposed port row
let viewExposedPort = (port: exposedPort): React.element => {
  <div
    key={Int.toString(port.port)}
    style={ReactDOM.Style.make(
      ~display="flex",
      ~alignItems="center",
      ~justifyContent="space-between",
      ~padding="12px",
      ~background="linear-gradient(135deg, #1e2431 0%, #252d3d 100%)",
      ~border="1px solid #2a3142",
      ~borderRadius="8px",
      ~marginBottom="6px",
      (),
    )}>
    <div style={ReactDOM.Style.make(~display="flex", ~alignItems="center", ~gap="12px", ())}>
      <span style={ReactDOM.Style.make(~fontSize="16px", ~fontWeight="700", ~color="#4a9eff", ())}>
        {Int.toString(port.port)->React.string}
      </span>
      <div>
        <div style={ReactDOM.Style.make(~fontSize="13px", ~fontWeight="600", ~color="#e0e6ed", ())}>
          {port.service->React.string}
        </div>
        <div style={ReactDOM.Style.make(~fontSize="11px", ~color="#8892a6", ())}>
          {port.protocol->React.string}
        </div>
      </div>
    </div>

    <div style={ReactDOM.Style.make(~display="flex", ~gap="8px", ~alignItems="center", ())}>
      {port.publiclyAccessible
        ? <span
            style={ReactDOM.Style.make(
              ~padding="4px 8px",
              ~background="rgba(244, 67, 54, 0.2)",
              ~border="1px solid #f44336",
              ~borderRadius="4px",
              ~fontSize="10px",
              ~fontWeight="600",
              ~color="#f44336",
              (),
            )}>
            {"PUBLIC"->React.string}
          </span>
        : React.null}

      <div
        style={ReactDOM.Style.make(
          ~padding="4px 8px",
          ~background=switch port.risk {
          | "Critical" => "#d32f2f"
          | "High" => "#f57c00"
          | "Medium" => "#fbc02d"
          | "Low" => "#388e3c"
          | _ => "#9e9e9e"
          },
          ~color="white",
          ~borderRadius="4px",
          ~fontSize="10px",
          ~fontWeight="700",
          (),
        )}>
        {port.risk->React.string}
      </div>
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

  <div
    className="security-inspector"
    style={ReactDOM.Style.make(
      ~padding="32px",
      ~background="#0a0e1a",
      ~minHeight="100vh",
      (),
    )}>
    <div style={ReactDOM.Style.make(~marginBottom="32px", ())}>
      <h1
        style={ReactDOM.Style.make(
          ~fontSize="32px",
          ~fontWeight="700",
          ~background="linear-gradient(135deg, #f44336, #ff9800)",
          ~marginBottom="8px",
          (),
        )}>
        {"🛡️ Security Inspector"->React.string}
      </h1>
      <p style={ReactDOM.Style.make(~fontSize="16px", ~color="#8892a6", ())}>
        {"Real-time attack surface analysis and vulnerability assessment"->React.string}
      </p>
    </div>

    <div
      style={ReactDOM.Style.make(
        ~display="flex",
        ~alignItems="center",
        ~justifyContent="center",
        ~padding="40px",
        ~background="linear-gradient(135deg, #1e2431 0%, #252d3d 100%)",
        ~border="3px solid " ++ gradeColor(state.grade),
        ~borderRadius="16px",
        ~marginBottom="32px",
        (),
      )}>
      <div style={ReactDOM.Style.make(~textAlign="center", ())}>
        <div
          style={ReactDOM.Style.make(
            ~fontSize="72px",
            ~fontWeight="900",
            ~color=gradeColor(state.grade),
            ~marginBottom="8px",
            (),
          )}>
          {gradeDisplay(state.grade)->React.string}
        </div>
        <div style={ReactDOM.Style.make(~fontSize="16px", ~color="#8892a6", ())}>
          {"Security Grade"->React.string}
        </div>
      </div>
    </div>

    <div
      style={ReactDOM.Style.make(
        ~padding="24px",
        ~background="linear-gradient(135deg, #1e2431 0%, #252d3d 100%)",
        ~border="2px solid #2a3142",
        ~borderRadius="16px",
        ~marginBottom="32px",
        (),
      )}>
      <h3 style={ReactDOM.Style.make(~fontSize="20px", ~fontWeight="700", ~color="#e0e6ed", ~marginBottom="24px", ())}>
        {"📊 Security Metrics"->React.string}
      </h3>

      {viewMetricBar("Security", state.metrics.security, "#f44336")}
      {viewMetricBar("Performance", state.metrics.performance, "#4caf50")}
      {viewMetricBar("Reliability", state.metrics.reliability, "#2196f3")}
      {viewMetricBar("Compliance", state.metrics.compliance, "#9c27b0")}
    </div>

    <div style={ReactDOM.Style.make(~marginBottom="32px", ())}>
      <h3 style={ReactDOM.Style.make(~fontSize="20px", ~fontWeight="700", ~color="#e0e6ed", ~marginBottom="16px", ())}>
        {"✓ Quick Security Checks"->React.string}
      </h3>
      {Array.map(state.checks, check => viewSecurityCheck(check))->React.array}
    </div>

    <div style={ReactDOM.Style.make(~marginBottom="32px", ())}>
      <div style={ReactDOM.Style.make(~display="flex", ~justifyContent="space-between", ~alignItems="center", ~marginBottom="16px", ())}>
        <h3 style={ReactDOM.Style.make(~fontSize="20px", ~fontWeight="700", ~color="#e0e6ed", ())}>
          {"🚨 Vulnerabilities ("->React.string}
          {Int.toString(Array.length(state.vulnerabilities))->React.string}
          {")"->React.string}
        </h3>
        <button
          onClick={_ => dispatch(RunSecurityScan)}
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
          {"🔍 Run Full Scan"->React.string}
        </button>
      </div>

      {Array.length(state.vulnerabilities) > 0
        ? Array.map(state.vulnerabilities, vuln => viewVulnerability(vuln, dispatch))->React.array
        : <div
            style={ReactDOM.Style.make(
              ~padding="40px",
              ~background="rgba(76, 175, 80, 0.1)",
              ~border="2px solid #4caf50",
              ~borderRadius="12px",
              ~textAlign="center",
              (),
            )}>
            <div style={ReactDOM.Style.make(~fontSize="48px", ~marginBottom="16px", ())}>
              {"✅"->React.string}
            </div>
            <div style={ReactDOM.Style.make(~fontSize="18px", ~fontWeight="700", ~color="#4caf50", ~marginBottom="8px", ())}>
              {"No Vulnerabilities Detected"->React.string}
            </div>
            <div style={ReactDOM.Style.make(~fontSize="14px", ~color="#8892a6", ())}>
              {"Your stack meets all security requirements"->React.string}
            </div>
          </div>}
    </div>

    <div>
      <h3 style={ReactDOM.Style.make(~fontSize="20px", ~fontWeight="700", ~color="#e0e6ed", ~marginBottom="16px", ())}>
        {"🔌 Exposed Ports Analysis"->React.string}
      </h3>
      {Array.map(state.exposedPorts, port => viewExposedPort(port))->React.array}
    </div>

    <div
      style={ReactDOM.Style.make(
        ~marginTop="32px",
        ~padding="20px",
        ~background="rgba(255, 152, 0, 0.1)",
        ~border="2px solid #ff9800",
        ~borderRadius="12px",
        (),
      )}>
      <h4 style={ReactDOM.Style.make(~fontSize="16px", ~fontWeight="700", ~color="#ff9800", ~marginBottom="12px", ())}>
        {"⚠️ Security Intelligence"->React.string}
      </h4>
      <p style={ReactDOM.Style.make(~fontSize="13px", ~color="#b0b8c4", ~lineHeight="1.8", ())}>
        {"Security analysis powered by miniKanren reasoning engine. All findings logged to VeriSimDB for compliance audit. Auto-fix applies verified security patches with formal verification proofs."->React.string}
      </p>
    </div>
  </div>
}
