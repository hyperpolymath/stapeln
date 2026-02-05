// SPDX-License-Identifier: PMPL-1.0-or-later
// GapAnalysis.res - Automated gap detection and remediation

// Gap categories
type gapCategory =
  | Security
  | Compliance
  | Performance
  | Reliability
  | BestPractice

// Gap severity
type gapSeverity =
  | Critical
  | High
  | Medium
  | Low

// Fix confidence level
type fixConfidence =
  | Verified    // Formally verified fix
  | High        // High confidence automated fix
  | Medium      // Moderate confidence, may need review
  | Low         // Low confidence, manual review required
  | Manual      // No automated fix available

// Issue provenance (where the issue was detected)
type issueSource =
  | ManualReview
  | AutomatedScan
  | CIWorkflow
  | MiniKanrenReasoning
  | VeriSimDBQuery
  | HypatiaAgent
  | ThirdPartyTool(string)

// Impact scope
type impactScope =
  | SingleComponent(string)
  | MultipleComponents(array<string>)
  | EntireStack
  | ExternalDependencies

// Gap definition
type gap = {
  id: string,
  title: string,
  category: gapCategory,
  severity: gapSeverity,
  description: string,
  impact: string,
  source: issueSource,
  scope: impactScope,
  affectedComponents: array<string>,
  fixAvailable: bool,
  fixConfidence: fixConfidence,
  fixDescription: option<string>,
  fixCommands: option<array<string>>,
  estimatedEffort: string, // e.g., "5 minutes", "1 hour"
  tags: array<string>,
}

// Fix status
type fixStatus =
  | NotStarted
  | InProgress
  | Applied
  | Verified
  | Failed(string)

type state = {
  gaps: array<gap>,
  appliedFixes: Map.t<string, fixStatus>,
  selectedGap: option<string>,
  filterCategory: option<gapCategory>,
  filterSeverity: option<gapSeverity>,
  showOnlyFixable: bool,
  sortBy: sortOption,
}

and sortOption =
  | BySeverity
  | ByCategory
  | ByEffort
  | BySource

// Message types
type msg =
  | SelectGap(string)
  | ApplyFix(string)
  | VerifyFix(string)
  | FilterByCategory(option<gapCategory>)
  | FilterBySeverity(option<gapSeverity>)
  | ToggleShowOnlyFixable
  | ChangeSortOrder(sortOption)
  | RunGapAnalysis
  | ApplyAllAutoFixes

// Initialize with sample gaps
let init: state = {
  gaps: [
    {
      id: "gap-1",
      title: "Missing health check endpoints",
      category: Reliability,
      severity: High,
      description: "None of the services have configured health check endpoints. This prevents proper monitoring and automated recovery.",
      impact: "Cannot detect service failures automatically. Increased downtime risk.",
      source: AutomatedScan,
      scope: EntireStack,
      affectedComponents: ["node-1", "node-2", "node-3"],
      fixAvailable: true,
      fixConfidence: High,
      fixDescription: Some("Add /health endpoints to all services. Configure readiness and liveness probes."),
      fixCommands: Some([
        "Add health check route to application code",
        "Update stack.yaml with healthcheck configuration",
        "Configure Dustfile recovery strategy",
      ]),
      estimatedEffort: "15 minutes",
      tags: ["monitoring", "reliability", "k8s"],
    },
    {
      id: "gap-2",
      title: "Unencrypted database connection",
      category: Security,
      severity: Critical,
      description: "Connection from Auth Service to PostgreSQL is not encrypted with TLS. Database credentials and query results transmitted in cleartext.",
      impact: "Sensitive data exposure. Violates compliance requirements (GDPR, HIPAA).",
      source: MiniKanrenReasoning,
      scope: SingleComponent("conn-2"),
      affectedComponents: ["node-2", "node-3"],
      fixAvailable: true,
      fixConfidence: Verified,
      fixDescription: Some("Enable TLS on PostgreSQL connection. Generate TLS certificates. Update connection string."),
      fixCommands: Some([
        "Generate TLS certificates with Ed448",
        "Configure PostgreSQL to require SSL",
        "Update connection string: postgresql://...?sslmode=require",
        "Mount certificates in containers",
      ]),
      estimatedEffort: "30 minutes",
      tags: ["encryption", "compliance", "database"],
    },
    {
      id: "gap-3",
      title: "Container running as root",
      category: Security,
      severity: High,
      description: "Auth Service container runs as root user (UID 0). Violates principle of least privilege.",
      impact: "Container escape could lead to host compromise. Elevated privilege risk.",
      source: AutomatedScan,
      scope: SingleComponent("node-2"),
      affectedComponents: ["node-2"],
      fixAvailable: true,
      fixConfidence: High,
      fixDescription: Some("Add USER directive to Containerfile. Create non-privileged user with minimal permissions."),
      fixCommands: Some([
        "Add to Containerfile: RUN adduser -D appuser",
        "Add to Containerfile: USER appuser",
        "Rebuild container image",
      ]),
      estimatedEffort: "10 minutes",
      tags: ["least-privilege", "hardening"],
    },
    {
      id: "gap-4",
      title: "Missing resource limits",
      category: Performance,
      severity: Medium,
      description: "PostgreSQL container has no CPU or memory limits. Could monopolize host resources.",
      impact: "Risk of resource starvation for other services. Unpredictable performance.",
      source: CIWorkflow,
      scope: SingleComponent("node-3"),
      affectedComponents: ["node-3"],
      fixAvailable: true,
      fixConfidence: Medium,
      fixDescription: Some("Add resource limits to stack.yaml. Recommended: 2 CPU, 4GB memory."),
      fixCommands: Some([
        "Update stack.yaml with deploy.resources.limits",
        "Test with realistic workload",
        "Adjust limits based on monitoring",
      ]),
      estimatedEffort: "20 minutes",
      tags: ["resources", "performance"],
    },
    {
      id: "gap-5",
      title: "No rate limiting on API Gateway",
      category: Security,
      severity: High,
      description: "API Gateway has no rate limiting configured. Vulnerable to DDoS and brute force attacks.",
      impact: "Service availability risk. Increased infrastructure costs from abuse.",
      source: HypatiaAgent,
      scope: SingleComponent("node-1"),
      affectedComponents: ["node-1"],
      fixAvailable: true,
      fixConfidence: High,
      fixDescription: Some("Configure nginx rate limiting. Recommended: 100 req/min per IP."),
      fixCommands: Some([
        "Add limit_req_zone to nginx.conf",
        "Configure burst and nodelay parameters",
        "Add rate limit headers to responses",
      ]),
      estimatedEffort: "25 minutes",
      tags: ["ddos", "rate-limiting", "nginx"],
    },
    {
      id: "gap-6",
      title: "Missing SBOM generation",
      category: Compliance,
      severity: Medium,
      description: "No Software Bill of Materials (SBOM) generated for container images. Required for supply chain security.",
      impact: "Cannot track vulnerabilities in dependencies. Violates supply chain policy.",
      source: VeriSimDBQuery,
      scope: EntireStack,
      affectedComponents: ["node-1", "node-2", "node-3"],
      fixAvailable: true,
      fixConfidence: Verified,
      fixDescription: Some("Add syft to build pipeline. Generate CycloneDX SBOMs for all images."),
      fixCommands: Some([
        "Install syft: curl -sSfL https://raw.githubusercontent.com/anchore/syft/main/install.sh | sh",
        "Add to Justfile: syft <image> -o cyclonedx-json > sbom.json",
        "Sign SBOMs with Ed448+Dilithium5",
        "Upload to VeriSimDB",
      ]),
      estimatedEffort: "45 minutes",
      tags: ["sbom", "supply-chain", "compliance"],
    },
  ],
  appliedFixes: Map.make(),
  selectedGap: None,
  filterCategory: None,
  filterSeverity: None,
  showOnlyFixable: false,
  sortBy: BySeverity,
}

// Update function
let update = (msg: msg, state: state): state => {
  switch msg {
  | SelectGap(gapId) =>
    {...state, selectedGap: Some(gapId)}

  | ApplyFix(gapId) =>
    let updatedFixes = Map.set(state.appliedFixes, gapId, InProgress)
    // In real implementation, would trigger fix application
    // For now, simulate immediate verification
    let finalFixes = Map.set(updatedFixes, gapId, Applied)
    {...state, appliedFixes: finalFixes}

  | VerifyFix(gapId) =>
    let updatedFixes = Map.set(state.appliedFixes, gapId, Verified)
    {...state, appliedFixes: updatedFixes}

  | FilterByCategory(category) =>
    {...state, filterCategory: category}

  | FilterBySeverity(severity) =>
    {...state, filterSeverity: severity}

  | ToggleShowOnlyFixable =>
    {...state, showOnlyFixable: !state.showOnlyFixable}

  | ChangeSortOrder(sortBy) =>
    {...state, sortBy: sortBy}

  | RunGapAnalysis =>
    // Trigger gap analysis (placeholder)
    state

  | ApplyAllAutoFixes =>
    // Apply all high-confidence fixes
    let updatedFixes = Array.reduce(state.gaps, state.appliedFixes, (fixes, gap) =>
      if gap.fixAvailable && (gap.fixConfidence == Verified || gap.fixConfidence == High) {
        Map.set(fixes, gap.id, Applied)
      } else {
        fixes
      }
    )
    {...state, appliedFixes: updatedFixes}
  }
}

// Helper: Get category color
let categoryColor = (category: gapCategory): string => {
  switch category {
  | Security => "#f44336"
  | Compliance => "#9c27b0"
  | Performance => "#4caf50"
  | Reliability => "#2196f3"
  | BestPractice => "#ff9800"
  }
}

// Helper: Get category label
let categoryLabel = (category: gapCategory): string => {
  switch category {
  | Security => "Security"
  | Compliance => "Compliance"
  | Performance => "Performance"
  | Reliability => "Reliability"
  | BestPractice => "Best Practice"
  }
}

// Helper: Get severity color
let severityColor = (severity: gapSeverity): string => {
  switch severity {
  | Critical => "#d32f2f"
  | High => "#f57c00"
  | Medium => "#fbc02d"
  | Low => "#388e3c"
  }
}

// Helper: Get severity label
let severityLabel = (severity: gapSeverity): string => {
  switch severity {
  | Critical => "Critical"
  | High => "High"
  | Medium => "Medium"
  | Low => "Low"
  }
}

// Helper: Get source icon
let sourceIcon = (source: issueSource): string => {
  switch source {
  | ManualReview => "👤"
  | AutomatedScan => "🤖"
  | CIWorkflow => "🔄"
  | MiniKanrenReasoning => "🧠"
  | VeriSimDBQuery => "🗄️"
  | HypatiaAgent => "🤖"
  | ThirdPartyTool(_) => "🔧"
  }
}

// Helper: Get source label
let sourceLabel = (source: issueSource): string => {
  switch source {
  | ManualReview => "Manual Review"
  | AutomatedScan => "Automated Scan"
  | CIWorkflow => "CI Workflow"
  | MiniKanrenReasoning => "miniKanren"
  | VeriSimDBQuery => "VeriSimDB"
  | HypatiaAgent => "Hypatia"
  | ThirdPartyTool(name) => name
  }
}

// Helper: Get confidence badge
let confidenceBadge = (confidence: fixConfidence): React.element => {
  let (label, color) = switch confidence {
  | Verified => ("Verified", "#4caf50")
  | High => ("High Confidence", "#66bb6a")
  | Medium => ("Medium Confidence", "#ff9800")
  | Low => ("Low Confidence", "#ff5722")
  | Manual => ("Manual Required", "#9e9e9e")
  }

  <span
    style={ReactDOM.Style.make(
      ~padding="4px 10px",
      ~background=color,
      ~color="white",
      ~borderRadius="6px",
      ~fontSize="11px",
      ~fontWeight="700",
      (),
    )}>
    {label->React.string}
  </span>
}

// Helper: Get fix status icon
let fixStatusIcon = (status: fixStatus): string => {
  switch status {
  | NotStarted => "⏳"
  | InProgress => "🔄"
  | Applied => "✅"
  | Verified => "🔐"
  | Failed(_) => "❌"
  }
}

// View: Gap card
let viewGap = (gap: gap, fixStatus: option<fixStatus>, dispatch: msg => unit): React.element => {
  <div
    key={gap.id}
    style={ReactDOM.Style.make(
      ~padding="20px",
      ~background="linear-gradient(135deg, #1e2431 0%, #252d3d 100%)",
      ~border="2px solid #2a3142",
      ~borderRadius="12px",
      ~marginBottom="16px",
      ~cursor="pointer",
      ~transition="all 0.2s",
      (),
    )}
    onClick={_ => dispatch(SelectGap(gap.id))}>
    <div style={ReactDOM.Style.make(~display="flex", ~justifyContent="space-between", ~alignItems="flex-start", ~marginBottom="12px", ())}>
      <div style={ReactDOM.Style.make(~flex="1", ())}>
        <div style={ReactDOM.Style.make(~display="flex", ~gap="8px", ~marginBottom="8px", ~flexWrap="wrap", ())}>
          <span
            style={ReactDOM.Style.make(
              ~padding="4px 12px",
              ~background=categoryColor(gap.category),
              ~color="white",
              ~borderRadius="6px",
              ~fontSize="11px",
              ~fontWeight="700",
              (),
            )}>
            {categoryLabel(gap.category)->React.string}
          </span>

          <span
            style={ReactDOM.Style.make(
              ~padding="4px 12px",
              ~background=severityColor(gap.severity),
              ~color="white",
              ~borderRadius="6px",
              ~fontSize="11px",
              ~fontWeight="700",
              (),
            )}>
            {severityLabel(gap.severity)->React.string}
          </span>

          {gap.fixAvailable ? confidenceBadge(gap.fixConfidence) : React.null}

          {switch fixStatus {
          | Some(status) =>
            <span style={ReactDOM.Style.make(~fontSize="18px", ())}>
              {fixStatusIcon(status)->React.string}
            </span>
          | None => React.null
          }}
        </div>

        <h3 style={ReactDOM.Style.make(~fontSize="18px", ~fontWeight="700", ~color="#e0e6ed", ~marginBottom="8px", ())}>
          {gap.title->React.string}
        </h3>
      </div>
    </div>

    <p style={ReactDOM.Style.make(~fontSize="13px", ~color="#8892a6", ~lineHeight="1.6", ~marginBottom="12px", ())}>
      {gap.description->React.string}
    </p>

    <div
      style={ReactDOM.Style.make(
        ~padding="12px",
        ~background="rgba(244, 67, 54, 0.1)",
        ~border="1px solid #f44336",
        ~borderRadius="8px",
        ~marginBottom="12px",
        (),
      )}>
      <strong style={ReactDOM.Style.make(~fontSize="12px", ~color="#f44336", ())}>
        {"Impact: "->React.string}
      </strong>
      <span style={ReactDOM.Style.make(~fontSize="12px", ~color="#b0b8c4", ())}>
        {gap.impact->React.string}
      </span>
    </div>

    <div style={ReactDOM.Style.make(~display="flex", ~gap="24px", ~marginBottom="16px", ~flexWrap="wrap", ())}>
      <div style={ReactDOM.Style.make(~fontSize="12px", ~color="#8892a6", ())}>
        <span style={ReactDOM.Style.make(~marginRight="6px", ())}>
          {sourceIcon(gap.source)->React.string}
        </span>
        <strong style={ReactDOM.Style.make(~color="#4a9eff", ())}>
          {sourceLabel(gap.source)->React.string}
        </strong>
      </div>

      <div style={ReactDOM.Style.make(~fontSize="12px", ~color="#8892a6", ())}>
        {"Affects: "->React.string}
        <strong style={ReactDOM.Style.make(~color="#ff9800", ())}>
          {Int.toString(Array.length(gap.affectedComponents))->React.string}
          {" component"->React.string}
          {Array.length(gap.affectedComponents) != 1 ? "s"->React.string : React.null}
        </strong>
      </div>

      <div style={ReactDOM.Style.make(~fontSize="12px", ~color="#8892a6", ())}>
        {"⏱️ "->React.string}
        <strong style={ReactDOM.Style.make(~color="#4caf50", ())}>
          {gap.estimatedEffort->React.string}
        </strong>
      </div>
    </div>

    <div style={ReactDOM.Style.make(~display="flex", ~gap="6px", ~marginBottom="16px", ~flexWrap="wrap", ())}>
      {Array.map(gap.tags, tag =>
        <span
          key={tag}
          style={ReactDOM.Style.make(
            ~padding="3px 8px",
            ~background="#2a3142",
            ~color="#8892a6",
            ~borderRadius="4px",
            ~fontSize="10px",
            ~fontWeight="600",
            (),
          )}>
          {"#"->React.string}
          {tag->React.string}
        </span>
      )->React.array}
    </div>

    {gap.fixAvailable
      ? <div>
          {switch gap.fixDescription {
          | Some(desc) =>
            <div
              style={ReactDOM.Style.make(
                ~padding="12px",
                ~background="rgba(76, 175, 80, 0.1)",
                ~border="1px solid #4caf50",
                ~borderRadius="8px",
                ~marginBottom="12px",
                (),
              )}>
              <strong style={ReactDOM.Style.make(~fontSize="12px", ~color="#4caf50", ())}>
                {"Fix: "->React.string}
              </strong>
              <span style={ReactDOM.Style.make(~fontSize="12px", ~color="#b0b8c4", ())}>
                {desc->React.string}
              </span>
            </div>
          | None => React.null
          }}

          {switch gap.fixCommands {
          | Some(commands) =>
            <div style={ReactDOM.Style.make(~marginBottom="12px", ())}>
              <div style={ReactDOM.Style.make(~fontSize="12px", ~fontWeight="600", ~color="#8892a6", ~marginBottom="6px", ())}>
                {"Steps:"->React.string}
              </div>
              <ol style={ReactDOM.Style.make(~paddingLeft="20px", ~fontSize="12px", ~color="#b0b8c4", ~lineHeight="1.8", ())}>
                {Array.map(commands, cmd =>
                  <li key={cmd}>
                    {cmd->React.string}
                  </li>
                )->React.array}
              </ol>
            </div>
          | None => React.null
          }}

          <div style={ReactDOM.Style.make(~display="flex", ~gap="8px", ())}>
            {switch fixStatus {
            | Some(Applied) | Some(Verified) =>
              <button
                disabled=true
                style={ReactDOM.Style.make(
                  ~padding="10px 20px",
                  ~background="#4caf50",
                  ~color="white",
                  ~border="none",
                  ~borderRadius="8px",
                  ~fontSize="13px",
                  ~fontWeight="600",
                  ~cursor="not-allowed",
                  ~opacity="0.7",
                  (),
                )}>
                {"✓ Fix Applied"->React.string}
              </button>
            | _ =>
              <button
                onClick={e => {
                  ReactEvent.Mouse.stopPropagation(e)
                  dispatch(ApplyFix(gap.id))
                }}
                style={ReactDOM.Style.make(
                  ~padding="10px 20px",
                  ~background="linear-gradient(135deg, #4caf50, #66bb6a)",
                  ~color="white",
                  ~border="none",
                  ~borderRadius="8px",
                  ~fontSize="13px",
                  ~fontWeight="600",
                  ~cursor="pointer",
                  (),
                )}>
                {"🔧 Apply Fix"->React.string}
              </button>
            }}

            {switch fixStatus {
            | Some(Applied) =>
              <button
                onClick={e => {
                  ReactEvent.Mouse.stopPropagation(e)
                  dispatch(VerifyFix(gap.id))
                }}
                style={ReactDOM.Style.make(
                  ~padding="10px 20px",
                  ~background="linear-gradient(135deg, #2196f3, #42a5f5)",
                  ~color="white",
                  ~border="none",
                  ~borderRadius="8px",
                  ~fontSize="13px",
                  ~fontWeight="600",
                  ~cursor="pointer",
                  (),
                )}>
                {"🔐 Verify Fix"->React.string}
              </button>
            | _ => React.null
            }}
          </div>
        </div>
      : <div
          style={ReactDOM.Style.make(
            ~padding="12px",
            ~background="rgba(158, 158, 158, 0.1)",
            ~border="1px solid #9e9e9e",
            ~borderRadius="8px",
            ~fontSize="12px",
            ~color="#9e9e9e",
            (),
          )}>
          {"⚠️ No automated fix available. Manual remediation required."->React.string}
        </div>}
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

  // Filter gaps
  let filteredGaps = Array.reduce(state.gaps, [], (acc, gap) => {
    let matchesCategory = switch state.filterCategory {
    | Some(cat) => gap.category == cat
    | None => true
    }
    let matchesSeverity = switch state.filterSeverity {
    | Some(sev) => gap.severity == sev
    | None => true
    }
    let matchesFixable = !state.showOnlyFixable || gap.fixAvailable

    if matchesCategory && matchesSeverity && matchesFixable {
      Array.concat(acc, [gap])
    } else {
      acc
    }
  })

  <div
    className="gap-analysis"
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
          ~background="linear-gradient(135deg, #4a9eff, #7b6cff)",
          ~webkitBackgroundClip="text",
          ~webkitTextFillColor="transparent",
          ~marginBottom="8px",
          (),
        )}>
        {"🔍 Gap Analysis"->React.string}
      </h1>
      <p style={ReactDOM.Style.make(~fontSize="16px", ~color="#8892a6", ())}>
        {"Automated detection and remediation of security and compliance gaps"->React.string}
      </p>
    </div>

    <div
      style={ReactDOM.Style.make(
        ~display="grid",
        ~gridTemplateColumns="repeat(4, 1fr)",
        ~gap="16px",
        ~marginBottom="32px",
        (),
      )}>
      {[
        ("Total Gaps", Int.toString(Array.length(state.gaps)), "#4a9eff"),
        ("Critical", Int.toString(Array.reduce(state.gaps, 0, (acc, g) => g.severity == Critical ? acc + 1 : acc)), "#f44336"),
        ("Auto-Fixable", Int.toString(Array.reduce(state.gaps, 0, (acc, g) => g.fixAvailable ? acc + 1 : acc)), "#4caf50"),
        ("Applied", Int.toString(Map.size(state.appliedFixes)), "#ff9800"),
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
            {count->React.string}
          </div>
          <div style={ReactDOM.Style.make(~fontSize="13px", ~color="#8892a6", ())}>
            {label->React.string}
          </div>
        </div>
      })->React.array}
    </div>

    <div
      style={ReactDOM.Style.make(
        ~display="flex",
        ~justifyContent="space-between",
        ~alignItems="center",
        ~padding="16px",
        ~background="linear-gradient(135deg, #1e2431 0%, #252d3d 100%)",
        ~border="2px solid #2a3142",
        ~borderRadius="12px",
        ~marginBottom="24px",
        (),
      )}>
      <div style={ReactDOM.Style.make(~display="flex", ~gap="12px", ())}>
        <button
          onClick={_ => dispatch(RunGapAnalysis)}
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
          {"🔍 Run Analysis"->React.string}
        </button>

        <button
          onClick={_ => dispatch(ApplyAllAutoFixes)}
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
          {"⚡ Apply All Auto-Fixes"->React.string}
        </button>
      </div>

      <label style={ReactDOM.Style.make(~display="flex", ~alignItems="center", ~gap="8px", ~fontSize="13px", ~color="#8892a6", ~cursor="pointer", ())}>
        <input
          type_="checkbox"
          checked={state.showOnlyFixable}
          onChange={_ => dispatch(ToggleShowOnlyFixable)}
        />
        {"Show only fixable"->React.string}
      </label>
    </div>

    <div>
      {Array.length(filteredGaps) > 0
        ? Array.map(filteredGaps, gap =>
            viewGap(gap, Map.get(state.appliedFixes, gap.id), dispatch)
          )->React.array
        : <div
            style={ReactDOM.Style.make(
              ~padding="60px",
              ~background="rgba(76, 175, 80, 0.1)",
              ~border="2px solid #4caf50",
              ~borderRadius="16px",
              ~textAlign="center",
              (),
            )}>
            <div style={ReactDOM.Style.make(~fontSize="64px", ~marginBottom="16px", ())}>
              {"✅"->React.string}
            </div>
            <div style={ReactDOM.Style.make(~fontSize="24px", ~fontWeight="700", ~color="#4caf50", ~marginBottom="12px", ())}>
              {"No Gaps Detected"->React.string}
            </div>
            <div style={ReactDOM.Style.make(~fontSize="16px", ~color="#8892a6", ())}>
              {"Your stack meets all security, compliance, and best practice requirements"->React.string}
            </div>
          </div>}
    </div>

    <div
      style={ReactDOM.Style.make(
        ~marginTop="32px",
        ~padding="20px",
        ~background="rgba(74, 158, 255, 0.1)",
        ~border="2px solid #4a9eff",
        ~borderRadius="12px",
        (),
      )}>
      <h4 style={ReactDOM.Style.make(~fontSize="16px", ~fontWeight="700", ~color="#4a9eff", ~marginBottom="12px", ())}>
        {"🤖 AI-Powered Gap Analysis"->React.string}
      </h4>
      <p style={ReactDOM.Style.make(~fontSize="13px", ~color="#b0b8c4", ~lineHeight="1.8", ())}>
        {"Gap detection powered by miniKanren reasoning engine, Hypatia neurosymbolic agent, and VeriSimDB historical analysis. Fixes verified with formal proofs before application. All changes logged to compliance audit trail."->React.string}
      </p>
    </div>
  </div>
}
