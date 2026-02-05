// SPDX-License-Identifier: PMPL-1.0-or-later
// StateSync.res - Cross-component state synchronization

// Port state affects security metrics
type portSecurityImpact = {
  openPorts: int,
  criticalPortsOpen: int,
  ephemeralPortsActive: int,
  securityDelta: int, // How much to adjust security score
}

// Security findings affect gap analysis
type securityToGapSync = {
  vulnerabilityCount: int,
  criticalVulns: int,
  gaps: array<string>, // Gap IDs to highlight
}

// Gap fixes affect security metrics
type gapToSecuritySync = {
  gapsFixed: int,
  securityImprovement: int,
  newComplianceScore: int,
}

// Calculate security impact from port configuration
let calculatePortSecurityImpact = (ports: array<PortConfigPanel.port>): portSecurityImpact => {
  let openPorts = Array.reduce(ports, 0, (acc, p) =>
    p.state == PortConfigPanel.Open ? acc + 1 : acc
  )

  let criticalPortsOpen = Array.reduce(ports, 0, (acc, p) =>
    p.state == PortConfigPanel.Open && p.risk == PortConfigPanel.Critical ? acc + 1 : acc
  )

  let ephemeralPortsActive = Array.reduce(ports, 0, (acc, p) =>
    switch p.state {
    | PortConfigPanel.Ephemeral(_) => acc + 1
    | _ => acc
    }
  )

  // Security scoring:
  // - Each open port: -2 points
  // - Each critical port open: -10 points
  // - Each ephemeral port: -1 point (temporary risk)
  let securityDelta = -(openPorts * 2 + criticalPortsOpen * 10 + ephemeralPortsActive * 1)

  {
    openPorts,
    criticalPortsOpen,
    ephemeralPortsActive,
    securityDelta,
  }
}

// Sync security vulnerabilities to gap analysis
let syncSecurityToGaps = (
  vulnerabilities: array<SecurityInspector.vulnerability>,
): securityToGapSync => {
  let vulnerabilityCount = Array.length(vulnerabilities)

  let criticalVulns = Array.reduce(vulnerabilities, 0, (acc, v) =>
    v.severity == SecurityInspector.Critical ? acc + 1 : acc
  )

  // Map vulnerability IDs to gap IDs that can fix them
  let gaps = Array.map(vulnerabilities, v => "gap-" ++ v.id)

  {
    vulnerabilityCount,
    criticalVulns,
    gaps,
  }
}

// Sync gap fixes back to security metrics
let syncGapFixesToSecurity = (gapsBeforeFix: int, gapsAfterFix: int): gapToSecuritySync => {
  let gapsFixed = gapsBeforeFix - gapsAfterFix

  // Each gap fixed: +5 security points
  let securityImprovement = gapsFixed * 5

  // Compliance improves with gap fixes
  let newComplianceScore = min(100, 70 + gapsFixed * 3)

  {
    gapsFixed,
    securityImprovement,
    newComplianceScore,
  }
}

// Update security metrics based on port changes
let updateSecurityMetricsFromPorts = (
  currentMetrics: SecurityInspector.securityMetrics,
  portImpact: portSecurityImpact,
): SecurityInspector.securityMetrics => {
  {
    security: max(0, min(100, currentMetrics.security + portImpact.securityDelta)),
    performance: currentMetrics.performance,
    reliability: currentMetrics.reliability,
    compliance: currentMetrics.compliance,
  }
}

// Update security metrics based on gap fixes
let updateSecurityMetricsFromGaps = (
  currentMetrics: SecurityInspector.securityMetrics,
  gapSync: gapToSecuritySync,
): SecurityInspector.securityMetrics => {
  {
    security: max(0, min(100, currentMetrics.security + gapSync.securityImprovement)),
    performance: currentMetrics.performance,
    reliability: currentMetrics.reliability,
    compliance: gapSync.newComplianceScore,
  }
}

// Generate security warnings from port configuration
let generatePortWarnings = (ports: array<PortConfigPanel.port>): array<string> => {
  let warnings = []

  // Check for open critical ports
  let openCriticalPorts = Array.keep(ports, p =>
    p.state == PortConfigPanel.Open && p.risk == PortConfigPanel.Critical
  )

  if Array.length(openCriticalPorts) > 0 {
    Array.forEach(openCriticalPorts, port => {
      Array.push(
        warnings,
        `CRITICAL: Port ${Int.toString(port.number)} (${port.protocol}) is open permanently`,
      )
    })
  }

  // Check for too many open ports
  let openPorts = Array.keep(ports, p => p.state == PortConfigPanel.Open)
  if Array.length(openPorts) > 5 {
    Array.push(
      warnings,
      `HIGH: ${Int.toString(
          Array.length(openPorts),
        )} ports open - consider reducing attack surface`,
    )
  }

  // Check for ephemeral ports nearing expiration
  Array.forEach(ports, port => {
    switch port.state {
    | PortConfigPanel.Ephemeral(duration) if duration < 60 =>
      Array.push(
        warnings,
        `INFO: Port ${Int.toString(port.number)} ephemeral pinhole expires in ${Int.toString(
            duration,
          )}s`,
      )
    | _ => ()
    }
  })

  warnings
}

// Calculate overall system health score
let calculateSystemHealth = (
  securityMetrics: SecurityInspector.securityMetrics,
  portImpact: portSecurityImpact,
  gapCount: int,
): int => {
  // Security metrics weighted average
  let metricsScore =
    (securityMetrics.security +
    securityMetrics.performance +
    securityMetrics.reliability +
    securityMetrics.compliance) / 4

  // Port security penalty
  let portPenalty = portImpact.criticalPortsOpen * 10 + portImpact.openPorts * 2

  // Gap penalty
  let gapPenalty = gapCount * 3

  // Overall health (0-100)
  max(0, min(100, metricsScore - portPenalty - gapPenalty))
}
