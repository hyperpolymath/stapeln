// SPDX-License-Identifier: PMPL-1.0-or-later
// DomMounterSecurity.res - Phase 2 security hardening

// ============================================================================
// PHASE 2: SECURITY HARDENING
// ============================================================================

type cspResult =
  | CSPValid
  | InvalidChars
  | ScriptDetected
  | TooLong

type auditSeverity =
  | Info
  | Warning
  | Error
  | Critical

type sandboxMode =
  | NoSandbox
  | IframeSandbox
  | ShadowDOMSandbox

type auditEntry = {
  timestamp: float,
  operation: string,
  elementId: string,
  severity: auditSeverity,
  message: string,
}

type securityPolicy = {
  requireCSP: bool,
  enableAuditLog: bool,
  sandboxMode: sandboxMode,
  maxElementIdLength: int,
}

// ============================================================================
// FFI BINDINGS
// ============================================================================

module FFI = {
  @val external validate_csp: string => int = "validate_csp"
  @val external sanitize_element_id: (string, array<int>, int) => int = "sanitize_element_id"
  @val external audit_log_entry: (string, string, int, string) => int = "audit_log_entry"
  @val external get_audit_log_count: unit => int = "get_audit_log_count"
  @val external clear_audit_log: unit => unit = "clear_audit_log"
  @val external create_sandboxed_mount: (string, int) => int = "create_sandboxed_mount"
  @val external validate_sandbox_config: (int, int, int) => int = "validate_sandbox_config"
  @val external apply_security_policy: (securityPolicy, string) => int = "apply_security_policy"

  let cspResultFromInt = (code: int): cspResult => {
    switch code {
    | 0 => CSPValid
    | 1 => InvalidChars
    | 2 => ScriptDetected
    | _ => TooLong
    }
  }

  let severityToInt = (sev: auditSeverity): int => {
    switch sev {
    | Info => 0
    | Warning => 1
    | Error => 2
    | Critical => 3
    }
  }

  let sandboxModeToInt = (mode: sandboxMode): int => {
    switch mode {
    | NoSandbox => 0
    | IframeSandbox => 1
    | ShadowDOMSandbox => 2
    }
  }
}

// ============================================================================
// CSP VALIDATION
// ============================================================================

let validateCSP = (elementId: string): cspResult => {
  let code = FFI.validate_csp(elementId)
  FFI.cspResultFromInt(code)
}

let isCSPValid = (elementId: string): bool => {
  switch validateCSP(elementId) {
  | CSPValid => true
  | _ => false
  }
}

let cspErrorMessage = (result: cspResult): string => {
  switch result {
  | CSPValid => "Element ID is CSP compliant"
  | InvalidChars => "Element ID contains invalid characters (only alphanumeric, dash, underscore allowed)"
  | ScriptDetected => "Element ID contains script injection attempt (script tags, javascript:, event handlers detected)"
  | TooLong => "Element ID is empty or exceeds 255 character limit"
  }
}

// ============================================================================
// AUDIT LOGGING
// ============================================================================

let logAudit = (
  operation: string,
  elementId: string,
  severity: auditSeverity,
  message: string,
): Result.t<unit, string> => {
  let code = FFI.audit_log_entry(operation, elementId, FFI.severityToInt(severity), message)

  if code == 0 {
    Ok()
  } else {
    Error("Failed to log audit entry (log may be full)")
  }
}

let logMountAttempt = (elementId: string, success: bool): unit => {
  let severity = if success {
    Info
  } else {
    Error
  }
  let message = if success {
    `Successfully mounted to ${elementId}`
  } else {
    `Failed to mount to ${elementId}`
  }

  let _ = logAudit("mount", elementId, severity, message)
}

let logSecurityViolation = (elementId: string, reason: string): unit => {
  let _ = logAudit("security_violation", elementId, Critical, `Security violation: ${reason}`)
}

let getAuditLogCount = (): int => {
  FFI.get_audit_log_count()
}

let clearAuditLog = (): unit => {
  FFI.clear_audit_log()
}

// ============================================================================
// SANDBOXING
// ============================================================================

let createSandboxedMount = (elementId: string, mode: sandboxMode): Result.t<unit, string> => {
  let code = FFI.create_sandboxed_mount(elementId, FFI.sandboxModeToInt(mode))

  switch code {
  | 0 => Ok()
  | -1 => Error("Invalid element ID for sandboxed mount")
  | -2 => Error("Invalid sandbox mode")
  | _ => Error("Failed to create sandboxed mount")
  }
}

let validateSandboxConfig = (allowScripts: bool, allowSameOrigin: bool, allowForms: bool): bool => {
  let code = FFI.validate_sandbox_config(
    allowScripts ? 1 : 0,
    allowSameOrigin ? 1 : 0,
    allowForms ? 1 : 0,
  )

  code == 0
}

// ============================================================================
// SECURITY POLICY
// ============================================================================

let defaultSecurityPolicy: securityPolicy = {
  requireCSP: true,
  enableAuditLog: true,
  sandboxMode: NoSandbox,
  maxElementIdLength: 255,
}

let secureMount = (elementId: string, policy: securityPolicy): Result.t<unit, string> => {
  // Step 1: CSP validation
  let cspResult = if policy.requireCSP {
    switch validateCSP(elementId) {
    | CSPValid => Ok()
    | InvalidChars => {
        if policy.enableAuditLog {
          logSecurityViolation(elementId, "Invalid characters in element ID")
        }
        Error("CSP validation failed: " ++ cspErrorMessage(InvalidChars))
      }
    | ScriptDetected => {
        if policy.enableAuditLog {
          logSecurityViolation(elementId, "Script injection detected in element ID")
        }
        Error("CSP validation failed: " ++ cspErrorMessage(ScriptDetected))
      }
    | TooLong => Error("CSP validation failed: " ++ cspErrorMessage(TooLong))
    }
  } else {
    Ok()
  }

  switch cspResult {
  | Error(e) => Error(e)
  | Ok() => // Step 2: Length validation
    if String.length(elementId) > policy.maxElementIdLength {
      Error(`Element ID exceeds maximum length of ${Int.toString(policy.maxElementIdLength)}`)
    } else {
      // Step 3: Log mount attempt
      if policy.enableAuditLog {
        logMountAttempt(elementId, true)
      }
      Ok()
    }
  }
}

// ============================================================================
// HIGH-LEVEL API
// ============================================================================

// Mount with security checks
let mountSecure = (elementId: string): Result.t<unit, string> => {
  secureMount(elementId, defaultSecurityPolicy)
}

// Mount with custom security policy
let mountWithPolicy = (elementId: string, policy: securityPolicy): Result.t<unit, string> => {
  secureMount(elementId, policy)
}

// Mount in sandbox
let mountSandboxed = (elementId: string, mode: sandboxMode): Result.t<unit, string> => {
  // Validate with security policy first
  switch secureMount(elementId, defaultSecurityPolicy) {
  | Error(e) => Error(e)
  | Ok() => createSandboxedMount(elementId, mode)
  }
}
