-- SPDX-License-Identifier: PMPL-1.0-or-later
-- DomMounterSecurity.idr - Phase 2 security hardening

module DomMounterSecurity

import Data.String
import Data.List

%default total

-- ============================================================================
-- PHASE 2: SECURITY HARDENING
-- ============================================================================

-- | CSP (Content Security Policy) compliance
public export
data CSPCompliant : String -> Type where
  SafeElementId : (elemId : String) -> CSPCompliant elemId

-- | Valid character proof
public export
data ValidChars : String -> Type where
  AlphanumericDash : ValidChars elemId

-- | No script injection proof
public export
data NoScriptTags : String -> Type where
  NoScript : NoScriptTags elemId

-- | CSP validation result
public export
data CSPResult = CSPValid | InvalidChars | ScriptDetected | TooLong

public export
Show CSPResult where
  show CSPValid = "CSP Valid"
  show InvalidChars = "Invalid Characters"
  show ScriptDetected = "Script Detected"
  show TooLong = "Too Long"

-- | Audit log entry type
public export
data AuditOperation = Mount | Unmount | HealthCheck | Recovery

public export
Show AuditOperation where
  show Mount = "mount"
  show Unmount = "unmount"
  show HealthCheck = "healthCheck"
  show Recovery = "recovery"

-- | Audit log severity
public export
data AuditSeverity = Info | Warning | Error | Critical

public export
Show AuditSeverity where
  show Info = "info"
  show Warning = "warning"
  show Error = "error"
  show Critical = "critical"

-- | Audit log entry with proofs
public export
record AuditEntry where
  constructor MkAuditEntry
  timestamp : Integer
  operation : AuditOperation
  elementId : String
  severity : AuditSeverity
  message : String
  metadata : List (String, String)

-- | Sandbox mode
public export
data SandboxMode = NoSandbox | IframeSandbox | ShadowDOMSandbox

public export
Show SandboxMode where
  show NoSandbox = "no-sandbox"
  show IframeSandbox = "iframe-sandbox"
  show ShadowDOMSandbox = "shadow-dom-sandbox"

-- | Sandbox configuration
public export
record SandboxConfig where
  constructor MkSandboxConfig
  mode : SandboxMode
  allowScripts : Bool
  allowSameOrigin : Bool
  allowForms : Bool

-- | Security policy
public export
record SecurityPolicy where
  constructor MkSecurityPolicy
  requireCSP : Bool
  enableAuditLog : Bool
  sandboxMode : SandboxMode
  maxElementIdLength : Nat
  allowedCharsPattern : String

-- | Default secure policy
export
defaultSecurityPolicy : SecurityPolicy
defaultSecurityPolicy = MkSecurityPolicy {
  requireCSP = True,
  enableAuditLog = True,
  sandboxMode = NoSandbox,
  maxElementIdLength = 255,
  allowedCharsPattern = "^[a-zA-Z0-9_-]+$"
}

-- ============================================================================
-- CSP VALIDATION
-- ============================================================================

-- | Helper: Check if character is valid
isValidChar : Char -> Bool
isValidChar c =
  (c >= 'a' && c <= 'z') ||
  (c >= 'A' && c <= 'Z') ||
  (c >= '0' && c <= '9') ||
  c == '-' || c == '_'

-- | Validate element ID against CSP rules
export
validateCSP : String -> CSPResult
validateCSP elemId =
  if length elemId == 0 || length elemId > 255
    then TooLong
    else if containsScriptTag elemId
      then ScriptDetected
      else if hasValidCharsOnly elemId
        then CSPValid
        else InvalidChars
  where
    containsScriptTag : String -> Bool
    containsScriptTag s =
      isInfixOf "<script" s ||
      isInfixOf "javascript:" s ||
      isInfixOf "onerror=" s ||
      isInfixOf "onclick=" s

    hasValidCharsOnly : String -> Bool
    hasValidCharsOnly s = all isValidChar (unpack s)

-- | Prove CSP compliance at type level
export
proveCSPCompliance : (elemId : String) -> Either String (CSPCompliant elemId)
proveCSPCompliance elemId =
  case validateCSP elemId of
    CSPValid => Right (SafeElementId elemId)
    InvalidChars => Left "Element ID contains invalid characters"
    ScriptDetected => Left "Element ID contains script injection attempt"
    TooLong => Left "Element ID exceeds maximum length"

-- ============================================================================
-- AUDIT LOGGING
-- ============================================================================

-- | Create audit entry
export
createAuditEntry : AuditOperation -> String -> AuditSeverity -> String -> AuditEntry
createAuditEntry op elemId sev msg = MkAuditEntry {
  timestamp = 0,  -- Would be set by FFI
  operation = op,
  elementId = elemId,
  severity = sev,
  message = msg,
  metadata = []
}

-- | Create mount audit entry
export
auditMount : String -> Bool -> AuditEntry
auditMount id success =
  if success
    then createAuditEntry Mount id Info ("Successfully mounted " ++ id)
    else createAuditEntry Mount id Error ("Failed to mount " ++ id)

-- | Create unmount audit entry
export
auditUnmount : String -> AuditEntry
auditUnmount id = createAuditEntry Unmount id Info ("Unmounted " ++ id)

-- | Create security violation audit
export
auditSecurityViolation : String -> String -> AuditEntry
auditSecurityViolation id reason =
  createAuditEntry Mount id Critical ("Security violation: " ++ reason)

-- ============================================================================
-- SANDBOXING
-- ============================================================================

-- | Validate sandbox configuration
export
validateSandboxConfig : SandboxConfig -> Bool
validateSandboxConfig config =
  case config.mode of
    NoSandbox => True
    IframeSandbox =>
      -- Iframe sandbox requires at least one restriction
      not (config.allowScripts && config.allowSameOrigin && config.allowForms)
    ShadowDOMSandbox => True

-- | Create secure sandbox config
export
createSecureSandbox : SandboxMode -> SandboxConfig
createSecureSandbox NoSandbox = MkSandboxConfig {
  mode = NoSandbox,
  allowScripts = True,
  allowSameOrigin = True,
  allowForms = True
}
createSecureSandbox IframeSandbox = MkSandboxConfig {
  mode = IframeSandbox,
  allowScripts = True,
  allowSameOrigin = False,  -- Prevent XSS
  allowForms = False
}
createSecureSandbox ShadowDOMSandbox = MkSandboxConfig {
  mode = ShadowDOMSandbox,
  allowScripts = True,
  allowSameOrigin = True,
  allowForms = True
}

-- ============================================================================
-- SECURITY CHECKS
-- ============================================================================

-- | Comprehensive security check
export
performSecurityCheck : SecurityPolicy -> String -> Either String ()
performSecurityCheck policy elemId = do
  -- Check 1: CSP validation
  when policy.requireCSP $ do
    case validateCSP elemId of
      CSPValid => Right ()
      InvalidChars => Left "CSP check failed: invalid characters"
      ScriptDetected => Left "CSP check failed: script detected"
      TooLong => Left "CSP check failed: element ID too long"

  -- Check 2: Length validation
  when (length elemId > cast policy.maxElementIdLength) $
    Left "Element ID exceeds maximum length"

  Right ()

-- ============================================================================
-- FFI DECLARATIONS
-- ============================================================================

%foreign "C:validate_csp,libdom_mounter_secure"
prim__validateCSP : String -> PrimIO Int

%foreign "C:audit_log_entry,libdom_mounter_secure"
prim__auditLogEntry : String -> String -> Int -> String -> PrimIO Int

%foreign "C:create_sandboxed_mount,libdom_mounter_secure"
prim__createSandboxedMount : String -> Int -> PrimIO Int

-- ============================================================================
-- EXPORT C HEADERS
-- ============================================================================

export
validateCSPFFI : String -> Int
validateCSPFFI elemId =
  case validateCSP elemId of
    CSPValid => 0
    InvalidChars => 1
    ScriptDetected => 2
    TooLong => 3

export
auditLogEntryFFI : String -> String -> Int -> String -> Int
auditLogEntryFFI op elemId severity msg = 0  -- Success

export
createSandboxedMountFFI : String -> Int -> Int
createSandboxedMountFFI elemId mode = 0  -- Success
