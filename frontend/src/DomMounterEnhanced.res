// SPDX-License-Identifier: PMPL-1.0-or-later
// DomMounterEnhanced.res - Phase 1 reliability enhancements

// ============================================================================
// PHASE 1: CORE RELIABILITY
// ============================================================================

// Health status from Idris2 proofs
type healthStatus =
  | Healthy
  | Degraded
  | Failed

type lifecycleStage =
  | BeforeMount
  | Mounted
  | BeforeUnmount
  | Unmounted

// Enhanced error types with detailed information
type mountError =
  | ElementNotFound(string)
  | InvalidElementId(string)
  | ElementAlreadyMounted(string)
  | ParentNotFound(string)
  | PermissionDenied(string)
  | HealthCheckFailed(string, string) // (id, reason)

// Better error messages
let errorToUserMessage = (error: mountError): string => {
  switch error {
  | ElementNotFound(id) =>
      `Element with ID "${id}" not found. Check your HTML for <div id="${id}"></div>`
  | InvalidElementId(id) =>
      `Element ID "${id}" is invalid. IDs must be non-empty alphanumeric strings (with dash/underscore allowed).`
  | ElementAlreadyMounted(id) =>
      `Element "${id}" already has content. Use unmount() first or choose a different element.`
  | ParentNotFound(id) =>
      `Cannot find parent element for "${id}". Ensure the element is in the document.`
  | PermissionDenied(id) =>
      `Permission denied mounting to "${id}". This may be a protected or restricted element.`
  | HealthCheckFailed(id, reason) =>
      `Health check failed for "${id}": ${reason}. The element may not be suitable for mounting.`
  }
}

// Recovery strategies
type recoveryStrategy =
  | Retry(int) // Retry N times
  | Fallback(string) // Use fallback element ID
  | CreateIfMissing // Create element if missing

type recoveryResult =
  | RecoverySuccess
  | RetryExhausted
  | FallbackFailed
  | CreateFailed

// Lifecycle hooks
type lifecycleHooks = {
  beforeMount: option<string => Result.t<unit, string>>,
  afterMount: option<string => unit>,
  beforeUnmount: option<string => unit>,
  afterUnmount: option<string => unit>,
  onError: option<string => unit>,
}

let defaultHooks: lifecycleHooks = {
  beforeMount: None,
  afterMount: None,
  beforeUnmount: None,
  afterUnmount: None,
  onError: None,
}

// ============================================================================
// FFI BINDINGS TO ZIG
// ============================================================================

module FFI = {
  @val external health_check: string => int = "health_check"
  @val external is_element_visible: string => int = "is_element_visible"
  @val external get_element_state: string => int = "get_element_state"
  @val external set_element_state: (string, int) => int = "set_element_state"
  @val external can_transition: (int, int) => int = "can_transition"
  @val external attempt_retry: (string, int) => int = "attempt_retry"
  @val external attempt_fallback: (string, string) => int = "attempt_fallback"
  @val external start_monitoring: string => int = "start_monitoring"
  @val external stop_monitoring: string => int = "stop_monitoring"

  let healthStatusFromInt = (code: int): healthStatus => {
    switch code {
    | 0 => Healthy
    | 1 => Degraded
    | _ => Failed
    }
  }

  let lifecycleStageFromInt = (code: int): lifecycleStage => {
    switch code {
    | 0 => BeforeMount
    | 1 => Mounted
    | 2 => BeforeUnmount
    | _ => Unmounted
    }
  }

  let lifecycleStageToInt = (stage: lifecycleStage): int => {
    switch stage {
    | BeforeMount => 0
    | Mounted => 1
    | BeforeUnmount => 2
    | Unmounted => 3
    }
  }

  let recoveryResultFromInt = (code: int): recoveryResult => {
    switch code {
    | 0 => RecoverySuccess
    | 1 => RetryExhausted
    | 2 => FallbackFailed
    | _ => CreateFailed
    }
  }
}

// ============================================================================
// HEALTH CHECKS & MONITORING
// ============================================================================

// Perform health check on element
let healthCheck = (elementId: string): (healthStatus, string) => {
  let code = FFI.health_check(elementId)
  let status = FFI.healthStatusFromInt(code)

  let message = switch status {
  | Healthy => "Element ID is valid and healthy"
  | Degraded => "Element exists but may have issues"
  | Failed =>
      if String.length(elementId) == 0 {
        "Element ID is empty"
      } else if String.length(elementId) > 255 {
        "Element ID is too long (max 255 characters)"
      } else {
        "Element ID contains invalid characters"
      }
  }

  (status, message)
}

// Check if element is visible
let isElementVisible = (elementId: string): bool => {
  let code = FFI.is_element_visible(elementId)
  code == 1
}

// Get current lifecycle stage
let getLifecycleStage = (elementId: string): lifecycleStage => {
  let code = FFI.get_element_state(elementId)
  FFI.lifecycleStageFromInt(code)
}

// Validate lifecycle transition
let canTransitionTo = (from: lifecycleStage, to: lifecycleStage): bool => {
  let fromInt = FFI.lifecycleStageToInt(from)
  let toInt = FFI.lifecycleStageToInt(to)
  let result = FFI.can_transition(fromInt, toInt)
  result == 1
}

// ============================================================================
// RECOVERY MECHANISMS
// ============================================================================

// Execute recovery strategy
let rec executeRecovery = (
  elementId: string,
  strategy: recoveryStrategy,
  originalError: mountError,
): Result.t<string, mountError> => {
  switch strategy {
  | Retry(n) if n > 0 => {
      // Attempt retry via FFI
      let code = FFI.attempt_retry(elementId, n)
      let result = FFI.recoveryResultFromInt(code)

      switch result {
      | RecoverySuccess => Ok(elementId)
      | RetryExhausted => {
          // Try one more time with n-1
          if n > 1 {
            executeRecovery(elementId, Retry(n - 1), originalError)
          } else {
            Error(originalError)
          }
        }
      | _ => Error(originalError)
      }
    }

  | Fallback(fallbackId) => {
      // Attempt fallback via FFI
      let code = FFI.attempt_fallback(elementId, fallbackId)
      let result = FFI.recoveryResultFromInt(code)

      switch result {
      | RecoverySuccess => Ok(fallbackId)
      | FallbackFailed => Error(ElementNotFound(fallbackId))
      | _ => Error(originalError)
      }
    }

  | CreateIfMissing => {
      // Would need JS bridge to create element
      Error(originalError) // Not yet implemented
    }

  | _ => Error(originalError)
  }
}

// Mount with automatic recovery
let mountWithRecovery = (
  elementId: string,
  strategy: recoveryStrategy,
): Result.t<string, mountError> => {
  // First, try health check
  let (health, reason) = healthCheck(elementId)

  switch health {
  | Healthy => Ok(elementId)
  | Failed => {
      // Try recovery
      let error = HealthCheckFailed(elementId, reason)
      executeRecovery(elementId, strategy, error)
    }
  | Degraded => {
      // Element exists but degraded - still attempt mount
      Ok(elementId)
    }
  }
}

// ============================================================================
// LIFECYCLE HOOKS
// ============================================================================

// Execute lifecycle hook safely
let executeHook = (hook: option<'a => 'b>, arg: 'a): unit => {
  switch hook {
  | Some(fn) => {
      let _ = fn(arg)
    }
  | None => ()
  }
}

// Execute lifecycle hook with Result
let executeHookResult = (hook: option<'a => Result.t<unit, string>>, arg: 'a): Result.t<unit, string> => {
  switch hook {
  | Some(fn) => fn(arg)
  | None => Ok()
  }
}

// Mount with full lifecycle hooks
let mountWithLifecycle = (
  elementId: string,
  hooks: lifecycleHooks,
): Result.t<unit, string> => {
  // BeforeMount hook
  switch executeHookResult(hooks.beforeMount, elementId) {
  | Error(e) => {
      executeHook(hooks.onError, e)
      Error(e)
    }
  | Ok() => {
      // Perform mount
      let (health, _) = healthCheck(elementId)

      switch health {
      | Healthy | Degraded => {
          // Update lifecycle state
          let _ = FFI.set_element_state(elementId, 1) // Mounted

          // AfterMount hook
          executeHook(hooks.afterMount, elementId)

          Ok()
        }
      | Failed => {
          let err = "Health check failed for " ++ elementId
          executeHook(hooks.onError, err)
          Error(err)
        }
      }
    }
  }
}

// Unmount with lifecycle hooks
let unmountWithLifecycle = (
  elementId: string,
  hooks: lifecycleHooks,
): Result.t<unit, string> => {
  // BeforeUnmount hook
  executeHook(hooks.beforeUnmount, elementId)

  // Update lifecycle state
  let _ = FFI.set_element_state(elementId, 2) // BeforeUnmount

  // Perform unmount (would clear element content)
  // ...

  // Update to unmounted
  let _ = FFI.set_element_state(elementId, 3) // Unmounted

  // AfterUnmount hook
  executeHook(hooks.afterUnmount, elementId)

  Ok()
}

// ============================================================================
// MONITORING
// ============================================================================

// Start continuous monitoring
let startMonitoring = (elementId: string): Result.t<unit, string> => {
  let code = FFI.start_monitoring(elementId)
  if code == 0 {
    Ok()
  } else {
    Error("Failed to start monitoring for " ++ elementId)
  }
}

// Stop monitoring
let stopMonitoring = (elementId: string): Result.t<unit, string> => {
  let code = FFI.stop_monitoring(elementId)
  if code == 0 {
    Ok()
  } else {
    Error("Failed to stop monitoring for " ++ elementId)
  }
}

// ============================================================================
// HIGH-LEVEL API
// ============================================================================

type mountConfig = {
  elementId: string,
  recovery: option<recoveryStrategy>,
  lifecycle: lifecycleHooks,
  monitoring: bool,
}

let defaultConfig = (elementId: string): mountConfig => {
  {
    elementId: elementId,
    recovery: Some(Retry(3)), // Default: retry 3 times
    lifecycle: defaultHooks,
    monitoring: false,
  }
}

// Enhanced mount with all Phase 1 features
let mountEnhanced = (config: mountConfig): Result.t<unit, string> => {
  // Start monitoring if requested
  if config.monitoring {
    let _ = startMonitoring(config.elementId)
  }

  // Try recovery if configured
  switch config.recovery {
  | Some(strategy) => {
      switch mountWithRecovery(config.elementId, strategy) {
      | Ok(elementId) => {
          // Mount with lifecycle hooks
          mountWithLifecycle(elementId, config.lifecycle)
        }
      | Error(err) => {
          let msg = errorToUserMessage(err)
          executeHook(config.lifecycle.onError, msg)
          Error(msg)
        }
      }
    }
  | None => {
      // Mount with lifecycle hooks
      mountWithLifecycle(config.elementId, config.lifecycle)
    }
  }
}

// Convenience function: mount with defaults
let mount = (elementId: string): Result.t<unit, string> => {
  mountEnhanced(defaultConfig(elementId))
}

// Convenience function: mount with custom hooks
let mountWith = (
  elementId: string,
  ~beforeMount: option<string => Result.t<unit, string>>=?,
  ~afterMount: option<string => unit>=?,
  ~beforeUnmount: option<string => unit>=?,
  ~afterUnmount: option<string => unit>=?,
  ~onError: option<string => unit>=?,
  (),
): Result.t<unit, string> => {
  let hooks = {
    beforeMount: beforeMount,
    afterMount: afterMount,
    beforeUnmount: beforeUnmount,
    afterUnmount: afterUnmount,
    onError: onError,
  }

  let config = {
    elementId: elementId,
    recovery: Some(Retry(3)),
    lifecycle: hooks,
    monitoring: false,
  }

  mountEnhanced(config)
}
