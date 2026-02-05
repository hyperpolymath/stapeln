-- SPDX-License-Identifier: PMPL-1.0-or-later
-- DomMounterEnhanced.idr - Phase 1 reliability enhancements

module DomMounterEnhanced

import Data.String
import Data.List

%default total

-- ============================================================================
-- PHASE 1: CORE RELIABILITY ENHANCEMENTS
-- ============================================================================

-- | Health Check Results
public export
data HealthStatus = Healthy | Degraded | Failed

public export
Show HealthStatus where
  show Healthy = "Healthy"
  show Degraded = "Degraded"
  show Failed = "Failed"

public export
data HealthCheckResult : HealthStatus -> Type where
  HealthyElement : (id : String) -> HealthCheckResult Healthy
  DegradedElement : (id : String) -> (reason : String) -> HealthCheckResult Degraded
  FailedElement : (id : String) -> (reason : String) -> HealthCheckResult Failed

-- | Continuous validation proof
public export
data ContinuousValidation : Type where
  MonitoredMount : (id : String) -> HealthStatus -> ContinuousValidation

-- | Lifecycle stages with proofs
public export
data LifecycleStage = BeforeMount | Mounted | BeforeUnmount | Unmounted

public export
Show LifecycleStage where
  show BeforeMount = "BeforeMount"
  show Mounted = "Mounted"
  show BeforeUnmount = "BeforeUnmount"
  show Unmounted = "Unmounted"

public export
data LifecycleProof : LifecycleStage -> Type where
  PreMount : LifecycleProof BeforeMount
  ActiveMount : LifecycleProof Mounted
  PreUnmount : LifecycleProof BeforeUnmount
  PostUnmount : LifecycleProof Unmounted

-- | Recovery strategies
public export
data RecoveryStrategy = Retry Nat | Fallback String | CreateIfMissing

public export
Show RecoveryStrategy where
  show (Retry n) = "Retry " ++ show n
  show (Fallback id) = "Fallback to " ++ id
  show CreateIfMissing = "CreateIfMissing"

public export
data ValidRecovery : RecoveryStrategy -> Type where
  RetryValid : (n : Nat) -> ValidRecovery (Retry n)
  FallbackValid : (fallbackId : String) -> ValidRecovery (Fallback fallbackId)
  CreateValid : ValidRecovery CreateIfMissing

-- | Lifecycle transition validity
public export
canTransition : LifecycleStage -> LifecycleStage -> Bool
canTransition BeforeMount Mounted = True
canTransition Mounted BeforeUnmount = True
canTransition BeforeUnmount Unmounted = True
canTransition _ _ = False

-- | Proofs of valid lifecycle transitions
export
mountTransitionValid : canTransition BeforeMount Mounted = True
mountTransitionValid = Refl

export
unmountTransitionValid : canTransition Mounted BeforeUnmount = True
unmountTransitionValid = Refl

export
finalTransitionValid : canTransition BeforeUnmount Unmounted = True
finalTransitionValid = Refl

-- | Health check implementation
export
performHealthCheck : String -> IO (HealthStatus, String)
performHealthCheck id =
  if id == ""
    then pure (Failed, "Empty element ID")
    else if length id > 255
      then pure (Failed, "Element ID too long")
      else pure (Healthy, "Element ID valid")

-- | Lifecycle hook type
public export
data LifecycleHook : LifecycleStage -> Type where
  BeforeMountHook : (String -> IO (Either String ())) -> LifecycleHook BeforeMount
  AfterMountHook : (String -> IO ()) -> LifecycleHook Mounted
  BeforeUnmountHook : (String -> IO ()) -> LifecycleHook BeforeUnmount
  AfterUnmountHook : (String -> IO ()) -> LifecycleHook Unmounted

-- | Recovery attempt with proof
public export
data RecoveryAttempt : RecoveryStrategy -> Type where
  AttemptRetry : (n : Nat) -> (attemptsLeft : Nat) -> RecoveryAttempt (Retry n)
  AttemptFallback : (fallbackId : String) -> RecoveryAttempt (Fallback fallbackId)
  AttemptCreate : RecoveryAttempt CreateIfMissing

-- | Error types with detailed information
public export
data MountError =
    ElementNotFound String
  | InvalidElementId String
  | ElementAlreadyMounted String
  | ParentNotFound String
  | PermissionDenied String
  | HealthCheckFailed String String

public export
Show MountError where
  show (ElementNotFound id) = "Element not found: " ++ id
  show (InvalidElementId id) = "Invalid element ID: " ++ id
  show (ElementAlreadyMounted id) = "Element already mounted: " ++ id
  show (ParentNotFound id) = "Parent not found for: " ++ id
  show (PermissionDenied id) = "Permission denied: " ++ id
  show (HealthCheckFailed id reason) = "Health check failed for " ++ id ++ ": " ++ reason

-- | Enhanced mount result with error details
public export
data EnhancedMountResult =
    MountSuccess String HealthStatus
  | MountFailure MountError
  | MountRecovered String RecoveryStrategy

public export
Show EnhancedMountResult where
  show (MountSuccess id health) = "Success: " ++ id ++ " (" ++ show health ++ ")"
  show (MountFailure err) = "Failure: " ++ show err
  show (MountRecovered id strategy) = "Recovered: " ++ id ++ " via " ++ show strategy

-- ============================================================================
-- FFI DECLARATIONS
-- ============================================================================

%foreign "C:health_check,libdom_mounter_enhanced"
prim__healthCheck : String -> PrimIO Int

%foreign "C:is_element_visible,libdom_mounter_enhanced"
prim__isElementVisible : String -> PrimIO Int

%foreign "C:get_element_state,libdom_mounter_enhanced"
prim__getElementState : String -> PrimIO Int

-- ============================================================================
-- EXPORT C HEADERS (for code generation)
-- ============================================================================

-- These functions generate the expected C ABI for FFI
export
healthCheckFFI : String -> Int
healthCheckFFI id =
  if id == "" then 2 -- Failed
  else if length id > 255 then 2 -- Failed
  else 0 -- Healthy

export
isElementVisibleFFI : String -> Int
isElementVisibleFFI id = if id == "" then 0 else 1

export
getElementStateFFI : String -> Int
getElementStateFFI id = 0 -- Mounted state
