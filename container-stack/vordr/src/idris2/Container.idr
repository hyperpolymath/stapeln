-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Container.idr — Core container lifecycle types with dependent types
--
-- This module defines the container state machine with formal guarantees:
-- - States are indexed by their validity conditions
-- - Transitions are proven to preserve invariants
-- - Resource usage is tracked in types

module Container

import Data.Vect
import Data.So

%default total

--------------------------------------------------------------------------------
-- Container Identity
--------------------------------------------------------------------------------

||| A container ID is a non-empty string (simplified for now)
public export
record ContainerId where
  constructor MkContainerId
  idValue : String
  {auto nonEmpty : So (length idValue > 0)}

||| Create a container ID, returning Nothing if empty
export
mkContainerId : (s : String) -> Maybe ContainerId
mkContainerId s = case choose (length s > 0) of
  Left prf  => Just (MkContainerId s)
  Right _   => Nothing

--------------------------------------------------------------------------------
-- Resource Limits
--------------------------------------------------------------------------------

||| Memory limit in bytes (must be positive)
public export
record MemoryLimit where
  constructor MkMemoryLimit
  bytes : Nat
  {auto positive : So (bytes > 0)}

||| CPU limit as millicores (1000 = 1 core)
public export
record CpuLimit where
  constructor MkCpuLimit
  millicores : Nat
  {auto positive : So (millicores > 0)}

||| Combined resource limits
public export
record ResourceLimits where
  constructor MkResourceLimits
  memory : MemoryLimit
  cpu    : CpuLimit

--------------------------------------------------------------------------------
-- Container States (Indexed)
--------------------------------------------------------------------------------

||| Container lifecycle states as a data type
public export
data ContainerState : Type where
  ||| Container image exists but no container created
  ImageOnly : ContainerState
  ||| Container created but not started
  Created : ContainerState
  ||| Container is running
  Running : ContainerState
  ||| Container is paused (can be resumed)
  Paused : ContainerState
  ||| Container has stopped (exit code available)
  Stopped : ContainerState
  ||| Container has been removed
  Removed : ContainerState

||| Equality for container states
public export
Eq ContainerState where
  ImageOnly == ImageOnly = True
  Created == Created = True
  Running == Running = True
  Paused == Paused = True
  Stopped == Stopped = True
  Removed == Removed = True
  _ == _ = False

||| Show instance for container states
public export
Show ContainerState where
  show ImageOnly = "ImageOnly"
  show Created = "Created"
  show Running = "Running"
  show Paused = "Paused"
  show Stopped = "Stopped"
  show Removed = "Removed"

--------------------------------------------------------------------------------
-- Valid Transitions (Type-Level)
--------------------------------------------------------------------------------

||| Proof that a state transition is valid
||| This encodes the container lifecycle state machine at the type level
public export
data ValidTransition : ContainerState -> ContainerState -> Type where
  ||| Can create container from image
  CreateFromImage : ValidTransition ImageOnly Created
  ||| Can start a created container
  StartCreated : ValidTransition Created Running
  ||| Can pause a running container
  PauseRunning : ValidTransition Running Paused
  ||| Can resume a paused container
  ResumeP : ValidTransition Paused Running
  ||| Can stop a running container
  StopRunning : ValidTransition Running Stopped
  ||| Can stop a paused container
  StopPaused : ValidTransition Paused Stopped
  ||| Can restart a stopped container
  RestartStopped : ValidTransition Stopped Running
  ||| Can remove a stopped container
  RemoveStopped : ValidTransition Stopped Removed
  ||| Can remove a created (never started) container
  RemoveCreated : ValidTransition Created Removed

||| Decide if a transition is valid (returns proof or Nothing)
public export
decideTransition : (from : ContainerState) -> (to : ContainerState) ->
                   Maybe (ValidTransition from to)
decideTransition ImageOnly Created = Just CreateFromImage
decideTransition Created Running = Just StartCreated
decideTransition Running Paused = Just PauseRunning
decideTransition Paused Running = Just ResumeP
decideTransition Running Stopped = Just StopRunning
decideTransition Paused Stopped = Just StopPaused
decideTransition Stopped Running = Just RestartStopped
decideTransition Stopped Removed = Just RemoveStopped
decideTransition Created Removed = Just RemoveCreated
decideTransition _ _ = Nothing

--------------------------------------------------------------------------------
-- Container Record (State-Indexed)
--------------------------------------------------------------------------------

||| A container indexed by its current state
||| This ensures operations can only be performed on containers in valid states
public export
data Container : ContainerState -> Type where
  ||| An image that can be instantiated
  MkImage : (imageRef : String) -> Container ImageOnly

  ||| A created container (has ID and limits)
  MkCreated : (cid : ContainerId) ->
              (imageRef : String) ->
              (limits : ResourceLimits) ->
              Container Created

  ||| A running container (has PID)
  MkRunning : (cid : ContainerId) ->
              (imageRef : String) ->
              (limits : ResourceLimits) ->
              (pid : Nat) ->
              Container Running

  ||| A paused container
  MkPaused : (cid : ContainerId) ->
             (imageRef : String) ->
             (limits : ResourceLimits) ->
             (pid : Nat) ->
             Container Paused

  ||| A stopped container (has exit code)
  MkStopped : (cid : ContainerId) ->
              (imageRef : String) ->
              (exitCode : Int) ->
              Container Stopped

  ||| A removed container (tombstone for audit)
  MkRemoved : (cid : ContainerId) ->
              (imageRef : String) ->
              Container Removed

||| Get container ID (only valid for created+ states)
public export
getContainerId : Container state -> {auto prf : Not (state = ImageOnly)} -> ContainerId
getContainerId (MkImage _) {prf} = void (prf Refl)
getContainerId (MkCreated cid _ _) = cid
getContainerId (MkRunning cid _ _ _) = cid
getContainerId (MkPaused cid _ _ _) = cid
getContainerId (MkStopped cid _ _) = cid
getContainerId (MkRemoved cid _) = cid

--------------------------------------------------------------------------------
-- State Transitions (Verified)
--------------------------------------------------------------------------------

||| Create a container from an image
||| Requires: proof that ImageOnly -> Created is valid
public export
createContainer : Container ImageOnly ->
                  ContainerId ->
                  ResourceLimits ->
                  (ValidTransition ImageOnly Created, Container Created)
createContainer (MkImage imgRef) cid limits =
  (CreateFromImage, MkCreated cid imgRef limits)

||| Start a created container
||| Requires: proof that Created -> Running is valid
public export
startContainer : Container Created ->
                 (pid : Nat) ->
                 (ValidTransition Created Running, Container Running)
startContainer (MkCreated cid imgRef limits) pid =
  (StartCreated, MkRunning cid imgRef limits pid)

||| Pause a running container
public export
pauseContainer : Container Running ->
                 (ValidTransition Running Paused, Container Paused)
pauseContainer (MkRunning cid imgRef limits pid) =
  (PauseRunning, MkPaused cid imgRef limits pid)

||| Resume a paused container
public export
resumeContainer : Container Paused ->
                  (ValidTransition Paused Running, Container Running)
resumeContainer (MkPaused cid imgRef limits pid) =
  (ResumeP, MkRunning cid imgRef limits pid)

||| Stop a running container
public export
stopRunning : Container Running ->
              (exitCode : Int) ->
              (ValidTransition Running Stopped, Container Stopped)
stopRunning (MkRunning cid imgRef _ _) exitCode =
  (StopRunning, MkStopped cid imgRef exitCode)

||| Stop a paused container
public export
stopPaused : Container Paused ->
             (exitCode : Int) ->
             (ValidTransition Paused Stopped, Container Stopped)
stopPaused (MkPaused cid imgRef _ _) exitCode =
  (StopPaused, MkStopped cid imgRef exitCode)

||| Remove a stopped container
public export
removeContainer : Container Stopped ->
                  (ValidTransition Stopped Removed, Container Removed)
removeContainer (MkStopped cid imgRef _) =
  (RemoveStopped, MkRemoved cid imgRef)

--------------------------------------------------------------------------------
-- Proofs
--------------------------------------------------------------------------------

||| Proof: Cannot transition from Removed to any other state
||| (Removed is a terminal state)
export
removedIsTerminal : ValidTransition Removed s -> Void
removedIsTerminal _ impossible

||| Proof: Cannot skip states (e.g., ImageOnly -> Running is invalid)
export
cannotSkipCreate : ValidTransition ImageOnly Running -> Void
cannotSkipCreate _ impossible

||| Proof: Cannot go from Stopped directly to Paused
export
cannotStopToPaused : ValidTransition Stopped Paused -> Void
cannotStopToPaused _ impossible
