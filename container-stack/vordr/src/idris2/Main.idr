-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Main.idr — CLI entry point for vordr-verify
--
-- Usage: vordr-verify <command> [args]
--   verify <image>     Verify container image
--   check <policy>     Type-check a policy file
--   prove <lifecycle>  Generate proofs for lifecycle

module Main

import Container
import Verification
import Attestation
import System
import System.File

%default covering

--------------------------------------------------------------------------------
-- CLI Commands
--------------------------------------------------------------------------------

||| Print usage information
printUsage : IO ()
printUsage = do
  putStrLn "vordr-verify — Formally verified container verification"
  putStrLn ""
  putStrLn "Usage: vordr-verify <command> [args]"
  putStrLn ""
  putStrLn "Commands:"
  putStrLn "  verify <image>      Verify container image attestations"
  putStrLn "  policy <file>       Check policy file syntax"
  putStrLn "  lifecycle           Show valid state transitions"
  putStrLn "  version             Show version information"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  vordr-verify verify docker.io/library/nginx:1.26"
  putStrLn "  vordr-verify lifecycle"

||| Print version info
printVersion : IO ()
printVersion = do
  putStrLn "vordr-verify 0.1.0-dev"
  putStrLn "Idris2 verification core"
  putStrLn "License: PMPL-1.0-or-later"

||| Print container lifecycle state machine
printLifecycle : IO ()
printLifecycle = do
  putStrLn "Container Lifecycle State Machine"
  putStrLn "================================="
  putStrLn ""
  putStrLn "States:"
  putStrLn "  ImageOnly  → Container image exists, no container"
  putStrLn "  Created    → Container created, not started"
  putStrLn "  Running    → Container is executing"
  putStrLn "  Paused     → Container suspended"
  putStrLn "  Stopped    → Container exited"
  putStrLn "  Removed    → Container deleted (terminal)"
  putStrLn ""
  putStrLn "Valid Transitions:"
  putStrLn "  ImageOnly → Created   (create)"
  putStrLn "  Created   → Running   (start)"
  putStrLn "  Created   → Removed   (remove without starting)"
  putStrLn "  Running   → Paused    (pause)"
  putStrLn "  Running   → Stopped   (stop)"
  putStrLn "  Paused    → Running   (resume)"
  putStrLn "  Paused    → Stopped   (stop while paused)"
  putStrLn "  Stopped   → Running   (restart)"
  putStrLn "  Stopped   → Removed   (remove)"
  putStrLn ""
  putStrLn "Formally Verified Properties:"
  putStrLn "  ✓ Removed is terminal (no transitions out)"
  putStrLn "  ✓ Cannot skip Created state (ImageOnly → Running is invalid)"
  putStrLn "  ✓ Cannot go Stopped → Paused directly"
  putStrLn "  ✓ All transitions preserve type safety"

||| Verify an image (stub for now)
verifyImage : String -> IO ()
verifyImage imageRef = do
  putStrLn $ "Verifying: " ++ imageRef
  putStrLn ""
  -- Create mock attestation bundle
  let bundle = emptyBundle imageRef
  let policy = strictPolicy
  let chain = verifyBundle policy bundle
  putStrLn $ show chain
  putStrLn ""
  if chainPassed chain
    then do
      putStrLn "✓ Verification PASSED"
      exitSuccess
    else do
      putStrLn "✗ Verification FAILED"
      exitFailure

--------------------------------------------------------------------------------
-- Main Entry Point
--------------------------------------------------------------------------------

main : IO ()
main = do
  args <- getArgs
  case args of
    [_, "verify", image] => verifyImage image
    [_, "lifecycle"] => printLifecycle
    [_, "version"] => printVersion
    [_, "--version"] => printVersion
    [_, "-v"] => printVersion
    [_, "help"] => printUsage
    [_, "--help"] => printUsage
    [_, "-h"] => printUsage
    _ => do
      printUsage
      exitFailure
