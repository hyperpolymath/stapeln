# SONNET-TASKS.md -- stapeln

**Date:** 2026-02-12
**Auditor:** Claude Opus 4.6
**Languages:** ReScript (frontend), Elixir/Phoenix (backend), Idris2 (ABI), Zig (FFI), JavaScript (WASM kernel), HTML/CSS
**Honest completion:** ~30%

STATE.scm claims 60%. STATUS.md claims "Phase 2 at 30%, Phase 3 at 0%". The 30% figure from STATUS.md is closest to reality. The frontend renders 8 views but most have no backend connectivity. The Zig FFI `main.zig` is 100% stub (hardcoded JSON). The Zig `bridge_cli.zig` is real and functional. The backend REST + GraphQL + gRPC are operational but thin. No database, no auth, no miniKanren, no VeriSimDB, no formal verification pipeline, no PQ crypto integration. Settings page renders but does not persist. Export produces placeholder compose/docker files with `image: TODO`. The Idris2 ABI layer has type definitions and FFI declarations but zero actual proofs.

---

## GROUND RULES FOR SONNET

1. Do NOT create new files unless a task explicitly requires it.
2. Run `mix test` in `backend/` after every backend change.
3. Run `npx rescript build` (or equivalent) in `frontend/` after every frontend change.
4. Run `zig build` in `ffi/zig/` after every Zig change.
5. Every task has a VERIFICATION block. If verification fails, the task is not done.
6. Do not "fix" things by deleting functionality. Implement what is missing.
7. Respect the TEA (The Elm Architecture) pattern in the frontend: state flows through `Model.res`, messages through `Msg.res`, updates through `Update.res`.
8. Backend uses Phoenix conventions. Controllers call context modules (e.g., `Stapeln.Stacks`), not GenServers directly.
9. License must be `PMPL-1.0-or-later` for all hyperpolymath-authored code.

---

## TASK 1: Fix SPDX license headers in Idris2 and Zig files

**Files:**
- `/var/mnt/eclipse/repos/stapeln/src/abi/Types.idr` (line 1)
- `/var/mnt/eclipse/repos/stapeln/src/abi/Foreign.idr` (line 1)
- `/var/mnt/eclipse/repos/stapeln/src/abi/Layout.idr` (line 1)
- `/var/mnt/eclipse/repos/stapeln/ffi/zig/src/main.zig` (line 1)
- `/var/mnt/eclipse/repos/stapeln/ffi/zig/src/bridge_cli.zig` (line 1)
- `/var/mnt/eclipse/repos/stapeln/ffi/zig/test/integration_test.zig` (line 1)
- `/var/mnt/eclipse/repos/stapeln/ffi/zig/build.zig` (line 1)

**Problem:** All seven files use `AGPL-3.0-or-later` as their SPDX license identifier. Per hyperpolymath standards, all original code must use `PMPL-1.0-or-later`. AGPL-3.0-or-later is the old license and is explicitly banned.

**What to do:**
In each file, change the first line from:
```
-- SPDX-License-Identifier: AGPL-3.0-or-later
```
(or `// SPDX-License-Identifier: AGPL-3.0-or-later` for Zig) to:
```
-- SPDX-License-Identifier: PMPL-1.0-or-later
```
(or `// SPDX-License-Identifier: PMPL-1.0-or-later` for Zig).

**Verification:**
```bash
# Must return zero matches:
grep -rn "AGPL-3.0-or-later" /var/mnt/eclipse/repos/stapeln/src/abi/ /var/mnt/eclipse/repos/stapeln/ffi/zig/
# Must return 7 matches:
grep -rn "PMPL-1.0-or-later" /var/mnt/eclipse/repos/stapeln/src/abi/ /var/mnt/eclipse/repos/stapeln/ffi/zig/
```

---

## TASK 2: Implement SaveStack and LoadStack in Update.res (backend API integration)

**Files:**
- `/var/mnt/eclipse/repos/stapeln/frontend/src/Update.res` (lines 227-237)

**Problem:** The `SaveStack` and `LoadStack` message handlers are stubs that only `Console.log`. There is no HTTP call to the backend API. The backend endpoints exist (`POST /api/stacks`, `GET /api/stacks`) and work, but the frontend never calls them.

Lines 227-237:
```rescript
| SaveStack =>
  Console.log("Saving stack...")
  // TODO: Send stack to backend API
  state

| LoadStack =>
  Console.log("Loading stack...")
  // TODO: Load stack from backend API
  state
```

**What to do:**

1. Add a `Fetch` or `XMLHttpRequest` FFI binding (or use an existing one) to make HTTP requests from the frontend.
2. In `SaveStack`: serialize the current `state.components` and `state.connections` using `DesignFormat.serializeDesign`, then `POST` to `/api/stacks` with the serialized JSON body. On success, show a toast (use `Toast.success`). On failure, show a toast with the error.
3. In `LoadStack`: `GET /api/stacks` to fetch the list, then populate `state.components` and `state.connections` from the response. On failure, show a toast with the error.
4. Since TEA does not support async effects directly, use a `Cmd`-style pattern or dispatch a follow-up message from a callback. At minimum, use `Js.Promise` or a raw `%raw` fetch call that dispatches back to the update loop.

**Verification:**
```bash
cd /var/mnt/eclipse/repos/stapeln/frontend
npx rescript build 2>&1 | grep -i error
# Must compile with no errors.

# Then verify no TODO remains on those lines:
grep -n "TODO: Send stack to backend API\|TODO: Load stack from backend API" src/Update.res
# Must return zero matches.
```

---

## TASK 3: Implement RunGapAnalysis in GapAnalysis.res

**Files:**
- `/var/mnt/eclipse/repos/stapeln/frontend/src/GapAnalysis.res` (line 269)

**Problem:** The `RunGapAnalysis` message handler is a no-op that just returns `state` unchanged. The gap analysis view has a "Run Analysis" button that does nothing.

Line 269:
```rescript
| RunGapAnalysis => // Trigger gap analysis (placeholder)
  state
```

**What to do:**
Implement `RunGapAnalysis` to actually analyze the current component model for security gaps. At minimum:
1. Check each component for missing security configurations (e.g., no TLS, no SBOM, no signature verification).
2. Check for components without network policies.
3. Check for open port configurations that conflict with security best practices.
4. Populate `state.gaps` with the discovered gaps, each containing an `id`, `title`, `severity`, `description`, and `remediation` field.
5. Update `state.lastRunTimestamp` with the current time.

**Verification:**
```bash
cd /var/mnt/eclipse/repos/stapeln/frontend
npx rescript build 2>&1 | grep -i error
# Must compile with no errors.

grep -n "RunGapAnalysis =>" src/GapAnalysis.res
# The line after RunGapAnalysis must NOT be just "state" with no logic.
```

---

## TASK 4: Implement RunSecurityScan in SecurityInspector.res

**Files:**
- `/var/mnt/eclipse/repos/stapeln/frontend/src/SecurityInspector.res` (line 1409)

**Problem:** The `RunSecurityScan` message handler is a no-op: `| RunSecurityScan => state`. The Security Inspector view has a scan button that does nothing. This is the core functionality of the security view.

**What to do:**
Implement `RunSecurityScan` to:
1. Call the backend panic-attacker API (`POST /api/security/panic-attacker` with `{"command": "ambush", "target": "/"}`) to trigger a security scan.
2. Poll `GET /api/security/panic-attacker/status` for results.
3. Parse the timeline events from the response and populate `state.vulnerabilities` with any findings.
4. Update `state.metrics` based on the scan results.
5. Set `state.scanStatus` to reflect the current scan state (running, completed, failed).

If full backend integration is too complex for a single task, at minimum implement a local scan that:
- Analyzes the component list for known vulnerability patterns (missing TLS, exposed debug ports, no SBOM).
- Populates `state.vulnerabilities` with the results.
- Updates `state.metrics.security` score based on findings.

**Verification:**
```bash
cd /var/mnt/eclipse/repos/stapeln/frontend
npx rescript build 2>&1 | grep -i error
# Must compile with no errors.

grep -n "RunSecurityScan => state" src/SecurityInspector.res
# Must return zero matches (the handler must have real logic now).
```

---

## TASK 5: Implement Zig FFI main.zig -- replace stubs with real logic

**Files:**
- `/var/mnt/eclipse/repos/stapeln/ffi/zig/src/main.zig`

**Problem:** Every exported function in `main.zig` returns a hardcoded JSON string. For example:
```zig
export fn stapeln_create_stack_json(...) callconv(.C) c_int {
    // ... writes '{"mode":"zig","op":"create_stack"}' to the output buffer
}
```
This means the FFI library is 100% fake. The `bridge_cli.zig` has real logic, but `main.zig` (the shared/static library) does not.

**What to do:**
Implement real logic in each exported function in `main.zig`:
1. `stapeln_create_stack_json`: Parse the input JSON, create a stack struct, serialize it to the output buffer.
2. `stapeln_get_stack_json`: Look up a stack by ID from an in-memory store, serialize to output.
3. `stapeln_update_stack_json`: Find and update a stack, serialize result.
4. `stapeln_validate_stack_json`: Run validation rules (empty services, duplicate names, missing kind, invalid ports) and return findings.
5. `stapeln_list_stacks_json`: Return all stacks as a JSON array.

You can reference `bridge_cli.zig` (which has a working implementation of all these operations) and adapt the logic for the C ABI exports. The bridge CLI uses `std.json` for parsing/serialization and `std.heap.ArenaAllocator` for memory management -- the library exports should do the same but with caller-provided buffers.

**Verification:**
```bash
cd /var/mnt/eclipse/repos/stapeln/ffi/zig
zig build 2>&1
# Must compile with no errors.

zig build test 2>&1
# Tests must pass (including the integration_test.zig that checks list_stacks output).

# Verify no hardcoded stub responses remain:
grep -n '"mode":"zig"' src/main.zig
# Must return zero matches.
```

---

## TASK 6: Wire up Settings page persistence

**Files:**
- `/var/mnt/eclipse/repos/stapeln/frontend/src/Settings.res` (lines 462-510)

**Problem:** The Settings page renders a full form with radio buttons, checkboxes, and text inputs, but:
1. The "Save" button (line 479) has no `onClick` handler -- it does nothing.
2. The "Reset to Defaults" button (line 463) has no `onClick` handler.
3. The "Cancel" button (line 495) has no `onClick` handler.
4. None of the form inputs have `onChange` handlers -- they use `defaultValue`/`defaultChecked` only, so changes are not tracked.
5. Settings are never persisted (no `localStorage`, no backend call).

**What to do:**
1. Convert the `view` function to a `@react.component` with `React.useState` for the settings state.
2. Add `onChange` handlers to all inputs that update the local state.
3. "Save" button: persist settings to `localStorage` (key: `"stapeln-settings"`).
4. "Reset to Defaults" button: reset state to `defaultSettings`.
5. "Cancel" button: reload settings from `localStorage` (or revert to defaults if none saved).
6. On component mount, load saved settings from `localStorage`.

**Verification:**
```bash
cd /var/mnt/eclipse/repos/stapeln/frontend
npx rescript build 2>&1 | grep -i error
# Must compile with no errors.

# Verify Save button has an onClick:
grep -A2 "Save settings" src/Settings.res | grep -c "onClick"
# Must return 1.

# Verify localStorage usage:
grep -c "localStorage" src/Settings.res
# Must return at least 2 (save + load).
```

---

## TASK 7: Fix dark mode detection in StackView.res

**Files:**
- `/var/mnt/eclipse/repos/stapeln/frontend/src/StackView.res` (line 328)

**Problem:** Dark mode is hardcoded to `false`:
```rescript
let isDarkMode = false // TODO: Detect from window.matchMedia('(prefers-color-scheme: dark)')
```

**What to do:**
Replace the hardcoded `false` with actual dark mode detection:
```rescript
let isDarkMode = {
  let mql: bool = %raw(`window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches`)
  mql
}
```

Also fix the old name reference on line 165: `"stackur"` should be `"stapeln"`.

**Verification:**
```bash
cd /var/mnt/eclipse/repos/stapeln/frontend
npx rescript build 2>&1 | grep -i error
# Must compile with no errors.

grep -n "isDarkMode = false" src/StackView.res
# Must return zero matches.

grep -n "stackur" src/StackView.res
# Must return zero matches.

grep -n "matchMedia" src/StackView.res
# Must return at least 1 match.
```

---

## TASK 8: Show import errors to the user in Update.res

**Files:**
- `/var/mnt/eclipse/repos/stapeln/frontend/src/Update.res` (line 223)

**Problem:** The `ImportDesignError` handler only logs to `Console.error` and does not notify the user:
```rescript
| ImportDesignError(err) =>
  Console.error2("Failed to import design:", err)
  // TODO: Show error message to user
  state
```

**What to do:**
Instead of just logging, dispatch a toast notification to the user. Since the app has a `Toast` module:
1. Import `Toast` if not already imported.
2. Create an error toast: `let newToastState = Toast.update(Toast.ShowToast(err, Toast.Error, 5000), state.toastState)`.
3. Return `{...state, toastState: newToastState}`.

If the `state` type in `Update.res` does not have a `toastState` field (it uses the simple `Model.model`), then at minimum add a visual error indicator in the model (e.g., `errorMessage: option<string>`) and display it in the UI.

**Verification:**
```bash
cd /var/mnt/eclipse/repos/stapeln/frontend
npx rescript build 2>&1 | grep -i error
# Must compile with no errors.

grep -n "TODO: Show error message to user" src/Update.res
# Must return zero matches.
```

---

## TASK 9: Fix Export.res compose.toml and docker-compose stubs

**Files:**
- `/var/mnt/eclipse/repos/stapeln/frontend/src/Export.res` (lines 53-110)

**Problem:**
1. `exportToSelurCompose` (line 53): outputs a comment `# TODO: Implement compose.toml generation` in the generated file, and the services section only includes `name` and `type` -- no image references, ports, volumes, or environment variables.
2. `exportToDockerCompose` (line 82): every service has `image: TODO` instead of actual image references derived from the component configuration.
3. `exportToPodmanCompose` (line 108): just calls `exportToDockerCompose` with no distinction.

**What to do:**
1. Remove the `# TODO` comment from the compose.toml output.
2. In `exportToSelurCompose`: generate proper compose.toml with `image`, `ports`, and `environment` fields pulled from `comp.config`.
3. In `exportToDockerCompose`: derive image names from `comp.componentType` (e.g., CerroTorre -> `ghcr.io/hyperpolymath/cerro-torre:latest`, LagoGrey -> `ghcr.io/hyperpolymath/lago-grey:latest`, etc.). Add port mappings from `comp.config` if present.
4. In `exportToPodmanCompose`: generate a proper podman-compose file (can share logic with docker-compose but change the header comment).

**Verification:**
```bash
cd /var/mnt/eclipse/repos/stapeln/frontend
npx rescript build 2>&1 | grep -i error
# Must compile with no errors.

grep -n "TODO" src/Export.res
# Must return zero matches.

grep -n "image: TODO" src/Export.res
# Must return zero matches.
```

---

## TASK 10: Implement crypto signature placeholders in LagoGreyExport.res

**Files:**
- `/var/mnt/eclipse/repos/stapeln/frontend/src/LagoGreyExport.res` (lines 151-155)

**Problem:** The `generateManifest` function outputs hardcoded `"TODO"` for all three cryptographic signatures:
```json
"signatures": {
  "dilithium5": "TODO",
  "ed448": "TODO",
  "sphincs": "TODO"
}
```

**What to do:**
Since actual cryptographic signing cannot happen in the browser, replace the TODOs with a clear placeholder that indicates the signatures need to be generated by the build pipeline:
1. Change `"TODO"` to `"UNSIGNED -- run 'cerro-torre sign' to generate"` for each signature field.
2. Add a boolean field `"signed": false` to the manifest.
3. Add a comment or note in the `generateBuildInstructions` output explaining that triple signing is required before deployment and referencing the `cerro-torre sign` command.

**Verification:**
```bash
cd /var/mnt/eclipse/repos/stapeln/frontend
npx rescript build 2>&1 | grep -i error
# Must compile with no errors.

grep -n '"TODO"' src/LagoGreyExport.res
# Must return zero matches.

grep -n "UNSIGNED" src/LagoGreyExport.res
# Must return 3 matches (one per signature field).
```

---

## TASK 11: Fix appendTraceEvent argument order in SecurityInspector.res

**Files:**
- `/var/mnt/eclipse/repos/stapeln/frontend/src/SecurityInspector.res` (line 355, approximately)

**Problem:** The `appendTraceEvent` function uses `Belt.Array.keepWithIndex` with arguments in potentially incorrect order. The `keepWithIndex` callback signature is `('a, int) => bool` but the code may have `(idx, _)` instead of `(_, idx)`. This causes trace event limiting to malfunction.

**What to do:**
1. Locate the `appendTraceEvent` function.
2. Find the `Belt.Array.keepWithIndex` call.
3. Verify the callback argument order is `(element, index)` not `(index, element)`.
4. If wrong, fix the order so that the timeline limiting logic works correctly (keeping only the most recent events up to the max timeline length).

**Verification:**
```bash
cd /var/mnt/eclipse/repos/stapeln/frontend
npx rescript build 2>&1 | grep -i error
# Must compile with no errors.

# Verify the keepWithIndex call has correct argument order:
grep -A1 "keepWithIndex" src/SecurityInspector.res
# The callback should have (element, index) or (_, idx) pattern, not (idx, _).
```

---

## TASK 12: Add frontend tests

**Files:**
- New file: `/var/mnt/eclipse/repos/stapeln/frontend/test/DesignFormat_test.res` (or equivalent test file)

**Problem:** The frontend has zero tests. There are no test files for any ReScript module. The `DesignFormat.res` module has complex serialization/deserialization logic (`serializeDesign`, `deserializeDesign`, `componentToJson`, `componentFromJson`) that could easily break silently.

**What to do:**
1. Set up a ReScript test framework (e.g., `rescript-test` or a simple assertion module).
2. Write tests for `DesignFormat.res`:
   - `serializeDesign` followed by `deserializeDesign` produces the original model (round-trip test).
   - `componentToJson` followed by `componentFromJson` preserves `id`, `componentType`, `position`, and `config`.
   - `connectionToJson` followed by `connectionFromJson` preserves `id`, `from`, `to`.
   - `deserializeDesign` with invalid JSON returns `Error(...)`.
   - `deserializeDesign` with missing fields returns `Error(...)`.
3. Write tests for `PacketMathWasm.res`:
   - `lerpCoordinate` with progress=0.0 returns source.
   - `lerpCoordinate` with progress=1.0 returns target.
   - `advanceProgress` with step that exceeds 1.0 clamps to 1.0 and sets `arrived=true`.
4. Write tests for `StateSync.res`:
   - `calculatePortSecurityImpact` with all closed ports returns securityDelta=0.
   - `calculatePortSecurityImpact` with open critical ports returns negative delta.

**Verification:**
```bash
cd /var/mnt/eclipse/repos/stapeln/frontend
npx rescript build 2>&1 | grep -i error
# Must compile with no errors.

# Run tests (exact command depends on test framework chosen):
node lib/es6/test/DesignFormat_test.js 2>&1
# Must exit 0 with all tests passing.
```

---

## TASK 13: Add validation rules to the Elixir ValidationEngine

**Files:**
- `/var/mnt/eclipse/repos/stapeln/backend/lib/stapeln/validation_engine.ex`

**Problem:** The validation engine has only 3 basic rules:
1. Empty services check
2. Duplicate service names check
3. Missing kind check

It lacks security-relevant rules that the README and roadmap promise:
- No port range validation (ports outside 1-65535)
- No duplicate port detection
- No privileged port warning (ports < 1024)
- No network policy checks
- No SBOM requirement checks
- No TLS/mTLS requirement checks

**What to do:**
Add these validation rules to the `validate/1` function:
1. **Invalid port range**: severity `:high`, if any service port is < 1 or > 65535.
2. **Duplicate ports**: severity `:medium`, if two services share the same port.
3. **Privileged ports**: severity `:info`, if any service uses a port < 1024 (informational warning).
4. **No description**: severity `:low`, if the stack has no description.
5. **Service name format**: severity `:low`, if a service name contains spaces or uppercase (convention check).

**Verification:**
```bash
cd /var/mnt/eclipse/repos/stapeln/backend
mix test test/stapeln_web/controllers/stack_controller_test.exs 2>&1
# Must pass all existing tests.

# Verify new rules exist:
grep -c "severity:" lib/stapeln/validation_engine.ex
# Must return at least 8 (3 original + 5 new).

# Run a validation test manually:
mix run -e '
  stack = %{
    id: 1,
    name: "test",
    description: nil,
    services: [
      %{name: "api", kind: "web", port: 80},
      %{name: "api2", kind: "web", port: 80},
      %{name: "Bad Name", kind: "", port: 99999}
    ],
    created_at: DateTime.utc_now(),
    updated_at: DateTime.utc_now()
  }
  report = Stapeln.ValidationEngine.validate(stack)
  IO.inspect(report.findings, label: "findings")
  IO.inspect(report.score, label: "score")
'
# Must show findings for: missing kind, duplicate port, invalid port range, privileged port, no description, bad service name format.
```

---

## TASK 14: Wire AppIntegrated.res port/security/gap callbacks properly

**Files:**
- `/var/mnt/eclipse/repos/stapeln/frontend/src/AppIntegrated.res` (lines 283-301)

**Problem:** The `onStateChange` callbacks for PortConfigPanel, SecurityInspector, GapAnalysis, and SimulationMode are all incorrect -- they dispatch hardcoded dummy messages instead of passing the actual new state:

```rescript
| PortConfigView =>
  <PortConfigPanel
    initialState={state.portConfig}
    onStateChange={newState => dispatch(PortConfigMsg(PortConfigPanel.SelectPort(0)))}
  />
```

This means `onStateChange` always sends `SelectPort(0)` regardless of what actually changed. Same pattern for SecurityInspector (`UpdateMetrics(newState.metrics)` is closer but ignores other state changes), GapAnalysis (`SelectGap("0")`), and SimulationMode (`ToggleStats`).

**What to do:**
1. Either remove `onStateChange` callbacks and have the child components dispatch messages directly via a shared dispatch function passed as a prop, OR
2. Create proper bridge messages that carry the full new child state, e.g.:
   - `PortConfigStateChanged(PortConfigPanel.state)`
   - `SecurityStateChanged(SecurityInspector.state)`
   - etc.
3. Handle these messages in the `update` function to replace the corresponding sub-state.

**Verification:**
```bash
cd /var/mnt/eclipse/repos/stapeln/frontend
npx rescript build 2>&1 | grep -i error
# Must compile with no errors.

# Verify no hardcoded dummy messages remain:
grep -n "SelectPort(0)" src/AppIntegrated.res
# Must return zero matches.

grep -n 'SelectGap("0")' src/AppIntegrated.res
# Must return zero matches.

grep -n "ToggleStats" src/AppIntegrated.res
# Must return zero matches (unless used for an actual toggle action).
```

---

## TASK 15: Implement FileIO.res readFile to return actual file contents

**Files:**
- `/var/mnt/eclipse/repos/stapeln/frontend/src/FileIO.res` (line 39)

**Problem:** The `readFile` function has a hardcoded return value:
```rescript
| Success => Ok("file contents") // Would be actual contents
```

The comment says "Would be actual contents" -- the FFI `read_file` external returns an `int` (error code), not the file contents. The function can never return real file data.

**What to do:**
1. If this module is meant to work via Zig FFI, redesign the FFI to return file contents (e.g., have `read_file` write contents to a buffer and return the buffer pointer).
2. If this module is meant for browser use, replace the Zig FFI calls with browser File API calls (which `Import.res` already does correctly via `WebAPI.FileReader`).
3. If neither is appropriate right now, mark the module as `@deprecated` and remove the misleading comment. Add a clear module-level doc comment explaining this is a future placeholder for verified file I/O.

**Verification:**
```bash
cd /var/mnt/eclipse/repos/stapeln/frontend
npx rescript build 2>&1 | grep -i error
# Must compile with no errors.

grep -n '"file contents"' src/FileIO.res
# Must return zero matches.
```

---

## TASK 16: Add tests for the gRPC server

**Files:**
- `/var/mnt/eclipse/repos/stapeln/backend/test/stapeln_grpc/stack_service_server_test.exs`

**Problem:** The gRPC tests exist and cover basic CRUD operations, but they do not test:
1. Validation with stacks that have no services (should produce findings).
2. Update with empty/blank fields (should not overwrite with blanks).
3. List stacks when store is empty (should return empty list).
4. Concurrent access patterns.

**What to do:**
Add test cases for:
1. `validate_stack` with an empty-services stack: verify `score < 100` and `findings` is non-empty.
2. `update_stack` with blank name: verify the original name is preserved (not overwritten with "").
3. `list_stacks` on empty store: verify response has empty `stacks` list.
4. `create_stack` with duplicate service names: verify the stack is created (validation happens separately).

**Verification:**
```bash
cd /var/mnt/eclipse/repos/stapeln/backend
mix test test/stapeln_grpc/stack_service_server_test.exs 2>&1
# Must pass all tests including the new ones.

# Count test cases:
grep -c "test \"" test/stapeln_grpc/stack_service_server_test.exs
# Must return at least 8 (4 existing + 4 new).
```

---

## TASK 17: Add Idris2 proofs for Types.idr

**Files:**
- `/var/mnt/eclipse/repos/stapeln/src/abi/Types.idr`
- `/var/mnt/eclipse/repos/stapeln/frontend/src/abi/FileIO.idr`

**Problem:** The Idris2 ABI layer has type definitions but zero actual dependent-type proofs. The types `ValidPath`, `SafeRead`, `AtomicWrite` in `FileIO.idr` are trivially inhabited (e.g., `MkValidPath : (s : String) -> ValidPath s` accepts any string including empty ones). There is no actual validation. The `safeReadFile` and `safeWriteFile` functions return hardcoded strings.

**What to do:**
1. In `Types.idr`: Add a proof that `ServiceSpec` names are non-empty:
   ```idris
   data NonEmptyName : String -> Type where
     MkNonEmptyName : (s : String) -> {auto prf : NonEmpty (unpack s)} -> NonEmptyName s
   ```
2. In `FileIO.idr`: Strengthen `ValidPath` to actually reject empty strings at the type level:
   ```idris
   data ValidPath : String -> Type where
     MkValidPath : (s : String) -> {auto prf : NonEmpty (unpack s)} -> ValidPath s
   ```
3. Add a proof that port numbers are in valid range (1-65535):
   ```idris
   data ValidPort : Nat -> Type where
     MkValidPort : (p : Nat) -> {auto prf : (1 `LTE` p, p `LTE` 65535)} -> ValidPort p
   ```

**Verification:**
```bash
# Verify Idris2 files type-check (requires idris2 compiler):
cd /var/mnt/eclipse/repos/stapeln
idris2 --check src/abi/Types.idr 2>&1
# Must succeed with no errors.

idris2 --check frontend/src/abi/FileIO.idr 2>&1
# Must succeed with no errors.

# Verify proofs exist:
grep -c "auto prf" src/abi/Types.idr
# Must return at least 1.
```

---

## TASK 18: Fix ErrorBoundary.res -- it does not actually catch errors

**Files:**
- `/var/mnt/eclipse/repos/stapeln/frontend/src/ErrorBoundary.res`

**Problem:** The `ErrorBoundary` component uses `React.useReducer` and has a `handleError` callback, but nothing ever calls `handleError`. React error boundaries require class components with `componentDidCatch` and `getDerivedStateFromError` lifecycle methods. ReScript's React bindings do not support class components, so this "error boundary" is purely decorative -- it will never catch any rendering errors.

**What to do:**
1. Create a JavaScript class component wrapper that implements the actual React error boundary pattern:
   ```javascript
   class ErrorBoundaryWrapper extends React.Component {
     constructor(props) { ... }
     static getDerivedStateFromError(error) { ... }
     componentDidCatch(error, errorInfo) { ... }
     render() { ... }
   }
   ```
2. Export it via `@module` FFI binding in ReScript.
3. Use this wrapper in `AppIntegrated.res` instead of the current ReScript-only `ErrorBoundary`.

Alternatively, use an existing library like `react-error-boundary` and bind to it via FFI.

**Verification:**
```bash
cd /var/mnt/eclipse/repos/stapeln/frontend
npx rescript build 2>&1 | grep -i error
# Must compile with no errors.

# Verify actual error boundary exists:
grep -n "getDerivedStateFromError\|componentDidCatch" src/ErrorBoundary.res src/ErrorBoundary.js 2>/dev/null
# Must return at least 2 matches (one for each lifecycle method).
```

---

## TASK 19: Add missing DomMounter source files

**Files:**
- `/var/mnt/eclipse/repos/stapeln/frontend/lib/ocaml/DomMounter.res`
- `/var/mnt/eclipse/repos/stapeln/frontend/lib/ocaml/DomMounterEnhanced.res`
- `/var/mnt/eclipse/repos/stapeln/frontend/lib/ocaml/DomMounterSecurity.res`

**Problem:** The adapter files (`ReactAdapter.res`, `SolidAdapter.res`, `VueAdapter.res`, `SSRAdapter.res`, `WebComponent.res`) all import `DomMounterEnhanced` and `DomMounterSecurity`, but these source files only exist in `frontend/lib/ocaml/` and `frontend/lib/bs/` (build output directories), not in `frontend/src/`. This means:
1. The source files are build artifacts, not source-controlled code.
2. The adapter modules will fail to compile if the build cache is cleared.
3. The adapters reference functions like `mount`, `unmount`, `mountWithLifecycle`, `unmountWithLifecycle`, `startMonitoring`, `stopMonitoring`, `healthCheck` that may not exist.

**What to do:**
1. Check if `DomMounter.res`, `DomMounterEnhanced.res`, and `DomMounterSecurity.res` exist as source files in `frontend/src/`. If not, create them based on the compiled output in `frontend/lib/ocaml/`.
2. Ensure all functions referenced by adapters are properly defined.
3. Add the source files to version control.
4. Verify the adapter files compile correctly.

**Verification:**
```bash
# Verify source files exist:
ls -la /var/mnt/eclipse/repos/stapeln/frontend/src/DomMounter.res \
       /var/mnt/eclipse/repos/stapeln/frontend/src/DomMounterEnhanced.res \
       /var/mnt/eclipse/repos/stapeln/frontend/src/DomMounterSecurity.res 2>&1
# All three must exist.

cd /var/mnt/eclipse/repos/stapeln/frontend
npx rescript build 2>&1 | grep -i error
# Must compile with no errors.
```

---

## TASK 20: Replace /tmp storage with configurable persistence in StackStore

**Files:**
- `/var/mnt/eclipse/repos/stapeln/backend/lib/stapeln/stack_store.ex`

**Problem:** The StackStore GenServer persists to `/tmp/stapeln-stack-store.json`. This path:
1. Is not configurable.
2. Uses `/tmp` which is cleared on reboot on most Linux systems.
3. Has no file locking, so concurrent access from multiple BEAM nodes would corrupt data.
4. STATUS.md explicitly lists "Durable persistence (disk/DB)" under "Not implemented yet."

**What to do:**
1. Make the persistence path configurable via application config: `Application.get_env(:stapeln, :stack_store_path, "priv/data/stack-store.json")`.
2. Use `priv/data/` as the default location (persists across reboots, within the project).
3. Add file locking using `:file.lock/2` or a simple lockfile pattern.
4. Add a config entry in `config/config.exs`:
   ```elixir
   config :stapeln, :stack_store_path, "priv/data/stack-store.json"
   ```
5. Create the `priv/data/` directory if it does not exist on startup.

**Verification:**
```bash
cd /var/mnt/eclipse/repos/stapeln/backend

# Verify config key is used:
grep -n "stack_store_path" lib/stapeln/stack_store.ex
# Must return at least 1 match.

grep -n "stack_store_path" config/config.exs
# Must return at least 1 match.

# Verify /tmp is no longer hardcoded:
grep -n "/tmp/stapeln" lib/stapeln/stack_store.ex
# Must return zero matches.

mix test 2>&1
# All tests must pass.
```

---

## FINAL VERIFICATION

Run these commands to verify the entire project is in a healthy state:

```bash
# 1. Backend: compile and test
cd /var/mnt/eclipse/repos/stapeln/backend
mix deps.get
mix compile --warnings-as-errors 2>&1
mix test 2>&1
echo "Backend exit code: $?"

# 2. Frontend: compile
cd /var/mnt/eclipse/repos/stapeln/frontend
npm install  # or deno install
npx rescript build 2>&1
echo "Frontend exit code: $?"

# 3. Zig FFI: build and test
cd /var/mnt/eclipse/repos/stapeln/ffi/zig
zig build 2>&1
zig build test 2>&1
echo "Zig exit code: $?"

# 4. License check: no AGPL anywhere in source
grep -rn "AGPL-3.0-or-later" \
  /var/mnt/eclipse/repos/stapeln/src/ \
  /var/mnt/eclipse/repos/stapeln/ffi/ \
  /var/mnt/eclipse/repos/stapeln/frontend/src/ \
  /var/mnt/eclipse/repos/stapeln/backend/lib/
echo "AGPL check (should be empty): $?"

# 5. No TODO/FIXME stubs in critical paths
grep -rn "TODO\|FIXME\|HACK" \
  /var/mnt/eclipse/repos/stapeln/frontend/src/Update.res \
  /var/mnt/eclipse/repos/stapeln/frontend/src/GapAnalysis.res \
  /var/mnt/eclipse/repos/stapeln/frontend/src/SecurityInspector.res \
  /var/mnt/eclipse/repos/stapeln/frontend/src/Export.res \
  /var/mnt/eclipse/repos/stapeln/ffi/zig/src/main.zig
echo "TODO check (should be empty): $?"

# 6. Verify test counts
echo "Backend tests:"
cd /var/mnt/eclipse/repos/stapeln/backend && mix test --trace 2>&1 | tail -3

echo "All checks complete."
```
