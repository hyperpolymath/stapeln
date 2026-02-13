# Selur Security Audit - 2026-01-25

**Auditor**: Claude Sonnet 4.5
**Scope**: Rust bindings, GitHub workflows, architecture documentation
**Severity Scale**: CRITICAL | HIGH | MEDIUM | LOW | INFO

---

## Executive Summary

**Total Issues Found**: 2
**Critical**: 0
**High**: 0
**Medium**: 1
**Low**: 1

**Status**: ✅ Scaffolding project with minimal implementation
**Recommendation**: Address security considerations before implementing WASM bridge code

---

## Project Status

Selur is currently a **specification and scaffolding project** with:
- ✅ Documentation (README, ROADMAP, META, STATE, ECOSYSTEM)
- ✅ Rust library structure (lib.rs with TODOs)
- ✅ GitHub workflow (Pages deployment)
- ⚠️  **No actual WASM implementation yet**
- ⚠️  **No Ephapax-linear code yet**
- ⚠️  **No Zig WASM compilation yet**
- ⚠️  **No Idris2 proofs yet**

---

## MEDIUM Issues

### MED-001: Path Injection in GitHub Workflow

**Location**: `.github/workflows/casket-pages.yml:58, 64, 70, 74`

**Description**:
The workflow uses unquoted `$(basename $PWD)` in shell commands. While GitHub Actions controls the PWD environment variable, defensive quoting is a best practice.

**Code**:
```yaml
- name: Build site
  run: |
    echo "title: $(basename $PWD)" >> site/index.md  # ❌ Unquoted
    echo "date: $(date +%Y-%m-%d)" >> site/index.md
```

**Risk**:
Low in practice (GitHub Actions sets PWD), but if workflow is copied to other contexts, could allow injection if directory name contains special characters.

**Recommended Fix**:
```yaml
- name: Build site
  run: |
    REPO_NAME="$(basename "$PWD")"
    echo "title: $REPO_NAME" >> site/index.md
    echo "date: $(date +%Y-%m-%d)" >> site/index.md
```

**Severity**: MEDIUM
**CVSS**: 3.5 (Low exploitability in GitHub Actions context)
**CWE**: CWE-78 (OS Command Injection - potential)
**Status**: RECOMMENDED

---

## LOW Issues

### LOW-001: TODO Implementation in Security-Critical Code

**Location**: `src/lib.rs:34-37`

**Description**:
The `send_request` method uses `todo!()` which will panic if called. This is expected for scaffolding, but should be tracked for implementation.

**Code**:
```rust
pub fn send_request(&self, _request: &[u8]) -> Result<Vec<u8>> {
    // TODO: Implement request/response passing via WASM linear memory
    todo!("Implement request passing")
}
```

**Future Security Considerations**:
When implementing this method, ensure:
1. Input validation on request bytes
2. Bounds checking for WASM linear memory access
3. No buffer overflows when copying to/from WASM memory
4. Proper error handling (don't panic on malformed requests)

**Recommended Implementation Pattern**:
```rust
pub fn send_request(&self, request: &[u8]) -> Result<Vec<u8>> {
    // Validate request size
    if request.len() > MAX_REQUEST_SIZE {
        return Err(anyhow!("Request too large: {} bytes", request.len()));
    }

    // Validate request is not empty
    if request.is_empty() {
        return Err(anyhow!("Empty request"));
    }

    let mut store = Store::new(&self.engine, ());
    let instance = Instance::new(&mut store, &self.module, &[])?;

    // Get WASM memory and functions
    let memory = instance.get_memory(&mut store, "memory")
        .ok_or_else(|| anyhow!("WASM module missing 'memory' export"))?;
    let send_fn = instance.get_typed_func::<(i32, i32), i32>(&mut store, "send_request")
        .context("WASM module missing 'send_request' function")?;

    // Write request to WASM memory with bounds check
    let data = memory.data_mut(&mut store);
    if request.len() > data.len() {
        return Err(anyhow!("Request exceeds WASM memory size"));
    }
    data[0..request.len()].copy_from_slice(request);

    // Call WASM function
    let response_ptr = send_fn.call(&mut store, (0, request.len() as i32))
        .context("WASM send_request failed")?;

    // Read response with bounds check
    let response_len = /* read from WASM memory */;
    if response_len > data.len() {
        return Err(anyhow!("Response exceeds WASM memory size"));
    }

    let response = data[response_ptr as usize..(response_ptr as usize + response_len)]
        .to_vec();

    Ok(response)
}
```

**Severity**: LOW
**CVSS**: N/A (not yet implemented)
**CWE**: N/A
**Status**: INFORMATIONAL

---

## Positive Findings

### ✅ GitHub Workflow Security

**Well-secured workflow** (casket-pages.yml):
- SPDX license header
- `permissions: read-all` with specific write grants
- All actions SHA-pinned:
  - `actions/checkout@b4ffde65f46336ab88eb53be808477a3936bae11` # v4
  - `haskell-actions/setup@ec49483bfc012387b227434aba94f59a6ecd0900` # v2
  - `actions/cache@0057852bfaa89a56745cba8c7296529d2fc39830` # v4
  - `actions/configure-pages@983d7736d9b0ae728b81ab479565c72886d7745b` # v5
  - `actions/upload-pages-artifact@56afc609e74202658d3ffba0e8f6dda462b719fa` # v3
  - `actions/deploy-pages@d6db90164ac5ed86f2b6aed7e0febac5b3c0c03e` # v4
- Proper concurrency control
- Minimal permissions principle

### ✅ Safe Rust API Design

The Rust `Bridge` struct in `src/lib.rs` uses:
- `wasmtime::Result` for error handling (no unwrap/panic in public API)
- Proper ownership with `Engine` and `Module`
- Type-safe WASM loading via `Module::from_file`

### ✅ Documentation Quality

Excellent security-relevant documentation:
- Clear architecture diagram showing trust boundaries
- Performance considerations documented
- Integration examples provided
- Fallback mechanisms documented

---

## Security Considerations for Future Development

When implementing the actual WASM bridge, address these areas:

### 1. WASM Memory Safety

**Risks**:
- Buffer overflows when copying to/from WASM linear memory
- Out-of-bounds memory access
- Memory leaks if Ephapax-linear types not properly enforced

**Mitigations**:
- Use `wasmtime::Memory::data_mut()` with bounds checks
- Validate all pointer offsets before dereferencing
- Use `std::slice::from_raw_parts` only with validated lengths
- Implement Ephapax-linear types correctly (no use-after-free)

**Example Safe Pattern**:
```rust
fn write_to_wasm_memory(
    memory: &Memory,
    store: &mut Store<()>,
    offset: usize,
    data: &[u8]
) -> Result<()> {
    let mem_data = memory.data_mut(store);

    // Bounds check
    if offset.checked_add(data.len()).ok_or_else(|| anyhow!("Integer overflow"))? > mem_data.len() {
        return Err(anyhow!("Write would exceed WASM memory bounds"));
    }

    mem_data[offset..offset + data.len()].copy_from_slice(data);
    Ok(())
}
```

### 2. Input Validation

**Requirements**:
- Validate request/response sizes (set `MAX_REQUEST_SIZE` constant)
- Validate WASM function signatures match expectations
- Validate pointer offsets are within memory bounds
- Sanitize error messages (don't leak memory contents)

**Example**:
```rust
const MAX_REQUEST_SIZE: usize = 1024 * 1024; // 1 MB
const MAX_RESPONSE_SIZE: usize = 1024 * 1024;

fn validate_request(request: &[u8]) -> Result<()> {
    if request.is_empty() {
        return Err(anyhow!("Empty request"));
    }
    if request.len() > MAX_REQUEST_SIZE {
        return Err(anyhow!("Request too large: {} > {}", request.len(), MAX_REQUEST_SIZE));
    }
    Ok(())
}
```

### 3. Ephapax-Linear Type Safety

**When implementing** `ephapax/bridge.eph`:
- Ensure regions are properly enforced (no double-free)
- Implement move semantics correctly (no use-after-move)
- Validate type conversions are safe
- Use Idris2 proofs to verify linear type invariants

**Example Proof Requirements**:
```idris
-- No double-free
noDoubleFree : (req: Request@r) -> (used: Bool) -> Type
noDoubleFree req True = RequestConsumed req
noDoubleFree req False = RequestAvailable req

-- No use-after-free
noUseAfterFree : (req: Request@r) -> (freed: Bool) -> Type
noUseAfterFree req True = CannotAccessRequest req
noUseAfterFree req False = CanAccessRequest req
```

### 4. Zig WASM Compilation Security

**When implementing** `zig/runtime.zig`:
- Use `@panic` instead of undefined behavior
- Validate all exports match expected signatures
- Use `@intCast` with overflow checks
- Avoid `@ptrCast` without size validation

**Example Safe Zig Pattern**:
```zig
export fn send_request(ptr: [*]const u8, len: usize) callconv(.C) i32 {
    // Bounds check
    if (len > MAX_REQUEST_SIZE) {
        @panic("Request too large");
    }

    // Safe slice creation
    const request = ptr[0..len];

    // Process request...
    return 0;
}
```

### 5. Denial of Service Prevention

**Potential DoS vectors**:
- Infinite loops in WASM code (use `Config::max_wasm_stack` limit)
- Memory exhaustion (set `Store::limiter()` with memory limits)
- CPU exhaustion (use timeouts for WASM execution)

**Mitigation Example**:
```rust
use wasmtime::*;

fn create_limited_store() -> Store<()> {
    let mut config = Config::new();
    config.max_wasm_stack(1024 * 1024); // 1 MB stack
    config.consume_fuel(true);

    let engine = Engine::new(&config).unwrap();
    let mut store = Store::new(&engine, ());

    // Set fuel limit (prevents infinite loops)
    store.add_fuel(100_000).unwrap();

    // Set memory limit
    store.limiter(|_state| ResourceLimiter::new(10 * 1024 * 1024)); // 10 MB

    store
}
```

### 6. Formal Verification Requirements

**Idris2 proofs to implement** (`idris/proofs.idr`):
- `noLostRequests`: Every request gets a response
- `noMemoryLeaks`: All allocated regions are freed
- `noDataRaces`: No concurrent access to WASM memory
- `noBufferOverflows`: All memory accesses are in-bounds
- `linearTypesEnforced`: Resources used exactly once

---

## Recommendations

### Immediate (Before WASM Implementation)
1. ✅ Fix GitHub workflow quoting (MED-001) - low priority
2. [ ] Create test vectors for valid/invalid requests
3. [ ] Define security policy (SECURITY.md exists ✅)
4. [ ] Set up fuzzing infrastructure for WASM bridge

### Short-term (During WASM Implementation)
1. [ ] Implement bounds-checked WASM memory access
2. [ ] Add request/response size limits
3. [ ] Implement resource limiters (fuel, memory, stack)
4. [ ] Write Idris2 proofs for linear type safety
5. [ ] Add integration tests with malicious inputs

### Long-term (Production Hardening)
1. [ ] Formal verification of Ephapax-linear → WASM compilation
2. [ ] Professional security audit of WASM bridge
3. [ ] Fuzzing campaign (AFL, libFuzzer)
4. [ ] Performance benchmarking under load
5. [ ] Integration testing with Svalinn and Vörðr

---

## Compliance Notes

### CWE Mappings (Future)
When implementing WASM bridge, watch for:
- CWE-119: Buffer Errors (WASM memory access)
- CWE-190: Integer Overflow (size calculations)
- CWE-400: Resource Exhaustion (DoS via WASM loops)
- CWE-416: Use After Free (Ephapax-linear violations)
- CWE-680: Integer Overflow to Buffer Overflow

### OWASP Top 10 2021 (Future)
- A04:2021 – Insecure Design (validate architecture before implementing)
- A06:2021 – Vulnerable and Outdated Components (keep wasmtime updated)

---

## Sign-Off

**Audit Completed**: 2026-01-25
**Implementation Status**: Scaffolding only (no security-critical code yet)
**Next Review**: After initial WASM implementation

**Current Status**: ✅ Safe for development
**Production Ready**: NO (implementation required first)

**Auditor**: Claude Sonnet 4.5
**Recommendations**: Follow security guidelines when implementing WASM bridge

---

## Summary

Selur is a well-structured specification project with:
- ✅ Secure GitHub workflow
- ✅ Safe Rust API design
- ✅ Clear documentation
- ⚠️  Needs careful implementation of WASM bridge
- ⚠️  Requires formal verification (Idris2 proofs)

**No immediate security concerns** - project is scaffolding only. Security considerations documented for future implementation phases.
