# SPARK Verification Requirements for Vörðr

## Overview

The Vörðr Ada/SPARK components provide formally verified security-critical functionality using SPARK's static analysis and proof system.

## Components Requiring Verification

### 1. Threshold Signatures (`threshold_signatures.ads/adb`)

**Ghost Predicates:**
- `Is_Valid_Scheme` - Ensures k ≤ n and valid bounds
- `Threshold_Reached` - Checks if k signatures collected

**Proofs Required:**
- No integer overflow in signature counting
- Share arrays never exceed bounds
- Duplicate signer detection is sound
- Authorization result is deterministic given same inputs

**SPARK Annotations:**
```ada
pragma SPARK_Mode (On);
```

**Preconditions:**
- `Create_Scheme`: k ≥ 1, n ≤ Max_Signers, k ≤ n
- `Add_Share`: Valid scheme state, signature is well-formed
- `Check_Authorization`: Scheme initialized

**Postconditions:**
- `Create_Scheme`: Returns valid scheme
- `Add_Share`: Share count incremented by 0 or 1
- `Check_Authorization`: Result matches predicate Threshold_Reached

### 2. Gatekeeper (`gatekeeper.ads/adb`)

**Security Properties:**
- Container policies enforced before execution
- No unauthorized privilege escalation
- All security checks complete before approval

**Proofs Required:**
- Policy evaluation never skips checks
- Approval requires all validators to pass
- Denial is fail-secure (default deny)

### 3. Container Policy (`container_policy.ads/adb`)

**Security Properties:**
- Capability bounds checking
- UID/GID range validation
- Network mode restrictions

**Proofs Required:**
- No capability list overflow
- UID/GID within valid ranges (0..65535)
- Policy composition is associative

### 4. OCI Parser (`oci_parser.ads/adb`)

**Security Properties:**
- JSON parsing bounds checked
- No buffer overflows in string operations
- Invalid JSON rejected safely

**Proofs Required:**
- String operations respect length bounds
- Array accesses within declared ranges
- Error handling complete (no unhandled exceptions)

## Running SPARK Verification

### Prerequisites

Install SPARK Community Edition:
```bash
# From https://www.adacore.com/community
curl -O https://community.download.adacore.com/v1/...
```

Or via Alire:
```bash
alr get gnatprove
```

### Verification Commands

```bash
cd /var/mnt/eclipse/repos/vordr/src/ada

# Check SPARK conformance (syntax + flow analysis)
gnatprove -P policy.gpr --mode=check

# Prove with automatic solvers (default: CVC5, Alt-Ergo)
gnatprove -P policy.gpr --level=2 --timeout=30

# Prove with maximum effort (may take hours)
gnatprove -P policy.gpr --level=4 --timeout=120 --prover=all

# Generate proof report
gnatprove -P policy.gpr --report=all --output-dir=proof_results
```

### Verification Levels

| Level | Description | Typical Time |
|-------|-------------|--------------|
| 0 | Flow analysis only | seconds |
| 1 | Fast proofs (simple properties) | minutes |
| 2 | Default (most properties) | 5-15 min |
| 3 | Advanced (complex invariants) | 30-60 min |
| 4 | Maximum (all solvers, high timeout) | hours |

### Expected Results

With full SPARK installation, all files should verify at level 2:

```
Phase 1 of 2: generation of Global contracts ...
Phase 2 of 2: flow analysis and proof ...

threshold_signatures.adb:42:19: info: overflow check proved
threshold_signatures.adb:58:22: info: range check proved
threshold_signatures.adb:64:26: info: precondition proved
...

Summary:
  100% (45/45) proved
```

## Proof Obligations

### Critical Proofs (Must Pass)

1. **No Runtime Errors**
   - Division by zero
   - Integer overflow
   - Array index out of bounds
   - Null pointer dereference

2. **Security Invariants**
   - Threshold ≥ 1 and ≤ Total_Signers
   - Share_Count ≤ Total_Signers
   - Authorization only when Threshold_Reached

3. **Type Safety**
   - All type conversions valid
   - No unchecked unions
   - No representation clauses violating alignment

### Optional Proofs (Nice to Have)

1. **Functional Correctness**
   - `Add_Share(s1) + Add_Share(s2) = Add_Share(s2) + Add_Share(s1)` (commutativity)
   - Duplicate detection finds all duplicates

2. **Performance Bounds**
   - `Check_Authorization` completes in O(n) time
   - Memory usage bounded by Max_Signers

## CI/CD Integration

Add to `.gitlab-ci.yml`:

```yaml
spark-proof:
  stage: verify
  image: adacore/gnat-ce:latest
  script:
    - cd src/ada
    - gnatprove -P policy.gpr --level=2 --timeout=60 --checks-as-errors
  artifacts:
    paths:
      - src/ada/gnatprove/
    reports:
      junit: src/ada/gnatprove/junit.xml
  allow_failure: false  # Block merge if proofs fail
```

## Troubleshooting

### Common Issues

**Issue:** `precondition might fail`
**Fix:** Add `pragma Assert` before call site to help prover

**Issue:** `overflow check might fail`
**Fix:** Use ranged subtypes or add explicit range checks

**Issue:** `proof timeout`
**Fix:** Increase `--timeout` or simplify loop invariants

### Getting Help

- SPARK User Guide: https://docs.adacore.com/spark2014-docs/html/ug/
- SPARK by Example: https://github.com/AdaCore/spark-by-example
- Community Forum: https://forum.adacore.com/

## Current Status

**As of 2026-01-24:**
- ✅ Code compiles with GNAT
- ❌ GNATprove not installed in development environment
- ⏳ Formal verification pending SPARK toolchain installation

**Next Steps:**
1. Install SPARK Community Edition or configure via Alire
2. Run `gnatprove --mode=check` for syntax verification
3. Fix any SPARK violations (dead code, aliasing, etc.)
4. Run `gnatprove --level=2` for full proof
5. Add proofs to CI/CD pipeline
