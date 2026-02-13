# Decision Record 0001: Adopt Ada/SPARK as Primary Implementation Language

**Status:** Accepted
**Date:** 2024-01-15
**Decision Makers:** Founder (pre-cooperative formation)

## Context

Cerro Torre requires a programming language for its core tooling that prioritises:
1. Memory safety without garbage collection
2. Formal verification capability for security-critical code
3. Long-term stability and readability
4. Strong typing to catch errors at compile time

## Options Considered

### Option A: Rust
- Pros: Memory safe, modern ecosystem, good tooling
- Cons: Rapid language evolution, complex borrow checker learning curve, no formal verification in standard tooling

### Option B: Ada/SPARK
- Pros: Proven in safety-critical systems, SPARK provides formal verification, stable language standard, explicit and readable
- Cons: Smaller ecosystem, steeper initial learning curve, less "trendy"

### Option C: Go
- Pros: Simple, good tooling, large ecosystem
- Cons: Garbage collected (unpredictable latency), limited type system, no formal verification

### Option D: C with formal methods tools
- Pros: Universal, maximum control
- Cons: Memory safety requires extreme discipline, formal tools are add-ons not integrated

## Decision

**Adopt Ada/SPARK** as the primary implementation language.

## Rationale

1. **Formal verification is essential** for the trust model. Users must be able to verify that cryptographic operations are correct. SPARK is the only mainstream language with integrated formal verification.

2. **Memory safety without GC** means predictable performance for build operations and no stop-the-world pauses.

3. **Stability over novelty** - Ada has been stable for decades. Code written today will compile in 20 years. This matches our stewardship commitment.

4. **Readability as documentation** - Ada's verbosity is a feature for security-critical code. The code documents itself.

5. **Heritage in safety-critical systems** - If Ada/SPARK is trusted for avionics and medical devices, it's appropriate for supply chain security.

## Consequences

- Smaller contributor pool initially (Ada developers are fewer than Rust/Go)
- Need to create bindings for some C libraries (libsodium, etc.)
- Build system uses Alire rather than more common tools
- May need Rust for specific components where Ada libraries don't exist

## Review

This decision should be reviewed after MVP if:
- Contributor acquisition is severely impacted
- Critical libraries prove impossible to use from Ada
- SPARK verification proves impractical for our use cases
