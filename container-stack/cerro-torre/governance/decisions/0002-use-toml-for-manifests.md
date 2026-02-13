# Decision Record 0002: Use TOML for Manifest Format

**Status:** Accepted
**Date:** 2024-01-15
**Decision Makers:** Founder (pre-cooperative formation)

## Context

The package manifest format (.ctp files) needs to be:
1. Human-readable and writable
2. Unambiguous to parse
3. Supported by existing tooling
4. Suitable for version control (diff-friendly)

## Options Considered

### Option A: YAML
- Pros: Very human-friendly, widely used
- Cons: Ambiguous syntax (Norway problem), complex specification, security issues with some parsers

### Option B: TOML
- Pros: Unambiguous, simple spec, good tooling, Cargo/Rust ecosystem precedent
- Cons: Less flexible than YAML, nested structures can be verbose

### Option C: JSON
- Pros: Universal support, unambiguous
- Cons: No comments, poor human writability, noisy diffs

### Option D: Custom format
- Pros: Perfect fit for our needs
- Cons: No existing tooling, maintenance burden, learning curve

### Option E: S-expressions
- Pros: Simple, unambiguous, Lisp heritage
- Cons: Unfamiliar to most developers, limited tooling

## Decision

**Use TOML** for manifest files.

## Rationale

1. **Unambiguous parsing** - Unlike YAML, TOML has a simple, unambiguous grammar. "Norway" parses as a string, not a boolean.

2. **Comments supported** - Unlike JSON, TOML allows comments, essential for documenting complex manifests.

3. **Cargo precedent** - Rust's Cargo.toml has normalised TOML for package manifests. Developers are familiar with it.

4. **Strong typing** - TOML distinguishes strings, integers, dates, etc., reducing parser bugs.

5. **Diff-friendly** - Changes to manifests produce clean, readable diffs.

## Consequences

- Need TOML parser for Ada (toml_slicer crate or similar)
- Some structures (like ordered patch lists) are slightly verbose
- Users familiar with YAML may need brief adjustment

## Example

```toml
[package]
name = "hello"
version = "2.10-1"

[provenance]
upstream-url = "https://ftp.gnu.org/gnu/hello/hello-2.10.tar.gz"
upstream-hash = { algorithm = "sha256", digest = "31e066..." }
```
