# MVP Manifest Specification

**Version**: 0.1.0-mvp
**Status**: Draft

This is the minimal manifest schema for the MVP. It is a strict subset of the full `manifest-format.md` specification.

## Purpose

Define the smallest `.ctp` manifest that still proves the Cerro Torre architecture works end-to-end.

## Schema

```toml
manifest-version = "0.1.0"

[package]
name = "<string>"           # Required. Package name (a-z, 0-9, -, +, .)
version = "<string>"        # Required. Upstream version
epoch = <integer>           # Optional. Default: 0
summary = "<string>"        # Required. One-line description

[provenance]
upstream-url = "<url>"      # Required. Source tarball URL
upstream-hash = { algorithm = "sha256", digest = "<hex>" }  # Required
imported-from = "<string>"  # Required. e.g., "debian:hello/2.10-3"
import-date = <datetime>    # Required. ISO 8601

[build]
system = "<string>"         # Required. One of: autotools, cmake, meson, make
dependencies = ["<string>"] # Required. Build-time dependencies

[build.commands]
configure = "<string>"      # Optional. Configure command
build = "<string>"          # Required. Build command
install = "<string>"        # Required. Install command (uses $DESTDIR)
```

## Constraints

### Package Name
- Length: 1-64 characters
- Characters: `[a-z0-9][a-z0-9+.-]*`
- Cannot start with digit
- Cannot contain consecutive dots

### Version
- Format: `[epoch:]upstream[-revision]`
- Epoch: non-negative integer
- Upstream: alphanumeric with `.`, `-`, `+`, `~`
- Revision: positive integer

### Hash
- Only `sha256` algorithm for MVP
- Digest: 64 hexadecimal characters (lowercase)

### URLs
- Must be `https://` or `http://` (https preferred)
- No `file://` or other schemes

## Example

```toml
manifest-version = "0.1.0"

[package]
name = "hello"
version = "2.10"
epoch = 0
summary = "Famous friendly greeting program"

[provenance]
upstream-url = "https://ftp.gnu.org/gnu/hello/hello-2.10.tar.gz"
upstream-hash = { algorithm = "sha256", digest = "31e066137a962676e89f69d1b65382de95a7ef7d914b8cb956f41ea72e0f516b" }
imported-from = "debian:hello/2.10-3"
import-date = 2025-01-15T14:30:00Z

[build]
system = "autotools"
dependencies = ["gcc", "make", "gettext"]

[build.commands]
configure = "./configure --prefix=/usr"
build = "make -j$(nproc)"
install = "make DESTDIR=$DESTDIR install"
```

## Validation Rules

1. **Required fields must be present** - Parse fails if any required field is missing
2. **Hash must verify** - Fetched source must match `upstream-hash`
3. **URL must be reachable** - Fetch must succeed (may be cached)
4. **Dependencies must resolve** - Listed dependencies must be available in build environment

## Error Codes

| Code | Name | Description |
|------|------|-------------|
| `E001` | `MANIFEST_PARSE_ERROR` | TOML syntax error |
| `E002` | `MANIFEST_MISSING_FIELD` | Required field not present |
| `E003` | `MANIFEST_INVALID_VALUE` | Field value fails validation |
| `E010` | `FETCH_FAILED` | Could not download source |
| `E011` | `HASH_MISMATCH` | Downloaded content doesn't match hash |
| `E020` | `BUILD_FAILED` | Build command returned non-zero |
| `E030` | `ATTESTATION_FAILED` | Could not generate provenance |

## Output Structure

A successful build produces:

```
dist/<package>-<version>/
├── image.tar              # OCI image tarball
├── sbom.spdx.json         # SPDX SBOM
├── provenance.jsonl       # in-toto/SLSA attestation
├── digests.txt            # SHA256 of all outputs
├── digests.txt.sig        # Ed25519 signature
└── build.log              # Build output for debugging
```

## Seam Invariants

These invariants must hold for MVP to be considered successful:

| Seam | Invariant |
|------|-----------|
| Manifest → Plan | `sha256(manifest) + sha256(inputs) → deterministic build plan` |
| Inputs → Attestation | Every byte influencing build is in provenance `materials` |
| Outputs → OCI | `subject.digest` in provenance = `sha256(image.tar)` |
| Keys → Trust | Signature verifiable with `keys/` trust root |
