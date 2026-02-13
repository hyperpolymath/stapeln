# Cerro Torre Package Manifest Format

**Version**: 0.1.0-draft  
**Status**: Draft Specification  
**SPDX-License-Identifier**: PMPL-1.0-or-later

## Overview

The Cerro Torre Package (CTP) manifest format is a declarative, Turing-incomplete specification language for defining software packages. It is designed to be:

1. **Formally verifiable** — All valid manifests can be statically analysed
2. **Deterministic** — Same manifest always produces same build
3. **Auditable** — Human-readable with complete provenance information
4. **Import-friendly** — Can represent packages from Debian, Fedora, Alpine, Nix

The format intentionally excludes arbitrary computation. There are no loops, conditionals that affect structure, or shell execution in manifest evaluation. Build-time computation happens in a separate, sandboxed phase.

## File Extension and Encoding

- Extension: `.ctp`
- Encoding: UTF-8, no BOM
- Line endings: LF (Unix-style)

## Syntax

CTP uses a structured text format inspired by TOML and Dhall, but simpler than either.

### Basic Structure

```ctp
# Comment lines start with hash
# Blank lines are ignored

[metadata]
name = "coreutils"
version = "9.4"
revision = 1
epoch = 0

[provenance]
upstream = "https://ftp.gnu.org/gnu/coreutils/coreutils-9.4.tar.xz"
upstream_hash = "sha256:abc123..."
imported_from = "debian:coreutils/9.4-2"
import_date = 2024-12-07T10:30:00Z

[dependencies]
runtime = ["libc", "libacl"]
build = ["gcc", "make", "autoconf"]

[build]
system = "autoconf"
configure_flags = ["--prefix=/usr", "--enable-no-install-program=groups,hostname,kill,uptime"]

[outputs]
primary = "coreutils"
split = ["coreutils-doc", "coreutils-dbgsym"]

[attestations]
require = ["source-signature", "reproducible-build"]
```

### Data Types

**String**: Double-quoted, UTF-8. Escape sequences: `\\`, `\"`, `\n`, `\t`.
```ctp
name = "hello-world"
description = "A \"simple\" package\nwith newlines"
```

**Integer**: Decimal only, no separators. Optional leading `-` for negative.
```ctp
revision = 1
epoch = 0
priority = -10
```

**Boolean**: Lowercase `true` or `false` only.
```ctp
essential = true
reproducible = false
```

**Date-Time**: ISO 8601 format with timezone. Always UTC preferred.
```ctp
import_date = 2024-12-07T10:30:00Z
```

**Date**: ISO 8601 date only.
```ctp
release_date = 2024-12-07
```

**List**: Square brackets, comma-separated. Trailing comma permitted.
```ctp
dependencies = ["libc", "libm", "libpthread"]
configure_flags = [
    "--prefix=/usr",
    "--sysconfdir=/etc",
    "--enable-shared",
]
```

**Hash**: Prefixed with algorithm. Supported: `sha256`, `sha384`, `sha512`, `blake3`.
```ctp
upstream_hash = "sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
source_hash = "blake3:af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262"
```

**Reference**: Package reference with optional version constraint.
```ctp
# Exact version
dep = "libc@2.38"

# Minimum version
dep = "libc>=2.38"

# Version range
dep = "libc>=2.38,<3.0"

# Any version
dep = "libc"
```

### Sections

#### `[metadata]` — Required

Identifies the package.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `name` | String | Yes | Package name, lowercase alphanumeric plus `-` |
| `version` | String | Yes | Upstream version |
| `revision` | Integer | Yes | Cerro Torre package revision |
| `epoch` | Integer | No | Version epoch for ordering (default: 0) |
| `summary` | String | Yes | One-line description |
| `description` | String | No | Full description |
| `license` | String | Yes | SPDX license expression |
| `homepage` | String | No | Upstream project URL |
| `maintainer` | String | Yes | Cerro Torre maintainer identifier |

```ctp
[metadata]
name = "nginx"
version = "1.26.0"
revision = 1
summary = "High-performance HTTP server and reverse proxy"
license = "BSD-2-Clause"
homepage = "https://nginx.org"
maintainer = "cerro-torre:web-team"
```

#### `[provenance]` — Required

Documents where the package came from.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `upstream` | String | Yes | Original source URL |
| `upstream_hash` | Hash | Yes | Cryptographic hash of upstream source |
| `upstream_signature` | String | No | URL to detached signature |
| `upstream_keyring` | String | No | Reference to verification keyring |
| `imported_from` | String | No | Source distribution reference |
| `import_date` | DateTime | No | When import occurred |
| `patches` | List[String] | No | List of patch file references |

```ctp
[provenance]
upstream = "https://nginx.org/download/nginx-1.26.0.tar.gz"
upstream_hash = "sha256:abc123..."
upstream_signature = "https://nginx.org/download/nginx-1.26.0.tar.gz.asc"
upstream_keyring = "cerro-torre:nginx-signing-keys"
imported_from = "debian:nginx/1.26.0-1"
import_date = 2024-12-07T10:30:00Z
patches = [
    "patches/01-fix-systemd-socket.patch",
    "patches/02-debian-config-paths.patch",
]
```

#### `[dependencies]` — Optional

Declares package dependencies.

| Field | Type | Description |
|-------|------|-------------|
| `runtime` | List[Reference] | Required at runtime |
| `build` | List[Reference] | Required to build |
| `check` | List[Reference] | Required to run tests |
| `optional` | List[Reference] | Optional enhancements |
| `conflicts` | List[Reference] | Cannot be installed alongside |
| `provides` | List[Reference] | Virtual packages this satisfies |
| `replaces` | List[Reference] | Packages this supersedes |

```ctp
[dependencies]
runtime = ["libc>=2.38", "openssl>=3.0", "pcre2"]
build = ["gcc", "make", "libssl-dev", "libpcre2-dev"]
check = ["perl", "test-nginx"]
optional = ["geoip2", "brotli"]
conflicts = ["nginx-light", "nginx-full"]
provides = ["httpd", "httpd-cgi"]
```

#### `[build]` — Required

Defines how to build the package.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `system` | String | Yes | Build system identifier |
| `configure_flags` | List[String] | No | Configuration arguments |
| `build_flags` | List[String] | No | Build-time arguments |
| `install_flags` | List[String] | No | Installation arguments |
| `environment` | Table | No | Environment variables |
| `phases` | Table | No | Custom phase overrides |

**Build Systems**: `autoconf`, `cmake`, `meson`, `cargo`, `alire`, `dune`, `mix`, `make`, `custom`

```ctp
[build]
system = "autoconf"
configure_flags = [
    "--prefix=/usr",
    "--sbin-path=/usr/sbin/nginx",
    "--conf-path=/etc/nginx/nginx.conf",
    "--with-http_ssl_module",
    "--with-http_v2_module",
]

[build.environment]
CFLAGS = "-O2 -fstack-protector-strong"
LDFLAGS = "-Wl,-z,relro,-z,now"

[build.phases]
# Phase overrides for custom build systems
# These are NOT arbitrary shell - see Build Phases section
```

#### `[outputs]` — Required

Defines what the build produces.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `primary` | String | Yes | Main output package name |
| `split` | List[String] | No | Additional split packages |
| `files` | Table | No | File installation mapping |

```ctp
[outputs]
primary = "nginx"
split = ["nginx-doc", "nginx-dbgsym"]

[outputs.files]
"/usr/sbin/nginx" = { mode = "0755", package = "nginx" }
"/etc/nginx/" = { mode = "0755", type = "directory", package = "nginx" }
"/usr/share/doc/nginx/" = { package = "nginx-doc" }
```

#### `[attestations]` — Optional

Specifies required attestations for this package.

| Field | Type | Description |
|-------|------|-------------|
| `require` | List[String] | Required attestation types |
| `recommend` | List[String] | Recommended attestation types |

**Attestation Types**: `source-signature`, `reproducible-build`, `sbom-complete`, `security-audit`, `fuzz-tested`

```ctp
[attestations]
require = ["source-signature", "reproducible-build", "sbom-complete"]
recommend = ["security-audit"]
```

#### `[selinux]` — Optional

SELinux policy hints for automatic policy generation.

| Field | Type | Description |
|-------|------|-------------|
| `domain` | String | SELinux domain type |
| `capabilities` | List[String] | Required Linux capabilities |
| `network` | Table | Network access requirements |
| `filesystem` | Table | Filesystem access requirements |

```ctp
[selinux]
domain = "nginx_t"
capabilities = ["net_bind_service", "setuid", "setgid"]

[selinux.network]
listen_tcp = [80, 443]
connect_tcp = ["upstream_t"]

[selinux.filesystem]
read = ["/etc/nginx/", "/usr/share/nginx/"]
write = ["/var/log/nginx/", "/var/cache/nginx/"]
execute = ["/usr/sbin/nginx"]
```

#### `[variants]` — Optional

Defines build variants (similar to Gentoo USE flags but declarative).

```ctp
[variants]
ssl = { default = true, description = "Enable TLS support" }
http2 = { default = true, description = "Enable HTTP/2" }
geoip = { default = false, description = "Enable GeoIP lookups" }

[variants.ssl.dependencies]
runtime = ["openssl>=3.0"]
build = ["libssl-dev"]

[variants.ssl.configure_flags]
add = ["--with-http_ssl_module"]

[variants.geoip.dependencies]
runtime = ["libmaxminddb"]

[variants.geoip.configure_flags]
add = ["--with-http_geoip_module"]
```

## Build Phases

For `system = "custom"`, build phases are defined declaratively, not as shell scripts.

Each phase is a sequence of **actions** from a fixed vocabulary:

| Action | Arguments | Description |
|--------|-----------|-------------|
| `run` | command, args | Execute a command |
| `copy` | from, to | Copy file or directory |
| `move` | from, to | Move file or directory |
| `delete` | path | Remove file or directory |
| `mkdir` | path, mode | Create directory |
| `chmod` | path, mode | Change permissions |
| `symlink` | target, link | Create symbolic link |
| `patch` | file, strip | Apply patch file |
| `substitute` | file, pattern, replacement | Text substitution |

```ctp
[build]
system = "custom"

[build.phases.configure]
actions = [
    { run = "sh", args = ["./configure", "--prefix=/usr"] },
]

[build.phases.build]
actions = [
    { run = "make", args = ["-j", "${JOBS}"] },
]

[build.phases.install]
actions = [
    { run = "make", args = ["install", "DESTDIR=${OUT}"] },
    { delete = "${OUT}/usr/share/info/dir" },
]
```

**Variables** available in actions (substituted, not evaluated):
- `${SRC}` — Source directory
- `${OUT}` — Output/staging directory  
- `${JOBS}` — Parallelism level
- `${VARIANT_*}` — Variant flags

This is intentionally limited. Complex build logic belongs in upstream build systems or in the importer, not in manifests.

## Version Ordering

Version comparison follows Debian's algorithm:
1. Epoch (integer comparison)
2. Upstream version (mixed alphanumeric comparison)
3. Revision (integer comparison)

The full version string is: `[epoch:]version-revision`

Examples:
- `1.0-1` < `1.0-2` < `1.1-1`
- `1:1.0-1` > `2.0-1` (epoch 1 beats epoch 0)
- `1.0~beta1-1` < `1.0-1` (tilde sorts before empty)

## Import Mapping

### From Debian (.dsc)

| Debian | CTP |
|--------|-----|
| `Source` | `metadata.name` |
| `Version` | `metadata.version`, `metadata.revision` |
| `Build-Depends` | `dependencies.build` |
| `Depends` | `dependencies.runtime` |
| `debian/rules` | `build.phases` (analysed) |
| `debian/patches/*` | `provenance.patches` |

### From Fedora (SRPM)

| Fedora | CTP |
|--------|-----|
| `Name` | `metadata.name` |
| `Version`, `Release` | `metadata.version`, `metadata.revision` |
| `BuildRequires` | `dependencies.build` |
| `Requires` | `dependencies.runtime` |
| `%build` | `build.phases` (analysed) |
| `*.patch` | `provenance.patches` |

### From Alpine (APKBUILD)

| Alpine | CTP |
|--------|-----|
| `pkgname` | `metadata.name` |
| `pkgver` | `metadata.version` |
| `pkgrel` | `metadata.revision` |
| `makedepends` | `dependencies.build` |
| `depends` | `dependencies.runtime` |
| `build()` | `build.phases` (analysed) |

## Formal Properties

The following properties are guaranteed by the format design and can be mechanically verified:

1. **Termination**: Manifest parsing always terminates (no recursion, no loops)
2. **Determinism**: Same manifest text produces same parsed structure
3. **Completeness**: All referenced files must be declared in provenance
4. **Hash Integrity**: All external resources have cryptographic hashes
5. **Acyclicity**: Dependency graph must be acyclic (statically checkable)

The SPARK-verified parser will prove these properties at compile time.

## Example: Complete Manifest

```ctp
# Cerro Torre Package Manifest
# nginx/1.26.0-1

[metadata]
name = "nginx"
version = "1.26.0"
revision = 1
summary = "High-performance HTTP server and reverse proxy"
description = """
nginx [engine x] is an HTTP and reverse proxy server, a mail proxy server,
and a generic TCP/UDP proxy server, originally written by Igor Sysoev.
"""
license = "BSD-2-Clause"
homepage = "https://nginx.org"
maintainer = "cerro-torre:web-team"

[provenance]
upstream = "https://nginx.org/download/nginx-1.26.0.tar.gz"
upstream_hash = "sha256:1234567890abcdef..."
upstream_signature = "https://nginx.org/download/nginx-1.26.0.tar.gz.asc"
upstream_keyring = "cerro-torre:nginx-signing-keys"
imported_from = "debian:nginx/1.26.0-1"
import_date = 2024-12-07T10:30:00Z
patches = [
    "patches/01-fix-systemd-socket.patch",
]

[dependencies]
runtime = ["libc>=2.38", "openssl>=3.0", "pcre2", "zlib"]
build = ["gcc", "make", "libssl-dev", "libpcre2-dev", "zlib-dev"]
check = ["perl", "test-nginx"]
optional = ["geoip2"]
provides = ["httpd"]

[build]
system = "autoconf"
configure_flags = [
    "--prefix=/usr",
    "--sbin-path=/usr/sbin/nginx",
    "--conf-path=/etc/nginx/nginx.conf",
    "--error-log-path=/var/log/nginx/error.log",
    "--http-log-path=/var/log/nginx/access.log",
    "--pid-path=/run/nginx.pid",
    "--lock-path=/run/nginx.lock",
    "--with-http_ssl_module",
    "--with-http_v2_module",
    "--with-http_realip_module",
    "--with-http_gzip_static_module",
    "--with-pcre-jit",
    "--with-threads",
]

[build.environment]
CFLAGS = "-O2 -fstack-protector-strong -Wformat -Werror=format-security"
LDFLAGS = "-Wl,-z,relro,-z,now"

[outputs]
primary = "nginx"
split = ["nginx-doc", "nginx-dbgsym"]

[attestations]
require = ["source-signature", "reproducible-build", "sbom-complete"]
recommend = ["security-audit"]

[selinux]
domain = "nginx_t"
capabilities = ["net_bind_service", "setuid", "setgid"]

[selinux.network]
listen_tcp = [80, 443]

[selinux.filesystem]
read = ["/etc/nginx/", "/usr/share/nginx/", "/var/www/"]
write = ["/var/log/nginx/", "/var/cache/nginx/", "/run/nginx.pid"]
execute = ["/usr/sbin/nginx"]

[variants]
geoip = { default = false, description = "Enable GeoIP lookups" }

[variants.geoip.dependencies]
runtime = ["libmaxminddb"]
build = ["libmaxminddb-dev"]

[variants.geoip.configure_flags]
add = ["--with-http_geoip_module=dynamic"]
```

## Extended Sections for Multi-Source Images

The following sections extend the basic manifest format for container images and other multi-source builds.

#### `[upstream]` — Optional

For multi-source manifests (container images), specifies the upstream distribution context.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `family` | String | Yes | Distribution family: `debian`, `fedora`, `alpine` |
| `suite` | String | Yes | Release codename: `bookworm`, `f39`, `edge` |
| `snapshot_service` | String | No | Snapshot service for reproducibility |
| `snapshot_timestamp` | DateTime | No | Pinned timestamp |

```ctp
[upstream]
family = "debian"
suite = "bookworm"
snapshot_service = "snapshot.debian.org"
snapshot_timestamp = 2025-12-20T00:00:00Z
```

#### `[[inputs.sources]]` — Optional

For multi-source builds, declares all source packages with artifacts.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `id` | String | Yes | Unique identifier for this source |
| `type` | String | Yes | Source type: `debian_dsc`, `fedora_srpm`, `tarball` |
| `name` | String | Yes | Package name |
| `version` | String | Yes | Package version |

Each source has nested `[[inputs.sources.artifacts]]`:

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `filename` | String | Yes | Artifact filename |
| `uri` | String | Yes | Download URL |
| `sha256` | Hash | Yes | SHA-256 hash |

```ctp
[[inputs.sources]]
id = "debian_source_bash"
type = "debian_dsc"
name = "bash"
version = "5.2.15-2+b7"

[[inputs.sources.artifacts]]
filename = "bash_5.2.15-2+b7.dsc"
uri = "https://snapshot.debian.org/.../bash_5.2.15-2+b7.dsc"
sha256 = "abc123..."
```

#### `[[build.plan]]` — Optional

Declarative build plan for multi-step builds. Alternative to `[build.phases]` for complex builds.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `step` | String | Yes | Step type (fixed vocabulary) |
| `using` | String | No | Build tool/method |
| `source` | String | No | Source ID reference |
| `sources` | List[String] | No | Multiple source IDs |
| `profile` | String | No | Build profile |

**Step Types**: `import`, `build_debian_source`, `build_fedora_source`, `assemble_rootfs`, `emit_oci_image`

```ctp
[[build.plan]]
step = "import"
using = "debian"
sources = ["debian_source_bash", "debian_source_coreutils"]

[[build.plan]]
step = "build_debian_source"
source = "debian_source_bash"
profile = "debian_rules"

[[build.plan]]
step = "emit_oci_image"

[build.plan.image]
entrypoint = ["/bin/bash"]
```

#### `[policy]` — Optional

Build policy requirements.

```ctp
[policy.provenance]
require_source_hashes = true
require_reproducible_build = true

[policy.attestations]
emit = ["in_toto", "sbom_spdx_json"]
```

## Grammar (EBNF)

```ebnf
manifest     = { section } ;
section      = "[" section_name "]" newline { assignment } ;
section_name = identifier { "." identifier } ;
assignment   = key "=" value newline ;
key          = identifier ;
value        = string | integer | boolean | datetime | date | hash | list | table ;

string       = '"' { char } '"' ;
integer      = [ "-" ] digit { digit } ;
boolean      = "true" | "false" ;
datetime     = date "T" time "Z" ;
date         = year "-" month "-" day ;
hash         = algorithm ":" hexstring ;
list         = "[" [ value { "," value } [ "," ] ] "]" ;
table        = "{" [ pair { "," pair } [ "," ] ] "}" ;
pair         = key "=" value ;

identifier   = letter { letter | digit | "_" | "-" } ;
algorithm    = "sha256" | "sha384" | "sha512" | "blake3" ;
```

## Changelog

### 0.1.1-draft (2025-12-28)
- Added `[upstream]` section for multi-source manifests
- Added `[[inputs.sources]]` for declaring multiple sources with artifacts
- Added `[[build.plan]]` for declarative multi-step builds
- Added `[policy]` section for provenance and attestation requirements
- Added `ctp_version` top-level field
- See `manifests/examples/ct-minbase.ctp` for canonical example

### 0.1.0-draft (2024-12-07)
- Initial draft specification
