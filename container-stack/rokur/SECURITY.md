<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->

# Security Policy — Rokur

## About Rokur

Rokur is a **security-critical gate component** in the Stapeln container stack.
It controls whether containers are authorized to start by verifying that all
required secrets are present and that policy constraints are satisfied. Rokur
operates on a **fail-closed** principle: if any check is ambiguous or fails, the
container start is denied.

Because Rokur sits on the critical path of container authorization, any
vulnerability in this component could allow unauthorized containers to start or
could leak secret metadata.

## Supported Versions

| Version | Supported |
| ------- | --------- |
| 0.1.x   | Yes       |

## Reporting a Vulnerability

If you discover a security vulnerability in Rokur, please report it
**privately** rather than opening a public issue.

**Contact:** [j.d.a.jewell@open.ac.uk](mailto:j.d.a.jewell@open.ac.uk)

Please include:

1. A description of the vulnerability and its potential impact.
2. Steps to reproduce (or a proof-of-concept).
3. The version of Rokur affected.
4. Any suggested mitigation or fix.

### Response Timeline

- **Acknowledgment:** within 48 hours.
- **Initial assessment:** within 7 days.
- **Fix or mitigation:** best effort within 30 days for critical issues.

## Security Design Principles

1. **Fail-closed** — any ambiguity or error results in denial.
2. **No secrets in responses** — Rokur never returns secret values; only
   presence/absence metadata.
3. **Per-IP rate limiting** — brute-force and enumeration attacks are throttled
   at the transport layer.
4. **Token authentication** — all mutating and status endpoints require a valid
   `X-Rokur-Token` header (configurable).
5. **Structured audit logging** — every authorization decision is logged with
   request ID, client IP, and outcome.
6. **Minimal permissions** — the Containerfile grants only `--allow-net`,
   `--allow-env`, `--allow-read`, and `--allow-write` (for audit logs).

## Scope

This policy covers the Rokur component at `stapeln/container-stack/rokur/`. For
vulnerabilities in other Stapeln components (Svalinn, Vordr, Cerro-Torre,
Selur), please refer to their respective SECURITY.md files or contact the same
email address above.
