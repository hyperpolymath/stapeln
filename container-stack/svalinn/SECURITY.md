<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->

# Security Policy

## Supported Versions

| Version | Supported |
|---------|-----------|
| 0.1.x | Yes |

## Reporting a Vulnerability

If you discover a security vulnerability in Svalinn, please report it responsibly.

**Do NOT open a public issue.**

Instead, please email: **j.d.a.jewell@open.ac.uk**

Include:
- Description of the vulnerability
- Steps to reproduce
- Impact assessment
- Suggested fix (if any)

You should receive an acknowledgement within 48 hours. We will work with you to understand and address the issue before any public disclosure.

## Security Measures

Svalinn implements defence-in-depth security:

- **Edge validation**: All requests validated against verified-container-spec JSON schemas
- **Authentication**: JWT/OIDC, API key, and mTLS client certificate authentication
- **Authorization**: Role-based access control (RBAC) with admin/operator/viewer/auditor roles
- **OWASP headers**: HSTS, CSP, X-Frame-Options, X-Content-Type-Options on all responses
- **CORS**: Strict origin allowlist (no wildcard)
- **Policy engine**: Configurable strict/standard/permissive security policies
- **Secrets gate**: Rokur integration for container start authorization
- **Web Crypto API**: No external cryptography dependencies (zero attack surface)

## Scope

This policy applies to the Svalinn Edge Gateway and its direct dependencies. For vulnerabilities in Vörðr (container engine) or other Stapeln components, please report to the respective repositories.
