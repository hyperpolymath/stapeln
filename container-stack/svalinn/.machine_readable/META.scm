;; SPDX-License-Identifier: PMPL-1.0-or-later
;; META.scm - Project metadata and architectural decisions for Svalinn
;; Media Type: application/meta+scheme

(define project-meta
  `((version . "1.0.0")

    (architecture-decisions
      ((id . "ADR-001")
       (title . "ReScript for type-safe gateway logic")
       (status . accepted)
       (date . "2025-01-10")
       (context . "Svalinn needs a language that compiles to JavaScript for
                   Deno runtime compatibility while providing strong static
                   type guarantees for security-critical request validation,
                   policy evaluation, and auth middleware.")
       (decision . "Use ReScript as the primary implementation language.
                    ReScript provides sound type inference, exhaustive pattern
                    matching, and zero-cost JS interop — all essential for an
                    edge gateway where type errors could open security holes.")
       (consequences . "Compile step required before running.  Team must know
                        ReScript.  Excellent JS output quality (readable, small).
                        Banned languages (TypeScript, Python, Go) are not used."))

      ((id . "ADR-002")
       (title . "Hono as HTTP framework")
       (status . accepted)
       (date . "2025-01-10")
       (context . "The gateway needs a lightweight HTTP framework that works
                   with Deno's native HTTP server and Web Standard APIs
                   (Request/Response).  Heavy frameworks (Express, Fastify)
                   carry Node.js baggage and unnecessary weight.")
       (decision . "Adopt Hono — a small, fast framework built on Web Standard
                    APIs.  It runs natively on Deno with zero adapter code and
                    provides middleware composition, routing, and context
                    management out of the box.")
       (consequences . "Minimal dependency surface.  Middleware is composable
                        and type-safe via ReScript bindings.  No Node.js
                        compatibility layer needed."))

      ((id . "ADR-003")
       (title . "MCP JSON-RPC 2.0 for Vordr delegation")
       (status . accepted)
       (date . "2025-01-12")
       (context . "Svalinn is an edge shield — it must NOT contain container
                   engine code.  All container operations (create, start, stop,
                   remove, exec) are delegated to Vordr.  The protocol between
                   them must be well-defined, retryable, and introspectable.")
       (decision . "Use Model Context Protocol (MCP) over JSON-RPC 2.0 for all
                    Svalinn-to-Vordr communication.  Each container operation
                    maps to a named MCP method (e.g. containers/create,
                    images/verify).  The client implements exponential backoff
                    retries and timeout via AbortSignal.")
       (consequences . "Clean separation of edge logic and runtime logic.
                        Structured error codes from Vordr.  Retry + backoff
                        built into McpClient.  MCP tooling can introspect the
                        gateway's capabilities."))

      ((id . "ADR-004")
       (title . "AJV for JSON Schema validation")
       (status . accepted)
       (date . "2025-01-14")
       (context . "Incoming HTTP requests must be validated against published
                   JSON Schemas from the verified-container-spec before being
                   forwarded to Vordr.  The validator must support JSON Schema
                   draft 2020-12, produce structured error reports, and be
                   usable from ReScript.")
       (decision . "Use AJV (Another JSON Validator) — the de-facto standard
                    JSON Schema validator for JavaScript.  Schemas are loaded
                    at startup from the spec directory and registered by id.
                    Validation results include per-field error objects with
                    dataPath, keyword, and message.")
       (consequences . "Rich error messages returned to API clients on 400.
                        Schemas are the single source of truth (shared with
                        verified-container-spec).  AJV is a runtime dependency."))

      ((id . "ADR-005")
       (title . "Web Crypto API for JWT verification")
       (status . accepted)
       (date . "2025-01-15")
       (context . "OIDC/OAuth2 bearer tokens must be verified before granting
                   access to protected endpoints.  Adding a large crypto
                   library (jose, jsonwebtoken) increases attack surface.
                   Deno ships the Web Crypto API natively.")
       (decision . "Use the Web Crypto API (SubtleCrypto) for JWT signature
                    verification.  JWKS keys are fetched from the OIDC
                    provider's jwksUri and imported as CryptoKey objects.
                    No external cryptography dependencies are introduced.")
       (consequences . "Zero additional crypto dependencies.  Limited to
                        algorithms supported by Web Crypto (RS256, ES256,
                        EdDSA).  JWKS key rotation handled via re-fetch.
                        Deno and all modern runtimes support this API.")))

    (development-practices
      ((code-style . "rescript")
       (security . "openssf-scorecard")
       (testing . "property-based")
       (versioning . "semver")
       (documentation . "asciidoc")
       (branching . "trunk-based")))

    (design-rationale
      ((principle . "edge-shield-pattern")
       (description . "Svalinn validates, authenticates, and policy-checks
                       requests at the edge before delegating to Vordr.  It
                       never touches container internals directly."))
      ((principle . "defence-in-depth")
       (description . "Multiple layers: schema validation, policy engine
                       (strict/permissive), RBAC scopes, security headers,
                       Rokur secrets gate — each independent.")))))
