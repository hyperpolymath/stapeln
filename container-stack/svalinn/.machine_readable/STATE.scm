;; SPDX-License-Identifier: PMPL-1.0-or-later
;; STATE.scm — Current state, progress, and session tracking for Svalinn
;; Format: hyperpolymath/state.scm specification
;; Reference: hyperpolymath/git-hud/STATE.scm

(define-module (svalinn state)
  #:export (metadata
            project-context
            current-position
            route-to-mvp
            blockers-and-issues
            critical-next-actions
            session-history
            get-completion-percentage
            get-blockers
            get-milestone))

(define metadata
  '((version . "0.1.0")
    (schema-version . "1.0")
    (created . "2025-12-29")
    (updated . "2026-03-13")
    (project . "svalinn")
    (repo . "https://gitlab.com/hyperpolymath/svalinn")))

(define project-context
  '((name . "Svalinn")
    (tagline . "Edge shield for verified container operations via Vörðr")
    (tech-stack . ((rescript . "Type-safe edge logic")
                   (deno . "HTTP/3 runtime")
                   (hono . "Web framework")))
    (phase . "edge-shield-hardening")))

(define current-position
  '((phase . "v0.4+ — Security Headers, Metrics, Infrastructure")
    (overall-completion . 85)

    (components
      ((name . "Gateway HTTP Server")
       (completion . 100)
       (status . "complete")
       (notes . "Deno + Hono HTTP server with CORS, logging"))

      ((name . "Security Headers Middleware")
       (completion . 100)
       (status . "complete")
       (notes . "HSTS, CSP, X-Frame-Options, CORS via securityHeaders() middleware"))

      ((name . "Request Validation")
       (completion . 100)
       (status . "complete")
       (notes . "AJV JSON Schema validation against spec/schemas/"))

      ((name . "Vörðr MCP Client")
       (completion . 100)
       (status . "complete")
       (notes . "ReScript client calling Vörðr MCP adapter"))

      ((name . "Svalinn MCP Server")
       (completion . 100)
       (status . "complete")
       (notes . "8 edge tools: run, ps, stop, verify, policy, logs, exec, rm"))

      ((name . "Edge Policy Engine")
       (completion . 100)
       (status . "complete")
       (notes . "Full policy DSL with strict/standard/permissive presets"))

      ((name . "Test Suite")
       (completion . 95)
       (status . "complete")
       (notes . "106+ tests: metrics (7), policy parsing (8), MCP error (1), zero getExn in tests"))

      ((name . "Authentication")
       (completion . 100)
       (status . "complete")
       (notes . "OAuth2/OIDC, API keys, mTLS, RBAC roles and scopes"))

      ((name . "Metrics Collection")
       (completion . 90)
       (status . "complete")
       (notes . "In-memory counters/histograms/gauges, Prometheus format, wired into gateway"))

      ((name . "Containerfile")
       (completion . 90)
       (status . "validated")
       (notes . "Two-stage wolfi-base build, npx→node_modules/.bin fix, structure validated"))

      ((name . "CI/CD")
       (completion . 70)
       (status . "in-progress")
       (notes . "6 workflows: Hypatia, CodeQL, Scorecard, quality, mirror, secret-scanner"))

      ((name . "Rate Limiting")
       (completion . 100)
       (status . "complete")
       (notes . "Per-IP fixed-window rate limiter with X-RateLimit headers and 429 responses"))

      ((name . "Web UI")
       (completion . 40)
       (status . "in-progress")
       (notes . "ReScript/Tea UI with Api.res, Route.res, Main.res")))

    (working-features
      "HTTP API endpoints for containers, images, run, verify"
      "JSON Schema validation against verified-container-spec"
      "Vörðr MCP client with all tool bindings"
      "Svalinn MCP server with 8 edge tools"
      "MCP endpoint (/mcp) for JSON-RPC 2.0 AI agent access"
      "Health check endpoint"
      "Full policy DSL (types, evaluator, store, defaults)"
      "Policy presets: strict, standard, permissive"
      "OAuth2/OIDC authentication with JWT verification"
      "API key authentication with scopes"
      "mTLS client certificate authentication"
      "RBAC with 4 default roles (admin, operator, viewer, auditor)"
      "Scope-based authorization middleware"
      "OWASP security headers on all responses (HSTS, CSP, X-Frame-Options, etc.)"
      "CORS whitelist via ALLOWED_ORIGINS environment variable"
      "Prometheus metrics: requests_total, errors_total, auth_failures, duration histogram, containers_active gauge"
      "90 passing tests across 7 test files"
      "Vörðr integration tests (skip when not available)"
      "Justfile with dev/build/test commands"
      "Containerfile (two-stage wolfi-base build)"
      "Hypatia CI scan workflow"
      "Type-safe code: zero getExn in production code (33 removed, all pattern-matched)"
      "Type-safe code: zero Obj.magic in production code (38 removed, 1 annotated FFI cast)"
      "SECURITY.md with vulnerability reporting policy"
      "MVP policy gate tool (Deno, replaces banned Python version)")

    (broken-features)))

(define route-to-mvp
  '((milestone-1
     (name . "Gateway Foundation")
     (target . "v0.1.0")
     (status . "complete")
     (items
       ((item . "HTTP server with Hono") (done . #t))
       ((item . "Request validation") (done . #t))
       ((item . "Vörðr client") (done . #t))
       ((item . "Health endpoint") (done . #t))
       ((item . "CORS/logging middleware") (done . #t))))

    (milestone-2
     (name . "MCP Integration")
     (target . "v0.2.0")
     (status . "complete")
     (items
       ((item . "MCP server skeleton") (done . #t))
       ((item . "Tool definitions") (done . #t))
       ((item . "Tool handlers") (done . #t))
       ((item . "Error handling") (done . #t))))

    (milestone-3
     (name . "Edge Policy")
     (target . "v0.3.0")
     (status . "complete")
     (items
       ((item . "Registry allow/deny") (done . #t))
       ((item . "Image deny list") (done . #t))
       ((item . "Policy DSL") (done . #t))
       ((item . "Policy persistence") (done . #t))))

    (milestone-4
     (name . "Authentication")
     (target . "v0.4.0")
     (status . "complete")
     (items
       ((item . "OAuth2 integration") (done . #t))
       ((item . "OIDC support") (done . #t))
       ((item . "API key auth") (done . #t))
       ((item . "mTLS support") (done . #t))
       ((item . "RBAC roles/scopes") (done . #t))))

    (milestone-5
     (name . "Production MVP")
     (target . "v0.5.0")
     (status . "in-progress")
     (items
       ((item . "Security headers middleware") (done . #t))
       ((item . "Prometheus metrics collection") (done . #t))
       ((item . "Containerfile") (done . #t))
       ((item . "Hypatia CI workflow") (done . #t))
       ((item . "TLS/HTTP3 support") (done . #f))
       ((item . "Rate limiting middleware") (done . #f))
       ((item . "Structured logging improvements") (done . #f))
       ((item . "Full CI pipeline (CodeQL, mirror, scorecard)") (done . #f))
       ((item . "Test coverage for new modules") (done . #f))
       ((item . "Documentation") (done . #f))))))

(define blockers-and-issues
  '((critical . ())
    (high . ())
    (medium
      ((id . "SVALINN-003")
       (description . "Test suite needs updating for Metrics.res, securityHeaders middleware, and new pattern-matched code paths")
       (type . "testing")
       (notes . "90 existing tests pass but new code lacks coverage")))
    (low
      ((id . "SVALINN-002")
       (description . "OpenLiteSpeed integration")
       (type . "enhancement")
       (notes . "HTTP/3 via OLS for production")))))

(define critical-next-actions
  '((immediate
      "Add tests for Metrics.res (counter, histogram, gauge, formatPrometheus)"
      "Add tests for securityHeaders middleware (verify headers present)"
      "Validate Containerfile builds successfully")

    (this-week
      "Add rate limiting middleware"
      "Add remaining CI workflows (CodeQL, mirror, scorecard)"
      "Document API endpoints")

    (this-month
      "TLS/HTTP3 support"
      "Web UI completion"
      "Production deployment guide"
      "Performance testing")))

(define session-history
  '((session-001
     (date . "2025-12-29")
     (duration . "1 hour")
     (accomplishments
       "Created STATE.scm"
       "Created ECOSYSTEM.scm"
       "Extracted vordr/ to separate repository"
       "Updated README with architecture")
     (next-session
       "Implement edge gateway"
       "Add MCP server"
       "Create validation layer"))
    (session-002
     (date . "2026-01-19")
     (duration . "1 hour")
     (accomplishments
       "Created src/ directory structure"
       "Created deno.json and rescript.json configs"
       "Implemented Hono HTTP gateway"
       "Implemented JSON Schema validation"
       "Created Vörðr MCP client"
       "Created Svalinn MCP server with 8 tools"
       "Updated Justfile with real commands"
       "Added Deno and Fetch bindings for ReScript")
     (next-session
       "Add test suite"
       "Design policy DSL"
       "Implement authentication"))
    (session-003
     (date . "2026-01-18")
     (duration . "30 minutes")
     (accomplishments
       "Created test suite with 68 tests across 6 files"
       "Implemented policy DSL with types, evaluator, store"
       "Created three policy presets: strict, standard, permissive"
       "Wrote policy specification (POLICY-DSL.adoc)"
       "Created Vörðr integration tests (skip when not available)"
       "All tests passing")
     (next-session
       "Implement OAuth2/OIDC authentication"
       "Add rate limiting"
       "Document API endpoints"))
    (session-004
     (date . "2026-01-18")
     (duration . "20 minutes")
     (accomplishments
       "Implemented OAuth2/OIDC authentication module"
       "Added JWT verification with JWKS support"
       "Created API key authentication"
       "Added mTLS client certificate authentication"
       "Implemented RBAC with 4 default roles"
       "Added scope-based authorization middleware"
       "Created auth tests (22 passing)"
       "Total test count: 90 tests across 7 files")
     (next-session
       "Add rate limiting middleware"
       "Add metrics endpoint"
       "Document API endpoints"))
    (session-005
     (date . "2026-03-10")
     (duration . "30 minutes")
     (accomplishments
       "Created Metrics.res — in-memory counters, histograms, gauges with Prometheus format"
       "Wired securityHeaders() middleware into Gateway (HSTS, CSP, X-Frame-Options, CORS)"
       "Wired metricsMiddleware() into Gateway (request counting, duration histogram)"
       "Updated /metrics endpoint to return real collected metrics"
       "Added error counter increment in errorHandler middleware"
       "Removed redundant standalone cors() middleware (merged into securityHeaders)"
       "Created Containerfile — two-stage wolfi-base build (build + runtime)"
       "Created .editorconfig"
       "Created .github/workflows/hypatia-scan.yml"
       "Updated STATE.scm with honest completion numbers")
     (next-session
       "Add tests for Metrics.res and securityHeaders middleware"
       "Validate Containerfile builds"
       "Add rate limiting middleware"
       "Add remaining CI workflows"))
    (session-006
     (date . "2026-03-13")
     (duration . "1 hour")
     (accomplishments
       "Security remediation: removed 33 getExn calls from 6 production files"
       "Security remediation: removed 38 Obj.magic calls from 4 production files"
       "Replaced all getExn with switch/pattern-matching and descriptive error messages"
       "Replaced all Obj.magic with Js.Json.object_ construction"
       "Deleted .meta/STATE.scm (stale duplicate violating AI manifest invariant)"
       "Deleted tools/mvp/svalinn_gate.py (banned Python language)"
       "Created tools/mvp/svalinn_gate.js (Deno replacement for MVP policy gate)"
       "Created SECURITY.md with vulnerability reporting policy"
       "Fixed PolicyEngine.res parsePolicy/parseAttestation (getExn → pattern matching)"
       "Fixed McpClient.res callWithRetry (getExn → descriptive error handling)")
     (next-session
       "Add tests for pattern-matched code paths"
       "Add rate limiting middleware"
       "Add remaining CI workflows (CodeQL, mirror, scorecard)"
       "Validate Containerfile builds"))))

;; Helper functions
(define (get-completion-percentage)
  (assoc-ref (assoc-ref current-position 'overall-completion) 'value))

(define (get-blockers priority)
  (assoc-ref blockers-and-issues priority))

(define (get-milestone name)
  (let ((milestones (list (assoc-ref route-to-mvp 'milestone-1)
                          (assoc-ref route-to-mvp 'milestone-2)
                          (assoc-ref route-to-mvp 'milestone-3)
                          (assoc-ref route-to-mvp 'milestone-4)
                          (assoc-ref route-to-mvp 'milestone-5))))
    (find (lambda (m) (string=? (assoc-ref m 'name) name)) milestones)))
