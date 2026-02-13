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
    (updated . "2026-01-19")
    (project . "svalinn")
    (repo . "https://gitlab.com/hyperpolymath/svalinn")))

(define project-context
  '((name . "Svalinn")
    (tagline . "Edge shield for verified container operations via Vörðr")
    (tech-stack . ((rescript . "Type-safe edge logic")
                   (deno . "HTTP/3 runtime")
                   (hono . "Web framework")))
    (phase . "edge-shield-skeleton")))

(define current-position
  '((phase . "v0.4 — Authentication Complete")
    (overall-completion . 85)

    (components
      ((name . "Gateway HTTP Server")
       (completion . 100)
       (status . "complete")
       (notes . "Deno + Hono HTTP server with CORS, logging"))

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
       (completion . 100)
       (status . "complete")
       (notes . "90 tests: gateway, validation, policy, MCP, Vörðr, auth"))

      ((name . "Authentication")
       (completion . 100)
       (status . "complete")
       (notes . "OAuth2/OIDC, API keys, mTLS, RBAC roles and scopes"))

      ((name . "Web UI")
       (completion . 40)
       (status . "in-progress")
       (notes . "ReScript/Tea UI with Api.res, Route.res, Main.res")))

    (working-features
      "HTTP API endpoints for containers, images, run, verify"
      "JSON Schema validation against verified-container-spec"
      "Vörðr MCP client with all tool bindings"
      "Svalinn MCP server with 8 edge tools"
      "Health check endpoint"
      "Full policy DSL (types, evaluator, store, defaults)"
      "Policy presets: strict, standard, permissive"
      "OAuth2/OIDC authentication with JWT verification"
      "API key authentication with scopes"
      "mTLS client certificate authentication"
      "RBAC with 4 default roles (admin, operator, viewer, auditor)"
      "Scope-based authorization middleware"
      "90 passing tests across 7 test files"
      "Vörðr integration tests (skip when not available)"
      "Justfile with dev/build/test commands")

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
     (status . "pending")
     (items
       ((item . "TLS/HTTP3 support") (done . #f))
       ((item . "Metrics endpoint") (done . #f))
       ((item . "Structured logging") (done . #f))
       ((item . "Documentation") (done . #f))))))

(define blockers-and-issues
  '((critical . ())
    (high . ())
    (medium . ())
    (low
      ((id . "SVALINN-002")
       (description . "OpenLiteSpeed integration")
       (type . "enhancement")
       (notes . "HTTP/3 via OLS for production")))))

(define critical-next-actions
  '((immediate
      "Add rate limiting middleware"
      "Document API endpoints"
      "Add metrics endpoint")

    (this-week
      "Add OpenTelemetry tracing"
      "TLS/HTTP3 support"
      "Structured logging")

    (this-month
      "Production deployment guide"
      "Web UI completion"
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
       "Document API endpoints"))))

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
