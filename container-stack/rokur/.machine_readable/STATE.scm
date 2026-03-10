;; SPDX-License-Identifier: PMPL-1.0-or-later
;; STATE.scm — Current state, progress, and session tracking for Rokur
;; Format: hyperpolymath/state.scm specification

(define-module (rokur state)
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
    (created . "2026-03-10")
    (updated . "2026-03-10")
    (project . "rokur")
    (repo . "https://github.com/hyperpolymath/stapeln")
    (path . "container-stack/rokur")))

(define project-context
  '((name . "Rokur")
    (tagline . "Secrets management gate for container-start authorization")
    (tech-stack . ((deno . "HTTP runtime")
                   (javascript . "Application logic")))
    (phase . "infrastructure-buildout")))

(define current-position
  '((phase . "v0.1.0 — Code Complete, Infrastructure In Progress")
    (overall-completion . 72)

    (components
      ((name . "HTTP Server & Routing")
       (completion . 100)
       (status . "complete")
       (notes . "Deno.serve with health, metrics, authorize-start, secrets/status, reload"))

      ((name . "Policy Engine")
       (completion . 100)
       (status . "complete")
       (notes . "Pluggable evaluator: built-in env check + external command backend"))

      ((name . "Rate Limiter")
       (completion . 100)
       (status . "complete")
       (notes . "Per-IP sliding window with auth-failure escalation"))

      ((name . "Audit Logging")
       (completion . 100)
       (status . "complete")
       (notes . "Structured JSON audit log with async file writes"))

      ((name . "Token Authentication")
       (completion . 100)
       (status . "complete")
       (notes . "X-Rokur-Token header, fail-closed in production"))

      ((name . "Graceful Shutdown")
       (completion . 100)
       (status . "complete")
       (notes . "SIGTERM/SIGINT shutdown, SIGHUP secret reload"))

      ((name . "Test Suite")
       (completion . 100)
       (status . "complete")
       (notes . "50 tests: engine, rate limiting, audit, integration"))

      ((name . "Infrastructure")
       (completion . 15)
       (status . "in-progress")
       (notes . "Containerfile created, CI/CD and deployment pending")))

    (working-features
      "GET /health — service health and configuration summary"
      "GET /v1/secrets/status — authenticated secret presence check"
      "POST /v1/authorize-start — policy-based container start authorization"
      "GET /metrics — operational metrics (counters, uptime, rate limiter stats)"
      "POST /v1/secrets/reload — hot-reload required secrets from environment"
      "Per-IP rate limiting with auth-failure escalation"
      "Structured JSON audit log for all decisions"
      "SIGHUP-triggered secret reload without restart"
      "50 passing tests across 4 test files")

    (broken-features)))

(define route-to-mvp
  '((milestone-1
     (name . "Core Gate Logic")
     (target . "v0.1.0")
     (status . "complete")
     (items
       ((item . "HTTP server with routing") (done . #t))
       ((item . "Policy engine (built-in + external)") (done . #t))
       ((item . "Token authentication") (done . #t))
       ((item . "Rate limiting") (done . #t))
       ((item . "Audit logging") (done . #t))
       ((item . "Graceful shutdown") (done . #t))
       ((item . "Test suite") (done . #t))))

    (milestone-2
     (name . "Container & CI")
     (target . "v0.2.0")
     (status . "in-progress")
     (items
       ((item . "Containerfile (multi-stage)") (done . #t))
       ((item . "LICENSE and community files") (done . #t))
       ((item . ".editorconfig and justfile") (done . #t))
       ((item . "CI/CD pipeline") (done . #f))
       ((item . "Container image publishing") (done . #f))))

    (milestone-3
     (name . "Production Hardening")
     (target . "v0.3.0")
     (status . "pending")
     (items
       ((item . "TLS termination") (done . #f))
       ((item . "OpenTelemetry tracing") (done . #f))
       ((item . "Prometheus metrics export") (done . #f))
       ((item . "Secret rotation support") (done . #f))))))

(define blockers-and-issues
  '((critical . ())
    (high . ())
    (medium
      ((id . "ROKUR-001")
       (description . "CI/CD pipeline not yet configured")
       (type . "infrastructure")
       (notes . "Need GitHub Actions workflow for test + container build")))
    (low . ())))

(define critical-next-actions
  '((immediate
      "Configure CI/CD workflow (GitHub Actions)"
      "Test Containerfile build with Podman"
      "Publish container image to registry")

    (this-week
      "Integrate with Svalinn for end-to-end authorization flow"
      "Add OpenTelemetry tracing spans"
      "Document deployment configuration")

    (this-month
      "TLS termination support"
      "Prometheus metrics endpoint"
      "Secret rotation without restart")))

(define session-history
  '((session-001
     (date . "2026-03-10")
     (duration . "3 hours")
     (accomplishments
       "Wrote main.js HTTP server with 5 endpoints"
       "Implemented policy engine with built-in and external backends"
       "Created rate limiter with per-IP sliding window"
       "Created audit logging module"
       "Wrote 50 tests across 4 test files"
       "All tests passing")
     (next-session
       "Create Containerfile and infrastructure files"
       "Set up CI/CD"))
    (session-002
     (date . "2026-03-10")
     (duration . "30 minutes")
     (accomplishments
       "Created Containerfile (multi-stage, Chainguard Wolfi)"
       "Created LICENSE, SECURITY.md, CONTRIBUTING.md"
       "Created .editorconfig, justfile"
       "Created .machine_readable/ SCM files"
       "Created 0-AI-MANIFEST.a2ml")
     (next-session
       "CI/CD pipeline"
       "Container image build + publish"
       "Svalinn integration testing"))))

;; Helper functions
(define (get-completion-percentage)
  (assoc-ref current-position 'overall-completion))

(define (get-blockers priority)
  (assoc-ref blockers-and-issues priority))

(define (get-milestone name)
  (let ((milestones (list (assoc-ref route-to-mvp 'milestone-1)
                          (assoc-ref route-to-mvp 'milestone-2)
                          (assoc-ref route-to-mvp 'milestone-3))))
    (find (lambda (m) (string=? (assoc-ref m 'name) name)) milestones)))
