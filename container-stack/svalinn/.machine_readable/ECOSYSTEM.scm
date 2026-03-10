;; SPDX-License-Identifier: PMPL-1.0-or-later
;; ECOSYSTEM.scm — Svalinn's position in the hyperpolymath ecosystem
;; Format: hyperpolymath/ECOSYSTEM.scm specification
;; Media Type: application/vnd.ecosystem+scm

(ecosystem
  (version . "1.1.0")
  (schema-version . "1.0")

  (name . "svalinn")
  (display-name . "Svalinn")
  (pronunciation . "/svɑːlɪn/")
  (etymology . "Old Norse: shield of the sun, protective barrier")
  (ascii-safe . "svalinn")

  (type . "edge-shield")
  (purpose . "ReScript/Deno edge layer guarding container operations via Vörðr")

  (language-identity
    (primary . ((rescript . "Edge shield logic and type-safe JS")
                (deno . "Runtime and HTTP/3 serving")))
    (paradigms . (functional
                  type-safe
                  edge-computing)))

  (position-in-ecosystem
    (role . "edge-gateway")
    (layer . "application")
    (parent . "stapeln")
    (description . "Svalinn is the edge-facing gateway within the Stapeln
                    container stack.  It receives container operation requests,
                    validates them against JSON Schemas, evaluates gatekeeper
                    policies, enforces RBAC, and delegates all runtime work to
                    Vörðr over MCP JSON-RPC 2.0.  It does NOT contain
                    container engine code — that lives in Vörðr."))

  (related-projects
    ;; Parent stack
    ((project (name . "stapeln")
              (relationship . "parent")
              (integration . "Svalinn is a component of the Stapeln container stack")
              (url . "https://github.com/hyperpolymath/stapeln")
              (notes . "Stapeln is the container ecosystem; Svalinn is its HTTP gateway")))

    ;; Core delegation target
    ((project (name . "vordr")
              (relationship . "core-dependency")
              (integration . "Container engine — Svalinn delegates all container ops to Vörðr
                             via MCP JSON-RPC 2.0 (containers/create, containers/start,
                             images/verify, etc.)")
              (url . "https://github.com/hyperpolymath/vordr")
              (notes . "Implementation code previously in svalinn/vordr/ now lives here")))

    ;; Secrets gate
    ((project (name . "rokur")
              (relationship . "sibling")
              (integration . "Secrets gate — Svalinn calls Rokur's /v1/authorize-start
                             endpoint before allowing container starts.  Rokur can deny
                             starts based on secret policy (e.g. missing sealed secrets).")
              (url . "https://github.com/hyperpolymath/rokur")
              (notes . "Gated via ROKUR_GATE_ENABLED env var; retries with backoff")))

    ;; Image verification
    ((project (name . "cerro-torre")
              (relationship . "sibling")
              (integration . "Build producer — creates verified .ctp bundles and signed
                             images that Svalinn gates via policy engine attestation checks")
              (url . "https://github.com/hyperpolymath/cerro-torre")))

    ;; Zero-copy IPC (future)
    ((project (name . "selur")
              (relationship . "future-integration")
              (integration . "Zero-copy IPC bridge — will replace HTTP fetch calls to Vörðr
                             with shared-memory transport for same-host deployments")
              (url . "https://github.com/hyperpolymath/selur")
              (notes . "Not yet integrated; Svalinn currently uses HTTP to Vörðr")))

    ;; Orchestration consumer
    ((project (name . "selur-compose")
              (relationship . "consumer")
              (integration . "Orchestration layer — selur-compose drives multi-container
                             deployments by calling Svalinn's /api/v1/run and
                             /api/v1/containers endpoints")
              (url . "https://github.com/hyperpolymath/selur-compose")
              (notes . "Consumes Svalinn's HTTP API as its gateway")))

    ;; Spec conformance
    ((project (name . "verified-container-spec")
              (relationship . "protocol-spec")
              (integration . "Conformance target — Svalinn validates against this spec's
                             JSON Schemas (gateway-run-request, gatekeeper-policy, etc.)")
              (url . "https://github.com/hyperpolymath/verified-container-spec")))

    ;; Orchestration (legacy name)
    ((project (name . "oblibeny")
              (relationship . "sibling")
              (integration . "Orchestration layer — coordinates with Svalinn for scaling")
              (url . "https://github.com/hyperpolymath/oblibeny")))

    ;; MCP hub
    ((project (name . "poly-ssg-mcp")
              (relationship . "hub")
              (integration . "MCP interface — Svalinn exposes edge tools via MCP")
              (url . "https://github.com/hyperpolymath/poly-ssg-mcp")))

    ;; Standards
    ((project (name . "rhodium-standard")
              (relationship . "sibling-standard")
              (integration . "Repository compliance standard")))

    ;; Infrastructure
    ((project (name . "git-hud")
              (relationship . "infrastructure")
              (integration . "Repository management tooling"))))

  (what-this-is
    "Svalinn is an edge shield that:"
    (items
      "Receives container operation requests at the edge"
      "Validates requests against verified-container-spec JSON Schemas (AJV)"
      "Evaluates gatekeeper policies (strict/permissive) with attestation checks"
      "Enforces RBAC with scoped permissions (admin/operator/viewer/auditor)"
      "Delegates container operations to Vörðr via MCP JSON-RPC 2.0"
      "Gates container starts through Rokur secrets authorization"
      "Applies OWASP security headers (HSTS, CSP, X-Frame-Options, etc.)"
      "Verifies JWT tokens using Web Crypto API (no external crypto deps)"
      "Provides HTTP API via Hono on Deno runtime"))

  (what-this-is-not
    "Svalinn is not:"
    (items
      "A container runtime (that's Vörðr)"
      "A container engine (that's Vörðr)"
      "A build system (that's Cerro Torre)"
      "A secrets manager (that's Rokur)"
      "A specification (that's verified-container-spec)"
      "An orchestrator (that's selur-compose / oblibeny)"))

  (extraction-history
    ((date . "2025-01-15")
     (action . "Extracted vordr/ directory to hyperpolymath/vordr")
     (reason . "Separation of concerns — edge shield vs container engine")
     (reference . "REFERENCE.adoc")))

  (standards-compliance
    ((standard . "RSR")
     (status . "compliant"))
    ((standard . "verified-container-spec")
     (status . "conformant"))))
