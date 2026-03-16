;; SPDX-License-Identifier: PMPL-1.0-or-later
;; ECOSYSTEM.scm — Rokur's position in the hyperpolymath ecosystem
;; Format: hyperpolymath/ECOSYSTEM.scm specification

(ecosystem
  (version . "1.0.0")
  (schema-version . "1.0")

  (name . "rokur")
  (display-name . "Rokur")
  (pronunciation . "/roː.kʊr/")
  (etymology . "Old Norse: related to 'rökr' (twilight/secrets), guardian of hidden things")
  (ascii-safe . "rokur")

  (type . "secrets-gate")
  (purpose . "Secrets management gate that authorizes container starts by verifying
              required secrets are present and policy constraints are met")

  (language-identity
    (primary . ((javascript . "HTTP server and gate logic")
                (deno . "Runtime environment")))
    (paradigms . (imperative
                  event-driven
                  fail-closed)))

  (position-in-ecosystem
    (role . "secrets-gate")
    (layer . "infrastructure")
    (description . "Rokur is the secrets verification gate in the Stapeln container
                    stack. Before a container is authorized to start, Rokur checks
                    that all required secrets are present in the environment and that
                    the policy engine approves the start request. Rokur does NOT
                    store or distribute secrets — it only verifies their presence."))

  (related-projects
    ((project (name . "svalinn")
              (relationship . "consumer")
              (integration . "Edge shield — calls Rokur to verify secrets before container start")
              (url . "https://github.com/hyperpolymath/stapeln")
              (path . "container-stack/svalinn")))

    ((project (name . "vordr")
              (relationship . "sibling")
              (integration . "Container engine — Rokur gates what Vordr executes")
              (url . "https://github.com/hyperpolymath/stapeln")
              (path . "container-stack/vordr")))

    ((project (name . "cerro-torre")
              (relationship . "sibling")
              (integration . "Attestation and signing — provides provenance that Rokur can verify")
              (url . "https://github.com/hyperpolymath/stapeln")
              (path . "container-stack/cerro-torre")))

    ((project (name . "selur")
              (relationship . "sibling")
              (integration . "Container sealing — Rokur verifies secrets before Selur seals")
              (url . "https://github.com/hyperpolymath/stapeln")
              (path . "container-stack/selur")))

    ((project (name . "ephapax")
              (relationship . "potential-integration")
              (integration . "One-time policy engine — could provide ephemeral policy decisions")
              (url . "https://github.com/hyperpolymath/ephapax")))

    ((project (name . "verified-container-spec")
              (relationship . "protocol-spec")
              (integration . "Conformance target — Rokur implements the secrets gate portion")
              (url . "https://github.com/hyperpolymath/stapeln")
              (path . "verified-container-spec")))

    ((project (name . "rhodium-standard")
              (relationship . "sibling-standard")
              (integration . "Repository compliance standard"))))

  (what-this-is
    "Rokur is a secrets management gate that:"
    (items
      "Verifies required secrets are present before authorizing container starts"
      "Evaluates policy constraints via a pluggable engine (built-in or external)"
      "Provides per-IP rate limiting to prevent brute-force attacks"
      "Logs all authorization decisions to a structured audit trail"
      "Supports hot-reload of secret requirements via SIGHUP or API"))

  (what-this-is-not
    "Rokur is not:"
    (items
      "A secrets store (use HashiCorp Vault, Infisical, etc.)"
      "A secrets distributor (it only checks presence, not delivery)"
      "A container runtime (that's Vordr)"
      "An edge gateway (that's Svalinn)"
      "A build system (that's Cerro-Torre)"))

  (standards-compliance
    ((standard . "RSR")
     (status . "compliant"))
    ((standard . "verified-container-spec")
     (status . "conformant"))
    ((standard . "12-factor")
     (status . "conformant")
     (notes . "All configuration via environment variables"))))
