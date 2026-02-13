;; SPDX-License-Identifier: PMPL-1.0-or-later
;; ECOSYSTEM.scm — Svalinn's position in the hyperpolymath ecosystem
;; Format: hyperpolymath/ECOSYSTEM.scm specification

(ecosystem
  (version . "1.0.0")
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
    (description . "Svalinn is the edge-facing gateway that receives container
                    operation requests and delegates verification to Vörðr.
                    It does NOT contain container engine code — that lives in Vörðr."))

  (related-projects
    ((project (name . "vordr")
              (relationship . "core-dependency")
              (integration . "Container engine — Svalinn delegates all container ops to Vörðr")
              (url . "https://github.com/hyperpolymath/vordr")
              (notes . "Implementation code previously in svalinn/vordr/ now lives here")))

    ((project (name . "cerro-torre")
              (relationship . "sibling")
              (integration . "Build producer — creates verified images that Svalinn gates")
              (url . "https://github.com/hyperpolymath/cerro-torre")))

    ((project (name . "verified-container-spec")
              (relationship . "protocol-spec")
              (integration . "Conformance target — Svalinn validates against this spec")
              (url . "https://github.com/hyperpolymath/verified-container-spec")))

    ((project (name . "oblibeny")
              (relationship . "sibling")
              (integration . "Orchestration layer — coordinates with Svalinn for scaling")
              (url . "https://github.com/hyperpolymath/oblibeny")))

    ((project (name . "poly-ssg-mcp")
              (relationship . "hub")
              (integration . "MCP interface — Svalinn exposes edge tools via MCP")
              (url . "https://github.com/hyperpolymath/poly-ssg-mcp")))

    ((project (name . "rhodium-standard")
              (relationship . "sibling-standard")
              (integration . "Repository compliance standard")))

    ((project (name . "git-hud")
              (relationship . "infrastructure")
              (integration . "Repository management tooling"))))

  (what-this-is
    "Svalinn is an edge shield that:"
    (items
      "Receives container operation requests at the edge"
      "Validates requests against verified-container-spec"
      "Delegates container operations to Vörðr"
      "Provides HTTP/3 API via OpenLiteSpeed/Deno"
      "Integrates with MCP for AI-assisted operations"))

  (what-this-is-not
    "Svalinn is not:"
    (items
      "A container runtime (that's Vörðr)"
      "A container engine (that's Vörðr)"
      "A build system (that's Cerro Torre)"
      "A specification (that's verified-container-spec)"))

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
