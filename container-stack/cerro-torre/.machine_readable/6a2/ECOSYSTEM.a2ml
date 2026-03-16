;; SPDX-License-Identifier: PMPL-1.0-or-later
;; ECOSYSTEM.scm - Ecosystem position for cerro-torre
;; Media-Type: application/vnd.ecosystem+scm

(ecosystem
  (version "1.0")
  (name "cerro-torre")
  (type "supply-chain-verification-tool")
  (purpose "Provenance-verified container distribution with formally verified tooling")

  (position-in-ecosystem
    (category "container-stack")
    (subcategory "supply-chain-integrity")
    (unique-value
      ("Ada/SPARK formal verification of crypto and parsing paths"
       "Democratic cooperative governance model"
       "Format-agnostic package imports (Debian, Fedora, Alpine, Nix)"
       "Complete cryptographic provenance chain for every package")))

  (related-projects
    ((name . "vordr")
     (relationship . sibling-standard)
     (role . "Runtime consumer of cerro-torre verified containers")
     (integration . "vordr pulls OCI images that cerro-torre has signed and attested"))

    ((name . "svalinn")
     (relationship . sibling-standard)
     (role . "Gateway and policy enforcement for container registries")
     (integration . "svalinn validates cerro-torre attestations before admitting images"))

    ((name . "selur")
     (relationship . sibling-standard)
     (role . "Seal operations -- tamper-evident packaging")
     (integration . "selur seal wraps cerro-torre .ctp bundles with integrity seals"))

    ((name . "rokur")
     (relationship . sibling-standard)
     (role . "Secrets management for signing keys and credentials")
     (integration . "rokur provides Ed25519 private keys to ct-sign at signing time"))

    ((name . "proven")
     (relationship . potential-consumer)
     (role . "Formal verification library for Idris2/Ada")
     (integration . "proven supplies formally verified parsing primitives; currently disabled due to compilation errors, targeted for v0.3"))

    ((name . "stapeln")
     (relationship . sibling-standard)
     (role . "Parent container orchestration framework")
     (integration . "cerro-torre is a component of the stapeln container stack")))

  (what-this-is
    ("A CLI tool (ct) for packing, signing, verifying, and distributing containers"
     "A supply-chain verification layer ensuring every package has cryptographic provenance"
     "An OCI-compatible registry client for pushing and pulling verified images"
     "A transparency log client for recording signing events in Rekor"))

  (what-this-is-not
    ("Not a container runtime (use vordr or podman for that)"
     "Not a package manager (it verifies and distributes, not resolves dependencies)"
     "Not a replacement for Sigstore (it uses Sigstore/Rekor as a backend)")))
