;; SPDX-License-Identifier: PMPL-1.0-or-later
;; ECOSYSTEM.scm - Ecosystem relationships for stapeln
;; Media-Type: application/vnd.ecosystem+scm

(ecosystem
  (version "1.0.0")
  (name "stapeln")
  (type "application")
  (purpose "Visual drag-and-drop container stack designer with built-in security analysis")

  (position-in-ecosystem
    "stapeln is the user-facing design tool in the hyperpolymath verified container ecosystem. "
    "It serves as the primary interface through which users compose and validate container stacks, "
    "integrating security analysis, export, and deployment capabilities.")

  (related-projects
    (dependency "cerro-torre" "Container image builder - .ctp bundles, Ed25519 signatures")
    (dependency "lago-grey" "Minimal base image designer - Alpine/Chainguard alternative")
    (dependency "svalinn" "Edge gateway with policy enforcement")
    (dependency "selur" "Zero-copy IPC bridge between containers")
    (dependency "vordr" "Formally verified container runtime")
    (dependency "selur-compose" "CLI orchestration tool - replaces docker-compose")
    (consumer "verisimdb" "Multi-modal database for security findings and audit trails")
    (consumer "hypatia" "Neurosymbolic CI/CD security scanning")
    (consumer "gitbot-fleet" "Automated quality enforcement bots")
    (sibling-standard "verified-container-spec" "Protocol specification for verified containers")
    (sibling-standard "rsr-template-repo" "Repository structure template"))

  (what-this-is
    "A visual tool for designing secure container stacks. "
    "Think game-like spaceship customizer, but for containers. "
    "8 views: Network topology, Stack hierarchy, Lago Grey image designer, "
    "Port configuration, Security inspector, Gap analysis, Simulation, Settings.")

  (what-this-is-not
    "Not a container runtime (that is Vordr). "
    "Not an image builder (that is Cerro Torre). "
    "Not an orchestrator (that is selur-compose). "
    "Not a CI/CD tool (that is Hypatia)."))
