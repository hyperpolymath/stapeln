;; SPDX-License-Identifier: PMPL-1.0-or-later
;; META.scm - Meta-level project information

(define meta
  '((architecture-decisions
     ((adr-001
       (status "accepted")
       (date "2026-02-04")
       (context "Initial project setup")
       (decision "Use standard hyperpolymath structure")
       (consequences "Consistent with other hyperpolymath projects"))

      (adr-002
       (status "accepted")
       (date "2026-02-05")
       (context "Need UI architecture for complex visual designer")
       (decision "Use ReScript-TEA (The Elm Architecture) with cadre-tea-router")
       (rationale "TEA provides predictable state management, excellent for complex interactive UIs with drag-and-drop. Type-safe, functional, testable.")
       (alternatives "React (too imperative), Vue (lacks types), Elm (can't use with Deno), Solid (smaller ecosystem)")
       (consequences "All UI state transitions are explicit and traceable. Updates are pure functions. Time-travel debugging possible."))

      (adr-003
       (status "accepted")
       (date "2026-02-05")
       (context "Need to avoid npm/node per hyperpolymath language policy")
       (decision "Use Deno as JavaScript runtime")
       (rationale "Deno supports ESM imports, has security sandboxing, TypeScript out-of-box. Can use npm: specifier for ReScript compiler. No package.json or node_modules.")
       (alternatives "Node.js (banned per policy), Bun (not mature enough)")
       (consequences "Zero npm/yarn/pnpm usage. Cleaner dependency model. Some packages may not work without adaptation."))

      (adr-004
       (status "accepted")
       (date "2026-02-05")
       (context "Need three distinct views for different user needs")
       (decision "Three-page UI: Paragon (vertical stack), Cisco (network topology), Settings")
       (rationale "Paragon view shows supply chain hierarchy (Cerro Torre → Svalinn → containers → supply chain). Cisco view shows network topology with configurable shapes. Settings for defaults.")
       (alternatives "Single unified view (too cluttered), tab-based (less visual separation)")
       (consequences "Users can choose view that matches mental model. More implementation work but better UX."))

      (adr-005
       (status "accepted")
       (date "2026-02-05")
       (context "Need highest accessibility compliance for government use")
       (decision "WCAG 2.3 AAA compliance (not just AA)")
       (rationale "Government cyberwar officer test user requires top accessibility. AAA includes Braille, semantic XML, full keyboard nav, 7:1 contrast, screen reader optimization.")
       (alternatives "WCAG 2.1 AA (insufficient for government), Section 508 (US-only)")
       (consequences "More development effort but usable by all. Required for government deployment."))

      (adr-006
       (status "accepted")
       (date "2026-02-05")
       (context "Need database for stack metadata, provenance, validation history")
       (decision "Use VeriSimDB with 6 modalities")
       (rationale "VeriSimDB is hyperpolymath standard, supports graph (provenance), semantic (RDF), temporal (history), vector (similarity), tensor (attestations), document (configs).")
       (alternatives "PostgreSQL (single-modal), Neo4j (graph-only), MongoDB (document-only)")
       (consequences "Richer queries possible. Can store all data types in one system. More complex setup."))

      (adr-007
       (status "accepted")
       (date "2026-02-05")
       (context "Need attested documentation for components")
       (decision "Use A2ML (Attested Markup Language)")
       (rationale "A2ML provides progressive strictness (lax → checked → attested). Cryptographic signatures on docs. Idris2 backend for formal verification.")
       (alternatives "Markdown (no attestation), DocBook (too complex), custom format")
       (consequences "Documentation is verifiable and trustworthy. Signatures prevent tampering."))

      (adr-008
       (status "accepted")
       (date "2026-02-05")
       (context "Need self-validating component configurations")
       (decision "Use K9-SVC with Nickel contracts")
       (rationale "K9-SVC provides security levels (Kennel → Yard → Hunt) and Nickel-based validation. Self-validating configs prevent misconfigurations.")
       (alternatives "JSON Schema (no execution model), Dhall (less flexible), CUE (younger)")
       (consequences "Configs validated before deployment. Runtime checks possible. Nickel learning curve."))

      (adr-009
       (status "accepted")
       (date "2026-02-05")
       (context "Need security analysis but LLMs hallucinate CVEs")
       (decision "Use miniKanren for deterministic security reasoning (NOT SLM)")
       (rationale "miniKanren provides deterministic logic programming. Can encode OWASP/CVE rules as relations. No hallucinations. Full provenance chain. Instantly updateable with new CVE feeds.")
       (alternatives "SLM like Phi-3 (hallucinates, black box), static analysis tools (limited scope), manual review (doesn't scale)")
       (consequences "100% deterministic and explainable security analysis. Fast (milliseconds). Tiny memory footprint. Daily CVE updates trivial."))

      (adr-010
       (status "accepted")
       (date "2026-02-05")
       (context "Need firewall management with temporary port openings")
       (decision "Ephemeral pinholes with auto-expiry (GenServer-based)")
       (rationale "Users need temporary port access for testing/debugging. Ephemeral pinholes auto-close after N seconds/minutes. Audit logged to VeriSimDB.")
       (alternatives "Manual firewall rules (error-prone), always-open ports (insecure), VPN-only (too restrictive)")
       (consequences "Safer than permanent rules. Convenience without sacrificing security. Requires GenServer implementation in Elixir."))

      (adr-011
       (status "accepted")
       (date "2026-02-05")
       (context "Need user authentication for tool security")
       (decision "PAM authentication (system user verification)")
       (rationale "stapeln is localhost-only by default. Verify user via system PAM (Pluggable Authentication Modules). No custom user database needed.")
       (alternatives "OAuth2 (overkill for local tool), JWT (no system integration), no auth (insecure)")
       (consequences "Users authenticate with their system credentials. Integrates with LDAP/Kerberos/etc via PAM. Simple and secure."))

      (adr-012
       (status "accepted")
       (date "2026-02-05")
       (context "Need to integrate OWASP security standards")
       (decision "OWASP ModSecurity CRS in Svalinn gateway")
       (rationale "ModSecurity Core Rule Set provides WAF protection. Paranoia level 3 for high security. Anomaly scoring mode. Blocks container escape attempts.")
       (alternatives "Custom WAF rules (reinventing wheel), no WAF (insecure), third-party service (adds dependency)")
       (consequences "Industry-standard WAF protection. Regular rule updates from OWASP. Svalinn gateway becomes hardened."))

      (adr-013
       (status "accepted")
       (date "2026-02-05")
       (context "Need game-like UX to convert container-haters")
       (decision "Spaceship customizer interface with real-time scoring")
       (rationale "Target user (cyberwar officer) hates containers. Need game-like experience: choose components, see stats update, visual feedback. Like customizing spaceship in game.")
       (alternatives "Traditional UI (boring, intimidating), CLI-only (defeats purpose), wizard (too constraining)")
       (consequences "More development effort but dramatically better UX. Users enjoy configuration instead of dreading it. 'If you have to read the manual, we failed.'"))

      (adr-014
       (status "accepted")
       (date "2026-02-05")
       (context "Need to validate stack before deployment")
       (decision "Simulation mode (pre-deployment dry-run)")
       (rationale "Users can test deployment without actually deploying. Animated packet flow shows what will happen. Catches errors before they occur. Like Cisco Packet Tracer.")
       (alternatives "Deploy and hope (risky), static validation only (misses runtime issues)")
       (consequences "Users gain confidence before deploying. Catches port conflicts, network issues, missing dependencies. More implementation work but critical for UX."))))

    (development-practices
     (code-style "Follow language-specific conventions: ReScript standard, Elixir Credo, Scheme R7RS")
     (security "SPDX headers, OpenSSF Scorecard compliance, WCAG 2.3 AAA, OWASP ModSecurity, miniKanren security reasoning")
     (testing "Required for critical functionality: security rules, ephemeral pinholes, validation logic, accessibility")
     (versioning "Semantic versioning")
     (documentation "README.adoc, inline comments, A2ML for component docs, UX manifesto, container-hater test")
     (branching "main branch, feature branches, PRs required")
     (accessibility "WCAG 2.3 AAA mandatory - full ARIA, Braille, semantic XML, keyboard nav, 7:1 contrast")
     (security-first "Attack surface analysis on every change, miniKanren validation, no deployment without security check"))

    (design-rationale
     (game-like-ux "Target user is container-hater. Must feel like game, not server configuration. Real-time feedback, visual scoring, actionable suggestions.")
     (three-views "Different users think differently: some want vertical hierarchy (Paragon), some want network topology (Cisco), some want settings.")
     (wcag-aaa "Government use requires highest accessibility. Braille support, semantic XML, full screen reader optimization.")
     (minikanren-not-llm "Security decisions must be deterministic and explainable. LLMs hallucinate CVEs. miniKanren provides provenance chain.")
     (ephemeral-pinholes "Users need temporary access for testing. Permanent holes are dangerous. Auto-expiry prevents forgetting to close.")
     (verisimdb-multimodal "Need graph (provenance), semantic (RDF), temporal (history), vector (search), tensor (attestations), document (configs) all in one.")
     (simulation-mode "Users fear deployment. Let them test safely first. Animated packet flow builds confidence.")
     (deno-not-node "npm/node banned per hyperpolymath policy. Deno provides security, ESM, TypeScript without node_modules bloat.")
     (rescript-tea "Predictable state management critical for complex drag-and-drop UI. TEA makes all state transitions explicit and testable."))))
