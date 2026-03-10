;; SPDX-License-Identifier: PMPL-1.0-or-later
;; META.scm — Architecture decisions and development practices for Rokur
;; Format: hyperpolymath/META.scm specification

(define project-meta
  `((version . "1.0.0")

    (architecture-decisions
      ((adr-001
        (title . "Deno over Node.js for HTTP runtime")
        (status . "accepted")
        (date . "2026-03-10")
        (context . "Rokur needs a JavaScript runtime for its HTTP server.
                     Node.js is the incumbent but is banned under RSR policy.
                     Deno provides built-in TypeScript support (unused here),
                     secure-by-default permissions, and native HTTP/2.")
        (decision . "Use Deno as the sole runtime. No Node.js, no npm.")
        (consequences . ("RSR compliant"
                         "Fine-grained permission model matches fail-closed philosophy"
                         "Deno.serve provides modern HTTP serving out of the box"
                         "deno.json replaces package.json for task and dependency management")))

      (adr-002
        (title . "Environment variables over configuration files")
        (status . "accepted")
        (date . "2026-03-10")
        (context . "Rokur needs configuration for port, token, required secrets,
                     and policy backend. Options: JSON/TOML config file, environment
                     variables, or CLI flags.")
        (decision . "Use environment variables exclusively, following 12-factor app
                     methodology. All configuration via ROKUR_* env vars.")
        (consequences . ("12-factor compliant"
                         "Container-native — works with Podman env injection"
                         "No config file to manage, mount, or secure"
                         "SIGHUP triggers re-read of env for secret hot-reload"
                         "Slightly harder to document complex configs")))

      (adr-003
        (title . "Fail-closed authorization policy")
        (status . "accepted")
        (date . "2026-03-10")
        (context . "When the policy engine encounters an error, ambiguous input,
                     or missing secrets, the gate must decide: allow or deny.")
        (decision . "Always deny on any ambiguity, error, or missing data.
                     Fail-closed is a non-negotiable invariant.")
        (consequences . ("Security-first — no container starts without explicit approval"
                         "May cause false denials during misconfiguration"
                         "Requires clear error messages to aid debugging"
                         "Health endpoint provides configuration visibility")))

      (adr-004
        (title . "Per-IP rate limiting with auth-failure escalation")
        (status . "accepted")
        (date . "2026-03-10")
        (context . "Rokur is a network service that could be targeted by
                     brute-force token guessing or denial-of-service attacks.")
        (decision . "Implement in-process per-IP sliding-window rate limiting.
                     Auth failures trigger escalated cooldowns.")
        (consequences . ("No external dependency (Redis, etc.) for rate limiting"
                         "Effective against single-IP brute-force"
                         "Does not protect against distributed attacks (use upstream firewall)"
                         "Memory bounded by number of unique client IPs")))

      (adr-005
        (title . "Chainguard Wolfi base image for containers")
        (status . "accepted")
        (date . "2026-03-10")
        (context . "Container base image selection affects security posture,
                     image size, and supply chain trust.")
        (decision . "Use cgr.dev/chainguard/wolfi-base:latest as the base image
                     for both build and runtime stages.")
        (consequences . ("Minimal attack surface (apk-based, no shell in static variant)"
                         "Signed images with Sigstore provenance"
                         "Regularly patched by Chainguard"
                         "Consistent with Stapeln container stack convention"))))

    (development-practices
      ((code-style . "deno-fmt")
       (linting . "deno-lint")
       (security . "openssf-scorecard")
       (testing . "deno-test")
       (versioning . "semver")
       (documentation . "markdown")
       (branching . "trunk-based")
       (containers . "podman")
       (ci . "github-actions")))

    (design-rationale
      ((principle . "fail-closed")
       (description . "Every code path that cannot definitively confirm authorization
                       must result in denial. This is the single most important
                       design constraint in Rokur."))

      ((principle . "no-secrets-in-responses")
       (description . "Rokur NEVER returns secret values in any API response.
                       It reports only presence/absence counts and policy decisions."))

      ((principle . "audit-everything")
       (description . "Every authorization decision — allow or deny — is recorded
                       in the structured audit log with full request context.")))))
