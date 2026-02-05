# Security Reasoning Engine: miniKanren vs SLM

**Decision**: Use miniKanren for deterministic rule reasoning, NOT an SLM

---

## Why miniKanren > SLM for Security

### The Problem with SLMs

| Issue | Impact | Example |
|-------|--------|---------|
| **Hallucination** | May invent CVEs that don't exist | "This has CVE-2024-99999" (fake!) |
| **Standards drift** | May contradict OWASP/NIST | "Port 22 is fine" (NO IT'S NOT) |
| **Non-deterministic** | Same input → different output | Unreliable for security decisions |
| **Training lag** | Misses recent CVEs | Trained on old data, misses 0-days |
| **No provenance** | Can't explain *why* | "Trust me, it's secure" (NOT ACCEPTABLE) |

### Why miniKanren Wins

| Feature | Benefit | Example |
|---------|---------|---------|
| **Deterministic** | Same input → same output, always | Reliable security decisions |
| **Provable** | Can trace reasoning path | "Port 22 open BECAUSE OWASP rule #7" |
| **Updateable** | Add new CVE rules immediately | Real-time threat intelligence |
| **Composable** | Rules build on rules | Complex security policies |
| **Transparent** | Shows *why* it flagged something | Audit trail for compliance |

---

## Architecture: miniKanren Rule Engine

```
┌────────────────────────────────────────────────────────────┐
│ stapeln Security Reasoning Engine                          │
├────────────────────────────────────────────────────────────┤
│                                                             │
│  ┌──────────────────┐         ┌──────────────────┐        │
│  │ Knowledge Base   │────────→│ miniKanren Core  │        │
│  │ (Scheme Rules)   │         │ (Logic Engine)   │        │
│  └──────────────────┘         └──────────────────┘        │
│         ▲                              │                   │
│         │ Updates                      │ Queries           │
│         │                              ▼                   │
│  ┌──────────────────┐         ┌──────────────────┐        │
│  │ CVE Feed         │         │ Attack Surface   │        │
│  │ OWASP Updates    │         │ Analyzer         │        │
│  │ CIS Benchmarks   │         │ (ReScript UI)    │        │
│  └──────────────────┘         └──────────────────┘        │
│                                                             │
└────────────────────────────────────────────────────────────┘
```

### Data Flow

1. **User configures stack** in UI (ReScript)
2. **Stack model sent** to reasoning engine (Scheme + miniKanren)
3. **Rules applied** to find violations
4. **Results returned** with provenance (why it's a problem)
5. **UI shows issues** with fix suggestions
6. **Rules updated** daily from CVE/OWASP feeds

---

## miniKanren Rule Examples

### Rule 1: No SSH on Public Internet

```scheme
;; security-rules/ssh-exposure.scm
;; SPDX-License-Identifier: PMPL-1.0-or-later

(use-modules (minikanren))

(define (expose-ssh-to-interneto component network)
  "Rule: SSH (port 22) must not be exposed to public internet"
  (fresh (port interface)
    (componento component)
    (exposed-porto component port interface)
    (== port 22)
    (== interface 'public)
    (networko network 'internet)))

;; Query: Find all SSH exposures
(run* (component)
  (expose-ssh-to-interneto component 'internet))
;; => (nginx-component-id svalinn-component-id)  ; VIOLATIONS FOUND!

;; Severity
(define ssh-exposure-severity 'critical)

;; Rationale
(define ssh-exposure-rationale
  "SSH on port 22 exposed to internet allows brute-force attacks.
   OWASP A01:2021 - Broken Access Control
   CIS Benchmark: 5.2.1 Ensure SSH access is limited
   CVE-2023-38408: OpenSSH pre-auth remote code execution")

;; Fix
(define ssh-exposure-fix
  '((close-port 22)
    (use-ephemeral-pinhole 22 300)  ; 5 minutes only
    (require-key-auth)
    (use-vpn-only)))
```

### Rule 2: No Root Containers

```scheme
;; security-rules/root-user.scm
;; SPDX-License-Identifier: PMPL-1.0-or-later

(define (running-as-rooto component)
  "Rule: Containers must not run as root"
  (fresh (user)
    (componento component)
    (container-usero component user)
    (conde
      [(== user 'root)]
      [(== user #nil)])))  ; No USER directive = root

;; Query: Find all root containers
(run* (component)
  (running-as-rooto component))
;; => (postgres-component-id redis-component-id)  ; VIOLATIONS!

;; Severity
(define root-user-severity 'high)

;; Rationale
(define root-user-rationale
  "Running as root enables container breakout attacks.
   OWASP A01:2021 - Broken Access Control
   CIS Benchmark: 4.1 Ensure containers run as non-root
   CVE-2019-5736: runc container breakout via root")

;; Fix
(define root-user-fix
  '((add-user-directive "appuser")
    (set-uid-gid 1000 1000)
    (drop-capabilities "ALL")))
```

### Rule 3: Unencrypted Traffic

```scheme
;; security-rules/unencrypted-traffic.scm
;; SPDX-License-Identifier: PMPL-1.0-or-later

(define (unencrypted-traffico component)
  "Rule: All public traffic must use TLS"
  (fresh (port protocol interface)
    (componento component)
    (exposed-porto component port interface)
    (== interface 'public)
    (protocolo port protocol)
    (conde
      [(== protocol 'http)]
      [(== protocol 'ftp)]
      [(== protocol 'telnet)]
      [(== protocol 'smtp)])))  ; Unencrypted protocols

;; Query: Find all unencrypted traffic
(run* (component port)
  (unencrypted-traffico component)
  (exposed-porto component port 'public))
;; => ((nginx-component-id 80))  ; HTTP exposed!

;; Severity
(define unencrypted-traffic-severity 'high)

;; Rationale
(define unencrypted-traffic-rationale
  "Unencrypted traffic exposes sensitive data to interception.
   OWASP A02:2021 - Cryptographic Failures
   CIS Benchmark: 5.13 Ensure only encrypted traffic
   NIST 800-52r2: Require TLS 1.2+ for all public endpoints")

;; Fix
(define unencrypted-traffic-fix
  '((enable-tls 1.3)
    (redirect-http-to-https)
    (close-port 80)
    (use-https-only)))
```

### Rule 4: Missing Health Checks

```scheme
;; security-rules/health-checks.scm
;; SPDX-License-Identifier: PMPL-1.0-or-later

(define (missing-health-checko component)
  "Rule: All long-running services must have health checks"
  (fresh (health-check service-type)
    (componento component)
    (service-typeo component service-type)
    (membero service-type '(web-server api database cache))
    (health-checko component health-check)
    (== health-check #nil)))  ; No health check defined

;; Query
(run* (component)
  (missing-health-checko component))
;; => (redis-component-id)

;; Severity
(define missing-health-check-severity 'medium)

;; Rationale
(define missing-health-check-rationale
  "Without health checks, failures go undetected.
   SRE Best Practice: Automated health monitoring
   Kubernetes: Liveness and readiness probes required
   Result: Cascading failures, poor user experience")

;; Fix
(define missing-health-check-fix
  '((add-http-health-check "/health")
    (set-interval 10)
    (set-timeout 5)
    (set-retries 3)))
```

---

## Knowledge Base Structure

```scheme
;; knowledge-base/security-kb.scm
;; SPDX-License-Identifier: PMPL-1.0-or-later

;; Component model
(define (componento c)
  "c is a valid component in the stack"
  (fresh (id type)
    (== c `(component ,id ,type))))

(define (container-usero component user)
  "component runs as user"
  (fresh (id type config)
    (== component `(component ,id ,type . ,config))
    (membero `(user . ,user) config)))

(define (exposed-porto component port interface)
  "component exposes port on interface (public|internal)"
  (fresh (id type config)
    (== component `(component ,id ,type . ,config))
    (membero `(port ,port ,interface) config)))

(define (protocolo port protocol)
  "port uses protocol"
  (conde
    [(== port 22) (== protocol 'ssh)]
    [(== port 80) (== protocol 'http)]
    [(== port 443) (== protocol 'https)]
    [(== port 5432) (== protocol 'postgres)]
    [(== port 6379) (== protocol 'redis)]))

(define (health-checko component health-check)
  "component has health-check configuration"
  (fresh (id type config)
    (== component `(component ,id ,type . ,config))
    (conde
      [(membero `(health-check . ,health-check) config)]
      [(== health-check #nil)])))  ; No health check

;; Example stack model
(define example-stack
  '((component nginx-1 web-server
      (user . root)  ; VIOLATION!
      (port 80 public)  ; VIOLATION!
      (port 443 public)
      (health-check . #nil))  ; VIOLATION!

    (component postgres-1 database
      (user . postgres)  ; OK (non-root)
      (port 5432 internal)  ; OK (internal only)
      (health-check . (interval 30)))  ; OK

    (component svalinn-1 gateway
      (user . gateway)
      (port 22 public)  ; VIOLATION!
      (port 8443 public)
      (health-check . (interval 10)))))
```

---

## Integration with CVE Feeds

### Daily Rule Updates

```scheme
;; updater/cve-feed-sync.scm
;; SPDX-License-Identifier: PMPL-1.0-or-later

(define (sync-cve-feed)
  "Fetch latest CVE database and generate miniKanren rules"

  ;; Fetch from NIST NVD
  (define cve-json
    (http-get "https://services.nvd.nist.gov/rest/json/cves/2.0"))

  ;; Parse and convert to rules
  (for-each
    (lambda (cve)
      (when (affects-containers? cve)
        (generate-rule-from-cve cve)))
    (parse-cve-json cve-json)))

(define (generate-rule-from-cve cve)
  "Convert CVE to miniKanren rule"
  (define cve-id (cve-field cve 'id))
  (define severity (cve-field cve 'severity))
  (define description (cve-field cve 'description))
  (define affected-ports (extract-ports cve))
  (define affected-images (extract-images cve))

  ;; Generate rule file
  (with-output-to-file
    (string-append "security-rules/auto/" cve-id ".scm")
    (lambda ()
      (write `(define (,cve-id-ruleo component)
                ,(string-append "Auto-generated rule for " cve-id)
                (fresh (image)
                  (componento component)
                  (image-nameo component image)
                  (membero image ',affected-images))))
      (write `(define ,(string->symbol (string-append cve-id "-severity"))
                ',severity))
      (write `(define ,(string->symbol (string-append cve-id "-rationale"))
                ,description)))))

;; Run daily via cron
;; 0 2 * * * guile /path/to/cve-feed-sync.scm
```

### OWASP Rule Sync

```scheme
;; updater/owasp-sync.scm
;; SPDX-License-Identifier: PMPL-1.0-or-later

(define (sync-owasp-top-10)
  "Update rules from OWASP Top 10"

  (define owasp-2021
    '((A01 broken-access-control
       (ssh-exposure root-containers privileged-mode))
      (A02 cryptographic-failures
       (unencrypted-traffic weak-ciphers no-tls))
      (A03 injection
       (sql-injection command-injection))
      (A04 insecure-design
       (no-rate-limiting no-health-checks))
      (A05 security-misconfiguration
       (default-credentials exposed-admin-ports))
      ;; ... etc
      ))

  ;; Generate rules for each category
  (for-each generate-owasp-rules owasp-2021))
```

---

## Query API (Elixir Backend)

```elixir
# backend/lib/stapeln/security_reasoner.ex
# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Stapeln.SecurityReasoner do
  @moduledoc """
  Interface to miniKanren reasoning engine.

  Converts stack model to Scheme S-expressions,
  queries miniKanren, parses results.
  """

  def analyze_stack(stack) do
    # Convert to Scheme format
    scheme_model = stack_to_scheme(stack)

    # Write to temp file
    model_file = write_temp_file(scheme_model)

    # Run miniKanren query
    {output, 0} = System.cmd("guile", [
      "-s", "security-rules/analyzer.scm",
      "--model", model_file
    ])

    # Parse results
    parse_violations(output)
  end

  defp stack_to_scheme(stack) do
    components = Enum.map(stack.components, fn comp ->
      """
      (component #{comp.id} #{comp.type}
        (user . #{comp.user || "root"})
        #{ports_to_scheme(comp.ports)}
        (health-check . #{comp.health_check || "#nil"}))
      """
    end)

    "(define stack '(#{Enum.join(components, "\n")}))"
  end

  defp ports_to_scheme(ports) do
    Enum.map_join(ports, "\n", fn port ->
      interface = if port.public, do: "public", else: "internal"
      "(port #{port.number} #{interface})"
    end)
  end

  defp parse_violations(output) do
    # Parse Scheme output into Elixir maps
    output
    |> String.split("\n")
    |> Enum.map(&parse_violation_line/1)
    |> Enum.reject(&is_nil/1)
  end

  defp parse_violation_line(line) do
    # (violation ssh-exposure nginx-1 critical "SSH exposed..." (fix-1 fix-2))
    # => %{rule: "ssh-exposure", component: "nginx-1", ...}
    # TODO: Implement S-expression parser
  end
end
```

---

## User-Facing Output

### In ReScript UI

When miniKanren finds violations, show them in the UI:

```rescript
// Example violation object
type violation = {
  rule: string,              // "ssh-exposure"
  component: string,         // "nginx-1"
  severity: securityLevel,   // Critical
  rationale: string,         // Full explanation with CVE/OWASP refs
  fixes: array<fix>,         // Possible fixes
  provenanceChain: array<string>,  // Shows reasoning steps
}

// Provenance example:
// ["Component nginx-1 has port 22 exposed",
//  "Port 22 uses SSH protocol",
//  "Interface is 'public' (internet-facing)",
//  "OWASP A01:2021 rule violation",
//  "CVE-2023-38408 affects this configuration",
//  "Therefore: CRITICAL vulnerability"]
```

### Transparency = Trust

Users can click **[Why?]** to see the full reasoning chain:

```
Why is this flagged?

Reasoning chain:
1. Component 'nginx-1' has port 22 exposed
   Source: Your configuration, line 42

2. Port 22 uses SSH protocol
   Source: IANA port registry

3. Interface is 'public' (internet-facing)
   Source: Your network configuration

4. Rule triggered: OWASP A01:2021 - Broken Access Control
   Source: OWASP Top 10 (2021 edition)

5. Known vulnerabilities affect this:
   - CVE-2023-38408: OpenSSH pre-auth RCE
   - CVE-2021-28041: OpenSSH privilege escalation
   Source: NIST National Vulnerability Database

Conclusion: CRITICAL severity
Recommendation: Close port or use ephemeral pinhole only
```

---

## Comparison: miniKanren vs SLM

| Aspect | miniKanren | SLM (e.g., Phi-3) |
|--------|------------|-------------------|
| **Correctness** | ✅ Guaranteed (logic) | ⚠️ Probabilistic |
| **Explainability** | ✅ Full provenance | ❌ Black box |
| **Updates** | ✅ Instant (add rule) | ❌ Requires retraining |
| **Determinism** | ✅ Always same result | ❌ May vary |
| **Speed** | ✅ Fast (milliseconds) | ⚠️ Slower (inference) |
| **Memory** | ✅ Tiny (<10 MB) | ❌ Large (>1 GB) |
| **Trustworthy** | ✅ Provable | ⚠️ "Trust me" |
| **Compliance** | ✅ Audit trail | ❌ Hard to audit |
| **Security risk** | ✅ No hallucination | ❌ May hallucinate CVEs |

**Winner**: miniKanren for security-critical decisions

---

## Optional: LLM for Natural Language *After* miniKanren

Use case: Explain violations in friendlier language

```
miniKanren output:
  (violation ssh-exposure nginx-1 critical
    "SSH on port 22 exposed to public internet allows brute-force attacks...")

↓ Send to LLM for rephrasing ↓

User-friendly output:
  "🔴 Your nginx container has a dangerous port open!

  Port 22 (SSH) is accessible from the internet, which means
  attackers can try to guess your password. This is like leaving
  your front door unlocked in a bad neighborhood.

  Fix: Close this port, or make it temporarily available only
  when you need it (ephemeral pinhole)."
```

**Key**: LLM explains *after* miniKanren decides. Logic first, language second.

---

## Implementation Plan

### Week 1: Core miniKanren Engine
- [ ] Set up Guile Scheme + miniKanren
- [ ] Define component model relations
- [ ] Implement 5 basic rules (SSH, root, TLS, health checks, ports)
- [ ] Test queries

### Week 2: CVE/OWASP Integration
- [ ] CVE feed sync script
- [ ] OWASP Top 10 rule generator
- [ ] CIS Benchmark rules
- [ ] Daily update cron job

### Week 3: Elixir Backend Integration
- [ ] Scheme IPC from Elixir
- [ ] S-expression parser
- [ ] GraphQL API for violations
- [ ] Provenance chain rendering

### Week 4: ReScript UI
- [ ] Display violations
- [ ] Show provenance (click [Why?])
- [ ] Auto-fix suggestions
- [ ] Real-time analysis as user configures

---

## Conclusion

**Use miniKanren, NOT an SLM**

Reasons:
1. **Deterministic** = Trustworthy for security
2. **Provable** = Full audit trail for compliance
3. **Updateable** = Add new CVEs immediately
4. **Transparent** = Users understand *why*
5. **Fast** = Milliseconds, not seconds
6. **Small** = No GPU, no 1GB models

**LLM role**: Optional, for natural language explanations *after* miniKanren decides

**Result**: stapeln becomes the *most trustworthy* container security tool because every decision is logically provable and traceable to OWASP/NIST/CVE sources.

---

## References

- miniKanren: http://minikanren.org/
- Guile Scheme: https://www.gnu.org/software/guile/
- NIST NVD API: https://nvd.nist.gov/developers/vulnerabilities
- OWASP Top 10: https://owasp.org/www-project-top-ten/
- CIS Benchmarks: https://www.cisecurity.org/cis-benchmarks

**Status**: Design specification
**Next**: Implement miniKanren proof-of-concept with 5 rules
