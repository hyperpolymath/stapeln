# stapeln Roadmap

**Goal**: Convert container-haters into container-users through game-like UX and built-in security

**Target 1.0 Release**: Q2 2026

---

## Phase 1: Design & Specifications ✅ (Complete)

**Duration**: 1 week (2026-02-05)
**Status**: 100% complete

### Deliverables ✅

- [x] ReScript-TEA architecture (Model, Msg, Update, View)
- [x] Three-page UI design (Paragon, Cisco, Settings)
- [x] WCAG 2.3 AAA accessibility specifications
- [x] GraphQL schema with a11y metadata
- [x] VeriSimDB integration spec (6 modalities)
- [x] A2ML integration spec (attested documentation)
- [x] K9-SVC integration spec (self-validating configs)
- [x] miniKanren security reasoning engine design
- [x] OWASP ModSecurity + firewall configuration spec
- [x] Ephemeral pinhole design (auto-expiring firewall rules)
- [x] UX Manifesto ("If you have to read the manual, we failed")
- [x] Container-Hater Test (ultimate UX challenge)
- [x] Game-like UI mockups (spaceship customizer style)
- [x] Security stack audit (identified 47% → 100% path)
- [x] 14 Architecture Decision Records (ADRs)

### Key Decisions

- **miniKanren over SLM** for deterministic security reasoning
- **Deno over Node** per hyperpolymath language policy
- **Three views** for different mental models (Paragon, Cisco, Settings)
- **WCAG 2.3 AAA** for government compliance
- **Ephemeral pinholes** for safe temporary port access
- **Game-like UX** with real-time scoring and visual feedback

---

## Phase 2: Frontend Implementation ⚠️  (In Progress - 70%)

**Duration**: 4 weeks
**Start**: 2026-02-05 (started early!)
**Target Completion**: 2026-03-05

### Week 1: Core UI Components ✅ (100% Complete)

- [x] Implement PortConfigPanel.res (475 lines)
  - [x] Visual port state toggles (Closed/Open/Ephemeral)
  - [x] Ephemeral duration selector (30s to 24h)
  - [x] Real-time countdown display with React.useEffect1
  - [x] Port risk indicators (🔴 Critical, 🟠 High, 🟡 Medium, ✅ Safe)
  - [x] Auto-expiry timer that closes ephemeral ports
  - [x] Security warnings and best practices display
- [x] Implement SecurityInspector.res (759 lines)
  - [x] Security score display with progress bars (Security/Performance/Reliability/Compliance)
  - [x] Overall grade display (A+ to F)
  - [x] Vulnerability list with severity badges (Critical/High/Medium/Low)
  - [x] Quick checks (signatures, SBOM, non-root, health checks, resource limits, network segmentation)
  - [x] Exposed ports analysis with risk levels
  - [x] CVE display with fix suggestions and auto-fix buttons
- [x] Implement GapAnalysis.res (852 lines)
  - [x] Gap detection by category (Security/Compliance/Performance/Reliability/BestPractice)
  - [x] Severity-based prioritization (Critical/High/Medium/Low)
  - [x] Auto-fix buttons with confidence levels (Verified/High/Medium/Low/Manual)
  - [x] Issue provenance (miniKanren, Hypatia, VeriSimDB, CI, Manual Review)
  - [x] Impact indicators with affected components
  - [x] Step-by-step fix commands
  - [x] Apply all auto-fixes functionality

### Week 2: Attack Surface Analyzer (0%)

- [ ] Real-time security scoring
  - [ ] Security: ████████░░░░ 67/100
  - [ ] Performance: ███████████████ 92/100
  - [ ] Reliability: ██████████░░░░ 73/100
  - [ ] Compliance: ██████░░░░░░░░ 51/100
  - [ ] Overall Grade display (A+ to F)
- [ ] Threat model visualization
  - [ ] Exposed attack vectors
  - [ ] Risk levels per component
  - [ ] Vulnerability details
  - [ ] Fix suggestions
- [ ] Compliance scorecard
  - [ ] OWASP Top 10 progress
  - [ ] CIS Benchmarks progress
  - [ ] NIST Cybersecurity progress
  - [ ] PCI-DSS / SOC 2 status
- [ ] Recommended improvements
  - [ ] Priority-ordered list
  - [ ] Impact estimates (points gained)
  - [ ] Effort estimates (time)
  - [ ] [Apply Top 3] button

### Week 3: Simulation Mode ✅ (100% Complete)

- [x] SimulationMode.res implemented (779 lines)
  - [x] Packet animation with smooth interpolation (Cisco Packet Tracer style)
  - [x] Multiple packet types (HTTP/HTTPS/TCP/UDP/ICMP/DNS)
  - [x] Packet status tracking (InTransit/Delivered/Dropped/Blocked)
  - [x] Visual node rendering with connection lines
  - [x] Real-time statistics (sent/delivered/dropped/in-transit)
  - [x] Event log with timestamped network events
  - [x] requestAnimationFrame for 60fps smooth animation
- [x] Simulation controls implemented
  - [x] Playback controls (Start/Pause/Stop)
  - [x] Speed controls (0.5x, 1.0x, 2.0x, 4.0x)
  - [x] Packet injection for testing
  - [x] Event log with clear functionality
  - [x] Statistics panel toggle
- [x] App.res integration
  - [x] Added 4 new navigation tabs (Ports/Security/Gaps/Simulation)
  - [x] Wired all components into page routing
  - [x] 8-page navigation now fully functional
  - [ ] [Simulate] [Stop] [Reset] buttons
  - [ ] Speed control (1x, 2x, 5x)
  - [ ] Step-through mode
- [ ] Simulation log
  - [ ] Timestamped events
  - [ ] Success/failure indicators
  - [ ] Error messages with fixes
- [ ] Pre-deployment dry-run
  - [ ] Catch errors before deploying
  - [ ] Show what will happen
  - [ ] "Looks good!" confirmation

### Week 4: Polish & Integration (0%)

- [ ] Authentication flow (PAM)
  - [ ] Login screen
  - [ ] System user verification
  - [ ] Session management
  - [ ] Logout functionality
- [ ] Router integration (cadre-tea-router)
  - [ ] Page navigation
  - [ ] URL routing
  - [ ] Browser history
- [ ] Deno dev server
  - [ ] Hot reload
  - [ ] Error overlay
  - [ ] Build optimization
- [ ] Accessibility testing
  - [ ] Keyboard navigation
  - [ ] Screen reader (NVDA, JAWS, Orca)
  - [ ] Braille display
  - [ ] Contrast ratios
- [ ] Dark mode refinement
  - [ ] System-aware detection
  - [ ] Manual toggle
  - [ ] All pages themed

### Frontend Deliverables

- [ ] All ReScript components implemented
- [ ] Game-like UI with real-time scoring
- [ ] Simulation mode working
- [ ] WCAG 2.3 AAA compliance verified
- [ ] Deno dev server running
- [ ] Authentication flow complete
- [ ] All pages functional

---

## Phase 3: Backend & Security 🚧 (Not Started - 0%)

**Duration**: 6 weeks
**Target Start**: 2026-03-12
**Target Completion**: 2026-04-22

### Week 1-2: miniKanren Security Engine

- [ ] Core miniKanren setup
  - [ ] Guile Scheme installation
  - [ ] miniKanren library
  - [ ] Component model relations (componento, exposed-porto, etc.)
- [ ] Security rules (at least 10)
  1. [ ] SSH exposure (port 22 on public interface)
  2. [ ] Root user (running as root)
  3. [ ] Unencrypted traffic (HTTP, FTP, Telnet)
  4. [ ] Missing health checks
  5. [ ] Port conflicts
  6. [ ] No SBOM
  7. [ ] Missing signature verification
  8. [ ] Privileged mode enabled
  9. [ ] No resource limits
  10. [ ] Exposed admin ports
- [ ] CVE feed sync
  - [ ] NIST NVD API integration
  - [ ] Daily cron job
  - [ ] Auto-generate rules from CVEs
- [ ] OWASP Top 10 rules
  - [ ] A01: Broken Access Control
  - [ ] A02: Cryptographic Failures
  - [ ] A03: Injection
  - [ ] A04: Insecure Design
  - [ ] A05: Security Misconfiguration
  - [ ] A06-A10: Additional rules
- [ ] CIS Benchmark rules
  - [ ] Container image security
  - [ ] Runtime security
  - [ ] Network security

### Week 3-4: Elixir Phoenix Backend

- [ ] Phoenix server setup
  - [ ] GraphQL API (Absinthe)
  - [ ] REST endpoints
  - [ ] WebSocket for real-time updates
- [ ] EphemeralPinhole GenServer
  - [ ] Open/close firewall rules
  - [ ] Auto-expiry with schedulers
  - [ ] Audit logging
  - [ ] VeriSimDB integration
- [ ] SecurityReasoner module
  - [ ] Scheme IPC (call miniKanren)
  - [ ] S-expression parser
  - [ ] Provenance chain generation
  - [ ] GraphQL resolvers
- [ ] PAM authentication
  - [ ] System user verification
  - [ ] Session management
  - [ ] LDAP/Kerberos via PAM
- [ ] Audit logger
  - [ ] VeriSimDB temporal modality
  - [ ] Immutable audit trail
  - [ ] Syslog integration

### Week 5: Database & Documentation

- [ ] VeriSimDB integration
  - [ ] Graph modality (provenance)
  - [ ] Semantic modality (RDF + SPARQL)
  - [ ] Temporal modality (history, audit log)
  - [ ] Vector modality (similarity search)
  - [ ] Tensor modality (attestations)
  - [ ] Document modality (configs)
- [ ] A2ML parser
  - [ ] Progressive strictness (lax → checked → attested)
  - [ ] Idris2 backend validation
  - [ ] Signature verification
  - [ ] ReScript renderer integration
- [ ] K9-SVC validator
  - [ ] Nickel contract evaluation
  - [ ] Security levels (Kennel → Yard → Hunt)
  - [ ] k9-sign integration
  - [ ] Elixir backend integration

### Week 6: Firewall & WAF

- [ ] OWASP ModSecurity CRS
  - [ ] Svalinn gateway integration
  - [ ] Paranoia Level 3 configuration
  - [ ] Anomaly scoring mode
  - [ ] Custom stapeln rules
  - [ ] Container escape blocking
- [ ] firewalld configuration
  - [ ] Default-deny rules
  - [ ] stapeln zone creation
  - [ ] Ephemeral pinhole scripts
  - [ ] Localhost-only by default
- [ ] Port conflict detection
  - [ ] Real-time checking
  - [ ] Fix suggestions
  - [ ] Auto-fix implementation
- [ ] Component security scanning
  - [ ] Grype CVE scanner integration
  - [ ] Signature verification (Rekor)
  - [ ] SBOM validation
  - [ ] Health check validation

### Backend Deliverables

- [ ] miniKanren engine with 10+ rules
- [ ] CVE/OWASP daily sync working
- [ ] Elixir Phoenix + GraphQL API
- [ ] Ephemeral pinholes functional
- [ ] VeriSimDB fully integrated
- [ ] A2ML parser working
- [ ] K9-SVC validator working
- [ ] OWASP ModSecurity configured
- [ ] firewalld rules deployed

---

## Phase 4: Integration & Testing 🧪 (Not Started)

**Duration**: 3 weeks
**Target Start**: 2026-04-23
**Target Completion**: 2026-05-13

### Week 1: End-to-End Integration

- [ ] Frontend ↔ Backend API
  - [ ] GraphQL queries working
  - [ ] WebSocket real-time updates
  - [ ] Error handling
- [ ] miniKanren ↔ Elixir
  - [ ] S-expression parsing
  - [ ] Rule violation queries
  - [ ] Provenance chain display
- [ ] VeriSimDB ↔ All services
  - [ ] Stack metadata storage
  - [ ] Audit log retrieval
  - [ ] SPARQL queries
- [ ] Firewall ↔ UI
  - [ ] Port configuration sync
  - [ ] Ephemeral pinhole UI
  - [ ] Real-time status

### Week 2: User Testing

- [ ] **Container-Hater Test** 🎯
  - [ ] Government cyberwar officer testing
  - [ ] 3-tier app deployment in <30 min
  - [ ] No documentation reading
  - [ ] Success rate measurement
- [ ] Accessibility testing
  - [ ] Screen reader (NVDA, JAWS, Orca)
  - [ ] Keyboard-only navigation
  - [ ] Braille display
  - [ ] Contrast ratio verification
- [ ] Performance testing
  - [ ] 100+ container stacks
  - [ ] UI responsiveness
  - [ ] Simulation speed
  - [ ] Backend latency
- [ ] Security testing
  - [ ] Penetration testing
  - [ ] ModSecurity rule validation
  - [ ] Ephemeral pinhole security
  - [ ] CVE detection accuracy

### Week 3: Bug Fixes & Polish

- [ ] Fix issues from user testing
- [ ] Performance optimization
- [ ] UI polish (animations, transitions)
- [ ] Documentation updates
- [ ] Tutorial video (30-second clips)

---

## Phase 5: Post-Quantum Crypto Upgrades 🔐 (Pending)

**Duration**: 8 weeks
**Target Start**: 2026-05-14
**Target Completion**: 2026-07-08

### Month 1: Hybrid Classical + PQ

- [ ] Add Dilithium5-AES (ML-DSA-87)
  - [ ] Alongside Ed25519
  - [ ] Verify with both
  - [ ] Clients start using PQ
- [ ] Add Kyber-1024 (ML-KEM-1024)
  - [ ] For TLS key exchange
  - [ ] Hybrid with X25519
- [ ] SPHINCS+ implementation
  - [ ] As fallback option
  - [ ] Conservative backup
- [ ] Testing
  - [ ] Interoperability
  - [ ] Performance benchmarks
  - [ ] Security validation

### Month 2: Full PQ Migration

- [ ] Dilithium5 becomes primary
  - [ ] Ed448 as fallback (deprecate Ed25519)
  - [ ] SPHINCS+ as conservative backup
- [ ] HTTP/3 + QUIC migration
  - [ ] Deno HTTP/3 support (if available)
  - [ ] Tauri + Rust Quiche (fallback)
  - [ ] Elixir Phoenix + quicer
  - [ ] End-to-end testing
- [ ] SHAKE3-512 hashing
  - [ ] Replace SHA-256
  - [ ] FIPS 202 compliance
  - [ ] All provenance hashes
- [ ] Argon2id passwords
  - [ ] 512 MiB, 8 iterations
  - [ ] PAM integration
- [ ] XChaCha20-Poly1305
  - [ ] Bundle encryption
  - [ ] IPC channel encryption
- [ ] ChaCha20-DRBG
  - [ ] Replace system RNG
  - [ ] 512-bit seed
  - [ ] Session token generation

### PQ Deliverables

- [ ] Dilithium5-AES signatures
- [ ] Kyber-1024 key exchange
- [ ] Ed448 hybrid mode
- [ ] SPHINCS+ backup
- [ ] HTTP/3 + QUIC
- [ ] SHAKE3-512 hashing
- [ ] Full FIPS 203/204 compliance
- [ ] 100% post-quantum ready

---

## Phase 6: 1.0 Release 🚀 (Target: Q2 2026)

**Target Date**: 2026-07-15

### Pre-Release Checklist

- [ ] All phases complete
- [ ] 100% test coverage (critical paths)
- [ ] WCAG 2.3 AAA verified
- [ ] Security audit passed
- [ ] Performance benchmarks met
- [ ] Container-hater test passed
- [ ] Documentation complete
- [ ] Tutorial videos published
- [ ] Blog post written
- [ ] Release notes prepared

### 1.0 Features

✅ **UX**
- Game-like visual interface
- Real-time security scoring
- One-click auto-fix
- Simulation mode
- Zero CLI commands needed

✅ **Security**
- miniKanren reasoning engine
- OWASP ModSecurity CRS
- Ephemeral pinholes
- Daily CVE updates
- Attack surface analysis
- Post-quantum crypto

✅ **Accessibility**
- WCAG 2.3 AAA
- Full keyboard navigation
- Screen reader optimized
- Braille support

✅ **Database**
- VeriSimDB (6 modalities)
- A2ML documentation
- K9-SVC validation
- Immutable audit trail

✅ **Compliance**
- OWASP Top 10: 100%
- CIS Benchmarks: 100%
- NIST Cybersecurity: 100%
- OpenSSF Scorecard: 100%

---

## Post-1.0 Roadmap 🔮

### v1.1: Advanced Features (Q3 2026)

- [ ] Multi-user collaboration
- [ ] Stack templates marketplace
- [ ] AI-powered optimization suggestions (LLM for *explanations* only, not decisions)
- [ ] Cost estimation (cloud deployment)
- [ ] Performance profiling
- [ ] Advanced simulation (chaos engineering)

### v1.2: Enterprise Features (Q4 2026)

- [ ] RBAC (Role-Based Access Control)
- [ ] SSO integration
- [ ] Compliance reporting
- [ ] Custom rule engine (user-defined security rules)
- [ ] CI/CD integration (GitHub Actions, GitLab CI)
- [ ] Helm chart generation

### v2.0: Kubernetes Integration (2027)

- [ ] Kubernetes YAML generation
- [ ] Helm charts
- [ ] Operators
- [ ] Service mesh integration (Istio, Linkerd)
- [ ] GitOps (FluxCD, ArgoCD)

---

## Success Metrics

### UX Metrics

- **Container-Hater Test**: Pass (3-tier app in <30 min, no docs)
- **User Retention**: >80% return after 1 week
- **Error Recovery**: >90% of errors auto-fixed
- **Time to First Deploy**: <5 minutes

### Security Metrics

- **False Positive Rate**: <5%
- **False Negative Rate**: <1%
- **CVE Detection Time**: <24 hours from NVD publication
- **Vulnerability Remediation**: >95% auto-fixable

### Performance Metrics

- **UI Render**: <16ms (60fps)
- **Drag Feedback**: <50ms
- **Validation**: <500ms
- **Large Stacks**: 100+ containers supported

### Compliance Metrics

- **OWASP Top 10**: 100%
- **CIS Benchmarks**: 100%
- **NIST Cybersecurity**: 100%
- **WCAG 2.3 AAA**: 100%
- **OpenSSF Scorecard**: 10/10

---

## Timeline Summary

| Phase | Duration | Target Completion | Status |
|-------|----------|-------------------|--------|
| Phase 1: Design | 1 week | 2026-02-05 | ✅ Complete |
| Phase 2: Frontend | 4 weeks | 2026-03-11 | ⚠️  30% |
| Phase 3: Backend | 6 weeks | 2026-04-22 | ❌ Not started |
| Phase 4: Testing | 3 weeks | 2026-05-13 | ❌ Not started |
| Phase 5: PQ Crypto | 8 weeks | 2026-07-08 | ❌ Not started |
| Phase 6: 1.0 Release | - | 2026-07-15 | ❌ Not started |

**Total**: ~22 weeks from design start to 1.0 release

---

## Key Risks

| Risk | Impact | Mitigation |
|------|--------|------------|
| cadre-tea-router unavailable | High | Vendor library or implement basic routing |
| miniKanren performance issues | Medium | Profile and optimize; use caching |
| Container-hater test fails | Critical | Iterate on UX until pass |
| PQ crypto complexity | High | Allow more time; hybrid mode first |
| VeriSimDB integration issues | Medium | Start with PostgreSQL, add VeriSimDB later |

---

## Resources Needed

- **Frontend**: 1 ReScript developer (4 weeks)
- **Backend**: 1 Elixir developer (6 weeks)
- **Security**: 1 Scheme/miniKanren expert (2 weeks)
- **UX Testing**: Container-hater + 5 additional testers (1 week)
- **Security Audit**: External firm (2 weeks)
- **Accessibility Testing**: Specialist tester (1 week)

---

**Last Updated**: 2026-02-05
**Status**: Phase 1 complete, Phase 2 in progress (30%)
