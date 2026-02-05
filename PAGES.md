# stapeln Multi-Page Architecture

## Overview

stapeln is a **three-page Tauri desktop application** for designing, simulating, and configuring verified container stacks.

## Page 1: Paragon View (Vertical Stack Designer)

**Purpose**: High-level stack visualization (like GParted disk partitions)

**Layout**:
```
┌─────────────────────────────────────────────────────────┐
│  🏔️ Cerro Torre (Build Layer)                          │
├─────────────────────────────────────────────────────────┤
│  🛡️ Svalinn (Gateway Layer)                            │
├─────────────────────────────────────────────────────────┤
│  🌉 selur (IPC Bridge Layer)                            │
├─────────────────────────────────────────────────────────┤
│  ⚔️ Vörðr (Runtime Layer)                              │
├─────────────────────────────────────────────────────────┤
│  🐳 Podman/Docker/nerdctl (Container Engine)            │
├─────────────────────────────────────────────────────────┤
│  📦 Application Containers (nginx, postgres, etc.)      │
└─────────────────────────────────────────────────────────┘
         ↓
   Supply Chain Visualization
   (Provenance, signatures, SBOMs)
```

**Features**:
- Visual block representation (vertical)
- Cerro Torre at pinnacle (top)
- Supply chain info at bottom
- Click any layer to jump to Page 2 for detailed config
- **Gap Analysis Panel**: Red highlights for weak points/missing coverage

**Weak Points Detection**:
- ❌ No signature verification
- ❌ Missing SBOM
- ❌ No network policy
- ❌ Insecure port bindings
- ❌ No resource limits
- ❌ Missing health checks
- ❌ No backup strategy
- ❌ Single point of failure

## Page 2: Cisco View (Network Topology Designer)

**Purpose**: Detailed container relationships and configurations (like Cisco Packet Tracer)

**Visual Elements**:

### Container Shapes
```
┌─────────────┐
│   Box       │  = Standard container
│  (Service)  │
└─────────────┘

 ╭──────────╮
│   Oval    │   = Database container
│ (Storage) │
 ╰──────────╯

╔═══════════╗
║  Thick    ║   = Security/gateway container
║  Border   ║
╚═══════════╝

┌─────────────────────┐
│  Central Container  │
│  ┌─────────────┐   │
│  │   Nested    │   │  = Nested/sidecar containers
│  │  Database   │   │
│  └─────────────┘   │
└─────────────────────┘

    ┌─────┐
───→│ Interface │───→   = Security interface (firewall)
    └─────┘
```

**Layout Example**:
```
        [Load Balancer]
              │
       ┌──────┴──────┐
       ↓             ↓
  [Svalinn]     [Svalinn]  ← Security interfaces
       │             │
       └──────┬──────┘
              ↓
       ╔═════════════╗
       ║  App Stack  ║
       ║ ┌─────────┐ ║
       ║ │  nginx  │ ║
       ║ └────┬────┘ ║
       ║      ↓      ║
       ║ ┌─────────┐ ║
       ║ │   API   │ ║
       ║ └────┬────┘ ║
       ║      ↓      ║
       ║ ╭─────────╮ ║
       ║ │ postgres│ ║  ← Nested oval
       ║ ╰─────────╯ ║
       ╚═════════════╝
              │
              ↓
       [Backup Volume]
```

**Interactions**:
1. **Drag-and-drop** from component palette
2. **Click container** → Configuration panel opens
3. **Draw connections** → Defines network paths
4. **Right-click** → Context menu (duplicate, delete, inspect)
5. **Simulate** button → Runs validation and shows traffic flow animation

**Configuration Panel (on click)**:
```
┌─────────────────────────────────────┐
│ Container: nginx-web-01             │
├─────────────────────────────────────┤
│ Shape: ☑ Box ☐ Oval ☐ Gateway      │
│                                     │
│ Image: nginx:latest.ctp             │
│ Ports:                              │
│   8080:80                           │
│   8443:443                          │
│                                     │
│ Environment:                        │
│   API_URL=http://api:3000           │
│                                     │
│ Resources:                          │
│   CPU: 1.0 cores                    │
│   Memory: 512 MB                    │
│                                     │
│ Volumes:                            │
│   /var/www/html → local             │
│                                     │
│ Health Check:                       │
│   HTTP GET /health                  │
│   Interval: 30s                     │
│                                     │
│ Security:                           │
│   ☑ Read-only root                  │
│   ☑ Drop all capabilities           │
│   ☐ Privileged mode                 │
│                                     │
│ [Validate] [Apply] [Cancel]        │
└─────────────────────────────────────┘
```

**Simulation Mode**:
- Animated packet flow (like Cisco)
- Highlight active connections
- Show latency/throughput estimates
- Red X for failed connections
- Green checkmarks for successful paths

## Page 3: Settings (Preferences & Defaults)

**Purpose**: Customize application behavior and default configurations

### Sections:

#### 1. Default Component Settings
```
┌─────────────────────────────────────────────────┐
│ Default Container Runtime                       │
│  ● Podman  ○ Docker  ○ nerdctl                  │
│                                                 │
│ Default Registry                                │
│  ghcr.io/hyperpolymath                         │
│                                                 │
│ Auto-verify signatures: ☑                       │
│ Require SBOM: ☑                                 │
│ Enforce network policies: ☑                     │
│                                                 │
│ Default Resource Limits                         │
│  CPU: 1.0 cores                                 │
│  Memory: 512 MB                                 │
│  Storage: 10 GB                                 │
└─────────────────────────────────────────────────┘
```

#### 2. Cerro Torre Integration
```
┌─────────────────────────────────────────────────┐
│ Cerro Torre CLI Path                            │
│  /usr/local/bin/ct                              │
│                                                 │
│ Default signing key                             │
│  ~/.ct/keys/default.key                         │
│                                                 │
│ Transparency log                                │
│  https://rekor.sigstore.dev                     │
│                                                 │
│ Build defaults                                  │
│  Base image: ghcr.io/hyperpolymath/base:latest  │
│  Compression: zstd                              │
│  Attestation format: in-toto                    │
└─────────────────────────────────────────────────┘
```

#### 3. Svalinn Gateway Settings
```
┌─────────────────────────────────────────────────┐
│ Gateway endpoint                                │
│  http://localhost:8000                          │
│                                                 │
│ Authentication                                  │
│  ● OAuth2  ○ API Key  ○ mTLS                    │
│                                                 │
│ Default policies                                │
│  ☑ Require verified images                      │
│  ☑ Block privileged containers                  │
│  ☑ Enforce resource quotas                      │
│  ☑ Enable audit logging                         │
└─────────────────────────────────────────────────┘
```

#### 4. selur IPC Configuration
```
┌─────────────────────────────────────────────────┐
│ IPC mode                                        │
│  ● Zero-copy WASM  ○ JSON/HTTP                  │
│                                                 │
│ Shared memory size                              │
│  256 MB                                         │
│                                                 │
│ Performance tuning                              │
│  Max throughput: 10,000 req/s                   │
│  Latency target: <1ms                           │
└─────────────────────────────────────────────────┘
```

#### 5. Vörðr Runtime Settings
```
┌─────────────────────────────────────────────────┐
│ Vörðr endpoint                                  │
│  http://localhost:8081                          │
│                                                 │
│ MCP protocol                                    │
│  ● JSON-RPC 2.0  ○ gRPC                         │
│                                                 │
│ Container lifecycle                             │
│  Auto-restart: ☑                                │
│  Max retries: 3                                 │
│  Backoff: exponential                           │
└─────────────────────────────────────────────────┘
```

#### 6. UI Preferences
```
┌─────────────────────────────────────────────────┐
│ Theme                                           │
│  ● System  ○ Light  ○ Dark                      │
│                                                 │
│ Accessibility                                   │
│  Font size: 16px                                │
│  High contrast: ☐                               │
│  Reduced motion: ☐                              │
│  Screen reader mode: ☐                          │
│                                                 │
│ Canvas                                          │
│  Grid snapping: ☑                               │
│  Grid size: 20px                                │
│  Auto-arrange: ☑                                │
│                                                 │
│ [Reset to Defaults] [Save] [Cancel]            │
└─────────────────────────────────────────────────┘
```

## Navigation

### Top Bar
```
┌────────────────────────────────────────────────────────┐
│ 🏔️ stapeln                                            │
│                                                        │
│ [Paragon View] [Cisco View] [Settings] [Export] [Help]│
└────────────────────────────────────────────────────────┘
```

### Cerro Torre Pinnacle (Always Visible)
```
         🏔️
      Cerro Torre
   (Build & Verify)
```

## Supply Chain Visualization (Bottom of Page 1)

```
┌───────────────────────────────────────────────────────┐
│ Supply Chain Provenance                               │
├───────────────────────────────────────────────────────┤
│                                                       │
│  Source Code → Build → Sign → Attest → Verify → Run │
│     ✅           ✅      ✅      ✅        ✅      ✅  │
│                                                       │
│  Transparency Log: rekor.sigstore.dev                │
│  Last verified: 2026-02-05 08:45:23 UTC              │
│  Attestation: in-toto SLSA Level 3                   │
│  SBOM: 347 packages, 0 CVEs                          │
│                                                       │
│  [View Full Provenance] [Verify Signatures]          │
└───────────────────────────────────────────────────────┘
```

## Gap Analysis & Weak Points

**Detection System** (on Page 1 sidebar):

```
┌───────────────────────────────────┐
│ ⚠️ Security Gap Analysis          │
├───────────────────────────────────┤
│ ❌ CRITICAL                       │
│  • No signature verification      │
│  • Privileged container detected  │
│                                   │
│ ⚠️ HIGH                           │
│  • Missing SBOM                   │
│  • No network policy              │
│  • Port 22 exposed (SSH)          │
│                                   │
│ ℹ️ MEDIUM                         │
│  • No resource limits             │
│  • Missing health checks          │
│                                   │
│ 💡 RECOMMENDATIONS                │
│  • Add ct verify step             │
│  • Enable Svalinn gateway         │
│  • Configure network isolation    │
│  • Add resource quotas            │
│                                   │
│ [Auto-Fix] [Dismiss] [Learn More]│
└───────────────────────────────────┘
```

### Weak Points Covered by stapeln:

| Weak Point | Traditional Tools | stapeln Coverage |
|------------|-------------------|------------------|
| **No signature verification** | ❌ docker-compose, podman-compose | ✅ Cerro Torre integration |
| **Missing SBOM** | ❌ Most tools | ✅ Automatic SBOM generation |
| **No transparency log** | ❌ Most tools | ✅ Rekor integration |
| **Insecure defaults** | ❌ Privileged by default | ✅ Secure-by-default configs |
| **No network isolation** | ⚠️ Manual setup | ✅ selur zero-trust networking |
| **Missing attestations** | ❌ Not supported | ✅ in-toto SLSA Level 3 |
| **No policy enforcement** | ❌ Not supported | ✅ Svalinn gateway policies |
| **Port conflicts** | ⚠️ Runtime errors | ✅ Pre-deployment validation |
| **Resource limits** | ⚠️ Manual setup | ✅ Enforced by default |
| **No health checks** | ⚠️ Manual setup | ✅ Auto-generated |
| **Supply chain gaps** | ❌ No visibility | ✅ Full provenance tracking |
| **No rollback** | ⚠️ Manual | ✅ One-click rollback |

## Tauri Desktop App

### Tech Stack
- **Frontend**: ReScript-TEA (same codebase)
- **Backend**: Rust (Tauri)
- **IPC**: Tauri commands
- **Database**: SQLite (local state)

### Platform Support
- ✅ Linux (primary)
- ✅ macOS
- ✅ Windows

### Tauri Configuration
```toml
[tauri]
bundle.identifier = "com.hyperpolymath.stapeln"
bundle.name = "stapeln"
bundle.version = "0.1.0"

[tauri.allowlist]
all = false
fs.scope = ["$HOME/.stapeln/**"]
shell.open = true
```

## Ultimate Container GUI Features

What makes stapeln the **ultimate container GUI**:

1. **Visual Design** (Paragon + Cisco hybrid)
2. **Formal Verification** (Idris2 proofs)
3. **Supply Chain Security** (Cerro Torre built-in)
4. **Gap Analysis** (Auto-detect weak points)
5. **Simulation Mode** (Test before deploy)
6. **Accessibility** (WCAG 2.3 AAA)
7. **Cross-platform** (Tauri desktop app)
8. **Zero-trust Networking** (selur IPC)
9. **Policy Enforcement** (Svalinn gateway)
10. **One-click Rollback** (Time-travel debugging)
11. **Collaborative** (Real-time multi-user editing)
12. **Extensible** (Plugin system for custom components)

## New Weak Points stapeln Addresses

**Supply chain gaps that NO other tool covers**:

1. **Build-time verification** → Cerro Torre ensures builds are reproducible
2. **Transparency logging** → Every action logged to Rekor
3. **Policy as code** → Gatekeeper policies in version control
4. **Zero-copy IPC** → No serialization attacks via selur
5. **Formal proofs** → Mathematical guarantees via Idris2
6. **Attestation chaining** → Full provenance from source to runtime
7. **Resource affinity** → Ephapax ensures resources used exactly once
8. **Dependency ordering** → Topological sort proves correct startup
9. **Type safety** → Linear types prevent use-after-free
10. **Visual security** → Weak points highlighted in UI
11. **Audit trail** → Complete history of all changes
12. **Compliance reports** → Auto-generate SOC2/ISO27001 docs

These are gaps that Docker Compose, Podman Compose, Kubernetes, and even advanced tools like Portainer/Rancher don't fully address.
