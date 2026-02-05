# stapeln Database & Document Integration

## Three Hyperpolymath Components

| Component | Purpose | Technology | stapeln Role |
|-----------|---------|------------|--------------|
| **verisimdb** | Multi-modal database | Elixir + Rust | Primary data store |
| **a2ml** | Attested markup | Idris2 + ReScript | Embedded documentation |
| **k9-svc** | Self-validating config | Nickel | Component definitions |

---

## 1. VeriSimDB - Primary Database

### Why VeriSimDB for stapeln?

**Traditional databases** (PostgreSQL, MongoDB):
- ❌ Single modality (relational OR document OR graph)
- ❌ No federation
- ❌ No semantic reasoning
- ❌ No vector search built-in

**VeriSimDB** advantages:
- ✅ **Six modalities** in one system
- ✅ **Federated** deployment option
- ✅ **Drift detection** across modalities
- ✅ **ZKP integration** (via `proven`)
- ✅ **Tiny core** (<5k LOC coordination)

### stapeln Use Cases

#### Use Case 1: Stack Metadata (Graph + Semantic)

```elixir
# Store stack as RDF graph
VeriSim.insert(%{
  uuid: "stack-abc123",
  modalities: [:graph, :semantic],
  data: %{
    graph: {
      # RDF triples
      {"stack:abc123", "rdf:type", "stapeln:Stack"},
      {"stack:abc123", "stapeln:hasComponent", "comp:nginx"},
      {"comp:nginx", "stapeln:dependsOn", "comp:postgres"},
    },
    semantic: """
      # SPARQL query
      PREFIX stapeln: <https://stapeln.hyperpolymath.dev/>
      SELECT ?component WHERE {
        <stack:abc123> stapeln:hasComponent ?component .
      }
    """
  }
})
```

#### Use Case 2: Component Embeddings (Vector)

```elixir
# Store component similarity vectors
VeriSim.insert(%{
  uuid: "comp:nginx",
  modalities: [:vector],
  data: %{
    vector: {
      embedding: [0.1, 0.2, ..., 0.512],  # 512-dim
      model: "sentence-transformers/all-MiniLM-L6-v2",
      similarity_search: true
    }
  }
})

# Find similar components
VeriSim.query(:vector, %{
  operation: :similarity_search,
  vector: user_query_embedding,
  top_k: 5
})
# Returns: ["nginx", "caddy", "traefik", "envoy", "haproxy"]
```

#### Use Case 3: Validation Results (Temporal + Document)

```elixir
# Store validation history with timestamps
VeriSim.insert(%{
  uuid: "validation-xyz789",
  modalities: [:temporal, :document],
  data: %{
    temporal: {
      timestamp: DateTime.utc_now(),
      event_type: "validation:stack",
      duration_ms: 245,
      valid_from: ~U[2026-02-05 10:00:00Z],
      valid_until: ~U[2026-02-06 10:00:00Z]
    },
    document: {
      result: %{
        valid: true,
        errors: [],
        warnings: ["Port 22 exposed"],
        recommendations: ["Use port 2222 instead"]
      }
    }
  }
})
```

#### Use Case 4: Supply Chain Provenance (Graph + Tensor)

```elixir
# Store provenance graph + attestation tensors
VeriSim.insert(%{
  uuid: "provenance-cerrotorre",
  modalities: [:graph, :tensor],
  data: %{
    graph: {
      # Supply chain graph
      {"image:nginx", "builtBy", "cerro-torre"},
      {"cerro-torre", "signedWith", "key:ed448"},
      {"key:ed448", "belongsTo", "author:jjewell"},
      {"image:nginx", "attestedBy", "rekor:uuid123"}
    },
    tensor: {
      # Attestation matrix (SLSA scores)
      shape: [4, 3],  # 4 stages × 3 metrics
      data: [
        [1.0, 1.0, 0.9],  # Build: reproducible, signed, SBOM
        [1.0, 0.8, 1.0],  # Scan: no CVEs, some warnings, passed
        [1.0, 1.0, 1.0],  # Attest: signed, logged, verified
        [0.9, 1.0, 1.0]   # Deploy: policy passed, audit logged
      ]
    }
  }
})
```

### VeriSimDB Deployment for stapeln

#### Option A: Standalone (Default)

```bash
# All modalities on one server
docker run -p 4001:4001 \
  -e VERISIM_MODE=standalone \
  -e VERISIM_MODALITIES=all \
  verisimdb/verisimdb:latest
```

**Pros**:
- ✅ Simple deployment
- ✅ Low latency (all local)
- ✅ No network coordination

**Cons**:
- ⚠️ Single point of failure
- ⚠️ Limited scale

#### Option B: Federated (Advanced)

```bash
# Coordinator node
docker run -p 4001:4001 \
  -e VERISIM_MODE=federated \
  -e VERISIM_ROLE=coordinator \
  verisimdb/verisimdb:latest

# Store node (institution A - graph only)
docker run -p 4002:4002 \
  -e VERISIM_MODE=federated \
  -e VERISIM_ROLE=store \
  -e VERISIM_MODALITIES=graph \
  verisimdb/verisimdb:latest

# Store node (institution B - vector + tensor)
docker run -p 4003:4003 \
  -e VERISIM_MODE=federated \
  -e VERISIM_ROLE=store \
  -e VERISIM_MODALITIES=vector,tensor \
  verisimdb/verisimdb:latest
```

**Pros**:
- ✅ Distributed across organizations
- ✅ Each institution controls their modalities
- ✅ Global namespace (UUID registry)

**Cons**:
- ⚠️ Network latency
- ⚠️ Coordination complexity

### stapeln ↔ VeriSimDB Integration

```elixir
# backend/lib/stapeln/database.ex
defmodule Stapeln.Database do
  @verisim_url Application.get_env(:stapeln, :verisim_url, "http://localhost:4001")

  def save_stack(stack) do
    uuid = generate_uuid()

    # Store in multiple modalities
    VeriSim.insert(%{
      uuid: uuid,
      modalities: [:graph, :document, :vector, :temporal],
      data: %{
        # Graph: stack topology
        graph: build_stack_graph(stack),

        # Document: full stack config
        document: stack,

        # Vector: for similarity search
        vector: compute_stack_embedding(stack),

        # Temporal: creation timestamp
        temporal: %{
          created_at: DateTime.utc_now(),
          updated_at: DateTime.utc_now()
        }
      }
    })
  end

  def search_similar_stacks(query_embedding, opts \\ []) do
    VeriSim.query(:vector, %{
      operation: :similarity_search,
      vector: query_embedding,
      top_k: Keyword.get(opts, :limit, 10)
    })
  end

  def get_stack_provenance(stack_id) do
    VeriSim.query(:graph, %{
      operation: :sparql,
      query: """
        PREFIX stapeln: <https://stapeln.hyperpolymath.dev/>
        SELECT ?component ?signature ?rekor
        WHERE {
          <stack:#{stack_id}> stapeln:hasComponent ?component .
          ?component stapeln:signedWith ?signature .
          ?signature stapeln:loggedTo ?rekor .
        }
      """
    })
  end
end
```

---

## 2. A2ML - Embedded Documentation

### Why A2ML for stapeln?

**Traditional formats** (Markdown, AsciiDoc):
- ❌ No structural validation
- ❌ No attestation support
- ❌ References can be broken
- ❌ No progressive strictness

**A2ML** advantages:
- ✅ **Typed core** with validation
- ✅ **Progressive strictness** (lax → checked → attested)
- ✅ **Reference resolution** (no broken links)
- ✅ **Opaque payloads** (byte-for-byte preservation)
- ✅ **Idris2 backend** (formal verification)

### stapeln Use Cases

#### Use Case 1: Component Documentation

```a2ml
# Component: nginx
# Mode: checked (requires all sections)

@abstract:
nginx is a high-performance HTTP server and reverse proxy.
It is the web server component in verified container stacks.
@end

@metadata:
id: comp-nginx-001
version: 1.27.0
author: Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
attestation: rekor:abc123
@end

@security:
- Runs as non-root user (nginx:nginx)
- Read-only root filesystem
- Drops all capabilities except CAP_NET_BIND_SERVICE
- No privileged mode
@end

@ports:
- 80/tcp (HTTP)
- 443/tcp (HTTPS)
@end

@volumes:
- /etc/nginx/conf.d (config)
- /var/www/html (static files)
- /var/log/nginx (logs - bind mount)
@end

@dependencies:
[1] → postgres (database backend)
[2] → redis (cache layer)
@end

@validation:
- Health check: HTTP GET /health
- Startup timeout: 30s
- Restart policy: unless-stopped
@end

@refs:
[1] Component: postgres → comp-postgres-001
[2] Component: redis → comp-redis-001
@end
```

**Benefits**:
- ✅ All references validated (links to postgres/redis exist)
- ✅ Required sections enforced (@security, @ports, @volumes)
- ✅ Unique IDs (comp-nginx-001)
- ✅ Can be attested with signature

#### Use Case 2: Stack Templates

```a2ml
# Template: LAMP Stack
# Mode: attested (cryptographically signed)

@abstract:
A classic LAMP stack (Linux, Apache, MySQL, PHP) implemented
with verified containers from Cerro Torre.
@end

@signature:
algorithm: Ed448+Dilithium5
pubkey: AAAAAAA...
signature: BBBBBBB...
rekor: uuid-123
@end

@components:
1. Apache HTTP Server (httpd:2.4.ctp)
2. MySQL Database (mysql:8.0.ctp)
3. PHP-FPM (php:8.3-fpm.ctp)
@end

@topology:
httpd:80 → php-fpm:9000 → mysql:3306
@end

@deployment:
runtime: podman
network: bridge
volumes: mysql-data
@end

@validation:
- All signatures verified: YES
- SBOM present: YES
- CVEs: 0
- SLSA Level: 3
@end

@refs:
[1] Cerro Torre documentation → https://cerro-torre.hyperpolymath.dev
[2] LAMP Stack best practices → https://httpd.apache.org/docs/
@end
```

### stapeln ↔ A2ML Integration

```rescript
// frontend/src/A2MLRenderer.res
// Render A2ML documentation in stapeln UI

type a2mlDocument = {
  abstract: string,
  metadata: Js.Dict.t<string>,
  sections: array<section>,
  references: array<reference>,
  signature: option<signature>
}

let renderA2MLDoc = (doc: a2mlDocument, isDark: bool) => {
  <article
    role="article"
    ariaLabel="Component documentation"
    style={`
      padding: 2rem;
      background-color: ${isDark ? "#000000" : "#FFFFFF"};
      color: ${isDark ? "#FFFFFF" : "#000000"};
    `}>

    // Abstract (always present in checked mode)
    <section>
      <h2>{"Overview" |> React.string}</h2>
      <p>{doc.abstract |> React.string}</p>
    </section>

    // Validation status
    {switch doc.signature {
    | Some(sig) =>
      <div
        role="status"
        style="padding: 1rem; background: #d4edda; border: 1px solid #c3e6cb; border-radius: 4px;">
        <strong>{"✅ Attested Document" |> React.string}</strong>
        <p>{("Signed with " ++ sig.algorithm ++ " by " ++ sig.author) |> React.string}</p>
        <p>
          <a href={"https://rekor.sigstore.dev/api/v1/log/entries/" ++ sig.rekorUuid}>
            {"View in Rekor" |> React.string}
          </a>
        </p>
      </div>
    | None => React.null
    }}

    // Sections
    {Array.map(doc.sections, section =>
      <section key=section.id>
        <h3>{section.title |> React.string}</h3>
        <div dangerouslySetInnerHTML={"__html": section.content} />
      </section>
    ) |> React.array}

    // References (all validated!)
    <section>
      <h3>{"References" |> React.string}</h3>
      <ol>
        {Array.map(doc.references, ref =>
          <li key=ref.id>
            <a href=ref.url ariaLabel=ref.description>
              {ref.description |> React.string}
            </a>
          </li>
        ) |> React.array}
      </ol>
    </section>
  </article>
}
```

---

## 3. K9-SVC - Self-Validating Component Definitions

### Why K9 for stapeln?

**Traditional config** (YAML, JSON):
- ❌ No validation until runtime
- ❌ No security levels
- ❌ No self-execution
- ❌ No attestation

**K9** advantages:
- ✅ **Self-validating** (carries own contracts)
- ✅ **Security levels** (Kennel → Yard → Hunt)
- ✅ **Nickel typing** (strong contracts)
- ✅ **Must-Just-Nickel** triad (portable)
- ✅ **Cryptographic signing** (k9-sign)

### stapeln Use Cases

#### Use Case 1: Component Definition (Kennel Level - Data Only)

```nickel
K9!
# SPDX-License-Identifier: PMPL-1.0-or-later
# Component: nginx verified container
# Security Level: Kennel (pure data)

{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "verified-container",
    security = {
      leash = 'Kennel,  # Data only, no execution
      trust_level = "data-only",
      allow_network = false,
      allow_filesystem_write = false,
    },
  },

  component = {
    name = "nginx",
    version = "1.27.0",
    image = "ghcr.io/hyperpolymath/nginx:1.27.0.ctp",

    signature = {
      algorithm = "Ed448+Dilithium5",
      public_key = "path/to/pubkey.pem",
      signature_file = "nginx-1.27.0.ctp.sig",
      rekor_uuid = "abc123...",
    },

    runtime = {
      engine = "podman",
      rootless = true,
      read_only_root = true,
      no_new_privileges = true,
    },

    ports = [
      { host = 8080, container = 80, protocol = "tcp" },
      { host = 8443, container = 443, protocol = "tcp" },
    ],

    volumes = [
      { host = "./config", container = "/etc/nginx/conf.d", readonly = true },
      { host = "./html", container = "/var/www/html", readonly = false },
    ],

    environment = {
      NGINX_HOST = "example.com",
      NGINX_PORT = "80",
    },

    health_check = {
      test = ["CMD", "curl", "-f", "http://localhost/health"],
      interval = "30s",
      timeout = "10s",
      retries = 3,
    },

    resources = {
      cpu_limit = 1.0,
      memory_limit = "512M",
      pids_limit = 100,
    },

    security = {
      capabilities_drop = ["ALL"],
      capabilities_add = ["CAP_NET_BIND_SERVICE"],
      seccomp_profile = "runtime/default",
      apparmor_profile = "docker-default",
    },
  },
}
```

#### Use Case 2: Stack Deployment (Hunt Level - Full Execution)

```nickel
K9!
# SPDX-License-Identifier: PMPL-1.0-or-later
# Stack: LAMP deployment
# Security Level: Hunt (requires signature)

{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "stack-deployment",
    security = {
      leash = 'Hunt,  # Full execution allowed
      trust_level = "signed-execution",
      require_signature = true,
      allow_network = true,
      allow_filesystem_write = true,
      allow_subprocess = true,
    },
    signature = {
      algorithm = "Ed448+Dilithium5",
      signature = "AAABBBCCC...",
      pubkey_fingerprint = "sha256:abc123...",
      signed_at = "2026-02-05T10:00:00Z",
    },
  },

  # Import component definitions
  components = {
    httpd = import "httpd.k9",
    php = import "php-fpm.k9",
    mysql = import "mysql.k9",
  },

  # Deployment workflow
  deploy = fun components => {
    # Step 1: Verify all signatures
    verify_step = components
      |> std.array.map (fun c => verify_signature c.signature),

    # Step 2: Pull images
    pull_step = verify_step
      |> std.array.map (fun c => cerro_torre_pull c.image),

    # Step 3: Create network
    network_step = create_network {
      name = "lamp-network",
      driver = "bridge",
      subnet = "172.20.0.0/16",
    },

    # Step 4: Deploy in dependency order
    deploy_step = [
      deploy_component components.mysql network_step,
      deploy_component components.php network_step,
      deploy_component components.httpd network_step,
    ],

    # Step 5: Health checks
    health_step = deploy_step
      |> std.array.map (fun c => wait_healthy c 60),

    # Return deployment summary
    {
      status = "success",
      components_deployed = std.array.length deploy_step,
      network = network_step.id,
      health = health_step,
    }
  },

  # Validation contracts
  contracts = {
    # All components must be signed
    all_signed = fun components =>
      std.array.all
        (fun c => c.signature != null)
        components,

    # No privileged containers
    no_privileged = fun components =>
      std.array.all
        (fun c => c.runtime.privileged == false)
        components,

    # Resource limits enforced
    has_resource_limits = fun components =>
      std.array.all
        (fun c => c.resources.cpu_limit != null && c.resources.memory_limit != null)
        components,
  },
}
```

### stapeln ↔ K9 Integration

```elixir
# backend/lib/stapeln/k9_validator.ex
defmodule Stapeln.K9Validator do
  @doc """
  Validate and execute a K9 component
  """
  def validate_and_execute(k9_file_path) do
    with {:ok, content} <- File.read(k9_file_path),
         {:ok, parsed} <- parse_k9(content),
         :ok <- validate_security_level(parsed),
         :ok <- verify_signature(parsed),
         {:ok, result} <- execute_k9(parsed) do
      {:ok, result}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp validate_security_level(%{pedigree: %{security: %{leash: leash}}}) do
    case leash do
      "Kennel" -> :ok  # Data only, always safe
      "Yard" -> :ok    # Nickel eval only, no I/O
      "Hunt" -> validate_hunt_requirements()  # Requires signature
    end
  end

  defp verify_signature(%{pedigree: %{signature: nil}}), do: :ok  # Kennel/Yard don't need sigs

  defp verify_signature(%{pedigree: %{signature: sig}}) do
    # Call k9-sign via Port
    case System.cmd("k9-sign", ["verify", "--signature", sig.signature, "--pubkey", sig.pubkey_fingerprint]) do
      {_output, 0} -> :ok
      {error, _} -> {:error, "Signature verification failed: #{error}"}
    end
  end

  defp execute_k9(%{pedigree: %{security: %{leash: "Kennel"}}}) do
    # Kennel: just return data, no execution
    {:ok, "data-only"}
  end

  defp execute_k9(%{pedigree: %{security: %{leash: "Yard"}}} = parsed) do
    # Yard: evaluate Nickel contracts
    case System.cmd("nickel", ["export", "--format", "json"], input: Jason.encode!(parsed)) do
      {output, 0} -> {:ok, Jason.decode!(output)}
      {error, _} -> {:error, "Nickel evaluation failed: #{error}"}
    end
  end

  defp execute_k9(%{pedigree: %{security: %{leash: "Hunt"}}} = parsed) do
    # Hunt: full execution via Must-Just-Nickel triad
    case System.cmd("must", ["run", "--k9", Jason.encode!(parsed)]) do
      {output, 0} -> {:ok, output}
      {error, _} -> {:error, "Hunt execution failed: #{error}"}
    end
  end
end
```

---

## Integration Architecture

```
┌──────────────────────────────────────────────────────────┐
│ stapeln (ReScript-TEA + Tauri)                           │
│  ├─ Drag-and-drop UI                                     │
│  ├─ A2ML documentation renderer                          │
│  └─ K9 component importer                                │
└────────────┬─────────────────────────────────────────────┘
             │ GraphQL + WebSocket
             ↓
┌──────────────────────────────────────────────────────────┐
│ Backend (Elixir Phoenix)                                 │
│  ├─ GraphQL API (Absinthe)                               │
│  ├─ K9 validator (Nickel + k9-sign)                      │
│  └─ A2ML parser (Idris2 FFI)                             │
└────────────┬─────────────────────────────────────────────┘
             │ HTTP/3 + QUIC
             ↓
┌──────────────────────────────────────────────────────────┐
│ VeriSimDB (Elixir + Rust)                                │
│  ├─ Graph (RDF triples)                                  │
│  ├─ Vector (embeddings)                                  │
│  ├─ Tensor (attestations)                                │
│  ├─ Semantic (SPARQL)                                    │
│  ├─ Document (JSON)                                      │
│  └─ Temporal (timestamps)                                │
└──────────────────────────────────────────────────────────┘
```

---

## File Organization

```
stapeln/
├── backend/
│   ├── lib/stapeln/
│   │   ├── database.ex          # VeriSimDB client
│   │   ├── k9_validator.ex      # K9 validation
│   │   └── a2ml_parser.ex       # A2ML parsing
│   └── priv/
│       ├── a2ml/                # A2ML docs
│       │   ├── nginx.a2ml
│       │   ├── postgres.a2ml
│       │   └── templates/
│       └── k9/                  # K9 components
│           ├── nginx.k9
│           ├── postgres.k9
│           └── stacks/
│               └── lamp.k9
├── docs/                        # User documentation (A2ML format)
│   ├── ARCHITECTURE.a2ml
│   ├── COMPONENTS.a2ml
│   └── TEMPLATES.a2ml
└── config/
    └── verisimdb.exs            # VeriSimDB connection config
```

---

## Summary

| Component | Role in stapeln | Integration Point |
|-----------|----------------|-------------------|
| **verisimdb** | Primary database for all stack metadata, provenance, and validation results | Elixir backend via HTTP |
| **a2ml** | Component documentation with attestation and validation | ReScript frontend + Idris2 backend |
| **k9-svc** | Self-validating component definitions with security levels | Elixir backend via Nickel + Must |

**Benefits**:
- ✅ Formal verification at every layer (Idris2 proofs in a2ml, Nickel contracts in k9, Idris2 proofs in validation)
- ✅ Multi-modal database (graph + vector + semantic + tensor + document + temporal)
- ✅ Self-validating components (k9 carries its own validation)
- ✅ Attested documentation (a2ml with cryptographic signatures)
- ✅ Federation-ready (verisimdb can federate across institutions)
- ✅ All hyperpolymath projects (consistent ecosystem)
