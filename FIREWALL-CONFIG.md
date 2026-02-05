# stapeln Firewall Configuration & Security

**Status**: Design specification for OWASP-compliant firewall with ephemeral pinholes

## Executive Summary

stapeln implements defense-in-depth security with:
1. **OWASP ModSecurity** - Web application firewall at gateway (Svalinn)
2. **Default-deny firewall** - Whitelist-only approach (firewalld/nftables)
3. **Ephemeral pinholes** - Temporary port openings with auto-expiry
4. **User-only access** - Strong authentication for stapeln tool itself
5. **Visual port control** - Simple UI, no CLI commands needed

## Design Principle

> "I want this port closed" → Click to close
> "Ephemeral on/off" → Toggle switch
> **Zero long docker commands**

---

## 1. OWASP ModSecurity Core Rule Set (CRS)

### Location
**Svalinn** (edge gateway) runs ModSecurity as reverse proxy

### Configuration

```nginx
# svalinn/modsecurity.conf
# SPDX-License-Identifier: PMPL-1.0-or-later

SecRuleEngine On
SecRequestBodyAccess On
SecResponseBodyAccess On

# Load OWASP CRS v4.0
Include /etc/modsecurity/coreruleset/*.conf

# Paranoia level 3 (high security for container management)
SecAction "id:900000,phase:1,nolog,pass,t:none,setvar:tx.paranoia_level=3"

# Anomaly scoring mode (recommended)
SecAction "id:900110,phase:1,pass,nolog,setvar:tx.inbound_anomaly_score_threshold=5"
SecAction "id:900120,phase:1,pass,nolog,setvar:tx.outbound_anomaly_score_threshold=4"

# Block known container exploit patterns
SecRule REQUEST_URI "@rx /\.\./" \
    "id:1000,phase:1,deny,status:403,msg:'Path traversal attack'"

SecRule REQUEST_URI "@rx docker|kubectl|podman" \
    "id:1001,phase:1,deny,status:403,msg:'Direct container CLI blocked'"

# Allow stapeln GraphQL endpoint
SecRule REQUEST_URI "@streq /graphql" \
    "id:1002,phase:1,pass,ctl:ruleRemoveById=942100"
```

### Rule Categories (OWASP CRS)

| Category | Rule ID Range | Purpose |
|----------|---------------|---------|
| **REQUEST-901** | Protocol enforcement | HTTP RFC compliance |
| **REQUEST-920** | Protocol attack | HTTP smuggling, response splitting |
| **REQUEST-930** | Application attack | LFI, RFI, RCE |
| **REQUEST-931** | Application attack | PHP injection, Node.js attack |
| **REQUEST-932** | Application attack | RCE (container escape vectors) |
| **REQUEST-941** | XSS attack | Script injection |
| **REQUEST-942** | SQLi attack | SQL injection (GraphQL protected) |
| **REQUEST-943** | Session fixation | Session hijacking |

### Stapeln-Specific Rules

```nginx
# Custom ruleset: stapeln-rules.conf
# Block direct container runtime access
SecRule REQUEST_URI "@rx ^/v[0-9.]+/(containers|images|networks)" \
    "id:2000,phase:1,deny,status:403,msg:'Direct Docker API blocked - use stapeln UI'"

# Require authentication token
SecRule &REQUEST_HEADERS:Authorization "@eq 0" \
    "id:2001,phase:1,deny,status:401,msg:'Authentication required',chain"
SecRule REQUEST_URI "!@streq /login"

# Rate limiting (10 requests/second per IP)
SecAction "id:2002,phase:5,pass,setvar:ip.requests=+1,expirevar:ip.requests=1"
SecRule IP:REQUESTS "@gt 10" \
    "id:2003,phase:1,deny,status:429,msg:'Rate limit exceeded'"

# Block container breakout attempts
SecRule ARGS "@rx \\.\\./|/proc/|/sys/|/dev/" \
    "id:2004,phase:2,deny,status:403,msg:'Container escape attempt'"
```

---

## 2. Firewalld/nftables Default-Deny Rules

### Philosophy
**Whitelist-only**: Everything blocked by default, explicit allow rules only

### Implementation

#### firewalld (Fedora/RHEL)

```bash
# stapeln-firewall-setup.sh
# SPDX-License-Identifier: PMPL-1.0-or-later

#!/bin/bash
set -euo pipefail

# Create stapeln zone (default-deny)
firewall-cmd --permanent --new-zone=stapeln
firewall-cmd --permanent --zone=stapeln --set-target=DROP

# Allow loopback (required for localhost communication)
firewall-cmd --permanent --zone=stapeln --add-interface=lo

# Allow stapeln UI (localhost only)
firewall-cmd --permanent --zone=stapeln --add-rich-rule='
  rule family="ipv4"
  source address="127.0.0.1"
  port protocol="tcp" port="8000"
  accept'

# Allow Svalinn gateway (authenticated users only)
firewall-cmd --permanent --zone=stapeln --add-rich-rule='
  rule family="ipv4"
  source address="127.0.0.1"
  port protocol="tcp" port="8443"
  accept'

# Block all container runtime ports by default
# Docker (2375/2376), Podman (8080), containerd (2379/2380)
firewall-cmd --permanent --zone=stapeln --add-rich-rule='
  rule family="ipv4"
  port protocol="tcp" port="2375-2380"
  reject type="icmp-port-unreachable"'

# Reload
firewall-cmd --reload
```

#### nftables (modern alternative)

```nft
# /etc/nftables/stapeln.nft
# SPDX-License-Identifier: PMPL-1.0-or-later

table inet stapeln {
  # Default-deny policy
  chain input {
    type filter hook input priority 0; policy drop;

    # Allow established connections
    ct state established,related accept

    # Allow loopback
    iif lo accept

    # Allow stapeln UI (localhost only)
    tcp dport 8000 ip saddr 127.0.0.1 accept

    # Allow Svalinn (localhost only)
    tcp dport 8443 ip saddr 127.0.0.1 accept

    # Drop everything else
    counter drop
  }

  chain forward {
    type filter hook forward priority 0; policy drop;
  }

  chain output {
    type filter hook output priority 0; policy accept;
  }
}
```

### Verification

```bash
# Check active rules
sudo firewall-cmd --zone=stapeln --list-all

# Test blocked port
curl -v http://localhost:2375  # Should fail

# Test allowed port (stapeln UI)
curl -v http://localhost:8000  # Should succeed (with auth)
```

---

## 3. Ephemeral Pinholes

### Concept
**Ephemeral pinhole** = Temporary firewall rule that auto-expires after N seconds/minutes

### Use Cases
- Testing new container port (open 8080 for 5 minutes)
- Temporary external access (database migration)
- Debugging (allow SSH for 10 minutes)

### Implementation Architecture

```
┌─────────────────────────────────────────────────┐
│ stapeln UI (Page 2 - Cisco View)               │
│ [nginx] → Port 8080 [🔓 Ephemeral: 5m ▼]      │
└────────────────┬────────────────────────────────┘
                 │ GraphQL Mutation
                 ▼
┌─────────────────────────────────────────────────┐
│ Phoenix Backend (Elixir)                        │
│ EphemeralPinhole.open(port, duration, source)   │
└────────────────┬────────────────────────────────┘
                 │ GenServer schedule
                 ▼
┌─────────────────────────────────────────────────┐
│ Firewall Controller (Elixir)                    │
│ - Add firewall rule                             │
│ - Schedule auto-removal                         │
│ - Log to audit trail                            │
└─────────────────────────────────────────────────┘
```

### Elixir Implementation

```elixir
# backend/lib/stapeln/ephemeral_pinhole.ex
# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Stapeln.EphemeralPinhole do
  @moduledoc """
  Manages ephemeral firewall pinholes with auto-expiry.

  Features:
  - Time-limited port openings (30s to 24h)
  - Source IP restrictions
  - Automatic cleanup
  - Audit logging
  """

  use GenServer
  require Logger

  @max_duration 86400  # 24 hours
  @min_duration 30     # 30 seconds

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  @doc """
  Open a temporary firewall pinhole.

  ## Examples

      iex> EphemeralPinhole.open(8080, 300, "192.168.1.100")
      {:ok, "pinhole-abc123"}
  """
  def open(port, duration_seconds, source_ip \\ "any") do
    GenServer.call(__MODULE__, {:open, port, duration_seconds, source_ip})
  end

  @doc """
  Close a pinhole immediately.
  """
  def close(pinhole_id) do
    GenServer.call(__MODULE__, {:close, pinhole_id})
  end

  @doc """
  List all active pinholes.
  """
  def list_active do
    GenServer.call(__MODULE__, :list_active)
  end

  # Server callbacks

  @impl true
  def init(_state) do
    # Load active pinholes from database on startup
    pinholes = Stapeln.Repo.all(Stapeln.Schema.Pinhole)

    state = %{
      pinholes: Map.new(pinholes, fn p -> {p.id, p} end)
    }

    # Reschedule existing pinholes
    Enum.each(pinholes, fn pinhole ->
      schedule_close(pinhole.id, pinhole.expires_at)
    end)

    {:ok, state}
  end

  @impl true
  def handle_call({:open, port, duration, source_ip}, _from, state) do
    with :ok <- validate_duration(duration),
         :ok <- validate_port(port),
         :ok <- check_port_not_used(port) do

      pinhole_id = generate_id()
      expires_at = DateTime.utc_now() |> DateTime.add(duration, :second)

      pinhole = %{
        id: pinhole_id,
        port: port,
        source_ip: source_ip,
        opened_at: DateTime.utc_now(),
        expires_at: expires_at,
        opened_by: get_current_user()
      }

      # Add firewall rule
      case add_firewall_rule(port, source_ip) do
        :ok ->
          # Save to database
          {:ok, _} = Stapeln.Repo.insert(struct(Stapeln.Schema.Pinhole, pinhole))

          # Schedule auto-close
          schedule_close(pinhole_id, expires_at)

          # Audit log
          Logger.info("Ephemeral pinhole opened",
            pinhole_id: pinhole_id,
            port: port,
            duration: duration,
            source_ip: source_ip
          )

          new_state = put_in(state.pinholes[pinhole_id], pinhole)
          {:reply, {:ok, pinhole_id}, new_state}

        {:error, reason} ->
          {:reply, {:error, reason}, state}
      end
    else
      {:error, reason} -> {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call({:close, pinhole_id}, _from, state) do
    case Map.get(state.pinholes, pinhole_id) do
      nil ->
        {:reply, {:error, :not_found}, state}

      pinhole ->
        # Remove firewall rule
        :ok = remove_firewall_rule(pinhole.port, pinhole.source_ip)

        # Remove from database
        Stapeln.Repo.delete_by_id(Stapeln.Schema.Pinhole, pinhole_id)

        # Audit log
        Logger.info("Ephemeral pinhole closed",
          pinhole_id: pinhole_id,
          port: pinhole.port
        )

        new_state = %{state | pinholes: Map.delete(state.pinholes, pinhole_id)}
        {:reply, :ok, new_state}
    end
  end

  @impl true
  def handle_call(:list_active, _from, state) do
    active = Map.values(state.pinholes)
    {:reply, active, state}
  end

  @impl true
  def handle_info({:auto_close, pinhole_id}, state) do
    # Auto-close expired pinhole
    case close(pinhole_id) do
      :ok ->
        Logger.info("Ephemeral pinhole auto-closed", pinhole_id: pinhole_id)

      {:error, :not_found} ->
        # Already closed, ignore
        :ok
    end

    {:noreply, state}
  end

  # Private functions

  defp validate_duration(duration) when duration < @min_duration do
    {:error, "Duration too short (min: #{@min_duration}s)"}
  end

  defp validate_duration(duration) when duration > @max_duration do
    {:error, "Duration too long (max: #{@max_duration}s)"}
  end

  defp validate_duration(_), do: :ok

  defp validate_port(port) when port < 1 or port > 65535 do
    {:error, "Invalid port number"}
  end

  defp validate_port(_), do: :ok

  defp check_port_not_used(port) do
    # Check if port already has a pinhole or permanent rule
    case :inet.getservbyport(port, :tcp) do
      {:error, :einval} -> :ok  # Port not in use
      _ -> {:error, "Port already in use"}
    end
  end

  defp add_firewall_rule(port, source_ip) do
    rule = if source_ip == "any" do
      """
      rule family="ipv4"
      port protocol="tcp" port="#{port}"
      accept
      """
    else
      """
      rule family="ipv4"
      source address="#{source_ip}"
      port protocol="tcp" port="#{port}"
      accept
      """
    end

    case System.cmd("firewall-cmd", ["--add-rich-rule=#{rule}"], stderr_to_stdout: true) do
      {_, 0} -> :ok
      {error, _} -> {:error, "Firewall error: #{error}"}
    end
  end

  defp remove_firewall_rule(port, source_ip) do
    rule = if source_ip == "any" do
      """
      rule family="ipv4"
      port protocol="tcp" port="#{port}"
      accept
      """
    else
      """
      rule family="ipv4"
      source address="#{source_ip}"
      port protocol="tcp" port="#{port}"
      accept
      """
    end

    case System.cmd("firewall-cmd", ["--remove-rich-rule=#{rule}"], stderr_to_stdout: true) do
      {_, 0} -> :ok
      {error, _} -> {:error, "Firewall error: #{error}"}
    end
  end

  defp schedule_close(pinhole_id, expires_at) do
    now = DateTime.utc_now()
    delay_ms = DateTime.diff(expires_at, now, :millisecond)

    if delay_ms > 0 do
      Process.send_after(self(), {:auto_close, pinhole_id}, delay_ms)
    else
      # Already expired, close immediately
      send(self(), {:auto_close, pinhole_id})
    end
  end

  defp generate_id do
    "pinhole-" <> (:crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower))
  end

  defp get_current_user do
    # Get from session context
    "user-placeholder"  # TODO: implement auth
  end
end
```

### Database Schema

```elixir
# backend/lib/stapeln/schema/pinhole.ex
# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Stapeln.Schema.Pinhole do
  use Ecto.Schema

  @primary_key {:id, :string, []}
  schema "ephemeral_pinholes" do
    field :port, :integer
    field :source_ip, :string
    field :opened_at, :utc_datetime
    field :expires_at, :utc_datetime
    field :opened_by, :string
    field :component_id, :string  # Which container this is for

    timestamps()
  end
end
```

### Migration

```elixir
# backend/priv/repo/migrations/20260205000001_create_ephemeral_pinholes.exs
# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Stapeln.Repo.Migrations.CreateEphemeralPinholes do
  use Ecto.Migration

  def change do
    create table(:ephemeral_pinholes, primary_key: false) do
      add :id, :string, primary_key: true
      add :port, :integer, null: false
      add :source_ip, :string, null: false
      add :opened_at, :utc_datetime, null: false
      add :expires_at, :utc_datetime, null: false
      add :opened_by, :string, null: false
      add :component_id, :string

      timestamps()
    end

    create index(:ephemeral_pinholes, [:expires_at])
    create index(:ephemeral_pinholes, [:port])
  end
end
```

### GraphQL API

```graphql
# backend/schema/firewall.graphql
# SPDX-License-Identifier: PMPL-1.0-or-later

type EphemeralPinhole {
  id: ID!
  port: Int!
  sourceIp: String!
  openedAt: DateTime!
  expiresAt: DateTime!
  openedBy: String!
  componentId: String
  remainingSeconds: Int!
}

input OpenPinholeInput {
  port: Int!
  durationSeconds: Int!
  sourceIp: String
  componentId: String
}

type Mutation {
  openEphemeralPinhole(input: OpenPinholeInput!): EphemeralPinhole!
  closeEphemeralPinhole(pinholeId: ID!): Boolean!
}

type Query {
  listActivePinholes: [EphemeralPinhole!]!
  getPinhole(id: ID!): EphemeralPinhole
}
```

---

## 4. User-Only Access Control

### Requirements
- Only the logged-in user can access stapeln
- No remote access (localhost only by default)
- Session-based authentication
- Optional: Biometric auth (fingerprint, face recognition)

### Authentication Strategy

```elixir
# backend/lib/stapeln/auth.ex
# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Stapeln.Auth do
  @moduledoc """
  User-only authentication for stapeln.

  Security model:
  - Localhost-only by default
  - Session tokens (httpOnly cookies)
  - Optional: System user verification (PAM)
  - Optional: Biometric auth
  """

  @session_ttl 3600  # 1 hour

  def authenticate_user(username, password) do
    # Verify against system user
    case verify_system_user(username, password) do
      :ok ->
        token = generate_session_token(username)
        {:ok, token}

      {:error, reason} ->
        {:error, reason}
    end
  end

  def verify_session(token) do
    case Stapeln.SessionStore.get(token) do
      nil -> {:error, :invalid_token}
      session -> {:ok, session.user}
    end
  end

  defp verify_system_user(username, password) do
    # Use PAM (Pluggable Authentication Modules)
    case System.cmd("pamtester", ["stapeln", username, "authenticate"], input: password) do
      {_, 0} -> :ok
      _ -> {:error, :invalid_credentials}
    end
  end

  defp generate_session_token(username) do
    token = :crypto.strong_rand_bytes(32) |> Base.url_encode64()

    session = %{
      token: token,
      user: username,
      created_at: DateTime.utc_now(),
      expires_at: DateTime.utc_now() |> DateTime.add(@session_ttl, :second)
    }

    Stapeln.SessionStore.put(token, session)
    token
  end
end
```

### PAM Configuration

```
# /etc/pam.d/stapeln
# SPDX-License-Identifier: PMPL-1.0-or-later

auth    required    pam_unix.so
account required    pam_unix.so
```

### Frontend Auth Flow

```rescript
// frontend/src/Auth.res
// SPDX-License-Identifier: PMPL-1.0-or-later

type authState =
  | NotAuthenticated
  | Authenticating
  | Authenticated({username: string, token: string})
  | AuthError(string)

type loginMsg =
  | LoginSubmit(string, string)
  | LoginResponse(result<string, string>)
  | Logout

let updateLogin = (state: authState, msg: loginMsg): (authState, effect<loginMsg>) => {
  switch msg {
  | LoginSubmit(username, password) =>
    let effect = async {
      try {
        let response = await fetch("/api/auth/login", {
          method: "POST",
          headers: {"Content-Type": "application/json"},
          body: JSON.stringify({"username": username, "password": password})
        })

        if response.ok {
          let data = await response.json()
          Ok(data.token)
        } else {
          Error("Invalid credentials")
        }
      } catch {
      | _ => Error("Network error")
      }
    }

    (Authenticating, effect)

  | LoginResponse(Ok(token)) =>
    // Store token in httpOnly cookie (handled by server)
    (Authenticated({username: "current-user", token: token}), None)

  | LoginResponse(Error(err)) =>
    (AuthError(err), None)

  | Logout =>
    // Clear session
    let effect = async {
      await fetch("/api/auth/logout", {method: "POST"})
      LoginResponse(Error("Logged out"))
    }
    (NotAuthenticated, effect)
  }
}

let renderLogin = (state: authState, dispatch: loginMsg => unit) => {
  switch state {
  | NotAuthenticated | AuthError(_) =>
    <div className="login-screen" role="main" ariaLabel="Login to stapeln">
      <h1>{"stapeln" |> React.string}</h1>
      <p>{"Visual Container Stack Designer" |> React.string}</p>

      <form onSubmit={e => {
        e->preventDefault
        let formData = e->target->FormData.new
        let username = formData->get("username")
        let password = formData->get("password")
        dispatch(LoginSubmit(username, password))
      }}>
        <label htmlFor="username">{"Username" |> React.string}</label>
        <input
          type_="text"
          id="username"
          name="username"
          required=true
          autoComplete="username"
          ariaLabel="Enter your system username"
        />

        <label htmlFor="password">{"Password" |> React.string}</label>
        <input
          type_="password"
          id="password"
          name="password"
          required=true
          autoComplete="current-password"
          ariaLabel="Enter your password"
        />

        <button type_="submit">
          {"Log In" |> React.string}
        </button>
      </form>

      {switch state {
      | AuthError(err) =>
        <div className="error" role="alert">
          {err |> React.string}
        </div>
      | _ => React.null
      }}
    </div>

  | Authenticating =>
    <div className="loading" role="status">
      <p>{"Authenticating..." |> React.string}</p>
    </div>

  | Authenticated(_) =>
    // Render main app
    React.null
  }
}
```

---

## 5. Simple Port Configuration UI

### Design Mockup (Page 2 - Cisco View)

```
┌────────────────────────────────────────────────────────────┐
│ stapeln - Stack Designer (Cisco View)                      │
├────────────────────────────────────────────────────────────┤
│                                                             │
│   ┌───────────┐            ┌───────────┐                  │
│   │  nginx    │───────────→│ postgres  │                  │
│   │           │            │           │                  │
│   │  :80 🔒   │            │  :5432 🔒 │                  │
│   │  :443 🔒  │            │           │                  │
│   └───────────┘            └───────────┘                  │
│                                                             │
│                                                             │
│ ┌─────────────────────────────────────────────────────┐   │
│ │ Selected: nginx                                      │   │
│ ├─────────────────────────────────────────────────────┤   │
│ │ Port Configuration                                   │   │
│ │                                                       │   │
│ │ Port 80 (HTTP)                                       │   │
│ │   ● Closed  ○ Open  ○ Ephemeral                    │   │
│ │                                                       │   │
│ │ Port 443 (HTTPS)                                     │   │
│ │   ○ Closed  ● Open  ○ Ephemeral                    │   │
│ │                                                       │   │
│ │ Port 8080 (Custom)                                   │   │
│ │   ○ Closed  ○ Open  ● Ephemeral [5 min ▼]          │   │
│ │                      Expires: 4:32 remaining         │   │
│ │                                                       │   │
│ │ [Add Port] [Security Scan]                           │   │
│ └─────────────────────────────────────────────────────┘   │
└────────────────────────────────────────────────────────────┘
```

### ReScript Implementation

```rescript
// frontend/src/PortConfigPanel.res
// SPDX-License-Identifier: PMPL-1.0-or-later

type portState = Closed | Open | Ephemeral({duration: int, expiresAt: DateTime.t})

type port = {
  number: int,
  protocol: string,
  state: portState,
  description: option<string>,
}

type portMsg =
  | ChangePortState(int, portState)
  | AddPort(int, string)
  | RemovePort(int)
  | UpdateEphemeralDuration(int, int)

let renderPortConfig = (ports: array<port>, dispatch: portMsg => unit) => {
  <section className="port-config-panel" role="region" ariaLabel="Port configuration">
    <h3>{"Port Configuration" |> React.string}</h3>

    {ports
     ->Array.map(port => {
       <div key={port.number->Int.toString} className="port-row">
         <label>
           {`Port ${port.number->Int.toString} (${port.protocol})` |> React.string}
         </label>

         <div className="port-state-selector" role="radiogroup" ariaLabel={`Port ${port.number->Int.toString} state`}>
           // Closed
           <label>
             <input
               type_="radio"
               name={`port-${port.number->Int.toString}`}
               value="closed"
               checked={port.state == Closed}
               onChange={_ => dispatch(ChangePortState(port.number, Closed))}
               ariaLabel="Close this port"
             />
             {"Closed 🔒" |> React.string}
           </label>

           // Open
           <label>
             <input
               type_="radio"
               name={`port-${port.number->Int.toString}`}
               value="open"
               checked={port.state == Open}
               onChange={_ => dispatch(ChangePortState(port.number, Open))}
               ariaLabel="Keep this port open permanently"
             />
             {"Open 🔓" |> React.string}
           </label>

           // Ephemeral
           <label>
             <input
               type_="radio"
               name={`port-${port.number->Int.toString}`}
               value="ephemeral"
               checked={switch port.state {
                 | Ephemeral(_) => true
                 | _ => false
               }}
               onChange={_ => dispatch(ChangePortState(port.number, Ephemeral({
                 duration: 300,  // 5 minutes default
                 expiresAt: DateTime.now()->DateTime.add(300, Second)
               })))}
               ariaLabel="Open this port temporarily"
             />
             {"Ephemeral ⏱️" |> React.string}
           </label>

           {switch port.state {
           | Ephemeral({duration, expiresAt}) =>
             <div className="ephemeral-controls">
               <select
                 value={duration->Int.toString}
                 onChange={e => {
                   let newDuration = e->target->value->Int.fromString->Option.getExn
                   dispatch(UpdateEphemeralDuration(port.number, newDuration))
                 }}
                 ariaLabel="Ephemeral duration">
                 <option value="30">{"30 seconds" |> React.string}</option>
                 <option value="60">{"1 minute" |> React.string}</option>
                 <option value="300">{"5 minutes" |> React.string}</option>
                 <option value="600">{"10 minutes" |> React.string}</option>
                 <option value="1800">{"30 minutes" |> React.string}</option>
                 <option value="3600">{"1 hour" |> React.string}</option>
               </select>

               <span className="countdown" role="timer">
                 {`Expires: ${formatCountdown(expiresAt)}` |> React.string}
               </span>
             </div>
           | _ => React.null
           }}
         </div>

         {switch port.description {
         | Some(desc) => <p className="port-description">{desc |> React.string}</p>
         | None => React.null
         }}
       </div>
     })
     ->React.array}

    <button onClick={_ => /* Open add port dialog */}>
      {"+ Add Port" |> React.string}
    </button>
  </section>
}

let formatCountdown = (expiresAt: DateTime.t): string => {
  let now = DateTime.now()
  let diff = DateTime.diff(expiresAt, now, Second)

  if diff <= 0 {
    "Expired"
  } else if diff < 60 {
    `${diff->Int.toString}s remaining`
  } else if diff < 3600 {
    let mins = diff / 60
    let secs = diff mod 60
    `${mins->Int.toString}:${secs->Int.toString->String.padStart(2, "0")} remaining`
  } else {
    let hours = diff / 3600
    let mins = (diff mod 3600) / 60
    `${hours->Int.toString}h ${mins->Int.toString}m remaining`
  }
}
```

### Real-Time Countdown

```rescript
// Update countdown every second
let useCountdownEffect = (ports: array<port>) => {
  React.useEffect1(() => {
    let interval = setInterval(() => {
      // Force re-render to update countdown
      forceUpdate()
    }, 1000)

    Some(() => clearInterval(interval))
  }, [ports])
}
```

---

## 6. Component Security Inspector

### Security Status Display

```rescript
// frontend/src/SecurityInspector.res
// SPDX-License-Identifier: PMPL-1.0-or-later

type securityLevel = Critical | High | Medium | Low | Safe

type securityIssue = {
  level: securityLevel,
  category: string,
  description: string,
  cve: option<string>,
  fix: option<string>,
}

type componentSecurity = {
  componentId: string,
  overallLevel: securityLevel,
  issues: array<securityIssue>,
  exposedPorts: array<int>,
  runningAsRoot: bool,
  hasHealthCheck: bool,
  signatureVerified: bool,
  sbomPresent: bool,
}

let renderSecurityInspector = (security: componentSecurity) => {
  <aside className="security-inspector" role="complementary" ariaLabel="Security analysis">
    <h3>{"Security Status" |> React.string}</h3>

    // Overall security badge
    <div className={`security-badge security-${securityLevelToString(security.overallLevel)}`}
         role="status"
         ariaLabel={`Security level: ${securityLevelToString(security.overallLevel)}`}>
      {securityLevelIcon(security.overallLevel)}
      {securityLevelToString(security.overallLevel) |> React.string}
    </div>

    // Quick checks
    <div className="security-checks">
      <div className="check-item">
        {security.signatureVerified ? "✅" : "❌" |> React.string}
        {" Signature verified" |> React.string}
      </div>

      <div className="check-item">
        {security.sbomPresent ? "✅" : "❌" |> React.string}
        {" SBOM present" |> React.string}
      </div>

      <div className="check-item">
        {!security.runningAsRoot ? "✅" : "❌" |> React.string}
        {" Non-root user" |> React.string}
      </div>

      <div className="check-item">
        {security.hasHealthCheck ? "✅" : "❌" |> React.string}
        {" Health check configured" |> React.string}
      </div>
    </div>

    // Exposed ports
    <section className="exposed-ports">
      <h4>{"Exposed Ports" |> React.string}</h4>
      {if Array.length(security.exposedPorts) == 0 {
        <p className="safe">{"No ports exposed" |> React.string}</p>
      } else {
        <ul>
          {security.exposedPorts
           ->Array.map(port => {
             <li key={port->Int.toString}>
               {`Port ${port->Int.toString}` |> React.string}
               {getPortRisk(port) |> renderRiskBadge}
             </li>
           })
           ->React.array}
        </ul>
      }}
    </section>

    // Issues list
    {if Array.length(security.issues) > 0 {
      <section className="security-issues">
        <h4>{"Security Issues" |> React.string}</h4>
        {security.issues
         ->Array.map((issue, idx) => {
           <div key={idx->Int.toString} className={`issue issue-${securityLevelToString(issue.level)}`}>
             <div className="issue-header">
               {securityLevelIcon(issue.level)}
               <strong>{issue.category |> React.string}</strong>
               {switch issue.cve {
               | Some(cve) => <span className="cve">{cve |> React.string}</span>
               | None => React.null
               }}
             </div>

             <p className="issue-description">{issue.description |> React.string}</p>

             {switch issue.fix {
             | Some(fix) =>
               <button className="fix-button" ariaLabel="Apply automatic fix">
                 {"🔧 " ++ fix |> React.string}
               </button>
             | None => React.null
             }}
           </div>
         })
         ->React.array}
      </section>
    } else {
      <p className="safe">{"✅ No security issues detected" |> React.string}</p>
    }}
  </aside>
}

let securityLevelIcon = (level: securityLevel): string => {
  switch level {
  | Critical => "🔴"
  | High => "🟠"
  | Medium => "🟡"
  | Low => "🟢"
  | Safe => "✅"
  }
}

let securityLevelToString = (level: securityLevel): string => {
  switch level {
  | Critical => "critical"
  | High => "high"
  | Medium => "medium"
  | Low => "low"
  | Safe => "safe"
  }
}

let getPortRisk = (port: int): securityLevel => {
  // Common vulnerable ports
  if port == 22 { High }        // SSH
  else if port == 23 { Critical }  // Telnet
  else if port == 3389 { High }    // RDP
  else if port < 1024 { Medium }   // Privileged ports
  else { Low }
}

let renderRiskBadge = (level: securityLevel) => {
  <span className={`risk-badge risk-${securityLevelToString(level)}`}>
    {securityLevelIcon(level) |> React.string}
  </span>
}
```

### Backend Security Scanner

```elixir
# backend/lib/stapeln/security_scanner.ex
# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Stapeln.SecurityScanner do
  @moduledoc """
  Scans container components for security issues.

  Checks:
  - Signature verification (Rekor)
  - SBOM presence
  - CVE scanning (Grype)
  - Exposed ports risk assessment
  - Running as root
  - Health check configuration
  """

  def scan_component(component_id) do
    component = Stapeln.Repo.get!(Stapeln.Schema.Component, component_id)

    %{
      component_id: component_id,
      overall_level: :safe,  # Will be updated
      issues: [],
      exposed_ports: component.ports,
      running_as_root: check_root(component),
      has_health_check: check_health_check(component),
      signature_verified: verify_signature(component),
      sbom_present: check_sbom(component)
    }
    |> add_cve_scan(component)
    |> add_port_risk_analysis()
    |> calculate_overall_level()
  end

  defp verify_signature(component) do
    # Check Rekor transparency log
    case Stapeln.Rekor.verify(component.image_digest) do
      {:ok, _} -> true
      {:error, _} -> false
    end
  end

  defp check_sbom(component) do
    # Check for in-toto attestation
    case Stapeln.InToto.get_attestation(component.image_digest) do
      {:ok, attestation} ->
        Map.has_key?(attestation, :sbom)

      {:error, _} -> false
    end
  end

  defp check_root(component) do
    # Parse Dockerfile/Containerfile for USER directive
    component.user == "root" || is_nil(component.user)
  end

  defp check_health_check(component) do
    not is_nil(component.health_check)
  end

  defp add_cve_scan(security, component) do
    # Run Grype CVE scanner
    case System.cmd("grype", [component.image_reference, "-o", "json"]) do
      {output, 0} ->
        vulnerabilities = Jason.decode!(output)["matches"] || []

        issues = Enum.map(vulnerabilities, fn vuln ->
          %{
            level: severity_to_level(vuln["vulnerability"]["severity"]),
            category: "CVE",
            description: vuln["vulnerability"]["description"],
            cve: vuln["vulnerability"]["id"],
            fix: vuln["vulnerability"]["fix"]
          }
        end)

        %{security | issues: security.issues ++ issues}

      _ ->
        security
    end
  end

  defp add_port_risk_analysis(security) do
    port_issues = Enum.map(security.exposed_ports, fn port ->
      case get_port_risk(port) do
        :safe -> nil
        level ->
          %{
            level: level,
            category: "Exposed Port",
            description: "Port #{port} is exposed (#{get_port_name(port)})",
            cve: nil,
            fix: "Close port or use ephemeral pinhole"
          }
      end
    end)
    |> Enum.reject(&is_nil/1)

    %{security | issues: security.issues ++ port_issues}
  end

  defp calculate_overall_level(security) do
    max_level = security.issues
    |> Enum.map(& &1.level)
    |> Enum.max(fn -> :safe end)

    %{security | overall_level: max_level}
  end

  defp severity_to_level("Critical"), do: :critical
  defp severity_to_level("High"), do: :high
  defp severity_to_level("Medium"), do: :medium
  defp severity_to_level("Low"), do: :low
  defp severity_to_level(_), do: :safe

  defp get_port_risk(22), do: :high      # SSH
  defp get_port_risk(23), do: :critical  # Telnet
  defp get_port_risk(3389), do: :high    # RDP
  defp get_port_risk(port) when port < 1024, do: :medium
  defp get_port_risk(_), do: :safe

  defp get_port_name(22), do: "SSH"
  defp get_port_name(23), do: "Telnet"
  defp get_port_name(80), do: "HTTP"
  defp get_port_name(443), do: "HTTPS"
  defp get_port_name(3389), do: "RDP"
  defp get_port_name(5432), do: "PostgreSQL"
  defp get_port_name(_), do: "Unknown"
end
```

---

## 7. Integration with Gap Analysis

### Firewall Checks in Gap Analysis

Gap analysis sidebar (Page 1) includes firewall status:

```rescript
type gapAnalysisItem =
  | MissingSignature(string)
  | NoSBOM(string)
  | PortConflict(int, string)
  | RunningAsRoot(string)
  | NoHealthCheck(string)
  | FirewallMisconfigured(string)  // NEW
  | ExposedPort(int, securityLevel)  // NEW

let renderGapAnalysis = (gaps: array<gapAnalysisItem>) => {
  <aside className="gap-analysis" role="complementary" ariaLabel="Security gap analysis">
    <h3>{"⚠️ Gap Analysis" |> React.string}</h3>

    {gaps
     ->Array.map((gap, idx) => {
       switch gap {
       | FirewallMisconfigured(msg) =>
         <div key={idx->Int.toString} className="gap-item critical">
           <span className="icon">{"🔥" |> React.string}</span>
           <div>
             <strong>{"Firewall Misconfigured" |> React.string}</strong>
             <p>{msg |> React.string}</p>
             <button className="fix-button">{"Configure Firewall" |> React.string}</button>
           </div>
         </div>

       | ExposedPort(port, level) =>
         <div key={idx->Int.toString} className={`gap-item ${securityLevelToString(level)}`}>
           <span className="icon">{"🔓" |> React.string}</span>
           <div>
             <strong>{`Port ${port->Int.toString} Exposed` |> React.string}</strong>
             <p>{"High-risk port is publicly accessible" |> React.string}</p>
             <button className="fix-button">{"Close Port" |> React.string}</button>
           </div>
         </div>

       | _ => /* Other gap types */
       }
     })
     ->React.array}
  </aside>
}
```

---

## 8. Summary: Zero Docker Commands

### Before stapeln (Traditional)

```bash
# Open port temporarily (15 steps!)
sudo firewall-cmd --zone=public --add-port=8080/tcp
docker run -d -p 8080:8080 myapp
# Wait 5 minutes...
sudo firewall-cmd --zone=public --remove-port=8080/tcp
docker stop myapp
```

### After stapeln (Visual)

1. Click component
2. Port section → Port 8080 → Select "Ephemeral"
3. Choose duration: 5 minutes
4. Done ✅

**Zero CLI commands. Zero YAML editing. Zero pain.**

---

## 9. Security Audit Trail

### Logging All Firewall Changes

```elixir
# backend/lib/stapeln/audit_log.ex
# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Stapeln.AuditLog do
  @moduledoc """
  Immutable audit trail for all security-sensitive operations.

  Stored in VeriSimDB temporal modality for tamper-proof logging.
  """

  def log_firewall_change(action, details) do
    entry = %{
      timestamp: DateTime.utc_now(),
      action: action,
      user: get_current_user(),
      ip_address: get_client_ip(),
      details: details,
      signature: sign_entry(action, details)
    }

    VeriSim.insert(entry, [:temporal, :semantic])

    # Also log to syslog for system administrators
    Logger.info("[AUDIT] Firewall change",
      action: action,
      user: entry.user,
      details: details
    )
  end

  def query_audit_trail(filters \\ %{}) do
    VeriSim.query("""
      SELECT ?timestamp ?action ?user ?details
      WHERE {
        ?entry stapeln:auditAction ?action ;
               stapeln:timestamp ?timestamp ;
               stapeln:user ?user ;
               stapeln:details ?details .
        #{build_filters(filters)}
      }
      ORDER BY DESC(?timestamp)
    """, modality: :semantic)
  end
end
```

### Audit Trail UI

```rescript
// frontend/src/AuditTrail.res
// SPDX-License-Identifier: PMPL-1.0-or-later

let renderAuditTrail = (entries: array<auditEntry>) => {
  <section className="audit-trail" role="region" ariaLabel="Security audit trail">
    <h3>{"🔍 Audit Trail" |> React.string}</h3>

    <table>
      <thead>
        <tr>
          <th>{"Time" |> React.string}</th>
          <th>{"Action" |> React.string}</th>
          <th>{"User" |> React.string}</th>
          <th>{"Details" |> React.string}</th>
        </tr>
      </thead>
      <tbody>
        {entries
         ->Array.map(entry => {
           <tr key={entry.id}>
             <td>{formatTimestamp(entry.timestamp) |> React.string}</td>
             <td>{entry.action |> React.string}</td>
             <td>{entry.user |> React.string}</td>
             <td>{entry.details |> React.string}</td>
           </tr>
         })
         ->React.array}
      </tbody>
    </table>
  </section>
}
```

---

## 10. Testing & Verification

### Security Test Suite

```bash
# tests/firewall-test.sh
# SPDX-License-Identifier: PMPL-1.0-or-later

#!/bin/bash
set -euo pipefail

echo "=== stapeln Firewall Security Tests ==="

# Test 1: Default-deny enforcement
echo "Test 1: Verify default-deny..."
if timeout 2 curl -s http://localhost:2375 &>/dev/null; then
  echo "❌ FAIL: Docker socket exposed"
  exit 1
else
  echo "✅ PASS: Docker socket blocked"
fi

# Test 2: Localhost-only access
echo "Test 2: Verify localhost-only stapeln UI..."
if curl -s http://localhost:8000/health | grep -q "ok"; then
  echo "✅ PASS: stapeln UI accessible on localhost"
else
  echo "❌ FAIL: stapeln UI not responding"
  exit 1
fi

# Test 3: Ephemeral pinhole lifecycle
echo "Test 3: Test ephemeral pinhole..."
PINHOLE_ID=$(curl -s -X POST http://localhost:8000/api/pinholes \
  -H "Content-Type: application/json" \
  -d '{"port": 9999, "duration": 5}' | jq -r '.id')

echo "  Pinhole opened: $PINHOLE_ID"
sleep 2

# Verify port is open
if timeout 2 nc -z localhost 9999; then
  echo "✅ PASS: Ephemeral port accessible"
else
  echo "❌ FAIL: Ephemeral port not open"
  exit 1
fi

# Wait for expiry
sleep 4

# Verify port is closed
if timeout 2 nc -z localhost 9999 &>/dev/null; then
  echo "❌ FAIL: Ephemeral port did not close"
  exit 1
else
  echo "✅ PASS: Ephemeral port auto-closed"
fi

# Test 4: Authentication required
echo "Test 4: Verify authentication..."
if curl -s http://localhost:8000/api/components | grep -q "Unauthorized"; then
  echo "✅ PASS: Authentication required"
else
  echo "❌ FAIL: No authentication check"
  exit 1
fi

echo ""
echo "=== All tests passed! ==="
```

---

## Next Steps

1. **Implement Elixir backend** with EphemeralPinhole GenServer
2. **Add ModSecurity** to Svalinn gateway
3. **Build ReScript UI** for port configuration
4. **Configure firewalld** default-deny rules
5. **Test with container-hater** (your son!)

---

## References

- OWASP CRS: https://coreruleset.org/
- ModSecurity: https://github.com/SpiderLabs/ModSecurity
- firewalld: https://firewalld.org/
- nftables: https://netfilter.org/projects/nftables/
- PAM (Linux): https://www.linux-pam.org/

**Document version**: 1.0
**Last updated**: 2026-02-05
**Status**: Design specification (ready for implementation)
