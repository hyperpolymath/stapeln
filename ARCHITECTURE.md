# stapeln Architecture

## Overview

stapeln is a visual drag-and-drop container stack designer built with:

- **Frontend**: ReScript-TEA (The Elm Architecture) + Deno
- **Backend**: Elixir (Phoenix) + Ephapax + Idris2 + Rust
- **Communication**: REST API + WebSocket

## Frontend Architecture (ReScript-TEA)

### The Elm Architecture (TEA)

```
┌─────────┐
│  Model  │  Current application state
└────┬────┘
     │
     ↓
┌─────────┐
│  View   │  Renders UI from Model
└────┬────┘
     │
     ↓ (User Interaction)
┌─────────┐
│   Msg   │  User events, API responses
└────┬────┘
     │
     ↓
┌─────────┐
│ Update  │  Model → Msg → (Model, Cmd)
└─────────┘
```

### Component Structure

```
frontend/src/
├── Main.res           # Entry point, TEA initialization
├── Model.res          # State types (component, connection, dragState)
├── Msg.res            # Event types (AddComponent, DragMove, etc.)
├── Update.res         # State transitions
├── View.res           # Main view renderer
├── Canvas.res         # Drag-and-drop canvas
├── Components.res     # Component library (palette)
├── Connections.res    # Connection line rendering
├── Config.res         # Configuration panel
├── Export.res         # Export menu
└── Router.res         # cadre-tea-router integration
```

### Canvas Implementation

**SVG-based canvas** for:
- Infinite canvas (pan/zoom)
- Crisp rendering at any zoom level
- Easy connection line drawing (SVG paths)
- CSS styling

**Drag-and-Drop**:
1. User clicks component in palette → `StartDragComponent`
2. Mouse moves → `DragMove(position)`
3. Mouse release → `DragEnd` → Add component at position

**Connection Lines**:
- Click component → select (highlight)
- Click another component → create connection
- Connections drawn as SVG paths (bezier curves or orthogonal lines)

## Backend Architecture (Elixir + Phoenix)

### Service Layers

```
┌──────────────────────────────────────────────┐
│ Phoenix Web Layer (REST + WebSocket)         │
│  - StackController (CRUD)                    │
│  - StackChannel (real-time updates)          │
│  - ExportController (generate compose files) │
└────────────┬─────────────────────────────────┘
             │
             ↓
┌──────────────────────────────────────────────┐
│ Business Logic (Elixir)                      │
│  - Stack management                          │
│  - Component orchestration                   │
│  - Validation coordination                   │
└────────────┬─────────────────────────────────┘
             │
      ┌──────┴──────┬──────────────┐
      ↓             ↓              ↓
┌──────────┐  ┌──────────┐  ┌──────────┐
│ Validator│  │ Prover   │  │ Codegen  │
│ (Ephapax)│  │ (Idris2) │  │ (Rust)   │
└──────────┘  └──────────┘  └──────────┘
```

### Validation Service (Ephapax + Idris2)

**Ephapax (Linear Types)**:
```elixir
defmodule Stackur.Validator.Ephapax do
  # Validate linear resource usage
  # - Each volume consumed exactly once
  # - No dangling port references
  # - Network resources properly scoped

  def validate_linear_types(stack) do
    # Call Ephapax via Port/NIF
    # Returns: {:ok, proof} | {:error, violations}
  end
end
```

**Idris2 (Formal Proofs)**:
```idris
-- validation/src/Stack.idr

data Stack = MkStack (List Component) (List Connection)

-- Proof: No circular dependencies
noCycles : (s : Stack) -> Dec (Acyclic s)

-- Proof: Dependencies form a DAG
dependencyDAG : (s : Stack) -> Either DAGError (TopologicalOrder s)

-- Proof: Port conflict detection
noPortConflicts : (s : Stack) -> Dec (UniquePortBindings s)
```

**Integration**:
```elixir
defmodule Stackur.Validator.Idris do
  # Calls compiled Idris2 executable via Port
  def prove_correctness(stack_json) do
    Port.open({:spawn_executable, "/usr/local/bin/stapeln-prover"},
      [:binary, :use_stdio, {:args, [stack_json]}])
  end
end
```

### Code Generation (Rust)

**Why Rust for Codegen?**
- Type-safe TOML generation
- Fast compilation
- Elixir NIF integration (Rustler)
- Memory safety guarantees

```rust
// backend/native/stapeln_codegen/src/lib.rs
use rustler::{Encoder, Env, Term};
use toml::Value;

#[rustler::nif]
fn generate_compose_toml(stack_json: String) -> Result<String, String> {
    let stack: Stack = serde_json::from_str(&stack_json)
        .map_err(|e| format!("Parse error: {}", e))?;

    let toml = compose_toml_from_stack(&stack)?;
    Ok(toml::to_string(&toml).unwrap())
}

fn compose_toml_from_stack(stack: &Stack) -> Result<Value, String> {
    // Build compose.toml structure
    // Handle services, volumes, networks
    // Validate against verified-container-spec
}
```

**Elixir NIF wrapper**:
```elixir
defmodule Stackur.Codegen do
  use Rustler, otp_app: :stapeln, crate: "stapeln_codegen"

  def generate_compose_toml(_stack_json), do: :erlang.nif_error(:nif_not_loaded)
  def generate_docker_compose(_stack_json), do: :erlang.nif_error(:nif_not_loaded)
  def generate_podman_compose(_stack_json), do: :erlang.nif_error(:nif_not_loaded)
end
```

### AffineScript Alternative (MVP)

**Note**: AffineScript compiler may not be production-ready.

**MVP Alternative**: Use Rust's affine type system:

```rust
// Affine types: used at most once
struct Volume(String);  // Can't be cloned

impl Volume {
    fn mount_once(self, container: &mut Container) {
        // Consumes self, can't be used again
        container.volumes.push(self);
    }
}

// Port conflict detection
fn check_port_conflicts(services: &[Service]) -> Result<(), String> {
    let mut ports = HashSet::new();
    for service in services {
        for port in &service.ports {
            if !ports.insert(port.host_port) {
                return Err(format!("Port {} already bound", port.host_port));
            }
        }
    }
    Ok(())
}
```

## API Design

### REST Endpoints

```
POST   /api/stacks              Create stack
GET    /api/stacks/:id          Get stack
PUT    /api/stacks/:id          Update stack
DELETE /api/stacks/:id          Delete stack
POST   /api/stacks/:id/validate Validate stack
POST   /api/stacks/:id/export   Export to compose file
```

### WebSocket Channel

```elixir
defmodule StackurWeb.StackChannel do
  use Phoenix.Channel

  def join("stack:" <> stack_id, _payload, socket) do
    {:ok, assign(socket, :stack_id, stack_id)}
  end

  def handle_in("update_component", payload, socket) do
    # Update component, broadcast to all clients
    broadcast(socket, "component_updated", payload)
    {:noreply, socket}
  end

  def handle_in("validate", _payload, socket) do
    stack_id = socket.assigns.stack_id

    # Async validation
    Task.start(fn ->
      result = Stackur.Validator.validate(stack_id)
      broadcast(socket, "validation_result", result)
    end)

    {:noreply, socket}
  end
end
```

## Data Flow

### 1. User Adds Component

```
Frontend (ReScript)                Backend (Elixir)
     │                                   │
     │ Drag component to canvas          │
     │ → AddComponent(Svalinn, {x,y})    │
     │                                   │
     │ WebSocket: "add_component"        │
     ├──────────────────────────────────→│
     │                                   │ Validate placement
     │                                   │ Save to database
     │                                   │
     │ ← "component_added"               │
     ←──────────────────────────────────┤
     │                                   │
     Update Model                        Broadcast to other clients
```

### 2. User Validates Stack

```
Frontend                          Backend                   Validator
    │                                 │                          │
    │ Click "Validate"                │                          │
    │ → ValidateStack                 │                          │
    │                                 │                          │
    │ POST /api/stacks/:id/validate   │                          │
    ├────────────────────────────────→│                          │
    │                                 │ Convert to Ephapax AST   │
    │                                 ├─────────────────────────→│
    │                                 │                          │
    │                                 │ ← Linear type check      │
    │                                 ←─────────────────────────┤
    │                                 │                          │
    │                                 │ Call Idris2 prover       │
    │                                 ├─────────────────────────→│
    │                                 │                          │
    │                                 │ ← Correctness proofs     │
    │                                 ←─────────────────────────┤
    │                                 │                          │
    │ ← ValidationResult({valid,      │                          │
    │    errors, warnings})           │                          │
    ←────────────────────────────────┤                          │
    │                                 │                          │
    Update Model with result          │                          │
```

### 3. User Exports Stack

```
Frontend                Backend                Codegen (Rust)
    │                       │                        │
    │ Click "Export as      │                        │
    │  selur-compose"       │                        │
    │ → ExportToSelurCompose│                        │
    │                       │                        │
    │ POST /api/stacks/:id/ │                        │
    │  export?format=selur  │                        │
    ├──────────────────────→│                        │
    │                       │ NIF call: generate_    │
    │                       │  compose_toml(stack)   │
    │                       ├───────────────────────→│
    │                       │                        │
    │                       │ ← compose.toml string  │
    │                       ←───────────────────────┤
    │                       │                        │
    │ ← File download       │                        │
    │  "stack.compose.toml" │                        │
    ←──────────────────────┤                        │
```

## Performance Considerations

### Frontend

- **Virtual DOM**: ReScript compiles to efficient JS
- **Debounced updates**: Drag events throttled to 60fps
- **Canvas optimization**: Only redraw changed components
- **WebSocket batching**: Group rapid updates

### Backend

- **Concurrent validation**: Elixir processes per stack
- **Caching**: Validation results cached with TTL
- **NIF for CPU-intensive**: Rust NIFs for codegen
- **Streaming**: Large exports streamed via chunks

## Security

### Frontend

- **CSP headers**: Prevent XSS
- **Input validation**: All user input sanitized
- **CORS**: Restrict API access

### Backend

- **Authentication**: JWT tokens
- **Authorization**: Stack ownership validation
- **Rate limiting**: Per-IP and per-user
- **Sandboxing**: Idris2/Ephapax run in isolated processes

## Deployment

```
┌─────────────────┐
│ Nginx (TLS)     │ Port 443
└────────┬────────┘
         │
    ┌────┴────┐
    │         │
┌───▼───┐ ┌──▼────┐
│ Deno  │ │Phoenix│ Ports 8000, 4000
│frontend│ │backend│
└───────┘ └───┬───┘
              │
      ┌───────┼───────┐
      │       │       │
  ┌───▼──┐ ┌──▼──┐ ┌─▼────┐
  │Postgres│Idris2│Ephapax│
  └───────┘ └─────┘ └──────┘
```

## Future Enhancements

1. **Collaborative editing** - Multiple users editing same stack
2. **Version control** - Git-like stack versioning
3. **Templates** - Pre-built stack templates
4. **AI suggestions** - ML-based component recommendations
5. **Performance profiling** - Predict stack resource usage
6. **Cost estimation** - Cloud deployment cost prediction

## References

- [The Elm Architecture](https://guide.elm-lang.org/architecture/)
- [ReScript-TEA](https://github.com/rescript-lang/rescript-tea)
- [Ephapax](https://github.com/hyperpolymath/ephapax)
- [Idris2](https://idris2.readthedocs.io/)
- [Phoenix Channels](https://hexdocs.pm/phoenix/channels.html)
- [Rustler](https://github.com/rusterlium/rustler)
