# Ephapax Modules for stapeln

Security-critical components implemented in Ephapax with linear/affine types for formally verified resource safety.

## Modules

| Module | Mode | Purpose | Status |
|--------|------|---------|--------|
| **fjord** | Linear | Secrets manager (secrets used exactly once) | ✅ Proof-of-concept |
| **cape** | Affine | Runtime monitor (events can be dropped) | 🚧 Planned |
| **strait** | Linear | Network policy (policies must be applied) | 🚧 Planned |

## Build

```bash
# Build Fjord
cd fjord && just build

# Build all modules
just build-all
```

## Type System Guarantees

**Fjord (Linear Mode):**
- ✅ Secrets used exactly once (no double-use)
- ✅ Secrets cannot leak (must be consumed)
- ✅ All code paths consume secrets
- ✅ Memory zeroed on consumption
- ✅ No GC needed (deterministic cleanup)

**Formally Verified:** Ephapax type system proven correct in Coq.

## Integration

Ephapax modules compile to WASM and are called from Elixir via Wasmex:

```elixir
# Create secret (returns linear Secret)
{:ok, secret} = Fjord.create_secret("my_password")

# Use secret (consumes it)
{:ok, secret_ref} = Fjord.use_secret(secret)

# Second use: ERROR (Ephapax prevents this at compile time)
# {:error, :already_consumed} = Fjord.use_secret(secret)
```

## Why Ephapax?

Your son (government cyberwar officer) can't break formally verified type systems. 🎯
