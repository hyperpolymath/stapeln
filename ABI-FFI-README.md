# Stapeln ABI/FFI

Stapeln uses this boundary model:
- ABI contract definitions in Idris2: `src/abi/Types.idr`, `src/abi/Layout.idr`, `src/abi/Foreign.idr`
- FFI implementation in Zig: `ffi/zig/src/main.zig`
- Backend entrypoint for API calls: `backend/lib/stapeln/native_bridge.ex`

## API Operations in the Contract

The ABI/FFI contract defines these operations:
- `stapeln_create_stack_json`
- `stapeln_get_stack_json`
- `stapeln_update_stack_json`
- `stapeln_validate_stack_json`
- `stapeln_list_stacks_json`
- `stapeln_free_buffer`
- `stapeln_contract_version`

The Elixir backend routes REST and GraphQL through `Stapeln.NativeBridge`, so both APIs share one interface surface.

## Runtime Mode

`Stapeln.NativeBridge` supports two modes:
- `zig_cli` when `STAPELN_NATIVE_FFI_BIN` is configured and runnable
- `elixir` fallback when native binary is not configured

This keeps local development and tests reliable while preserving a single contract boundary for production hardening.

## Build Zig FFI

```bash
cd ffi/zig
zig build
zig build test
```

## Verify Backend API

```bash
cd backend
mix test
```

