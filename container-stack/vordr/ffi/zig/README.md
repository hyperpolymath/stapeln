# Vörðr Zig FFI

**Per ABI/FFI Universal Standard**

## Architecture

```
┌─────────────────────────────────────────┐
│         Idris2 ABI (src/abi/)            │
│  • Types.idr  - Type definitions + proofs │
│  • Layout.idr - Memory layout verification│
│  • Foreign.idr - FFI declarations        │
└───────────────────┬─────────────────────┘
                    ↓
┌───────────────────────────────────────────┐
│         Zig FFI (ffi/zig/src/)            │
│  • main.zig - C-compatible implementation │
│  • NO C header files (direct ABI)        │
└───────────────────────────────────────────┘
```

## Why Idris2 + Zig?

**Idris2 ABI:**
- Dependent types prove interface correctness
- Formal verification of memory layouts
- Platform-specific ABIs with compile-time selection
- Type-level guarantees impossible in C/Zig/Rust

**Zig FFI:**
- Native C ABI compatibility (no `extern "C"` needed)
- Memory-safe by default
- Cross-compilation built-in
- Zero-cost abstractions
- No runtime dependencies

**No C Headers:**
- Idris2 ABI defines the interface directly
- Zig implements C-compatible functions
- No intermediate .h files needed
- Reduces coupling and build complexity

## Building

```bash
# Build Zig FFI
cd ffi/zig
zig build -Doptimize=ReleaseFast

# Install library
cp zig-out/lib/libvordr.so /usr/local/lib/  # Linux
cp zig-out/lib/libvordr.dylib /usr/local/lib/  # macOS
cp zig-out/lib/vordr.dll C:/Windows/System32/  # Windows

# Test
zig build test
```

## Integration

**Idris2:**
```idris
import Vordr.ABI.Foreign

main : IO ()
main = do
  Just handle <- init
    | Nothing => putStrLn "Init failed"
  putStrLn !version
  free handle
```

**Elixir (via NIF or Port):**
```elixir
{:ok, handle} = Vordr.FFI.init()
version = Vordr.FFI.version()
Vordr.FFI.free(handle)
```

## Functions Implemented

### Core Lifecycle
- `vordr_init()` - Initialize library
- `vordr_free(handle)` - Clean up resources

### Container Operations
- `vordr_verify_image(handle, image_name)` - Verify signature
- `vordr_create_container(handle, name, image)` - Create container
- `vordr_start_container(handle, container_id)` - Start
- `vordr_stop_container(handle, container_id)` - Stop
- `vordr_list_containers(handle, output)` - List all

### String Operations
- `vordr_get_string(handle)` - Get result string
- `vordr_free_string(ptr)` - Free allocated string

### Error Handling
- `vordr_last_error()` - Get error message

### Utilities
- `vordr_version()` - Get version
- `vordr_build_info()` - Get build info
- `vordr_is_initialized(handle)` - Check init status

## Memory Safety

**Idris2 Guarantees:**
- Non-null pointers enforced at type level
- Buffer bounds checked statically
- No memory leaks (proof obligations)

**Zig Guarantees:**
- No undefined behavior (comptime checks)
- Memory allocator tracking
- Optional types prevent null dereference

## Testing

```bash
cd ffi/zig
zig build test

# Manual testing
zig build
ldd zig-out/lib/libvordr.so  # Check dependencies
nm zig-out/lib/libvordr.so | grep vordr_  # Check exports
```

## License

SPDX-License-Identifier: PMPL-1.0-or-later
