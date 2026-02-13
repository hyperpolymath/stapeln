# ABI/FFI Universal Standard: DOM Mounter

High-assurance DOM mounting with formal verification using the Idris2 → Zig → ReScript architecture.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│ Layer 1: Idris2 ABI (src/abi/DomMounter.idr)              │
│ - Dependent types with formal proofs                        │
│ - ValidElementId proof (non-empty string)                  │
│ - NoMemoryLeak proof                                        │
│ - AtomicMount proof (thread safety)                        │
│ - C FFI export declarations                                 │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│ Layer 2: Zig FFI (ffi/zig/src/dom_mounter.zig)            │
│ - C-compatible ABI implementation                           │
│ - Memory-safe by default                                    │
│ - Zero-cost abstractions                                    │
│ - Built-in testing framework                                │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│ Layer 3: ReScript Bindings (src/DomMounter.res)           │
│ - Type-safe external FFI bindings                          │
│ - High-level API with Result types                         │
│ - React integration helpers                                 │
└─────────────────────────────────────────────────────────────┘
```

## Why This Architecture?

### Idris2 for ABI
- **Dependent types** prove interface correctness at compile-time
- **Formal verification** of memory layout and safety properties
- **Platform-specific ABIs** with compile-time selection
- **Provable backward compatibility**
- Type-level guarantees impossible in C/Zig/Rust

### Zig for FFI
- **Native C ABI compatibility** without overhead
- **Memory-safe by default** (no null pointers, bounds checking)
- **Cross-compilation built-in** (WASM, native, etc.)
- **Zero runtime dependencies**
- **Zero-cost abstractions**

### ReScript for Application
- **Type-safe** JavaScript generation
- **React integration** with JSX support
- **Deno-first** runtime without npm
- **Excellent interop** with JavaScript ecosystem

## Building

### Prerequisites
```bash
# Install Idris2 (for ABI definitions)
pack install-app idris2

# Install Zig (for FFI layer)
curl https://ziglang.org/download/index.json | jq -r '.master.version' | xargs -I {} \
  curl -OL https://ziglang.org/builds/zig-linux-x86_64-{}.tar.xz

# ReScript already configured in package.json
```

### Build Steps

1. **Compile Idris2 ABI to C headers:**
```bash
cd src/abi
idris2 --codegen c DomMounter.idr -o ../../generated/abi/DomMounter.h
```

2. **Build Zig FFI library:**
```bash
cd ffi/zig
zig build
# Generates libdom_mounter.so
```

3. **Compile ReScript:**
```bash
deno task build
# Uses DomMounter.res bindings
```

## Usage

### Basic Mounting

```rescript
// Simple mount to #app element
switch DomMounter.mountToApp() {
| Ok() => Js.Console.log("Mounted successfully!")
| Error(msg) => Js.Console.error("Mount failed: " ++ msg)
}
```

### With React

```rescript
// Mount React app with error handling
let app = () => <App />
DomMounter.mountReactApp(app)
  ->Result.mapError(msg => {
    Js.Console.error("Failed to mount React app: " ++ msg)
  })
```

### Custom Element

```rescript
// Mount to custom element with callbacks
DomMounter.mountWithCallback(
  "my-custom-root",
  () => Js.Console.log("Success!"),
  (err) => Js.Console.error(err)
)
```

## Formal Guarantees

The Idris2 layer provides compile-time proofs of:

1. **ValidElementId**: Element IDs are non-empty strings
2. **NoMemoryLeak**: Mounting doesn't leak memory
3. **AtomicMount**: Mounting is thread-safe and atomic
4. **ElementExists**: Element must exist before mounting

These proofs are checked at compile-time by Idris2's dependent type system.

## Testing

### Zig FFI Tests
```bash
cd ffi/zig
zig build test
```

### Integration Tests
```bash
deno task test
```

## References

- [Idris2 Documentation](https://idris2.readthedocs.io/)
- [Zig Language Reference](https://ziglang.org/documentation/master/)
- [ReScript Documentation](https://rescript-lang.org/)
- [ABI/FFI Universal Standard (CLAUDE.md)](../../.claude/CLAUDE.md)

## License

PMPL-1.0-or-later
