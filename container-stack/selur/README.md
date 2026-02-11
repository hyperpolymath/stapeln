# selur

Zero-copy WASM bridge between Svalinn and Vörðr, enforcing Ephapax-linear type safety. Source: `/var/mnt/eclipse/repos/selur`.

## Purpose
- Eliminates serialization overhead by sharing linear memory through a Zig-compiled WASM sealant (`selur.wasm`).
- Formal verification via Idris2 proofs ensures no leaks and correct request/response pairing.
- Designed for rootless containers while remaining compatible with `.ctp` bundles.

## Build hints
- Justfile drives Zig/Ephapax build steps; `just build` produces the sealant module.
- Compose with Svalinn/Vörðr by loading the WebAssembly module next to their runtimes.
