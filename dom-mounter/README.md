# stapeln DOM Mounter

This repo preserves the **DOM‑mounter** design and implementation work so it is not lost while stapeln ships its end‑user product.
It is **not on the critical path** for the container‑hater MVP.

## Why This Exists

- The DOM‑mounter work is substantial and valuable.
- It is enabling technology, not the product target for end users.
- Keeping it here avoids losing the design while reducing product confusion.

## What Is Included

- Documentation and design history in `docs/`.
- Example artifacts in `examples/`.
- Test artifacts in `tests/`.
- A manifest of relevant code locations in `MANIFEST.md`.

## Where The Code Lives

- ReScript bindings: `src/DomMounter*.res`.
- Framework adapters: `src/ReactAdapter.res`, `src/SolidAdapter.res`, `src/VueAdapter.res`, `src/WebComponent.res`, `src/SSRAdapter.res`.
- Idris2 proofs: `src/abi/DomMounter.idr`.
- Zig FFI: `ffi/zig/src/dom_mounter*.zig`.

## Relationship To stapeln

The stapeln product repo remains focused on end‑user container workflows.
This repo exists to keep the DOM‑mounter work intact and ready for future productization.
