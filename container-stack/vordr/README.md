# Vörðr (vordr)

Formally verified container runtime that enforces the verified-container-spec via Zig/Idris2/Elixir/Rust multi-language layers. Source at `/var/mnt/eclipse/repos/vordr`.

## Highlights
- Idris2-proven ABI definitions and Zig FFI for safe interop.
- ReScript LSP + MCP adapters with Deno tooling; optional Rust CLI for container ops.
- Elixir GenStateMachine orchestrator that can reverse operations (`vordr undo`, `vordr rollback`).

## Getting started
- Build FFI layers (`ffi/zig`), compile Rescript servers (`src/lsp`, `src/mcp-adapter`), and optionally build the CLI (`src/rust`).
- Run LSP (stdio/http), start MCP server (`deno run --allow-net HttpServer.res.js`), and use the CLI to manage containers.
