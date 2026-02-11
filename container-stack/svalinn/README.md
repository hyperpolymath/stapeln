# Svalinn

Edge gateway for verified-container operations, wired to the verified-container-spec protocol and delegating to Vörðr. Source: `/var/mnt/eclipse/repos/svalinn`.

## Mission
- Deno/ReScript HTTP gateway (`just dev`, `just serve`, `deno run`) validating JSON Schema and policy decisions for container requests.
- Delegation bridge to Vörðr through JSON-RPC 2.0 (MCP protocols) plus authentication (OAuth2/JWT/API keys).
- Compose-style orchestration via `svalinn-compose` CLI that can start, scale, and stop multi-container topologies.

## Key sections
- Gateway code and policy engine under `src/`; UI lives in `ui/` with Rescript tooling.
- Helpers: `Justfile` tasks, `deno.json` tooling, `docs/` coverage of spec compliance.
