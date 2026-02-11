# Rokur

Rokur is envisioned as the secrets-management seat of the container stack that stitches together PanLL, panic-attacker, and the verified-container ecosystem. We are using `stapeln/container-stack` as the holding area for all of these artifacts so that we can build, verify, and compose them in one place before moving integrations elsewhere.

## Mission
- Provide a hardened secret store that can deliver credentials, attestations, and configuration to PanLL, panic-attacker, and the selur-compose topology.
- Expose a minimal API (REST/JSON-RPC) that the stack’s orchestrator (Vörðr/Svalinn) and GUI (PanLL) can call without embedding secrets in the UI or CLI payloads.
- Serve as the single point where verified `.ctp` bundles can request vault data before launching, so an operator’s policy can gate container launches when credentials are stale or missing.

## Suggested approach
1. Keep Rokur lean at first—a tiny service under `stapeln/rokur/` with a placeholder config, stub REST endpoints, and a README describing the secrets domain.
2. Document expected secrets (e.g., panic-attack ambush profile files, PanLL API tokens, Chainguard registry creds) so future developers know what Rokur must expose.
3. Wire Rokur into `runtime/compose.toml` via selur-compose (it already references Rokur as a service slot) and ensure the Chainguard/Cerro Torre build artifacts include it as a dependency with a signed `.ctp` reference.
4. After panic-attacker containerization stabilizes, expand Rokur with real secret handling and policies (Secrets rotation, attestation, etc.) before extracting it into its own repo.

## Next steps
- Use this directory as the landing pad while panic-attacker/panll container work completes.
- Once the integration stage is ironed out, promote Rokur to a dedicated repo (with its own README, API, and verified container specs) and replace this placeholder with a symlink or fetch script.
