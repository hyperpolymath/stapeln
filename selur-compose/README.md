# selur-compose

selur-compose is the orchestrator that links selur, Svalinn, Vörðr, Cerro Torre bundles, Rokur, and the PanLL stack into a verified-container-secure topology. Placing it at the root of `stapeln/` ensures the compose files/definitions survive even as we restructure other directories.

## Role
- Provides a `compose.toml`/`compose.yaml` (or similar) that defines the services, networks, and verification hooks for the whole container mesh.
- Bridges the Chainguard `.ctp` bundles back into a runnable environment through Svalinn/Vörðr plus supporting services like Rokur.
- Can import policy overrides, secrets, and runtime configuration for those services in one place.

## Getting started
- Keep compose definitions here under version control so incremental changes to the UX stack don’t accidentally delete the selur-compose descriptors.
- Reference this directory from `stapeln/container-stack` tooling once the veriﬁed bundles are published.
