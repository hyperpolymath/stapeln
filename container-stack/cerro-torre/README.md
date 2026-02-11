# Cerro Torre

Provenance-verifiable builder that packs containers into signed `.ctp` bundles with Ada/SPARK assurance. Original repo: `/var/mnt/eclipse/repos/cerro-torre`.

## Role
- Ada/SPARK framework covering manifest parsing, cryptography, attestation generation, and transparency-log proof handling.
- CLI (`ct pack`, `ct verify`) for creating and verifying Chainguard-style bundles referenced by the verified-container-spec.
- Produces artifacts that Svalinn/Vörðr/selur consume inside selur-compose topologies.

## Getting started
- Build via `alr build` (Alire) and run `./bin/ct fetch|pack|push` against registries.
- Generate attestations/log proofs, sign bundles, and provide them to `ct verify` before handing them to runtimes.
