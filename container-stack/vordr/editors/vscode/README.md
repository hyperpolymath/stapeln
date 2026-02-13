# Vörðr VS Code Extension

Language support for verified container workflows using Vörðr.

## Features

- **Syntax highlighting** for `.ctp` bundle files
- **Diagnostics** for unsigned/unverified container images
- **Auto-completion** for image names and tags
- **Hover information** showing signature status and metadata
- **Code actions** to sign images with Cerro Torre
- **Policy validation** for Gatekeeper policies

## Requirements

- Deno runtime (for LSP server)
- Vörðr installed
- Cerro Torre (optional, for signing)

## Extension Settings

- `vordr.lsp.enable`: Enable/disable the LSP server
- `vordr.lsp.trace.server`: Set trace level for debugging
- `vordr.verification.autoVerify`: Auto-verify images when opened
- `vordr.cerroTorre.path`: Path to cerro-pack binary

## Commands

- `Vörðr: Sign Container Image` - Sign an image with Cerro Torre
- `Vörðr: Verify Container Image` - Verify an image signature
- `Vörðr: Create CTP Bundle` - Create a new .ctp bundle
- `Vörðr: Validate Gatekeeper Policy` - Validate policy syntax

## File Types

### `.ctp` - Container Bundles
Signed, verified container bundles with:
- Image manifest
- Ed25519 signatures
- Policy attestations
- Metadata

### `.gatekeeper.yaml` - Gatekeeper Policies
Access control policies for container admission.

## License

PMPL-1.0-or-later
