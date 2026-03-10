<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->

# Contributing to Rokur

Thank you for considering a contribution to Rokur, the secrets management gate
for the Stapeln container stack.

## Prerequisites

- [Deno](https://deno.com/) (v2.x or later)
- [just](https://github.com/casey/just) (command runner)

## Getting Started

```sh
# Clone the Stapeln repository (Rokur lives inside it).
git clone https://github.com/hyperpolymath/stapeln
cd stapeln/container-stack/rokur

# Run the full check suite before making changes.
just check
```

## Development Workflow

1. **Create a feature branch** from `main`.
2. **Write or update tests** in `test/` for any new behaviour.
3. **Run all checks** before submitting a pull request:

   ```sh
   just check   # runs: lint, fmt-check, test
   ```

4. **Open a pull request** with a clear description of the change.

## Code Style

- Rokur is written in **JavaScript** running on **Deno**.
- Format with `just fmt` (uses `deno fmt`).
- Lint with `just lint` (uses `deno lint`).
- Two-space indentation, UTF-8, LF line endings (see `.editorconfig`).

## Testing

```sh
just test          # all tests
deno task test:unit        # unit tests only
deno task test:integration # integration tests only
```

Tests require environment variables for the server configuration. See
`README.md` and the test files under `test/` for required setup.

## Security

Rokur is a security-critical component. Please:

- Never log or return actual secret values.
- Preserve the fail-closed default in all code paths.
- Add test coverage for any new authorization or validation logic.
- Report vulnerabilities privately (see `SECURITY.md`).

## License

By contributing to Rokur you agree that your contributions will be licensed
under the **Palimpsest License (PMPL-1.0-or-later)**. See `LICENSE` for details.

## Code of Conduct

Be respectful, constructive, and inclusive. Harassment or discriminatory
behaviour will not be tolerated.
