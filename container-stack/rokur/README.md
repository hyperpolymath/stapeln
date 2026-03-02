# Rokur

Rokur is the secrets gate used by the stack runtime plane before container start operations.

## What Is Implemented
- `GET /health`: liveness with non-sensitive configuration status.
- `GET /v1/secrets/status`: current allow/deny status from required-secret checks.
- `POST /v1/authorize-start`: pre-start authorization for runtime calls (used by Svalinn).

## Response Shape
- Authorization responses include `allowed`, `policy`, `code`, and secret counts.
- Secret names are intentionally not exposed in API responses.

## Policy Engine Boundary
- `ROKUR_POLICY_BACKEND=builtin` (default): evaluate required secrets directly in Rokur.
- `ROKUR_POLICY_BACKEND=external`: evaluate via an external command (intended Ephapax adapter path).
- `ROKUR_POLICY_BACKEND=auto`: use external when configured, otherwise builtin.

### External Policy Contract
- Rokur sends JSON on stdin to the external command:
  - `version`, `timestamp`, `requiredSecrets`, and `input` (`image`, `name`).
- External command must print one JSON decision to stdout with:
  - `allowed` (boolean), optional `policy`, `code`, `requiredSecretCount`, `missingSecretCount`.
- Non-zero exit, invalid JSON, or timeout is fail-closed (`deny`).

## Secret Policy Model (MVP)
- Configure required secret names via `ROKUR_REQUIRED_SECRETS` (comma-separated).
- For each required secret `name`, Rokur expects env var `ROKUR_SECRET_<NAME>`.
  - Example: `panic_profile` -> `ROKUR_SECRET_PANIC_PROFILE`
- Authorization is denied when any required secret is missing or blank.

## Authentication
- Set `ROKUR_API_TOKEN` to require `X-Rokur-Token` header on API calls.
- By default, Rokur fails to start if `ROKUR_API_TOKEN` is missing.
- For non-production local development only, you can bypass with `ROKUR_ALLOW_UNAUTHENTICATED=true`.

## Secure Defaults
- Default bind host is `127.0.0.1` (set `ROKUR_HOST` to override).
- Startup fails if `ROKUR_REQUIRED_SECRETS` is empty.
- Startup fails if `ROKUR_API_TOKEN` is missing.
- In `ROKUR_ENV=production`, insecure bypass flags are rejected by policy.

## Development Overrides (Non-Production)
- `ROKUR_ALLOW_UNAUTHENTICATED=true`: allow requests without `X-Rokur-Token`.
- `ROKUR_ALLOW_EMPTY_REQUIRED_SECRETS=true`: allow empty required-secrets policy.

## Policy Engine Configuration
- `ROKUR_POLICY_BACKEND`: `builtin` | `external` | `auto` (default `builtin`).
- `ROKUR_POLICY_COMMAND`: executable path when backend resolves to `external`.
- `ROKUR_POLICY_COMMAND_ARGS`: optional args as JSON array or comma-separated list.
- `ROKUR_POLICY_TIMEOUT_MS`: external-policy timeout in milliseconds (default `1500`).

## Run
```bash
cd container-stack/rokur
deno task dev
```

## Example
```bash
export ROKUR_ENV=development
export ROKUR_REQUIRED_SECRETS=panic_profile,panll_api_token
export ROKUR_SECRET_PANIC_PROFILE=enabled
export ROKUR_SECRET_PANLL_API_TOKEN=abc123
export ROKUR_API_TOKEN=dev-token
deno task dev
```

## External-Policy Example (Ephapax Adapter Slot)
```bash
export ROKUR_POLICY_BACKEND=external
export ROKUR_POLICY_COMMAND=deno
export ROKUR_POLICY_COMMAND_ARGS='["run","--quiet","policy/ephapax_adapter_example.js"]'
deno task dev:external
```
