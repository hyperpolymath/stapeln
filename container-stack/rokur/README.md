# Rokur

Rokur is the secrets gate used by the stapeln runtime plane before container
start operations. It validates that required secrets are present, evaluates
authorization policy, and provides audit logging of all decisions.

## Endpoints

| Method | Path                  | Auth | Description                                                  |
| ------ | --------------------- | ---- | ------------------------------------------------------------ |
| `GET`  | `/health`             | No   | Liveness check with non-sensitive configuration status       |
| `GET`  | `/v1/secrets/status`  | Yes  | Current allow/deny status from required-secret checks        |
| `POST` | `/v1/authorize-start` | Yes  | Pre-start authorization for runtime calls (used by Svalinn)  |
| `GET`  | `/metrics`            | No   | Operational metrics (counters, uptime, rate limiter stats)   |
| `POST` | `/v1/secrets/reload`  | Yes  | Hot-reload required secrets from environment without restart |

## Response Shape

Authorization responses include:

- `allowed` (boolean), `policy` ("allow"/"deny"), `code` (decision reason)
- `requiredSecretCount`, `missingSecretCount` — numeric counts only
- `policyEngine` ("builtin"/"external"), `decisionTimestamp`
- `requestId` — unique per-request trace identifier
- Secret names are intentionally **never** exposed in API responses.

## Policy Engine Boundary

- `ROKUR_POLICY_BACKEND=builtin` (default): evaluate required secrets directly
  in Rokur.
- `ROKUR_POLICY_BACKEND=external`: evaluate via an external command (Ephapax
  adapter path).
- `ROKUR_POLICY_BACKEND=auto`: use external when configured, otherwise builtin.

### External Policy Contract

Rokur sends JSON on stdin to the external command:

- `version`, `timestamp`, `requiredSecrets`, and `input` (`image`, `name`).

External command must print one JSON decision to stdout:

- `allowed` (boolean), optional `policy`, `code`, `requiredSecretCount`,
  `missingSecretCount`.

Non-zero exit, invalid JSON, or timeout is **fail-closed** (`deny`).

## Secret Policy Model

- Configure required secret names via `ROKUR_REQUIRED_SECRETS`
  (comma-separated).
- For each required secret `name`, Rokur expects env var `ROKUR_SECRET_<NAME>`.
  - Example: `panic_profile` → `ROKUR_SECRET_PANIC_PROFILE`
- Authorization is denied when any required secret is missing or blank.

## Authentication

- Set `ROKUR_API_TOKEN` to require `X-Rokur-Token` header on API calls.
- By default, Rokur fails to start if `ROKUR_API_TOKEN` is missing.
- For non-production local development only: `ROKUR_ALLOW_UNAUTHENTICATED=true`.

## Security Features

### Rate Limiting

Per-IP sliding window rate limiting protects against brute-force token guessing:

- General request limit: `ROKUR_RATE_LIMIT_MAX` per `ROKUR_RATE_LIMIT_WINDOW_MS`
  (default 60/min)
- Auth failure limit: `ROKUR_RATE_LIMIT_AUTH_FAIL_MAX` per window (default
  5/min, stricter)
- Rate-limited requests receive `429 Too Many Requests` with `Retry-After`
  header.

### Audit Logging

Every authorization decision and authentication failure is recorded:

- Structured JSONL format with timestamps, request IDs, and client IPs.
- File output: set `ROKUR_AUDIT_LOG_PATH` to write to a file (append-only).
- Stderr fallback: when no file path is configured, audit records go to stderr.
- Disable: `ROKUR_AUDIT_LOG=false`.

### Request Tracing

- Every response includes `X-Request-Id` header.
- Clients can send their own `X-Request-Id` for distributed trace correlation.
- Request IDs appear in all logs and audit records.

### Secure Defaults

- Default bind host is `127.0.0.1` (set `ROKUR_HOST` to override).
- Startup fails if `ROKUR_REQUIRED_SECRETS` is empty.
- Startup fails if `ROKUR_API_TOKEN` is missing.
- In `ROKUR_ENV=production`, insecure bypass flags are rejected.
- Fail-closed: any policy engine error results in `deny`.

### Graceful Shutdown

- `SIGTERM` / `SIGINT`: graceful shutdown (closes audit log, drains
  connections).
- `SIGHUP`: hot-reload secrets from environment without restart.

## Configuration Reference

| Variable                             | Default       | Description                                     |
| ------------------------------------ | ------------- | ----------------------------------------------- |
| `ROKUR_HOST`                         | `127.0.0.1`   | Bind host                                       |
| `ROKUR_PORT`                         | `9090`        | Bind port                                       |
| `ROKUR_ENV`                          | `development` | Environment (`production` enforces strict mode) |
| `ROKUR_API_TOKEN`                    | _(required)_  | API authentication token                        |
| `ROKUR_REQUIRED_SECRETS`             | _(required)_  | Comma-separated secret names                    |
| `ROKUR_POLICY_BACKEND`               | `builtin`     | Policy engine: `builtin`, `external`, `auto`    |
| `ROKUR_POLICY_COMMAND`               |               | External policy command path                    |
| `ROKUR_POLICY_COMMAND_ARGS`          |               | Args as JSON array or CSV                       |
| `ROKUR_POLICY_TIMEOUT_MS`            | `1500`        | External policy timeout (100–60000)             |
| `ROKUR_ALLOW_UNAUTHENTICATED`        | `false`       | Dev-only: skip token auth                       |
| `ROKUR_ALLOW_EMPTY_REQUIRED_SECRETS` | `false`       | Dev-only: allow empty secrets                   |
| `ROKUR_TRUST_PROXY`                  | `false`       | Trust `X-Forwarded-For` for client IP           |
| `ROKUR_REQUEST_LOG`                  | `true`        | Enable request logging to stdout                |
| `ROKUR_AUDIT_LOG`                    | `true`        | Enable audit logging                            |
| `ROKUR_AUDIT_LOG_PATH`               |               | File path for audit log (JSONL)                 |
| `ROKUR_RATE_LIMIT_WINDOW_MS`         | `60000`       | Rate limit sliding window                       |
| `ROKUR_RATE_LIMIT_MAX`               | `60`          | Max requests per window per IP                  |
| `ROKUR_RATE_LIMIT_AUTH_FAIL_MAX`     | `5`           | Max auth failures per window per IP             |

## Development

```bash
cd container-stack/rokur

# Run in development mode
export ROKUR_ENV=development
export ROKUR_REQUIRED_SECRETS=panic_profile,panll_api_token
export ROKUR_SECRET_PANIC_PROFILE=enabled
export ROKUR_SECRET_PANLL_API_TOKEN=abc123
export ROKUR_API_TOKEN=dev-token
deno task dev

# Run tests
deno task test

# Run unit tests only
deno task test:unit

# Run integration tests only
deno task test:integration

# Type check
deno task check

# Lint and format
deno task lint
deno task fmt:check
```

## External-Policy Example (Ephapax Adapter Slot)

```bash
export ROKUR_POLICY_BACKEND=external
export ROKUR_POLICY_COMMAND=deno
export ROKUR_POLICY_COMMAND_ARGS='["run","--quiet","policy/ephapax_adapter_example.js"]'
deno task dev:external
```
