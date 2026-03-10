# SPDX-License-Identifier: PMPL-1.0-or-later
# justfile — Rokur secrets management gate

# Run the test suite.
test:
    deno task test

# Lint all source files.
lint:
    deno task lint

# Format all source files.
fmt:
    deno task fmt

# Check formatting without modifying files.
fmt-check:
    deno task fmt:check

# Run the development server with all permissions.
dev:
    deno run --allow-net --allow-env --allow-read --allow-write main.js

# Run all checks: lint, format verification, and tests.
check: lint fmt-check test
