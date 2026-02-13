#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Svalinn Gateway - Manual Testing Script

set -e

cd "$(dirname "$0")"

echo "=== Svalinn Gateway Test Suite ==="
echo

# Start server in background
echo "Starting server..."
deno run --config deno.json --allow-net --allow-read --allow-env src/Main.res.js > /tmp/svalinn-test.log 2>&1 &
SERVER_PID=$!
echo "Server PID: $SERVER_PID"

# Wait for server to start
sleep 3

# Function to test endpoint
test_endpoint() {
    local name="$1"
    local method="$2"
    local url="$3"
    local data="$4"

    echo "Testing: $name"
    if [ -z "$data" ]; then
        curl -s -X "$method" "http://localhost:8000$url" | jq . 2>/dev/null || echo "(response not JSON)"
    else
        curl -s -X "$method" "http://localhost:8000$url" \
            -H "Content-Type: application/json" \
            -d "$data" | jq . 2>/dev/null || echo "(response not JSON)"
    fi
    echo
}

# Run tests
test_endpoint "Health Check" GET "/health"
test_endpoint "Readiness Check" GET "/ready"
test_endpoint "List Containers" GET "/api/v1/containers"
test_endpoint "List Images" GET "/api/v1/images"

test_endpoint "Create Container (invalid)" POST "/api/v1/containers" \
    '{"invalid": "data"}'

test_endpoint "Create Container (valid)" POST "/api/v1/containers" \
    '{
        "imageName": "alpine:latest",
        "imageDigest": "sha256:abc123",
        "name": "test-container"
    }'

test_endpoint "Verify Image" POST "/api/v1/verify" \
    '{
        "imageName": "alpine:latest",
        "imageDigest": "sha256:abc123"
    }'

# Cleanup
echo "Stopping server (PID: $SERVER_PID)..."
kill $SERVER_PID 2>/dev/null || true
wait $SERVER_PID 2>/dev/null || true

echo
echo "=== Test Complete ==="
echo "Server logs: /tmp/svalinn-test.log"
