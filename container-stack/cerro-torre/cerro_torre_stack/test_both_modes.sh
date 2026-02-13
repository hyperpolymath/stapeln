#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# Test script for Cerro Torre snap-in architecture
# Demonstrates separate vs snapped deployment modes

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
SVALINN_PORT=${SVALINN_PORT:-8000}
VORDR_PORT=${VORDR_PORT:-8080}
SVALINN_URL="http://localhost:${SVALINN_PORT}"
VORDR_URL="http://localhost:${VORDR_PORT}"

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Helper functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[✓]${NC} $*"
}

log_error() {
    echo -e "${RED}[✗]${NC} $*"
}

log_warning() {
    echo -e "${YELLOW}[⚠]${NC} $*"
}

test_endpoint() {
    local name=$1
    local url=$2
    local expected_status=${3:-200}
    local method=${4:-GET}
    local data=${5:-}

    TESTS_RUN=$((TESTS_RUN + 1))

    log_info "Testing: $name"

    if [ -n "$data" ]; then
        response=$(curl -s -w "\n%{http_code}" -X "$method" "$url" \
            -H "Content-Type: application/json" \
            -d "$data" 2>/dev/null || echo "000")
    else
        response=$(curl -s -w "\n%{http_code}" -X "$method" "$url" 2>/dev/null || echo "000")
    fi

    status_code=$(echo "$response" | tail -n1)
    body=$(echo "$response" | sed '$d')

    if [ "$status_code" = "$expected_status" ]; then
        log_success "$name - HTTP $status_code"
        echo "$body" | jq '.' 2>/dev/null || echo "$body"
        TESTS_PASSED=$((TESTS_PASSED + 1))
        return 0
    else
        log_error "$name - Expected $expected_status, got $status_code"
        echo "$body"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    fi
}

check_mode() {
    log_info "Checking deployment mode..."

    mode_info=$(curl -s "$SVALINN_URL/adapter" 2>/dev/null || echo "{}")
    mode=$(echo "$mode_info" | jq -r '.mode // "unknown"')
    connected=$(echo "$mode_info" | jq -r '.connected // false')

    echo -e "\n${BLUE}═══════════════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}Deployment Mode Information${NC}"
    echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
    echo "$mode_info" | jq '.'
    echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}\n"

    if [ "$mode" = "snapped" ]; then
        log_success "Running in SNAPPED mode (direct calls, ~1μs latency)"
        return 0
    elif [ "$mode" = "separate" ]; then
        if [ "$connected" = "true" ]; then
            log_success "Running in SEPARATE mode (HTTP/MCP, ~1-5ms latency) - Connected"
        else
            log_warning "Running in SEPARATE mode but Vörðr is NOT connected"
            log_warning "Start Vörðr separately or add it to umbrella for snap mode"
        fi
        return 1
    else
        log_error "Unknown deployment mode: $mode"
        return 2
    fi
}

benchmark_endpoint() {
    local name=$1
    local url=$2
    local iterations=${3:-100}

    log_info "Benchmarking: $name ($iterations iterations)"

    # Warmup
    curl -s "$url" > /dev/null 2>&1 || true

    # Benchmark
    local start=$(date +%s%N)
    for ((i=1; i<=iterations; i++)); do
        curl -s "$url" > /dev/null 2>&1 || true
    done
    local end=$(date +%s%N)

    local total_ns=$((end - start))
    local avg_ns=$((total_ns / iterations))
    local avg_us=$((avg_ns / 1000))
    local avg_ms=$((avg_ns / 1000000))

    if [ $avg_ms -gt 0 ]; then
        log_info "  Average: ${avg_ms}ms (${avg_us}μs)"
    else
        log_info "  Average: ${avg_us}μs"
    fi
}

run_health_tests() {
    echo -e "\n${GREEN}═══════════════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}Health Check Tests${NC}"
    echo -e "${GREEN}═══════════════════════════════════════════════════════════${NC}\n"

    test_endpoint "GET /health" "$SVALINN_URL/health" 200

    # /ready can be 200 (ready) or 503 (not ready) - both are valid responses
    TESTS_RUN=$((TESTS_RUN + 1))
    log_info "Testing: GET /ready"

    ready_response=$(curl -s -w "\n%{http_code}" "$SVALINN_URL/ready" 2>/dev/null || echo "000")
    ready_status=$(echo "$ready_response" | tail -n1)
    ready_body=$(echo "$ready_response" | sed '$d')

    if [ "$ready_status" = "200" ] || [ "$ready_status" = "503" ]; then
        log_success "GET /ready - HTTP $ready_status ($(echo "$ready_body" | jq -r '.ready // "error"'))"
        echo "$ready_body" | jq '.' 2>/dev/null || echo "$ready_body"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        log_error "GET /ready - Expected 200 or 503, got $ready_status"
        echo "$ready_body"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi

    test_endpoint "GET /adapter" "$SVALINN_URL/adapter" 200
}

run_container_tests() {
    echo -e "\n${GREEN}═══════════════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}Container API Tests${NC}"
    echo -e "${GREEN}═══════════════════════════════════════════════════════════${NC}\n"

    # Check if Vörðr is connected
    mode_info=$(curl -s "$SVALINN_URL/adapter" 2>/dev/null || echo "{}")
    connected=$(echo "$mode_info" | jq -r '.connected // false')

    if [ "$connected" = "false" ]; then
        log_warning "Vörðr not connected - skipping container tests"
        log_warning "Start Vörðr or add to umbrella to test container operations"
        return 0
    fi

    # List containers
    test_endpoint "GET /api/v1/containers" "$SVALINN_URL/api/v1/containers" 200

    # Create container
    create_payload='{
        "imageName": "alpine:latest",
        "imageDigest": "sha256:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef",
        "name": "test-alpine",
        "config": {
            "cmd": ["/bin/sh", "-c", "sleep 30"],
            "env": {"TEST": "true"}
        }
    }'

    test_endpoint "POST /api/v1/containers" "$SVALINN_URL/api/v1/containers" 201 POST "$create_payload"

    # Get container (if create succeeded)
    # Note: Would need to parse container ID from previous response

    # Verify image
    verify_payload='{
        "imageName": "alpine:latest",
        "imageDigest": "sha256:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
    }'

    test_endpoint "POST /api/v1/verify" "$SVALINN_URL/api/v1/verify" 200 POST "$verify_payload"
}

run_benchmark() {
    echo -e "\n${GREEN}═══════════════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}Performance Benchmark${NC}"
    echo -e "${GREEN}═══════════════════════════════════════════════════════════${NC}\n"

    benchmark_endpoint "Health check" "$SVALINN_URL/health" 100
    benchmark_endpoint "Adapter info" "$SVALINN_URL/adapter" 100

    mode_info=$(curl -s "$SVALINN_URL/adapter" 2>/dev/null || echo "{}")
    connected=$(echo "$mode_info" | jq -r '.connected // false')

    if [ "$connected" = "true" ]; then
        benchmark_endpoint "List containers" "$SVALINN_URL/api/v1/containers" 50
    fi
}

print_summary() {
    echo -e "\n${BLUE}═══════════════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}Test Summary${NC}"
    echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
    echo -e "Total tests:  $TESTS_RUN"
    echo -e "${GREEN}Passed:${NC}       $TESTS_PASSED"
    if [ $TESTS_FAILED -gt 0 ]; then
        echo -e "${RED}Failed:${NC}       $TESTS_FAILED"
    fi
    echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}\n"

    if [ $TESTS_FAILED -gt 0 ]; then
        return 1
    fi
    return 0
}

print_usage() {
    cat <<EOF
Usage: $0 [OPTIONS]

Test script for Cerro Torre snap-in architecture.

OPTIONS:
    --health-only       Run health check tests only
    --no-benchmark      Skip performance benchmarks
    --iterations N      Number of benchmark iterations (default: 100)
    --help              Show this help message

ENVIRONMENT VARIABLES:
    SVALINN_PORT        Svalinn HTTP port (default: 8000)
    VORDR_PORT          Vörðr HTTP port (default: 8080)

EXAMPLES:
    # Run all tests with default settings
    $0

    # Run health checks only
    $0 --health-only

    # Run with more benchmark iterations
    $0 --iterations 1000

    # Test custom port
    SVALINN_PORT=9000 $0
EOF
}

main() {
    local health_only=false
    local run_benchmark=true
    local iterations=100

    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            --health-only)
                health_only=true
                shift
                ;;
            --no-benchmark)
                run_benchmark=false
                shift
                ;;
            --iterations)
                iterations=$2
                shift 2
                ;;
            --help)
                print_usage
                exit 0
                ;;
            *)
                log_error "Unknown option: $1"
                print_usage
                exit 1
                ;;
        esac
    done

    echo -e "${BLUE}╔═══════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║  Cerro Torre Stack - Snap-In Architecture Test Suite     ║${NC}"
    echo -e "${BLUE}╚═══════════════════════════════════════════════════════════╝${NC}\n"

    # Check if Svalinn is running
    if ! curl -s "$SVALINN_URL/health" > /dev/null 2>&1; then
        log_error "Svalinn is not running on $SVALINN_URL"
        log_info "Start it with: mix phx.server --prefix svalinn"
        exit 1
    fi

    log_success "Svalinn is running on $SVALINN_URL"

    # Check deployment mode (allow non-zero return - it just indicates mode)
    set +e
    check_mode
    is_snapped=$?
    set -e

    # Run tests
    run_health_tests

    if [ "$health_only" = false ]; then
        run_container_tests
    fi

    if [ "$run_benchmark" = true ]; then
        run_benchmark
    fi

    # Print summary
    print_summary
    exit_code=$?

    # Mode-specific advice
    echo -e "\n${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    if [ $is_snapped -eq 0 ]; then
        echo -e "${YELLOW}Running in SNAPPED mode - Maximum performance! 🚀${NC}"
        echo -e "${YELLOW}Typical latency: ~1μs for direct function calls${NC}"
    else
        echo -e "${YELLOW}Running in SEPARATE mode${NC}"
        echo -e "${YELLOW}For 1000× performance boost:${NC}"
        echo -e "${YELLOW}  1. Add Vörðr to apps/vordr/${NC}"
        echo -e "${YELLOW}  2. Run: mix clean && mix compile${NC}"
        echo -e "${YELLOW}  3. Run: mix phx.server${NC}"
        echo -e "${YELLOW}  4. Check mode: curl $SVALINN_URL/adapter${NC}"
    fi
    echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}\n"

    exit $exit_code
}

# Check dependencies
for cmd in curl jq; do
    if ! command -v $cmd &> /dev/null; then
        log_error "$cmd is required but not installed"
        exit 1
    fi
done

main "$@"
