#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later

set -u

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PASS_COUNT=0
FAIL_COUNT=0

run_gate() {
  local gate_name="$1"
  shift
  echo
  echo "== Gate: ${gate_name}"
  if "$@"; then
    echo "PASS: ${gate_name}"
    PASS_COUNT=$((PASS_COUNT + 1))
  else
    echo "FAIL: ${gate_name}"
    FAIL_COUNT=$((FAIL_COUNT + 1))
  fi
}

gate_repo_clean() {
  cd "${ROOT_DIR}" || return 1
  local untracked
  untracked="$(git ls-files --others --exclude-standard)"
  git diff --quiet &&
    git diff --cached --quiet &&
    [[ -z "${untracked}" ]]
}

gate_rescript_locks_untracked() {
  cd "${ROOT_DIR}" || return 1
  ! git ls-files | grep -q 'rescript.lock$'
}

gate_deno_tests() {
  cd "${ROOT_DIR}" || return 1
  deno test --allow-read --allow-write tests/stapeln.test.js
}

gate_frontend_build() {
  cd "${ROOT_DIR}/frontend" || return 1
  local compiler_info_file="${ROOT_DIR}/frontend/lib/bs/compiler-info.json"
  local react_compiler_info_file="${ROOT_DIR}/frontend/node_modules/@rescript/react/lib/bs/compiler-info.json"
  local compiler_info_snapshot=""
  local react_compiler_info_snapshot=""

  if [[ -f "${compiler_info_file}" ]]; then
    compiler_info_snapshot="$(cat "${compiler_info_file}")"
  fi

  if [[ -f "${react_compiler_info_file}" ]]; then
    react_compiler_info_snapshot="$(cat "${react_compiler_info_file}")"
  fi

  if ! timeout 300s rescript build; then
    return 1
  fi

  # ReScript rewrites generated_at timestamps in tracked metadata files.
  # Restore snapshots to keep readiness checks non-mutating.
  if [[ -n "${compiler_info_snapshot}" ]]; then
    printf "%s" "${compiler_info_snapshot}" >"${compiler_info_file}"
  fi

  if [[ -n "${react_compiler_info_snapshot}" ]]; then
    printf "%s" "${react_compiler_info_snapshot}" >"${react_compiler_info_file}"
  fi

  return 0
}

gate_backend_tests() {
  cd "${ROOT_DIR}/backend" || return 1
  MIX_OS_CONCURRENCY_LOCK=0 mix deps.get >/dev/null &&
    MIX_OS_CONCURRENCY_LOCK=0 mix test
}

echo "stapeln readiness checks"
echo "root: ${ROOT_DIR}"

run_gate "Repo clean (tracked + untracked)" gate_repo_clean
run_gate "ReScript lockfiles not tracked" gate_rescript_locks_untracked
run_gate "Root Deno tests" gate_deno_tests
run_gate "Frontend ReScript build" gate_frontend_build
run_gate "Backend Mix tests" gate_backend_tests

echo
echo "== Summary"
echo "Passed: ${PASS_COUNT}"
echo "Failed: ${FAIL_COUNT}"

if [[ "${FAIL_COUNT}" -ne 0 ]]; then
  exit 1
fi

exit 0
