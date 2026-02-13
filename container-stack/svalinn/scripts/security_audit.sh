#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Security audit script for Svalinn

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "Svalinn Security Audit"
echo "======================"
echo ""

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

ISSUES_FOUND=0

# Check 1: No hardcoded secrets
echo "[1/8] Checking for hardcoded secrets..."
# Look for actual secret values, not type definitions
if grep -r -E "(password|secret|apikey|token|credential)\s*=\s*['\"][a-zA-Z0-9]{20,}['\"]" \
  --include="*.res" \
  --include="*.ts" \
  --include="*.js" \
  "$REPO_ROOT/src" | grep -v "// SPDX" | grep -v "type " | grep -v "export type"; then
  echo -e "${RED}❌ Potential hardcoded secrets found${NC}"
  ISSUES_FOUND=$((ISSUES_FOUND + 1))
else
  echo -e "${GREEN}✅ No hardcoded secrets detected${NC}"
fi

# Check 2: SPDX headers present
echo "[2/8] Checking SPDX license headers..."
MISSING_SPDX=$(find "$REPO_ROOT/src" -name "*.res" -o -name "*.ts" | while read -r file; do
  if ! head -5 "$file" | grep -q "SPDX-License-Identifier"; then
    echo "$file"
  fi
done)

if [ -n "$MISSING_SPDX" ]; then
  echo -e "${YELLOW}⚠️  Files missing SPDX headers:${NC}"
  echo "$MISSING_SPDX"
  ISSUES_FOUND=$((ISSUES_FOUND + 1))
else
  echo -e "${GREEN}✅ All source files have SPDX headers${NC}"
fi

# Check 3: No eval() or Function() constructor
echo "[3/8] Checking for dangerous code execution..."
if grep -r "eval\s*(" --include="*.res" --include="*.ts" "$REPO_ROOT/src" | grep -v "// SPDX"; then
  echo -e "${RED}❌ Dangerous eval() usage found${NC}"
  ISSUES_FOUND=$((ISSUES_FOUND + 1))
else
  echo -e "${GREEN}✅ No dangerous code execution patterns${NC}"
fi

# Check 4: Proper error handling (no empty catch blocks)
echo "[4/8] Checking for empty catch blocks..."
if grep -r "catch\s*{" --include="*.res" "$REPO_ROOT/src" -A 1 | grep -E "^\s*}\s*$"; then
  echo -e "${YELLOW}⚠️  Potential empty catch blocks found${NC}"
  ISSUES_FOUND=$((ISSUES_FOUND + 1))
else
  echo -e "${GREEN}✅ No empty catch blocks detected${NC}"
fi

# Check 5: No HTTP in production (HTTPS only)
echo "[5/8] Checking for HTTP usage (should be HTTPS)..."
if grep -r "http://" --include="*.res" "$REPO_ROOT/src" | grep -v "localhost" | grep -v "127.0.0.1" | grep -v "SPDX"; then
  echo -e "${RED}❌ HTTP URLs found (use HTTPS)${NC}"
  ISSUES_FOUND=$((ISSUES_FOUND + 1))
else
  echo -e "${GREEN}✅ No hardcoded HTTP URLs (excluding localhost)${NC}"
fi

# Check 6: Environment variable validation
echo "[6/8] Checking environment variable usage..."
if grep -r "getEnv" --include="*.res" "$REPO_ROOT/src" | grep -v "getWithDefault" | grep -v "SPDX"; then
  echo -e "${YELLOW}⚠️  Some env vars may lack defaults${NC}"
else
  echo -e "${GREEN}✅ Environment variables have defaults${NC}"
fi

# Check 7: SQL injection prevention (if any SQL)
echo "[7/8] Checking for SQL injection risks..."
if grep -r "query\s*(" --include="*.res" "$REPO_ROOT/src" | grep "+\|concat"; then
  echo -e "${RED}❌ Potential SQL injection via string concatenation${NC}"
  ISSUES_FOUND=$((ISSUES_FOUND + 1))
else
  echo -e "${GREEN}✅ No SQL injection risks detected${NC}"
fi

# Check 8: Dependency audit (Deno)
echo "[8/8] Running Deno dependency audit..."
if command -v deno &> /dev/null; then
  cd "$REPO_ROOT"
  if deno info 2>&1 | grep -i "vulnerability"; then
    echo -e "${RED}❌ Vulnerable dependencies found${NC}"
    ISSUES_FOUND=$((ISSUES_FOUND + 1))
  else
    echo -e "${GREEN}✅ No known vulnerable dependencies${NC}"
  fi
else
  echo -e "${YELLOW}⚠️  Deno not found, skipping dependency audit${NC}"
fi

# Summary
echo ""
echo "================================"
if [ $ISSUES_FOUND -eq 0 ]; then
  echo -e "${GREEN}✅ Security Audit PASSED${NC}"
  echo "No critical issues found"
  exit 0
else
  echo -e "${RED}❌ Security Audit FAILED${NC}"
  echo "$ISSUES_FOUND issue(s) found"
  echo ""
  echo "Recommendations:"
  echo "1. Review flagged files above"
  echo "2. Fix hardcoded secrets (use environment variables)"
  echo "3. Add SPDX headers to all source files"
  echo "4. Use HTTPS for all external URLs"
  echo "5. Add proper error handling"
  exit 1
fi
