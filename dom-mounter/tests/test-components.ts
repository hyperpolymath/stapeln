#!/usr/bin/env -S deno run --allow-read
// SPDX-License-Identifier: PMPL-1.0-or-later
// test-components.ts - Verify all ReScript components compile and structure

import { existsSync } from "https://deno.land/std@0.224.0/fs/mod.ts";

console.log(`
╔════════════════════════════════════════════════════════════╗
║  🧪 stapeln Component Test Suite                           ║
║  Testing ReScript compilation and structure                ║
╚════════════════════════════════════════════════════════════╝
`);

interface TestResult {
  name: string;
  passed: boolean;
  message: string;
}

const results: TestResult[] = [];

// Helper function to test file existence
function testFileExists(path: string, description: string): boolean {
  const exists = existsSync(path);
  results.push({
    name: description,
    passed: exists,
    message: exists ? `✓ ${path}` : `✗ ${path} not found`,
  });
  return exists;
}

// Helper function to test file content
async function testFileContains(
  path: string,
  searchString: string,
  description: string
): Promise<boolean> {
  try {
    const content = await Deno.readTextFile(path);
    const contains = content.includes(searchString);
    results.push({
      name: description,
      passed: contains,
      message: contains
        ? `✓ ${path} contains "${searchString}"`
        : `✗ ${path} missing "${searchString}"`,
    });
    return contains;
  } catch {
    results.push({
      name: description,
      passed: false,
      message: `✗ ${path} could not be read`,
    });
    return false;
  }
}

// Test 1: Core UI Components
console.log("📦 Testing Core UI Components...\n");

testFileExists("src/App.res", "App.res exists");
testFileExists("src/Model.res", "Model.res exists");
testFileExists("src/Msg.res", "Msg.res exists");
testFileExists("src/Update.res", "Update.res exists");
testFileExists("src/View.res", "View.res exists");
testFileExists("src/CiscoView.res", "CiscoView.res exists");

// Test 2: Phase 2 Components
console.log("\n🔐 Testing Phase 2 Security Components...\n");

testFileExists("src/PortConfigPanel.res", "PortConfigPanel.res exists");
testFileExists(
  "src/SecurityInspector.res",
  "SecurityInspector.res exists"
);
testFileExists("src/GapAnalysis.res", "GapAnalysis.res exists");
testFileExists("src/SimulationMode.res", "SimulationMode.res exists");

// Test 3: Proven Libraries
console.log("\n⚡ Testing Idris² Proven Integration...\n");

testFileExists("src/abi/DomMounter.idr", "Idris2 ABI: DomMounter.idr");
testFileExists("src/DomMounter.res", "ReScript binding: DomMounter.res");
testFileExists("src/IdrisBadge.res", "Idris² badge component");
testFileExists("ffi/zig/src/dom_mounter.zig", "Zig FFI implementation");

// Test 4: Entry Point
console.log("\n🚀 Testing Application Entry Point...\n");

testFileExists("src/Index.res", "Index.res entry point");

// Test 5: Content Checks
console.log("\n🔍 Testing Component Content...\n");

await testFileContains(
  "src/PortConfigPanel.res",
  "Ephemeral",
  "PortConfigPanel has ephemeral support"
);
await testFileContains(
  "src/SecurityInspector.res",
  "SecurityMetrics",
  "SecurityInspector has metrics"
);
await testFileContains(
  "src/GapAnalysis.res",
  "gapCategory",
  "GapAnalysis has gap categorization"
);
await testFileContains(
  "src/SimulationMode.res",
  "packetType",
  "SimulationMode has packet types"
);
await testFileContains(
  "src/App.res",
  "PortConfigView",
  "App.res integrates PortConfigPanel"
);
await testFileContains(
  "src/App.res",
  "SecurityView",
  "App.res integrates SecurityInspector"
);
await testFileContains(
  "src/App.res",
  "GapAnalysisView",
  "App.res integrates GapAnalysis"
);
await testFileContains(
  "src/App.res",
  "SimulationView",
  "App.res integrates SimulationMode"
);
await testFileContains(
  "src/App.res",
  "IdrisBadge",
  "App.res includes Idris² badge"
);

// Test 6: Idris2 Proofs
console.log("\n🔐 Testing Formal Proofs...\n");

await testFileContains(
  "src/abi/DomMounter.idr",
  "ValidElementId",
  "Idris2: ValidElementId proof exists"
);
await testFileContains(
  "src/abi/DomMounter.idr",
  "NoMemoryLeak",
  "Idris2: NoMemoryLeak proof exists"
);
await testFileContains(
  "src/abi/DomMounter.idr",
  "AtomicMount",
  "Idris2: AtomicMount proof exists"
);
await testFileContains(
  "src/abi/DomMounter.idr",
  "%default total",
  "Idris2: Totality checking enabled"
);

// Test 7: Documentation
console.log("\n📚 Testing Documentation...\n");

testFileExists("../PROVEN-LIBRARIES.md", "Proven libraries documentation");
testFileExists("../ROADMAP.md", "Project roadmap");
testFileExists("../STATE.scm", "Project state");
testFileExists(
  "../SESSION-SUMMARY-2026-02-05.md",
  "Session summary documentation"
);

// Test 8: ABI Directory Cleanliness
console.log("\n🧹 Testing ABI Directory Purity...\n");

const abiHasZig = existsSync("src/abi/build.zig");
const abiHasZigSrc = existsSync("src/abi/src/main.zig");

results.push({
  name: "ABI directory is clean (no Zig files)",
  passed: !abiHasZig && !abiHasZigSrc,
  message: !abiHasZig && !abiHasZigSrc
    ? "✓ src/abi/ contains only Idris2 code"
    : "✗ src/abi/ contains non-Idris2 files (contamination)",
});

// Test 9: Compiled Outputs
console.log("\n📦 Testing Compiled Outputs...\n");

testFileExists("lib/bs/src/App.ast", "ReScript compilation: App.ast");
testFileExists(
  "lib/bs/src/PortConfigPanel.ast",
  "ReScript compilation: PortConfigPanel.ast"
);
testFileExists(
  "lib/bs/src/SecurityInspector.ast",
  "ReScript compilation: SecurityInspector.ast"
);
testFileExists(
  "lib/bs/src/GapAnalysis.ast",
  "ReScript compilation: GapAnalysis.ast"
);
testFileExists(
  "lib/bs/src/SimulationMode.ast",
  "ReScript compilation: SimulationMode.ast"
);

// Print Results
console.log("\n" + "=".repeat(60));
console.log("📊 TEST RESULTS");
console.log("=".repeat(60) + "\n");

let passCount = 0;
let failCount = 0;

results.forEach((result) => {
  if (result.passed) {
    passCount++;
    console.log(`✓ ${result.name}`);
  } else {
    failCount++;
    console.log(`✗ ${result.name}`);
    console.log(`  ${result.message}`);
  }
});

console.log("\n" + "=".repeat(60));
console.log(`Total: ${results.length} tests`);
console.log(`✓ Passed: ${passCount}`);
console.log(`✗ Failed: ${failCount}`);
console.log("=".repeat(60));

if (failCount === 0) {
  console.log("\n🎉 All tests passed! Components are ready for deployment.\n");
  Deno.exit(0);
} else {
  console.log(
    `\n⚠️  ${failCount} test(s) failed. Please review and fix issues.\n`
  );
  Deno.exit(1);
}
