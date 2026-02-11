// SPDX-License-Identifier: PMPL-1.0-or-later
// Minimal local assertion helpers for offline test execution.

function isObject(value) {
  return value !== null && typeof value === "object";
}

function deepEqual(a, b) {
  if (Object.is(a, b)) return true;

  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i += 1) {
      if (!deepEqual(a[i], b[i])) return false;
    }
    return true;
  }

  if (isObject(a) && isObject(b)) {
    const aKeys = Object.keys(a).sort();
    const bKeys = Object.keys(b).sort();
    if (!deepEqual(aKeys, bKeys)) return false;
    for (const key of aKeys) {
      if (!deepEqual(a[key], b[key])) return false;
    }
    return true;
  }

  return false;
}

export function assert(condition, message = "Assertion failed") {
  if (!condition) {
    throw new Error(message);
  }
}

export function assertEquals(actual, expected, message = "Values are not equal") {
  if (!deepEqual(actual, expected)) {
    throw new Error(`${message}\nactual: ${JSON.stringify(actual)}\nexpected: ${JSON.stringify(expected)}`);
  }
}

export function assertExists(value, message = "Expected value to exist") {
  if (value === null || value === undefined) {
    throw new Error(message);
  }
}
