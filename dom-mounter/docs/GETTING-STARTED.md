# Getting Started with stapeln-frontend

A step-by-step guide to using the formally verified DOM mounting library.

## Table of Contents

1. [Quick Start](#quick-start)
2. [Installation](#installation)
3. [Your First Mount](#your-first-mount)
4. [Adding Lifecycle Hooks](#adding-lifecycle-hooks)
5. [Security Hardening](#security-hardening)
6. [Framework Integration](#framework-integration)
7. [Advanced Features](#advanced-features)
8. [Troubleshooting](#troubleshooting)

---

## Quick Start

Get up and running in 5 minutes:

```bash
# Install from npm (once published)
npm install @hyperpolymath/stapeln-frontend

# Or build from source
git clone https://github.com/hyperpolymath/stapeln-frontend.git
cd stapeln-frontend
npm install
npm run build
```

## Installation

### Prerequisites

For **using** the library (npm package):
- Node.js 18+
- npm 8+

For **building** from source:
- [Idris2](https://idris2.readthedocs.io/) (for formal verification)
- [Zig 0.15.2+](https://ziglang.org/) (for FFI)
- [ReScript](https://rescript-lang.org/) (for bindings)

### Option 1: NPM Package (Recommended)

```bash
npm install @hyperpolymath/stapeln-frontend
```

### Option 2: Build from Source

```bash
# Clone repository
git clone https://github.com/hyperpolymath/stapeln-frontend.git
cd stapeln-frontend

# Install JavaScript dependencies
npm install

# Build Zig FFI libraries
cd ffi/zig
./build.sh
cd ../..

# Build ReScript bindings
npx rescript build

# Run tests
npm test
```

---

## Your First Mount

Let's create a simple example that mounts an element to the DOM.

### Step 1: Create HTML

Create `index.html`:

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>stapeln-frontend Demo</title>
</head>
<body>
  <div id="app-root"></div>
  <script type="module" src="main.js"></script>
</body>
</html>
```

### Step 2: Write JavaScript

Create `main.js`:

```javascript
import { mount } from '@hyperpolymath/stapeln-frontend';

// Mount the element
const result = mount('app-root');

if (result.tag === 'Ok') {
  console.log('✓ Element mounted successfully!');
  document.getElementById('app-root').textContent = 'Hello from stapeln-frontend!';
} else {
  console.error('✗ Mount failed:', result.error);
}
```

### Step 3: Run It

```bash
# Using a simple HTTP server
npx http-server -p 8000

# Open http://localhost:8000 in your browser
```

You should see "Hello from stapeln-frontend!" in your browser and a success message in the console.

---

## Adding Lifecycle Hooks

Lifecycle hooks let you run code at specific points in the mounting process.

### Example: Logging Lifecycle Events

```javascript
import { mountWithLifecycle } from '@hyperpolymath/stapeln-frontend';

const hooks = {
  beforeMount: (elementId) => {
    console.log(`[Before] About to mount ${elementId}`);
    // Return Ok() to proceed, Error(msg) to abort
    return { tag: 'Ok', value: undefined };
  },

  afterMount: (elementId) => {
    console.log(`[After] Successfully mounted ${elementId}`);
    // Add content to the mounted element
    document.getElementById(elementId).innerHTML = `
      <h1>Application Mounted</h1>
      <p>Lifecycle hooks are working!</p>
    `;
  },

  onError: (errorMessage) => {
    console.error(`[Error] ${errorMessage}`);
    alert('Mount failed! Check console for details.');
  }
};

const result = mountWithLifecycle('app-root', hooks);
```

### Example: Validation in beforeMount

```javascript
const hooks = {
  beforeMount: (elementId) => {
    const element = document.getElementById(elementId);

    // Validate element exists
    if (!element) {
      return {
        tag: 'Error',
        error: `Element ${elementId} not found in DOM`
      };
    }

    // Validate element is empty
    if (element.children.length > 0) {
      return {
        tag: 'Error',
        error: `Element ${elementId} must be empty before mounting`
      };
    }

    // All good, proceed
    return { tag: 'Ok', value: undefined };
  },

  afterMount: (elementId) => {
    console.log('Validation passed, element mounted!');
  }
};

mountWithLifecycle('app-root', hooks);
```

---

## Security Hardening

Enable CSP validation and audit logging for security-critical applications.

### Example: Secure Mounting

```javascript
import { secureMount } from '@hyperpolymath/stapeln-frontend';

const securityPolicy = {
  requireCSP: true,        // Validate element ID against CSP rules
  enableAuditLog: true,    // Log all operations
  sandboxMode: 'NoSandbox' // Options: 'NoSandbox', 'IframeSandbox', 'ShadowSandbox'
};

const result = secureMount('app-root', securityPolicy);

if (result.tag === 'Ok') {
  console.log('✓ Secure mount successful');
} else {
  console.error('✗ Security validation failed:', result.error);
}
```

### Example: CSP Validation

```javascript
import { validateCSP } from '@hyperpolymath/stapeln-frontend';

// Test different element IDs
const testIds = [
  'valid-element-id',
  'invalid<script>',
  'too-long-' + 'x'.repeat(300),
  'invalid@chars!'
];

testIds.forEach(id => {
  const result = validateCSP(id);
  console.log(`${id}: ${result}`);
  // Results: CSPValid, ScriptDetected, TooLong, InvalidChars
});
```

### Example: Audit Logging

```javascript
import { secureMount, getAuditLog } from '@hyperpolymath/stapeln-frontend';

// Mount with audit logging enabled
secureMount('app-root', {
  requireCSP: true,
  enableAuditLog: true,
  sandboxMode: 'NoSandbox'
});

// Retrieve audit log
const logs = getAuditLog();
logs.forEach(entry => {
  console.log(`[${entry.timestamp}] ${entry.operation}: ${entry.message}`);
});
```

---

## Framework Integration

stapeln-frontend provides adapters for popular frameworks.

### React

```jsx
import { useDomMounter } from '@hyperpolymath/stapeln-frontend/react';

function App() {
  const [mounted, error] = useDomMounter('app-root');

  if (error) {
    return <div className="error">Error: {error}</div>;
  }

  if (!mounted) {
    return <div className="loading">Loading...</div>;
  }

  return (
    <div className="app">
      <h1>Application Mounted!</h1>
      <p>React integration working.</p>
    </div>
  );
}
```

### Solid.js

```jsx
import { createDomMounter } from '@hyperpolymath/stapeln-frontend/solid';
import { Show } from 'solid-js';

function App() {
  const [mounted, error] = createDomMounter('app-root');

  return (
    <Show
      when={mounted()}
      fallback={<div>Loading...</div>}
    >
      <div class="app">
        <h1>Application Mounted!</h1>
        <p>Solid.js integration working.</p>
      </div>
    </Show>
  );
}
```

### Vue 3

```vue
<template>
  <div v-if="error" class="error">Error: {{ error }}</div>
  <div v-else-if="mounted" class="app">
    <h1>Application Mounted!</h1>
    <p>Vue 3 integration working.</p>
  </div>
  <div v-else class="loading">Loading...</div>
</template>

<script setup>
import { useDomMounter } from '@hyperpolymath/stapeln-frontend/vue';

const { mounted, error } = useDomMounter('app-root');
</script>
```

### Web Components

```html
<!DOCTYPE html>
<html>
<head>
  <script type="module">
    import '@hyperpolymath/stapeln-frontend/web-component';
  </script>
</head>
<body>
  <dom-mounter
    element-id="app-root"
    enable-csp
    enable-audit-log
  ></dom-mounter>

  <div id="app-root"></div>
</body>
</html>
```

---

## Advanced Features

### Shadow DOM

Isolate your application in Shadow DOM:

```javascript
import { mountToShadowRoot } from '@hyperpolymath/stapeln-frontend';

// Open mode (accessible from outside)
mountToShadowRoot('app-root', 'Open');

// Closed mode (fully encapsulated)
mountToShadowRoot('app-root', 'Closed');

// Add styles to shadow DOM
const element = document.getElementById('app-root');
const shadowRoot = element.shadowRoot;
shadowRoot.innerHTML = `
  <style>
    h1 { color: blue; }
  </style>
  <h1>Isolated Styles!</h1>
`;
```

### Batch Operations

Mount multiple elements at once:

```javascript
import { mountBatch } from '@hyperpolymath/stapeln-frontend';

const elementIds = ['header', 'main', 'footer'];
const result = mountBatch(elementIds);

console.log('Successful:', result.successful);
// ['header', 'main', 'footer']

console.log('Failed:', result.failed);
// [['sidebar', 'Element not found']]
```

### Lazy Loading

Mount elements when they come into view:

```javascript
import { mountLazy } from '@hyperpolymath/stapeln-frontend';

const options = {
  rootMargin: '50px',  // Start loading 50px before visible
  threshold: 0.1       // Trigger when 10% visible
};

mountLazy('app-root', options);
```

### Health Checking

Check element health before mounting:

```javascript
import { healthCheck } from '@hyperpolymath/stapeln-frontend';

const [status, message] = healthCheck('app-root');

switch (status) {
  case 'Healthy':
    console.log('✓ Element is ready');
    break;
  case 'Degraded':
    console.warn('⚠ Element has issues:', message);
    break;
  case 'Failed':
    console.error('✗ Element check failed:', message);
    break;
}
```

---

## Troubleshooting

### Common Issues

#### Issue: "Element not found"

```javascript
// ✗ Wrong: Element doesn't exist yet
const result = mount('app-root');

// ✓ Correct: Wait for DOM to be ready
document.addEventListener('DOMContentLoaded', () => {
  const result = mount('app-root');
});
```

#### Issue: "CSP validation failed"

```javascript
// ✗ Wrong: Invalid characters in ID
mount('app-root<script>');

// ✓ Correct: Use valid ID (alphanumeric, hyphens, underscores)
mount('app-root');
```

#### Issue: Build errors with Zig

```bash
# Make sure you have Zig 0.15.2+
zig version

# Rebuild libraries
cd ffi/zig
./build.sh
```

#### Issue: ReScript compilation errors

```bash
# Clean and rebuild
npx rescript clean
npx rescript build
```

### Getting Help

- **Documentation**: [README.md](./README.md)
- **Migration Guide**: [MIGRATION-GUIDE.md](./MIGRATION-GUIDE.md)
- **Performance**: [BENCHMARKS.md](./BENCHMARKS.md)
- **Issues**: https://github.com/hyperpolymath/stapeln-frontend/issues
- **Discussions**: https://github.com/hyperpolymath/stapeln-frontend/discussions

---

## Next Steps

1. **Read the full API reference**: [dom_mounter.d.ts](./dom_mounter.d.ts)
2. **Explore migration guides**: [MIGRATION-GUIDE.md](./MIGRATION-GUIDE.md)
3. **Check performance benchmarks**: [BENCHMARKS.md](./BENCHMARKS.md)
4. **Review architecture decisions**: [META.scm](./META.scm)
5. **See what's next**: [ROADMAP.md](./ROADMAP.md)

---

**Happy mounting! 🚀**

For questions or feedback, open an issue at:
https://github.com/hyperpolymath/stapeln-frontend/issues
