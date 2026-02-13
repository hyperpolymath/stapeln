# stapeln-frontend Examples

Interactive examples demonstrating the features of stapeln-frontend.

## Available Examples

### 1. Basic Example

**File**: `basic/index.html`

Demonstrates:
- Basic DOM mounting
- Lifecycle hooks (beforeMount, afterMount)
- Unmount and remount operations
- Health checking
- Console logging

**How to run**:
```bash
cd examples/basic
npx http-server -p 8000
# Open http://localhost:8000
```

### 2. Security Example

**File**: `security/index.html`

Demonstrates:
- CSP validation
- Secure mounting
- Audit logging
- Security policy enforcement
- Attack vector prevention

**How to run**:
```bash
cd examples/security
npx http-server -p 8001
# Open http://localhost:8001
```

### 3. React Example (Coming Soon)

Full React application using `useDomMounter` hook.

### 4. Solid.js Example (Coming Soon)

Solid.js application using `createDomMounter` primitive.

### 5. Vue 3 Example (Coming Soon)

Vue 3 application using `useDomMounter` composable.

## Running Examples

### Option 1: Simple HTTP Server

```bash
# Install http-server globally
npm install -g http-server

# Run in any example directory
cd examples/basic
http-server -p 8000
```

### Option 2: Python HTTP Server

```bash
cd examples/basic
python3 -m http.server 8000
```

### Option 3: Using the Package

Once published to npm:

```bash
npm install @hyperpolymath/stapeln-frontend
```

Then import in your HTML:

```html
<script type="module">
  import { mount } from '@hyperpolymath/stapeln-frontend';
  mount('app-root');
</script>
```

## Example Features

Each example demonstrates:

- ✅ **Type Safety**: TypeScript definitions for all APIs
- ✅ **Error Handling**: Proper Result types and error messages
- ✅ **Performance**: Fast mounting (<0.25ms typical)
- ✅ **Security**: CSP validation, audit logging
- ✅ **Developer Experience**: Clear console output, visual feedback

## Creating Your Own Example

1. Create a new directory in `examples/`
2. Add `index.html` with basic structure
3. Import stapeln-frontend (when published) or use mock functions
4. Add specific feature demonstrations
5. Update this README with your example

## Documentation

For more information:

- [Getting Started Guide](../GETTING-STARTED.md)
- [API Reference](../dom_mounter.d.ts)
- [Migration Guide](../MIGRATION-GUIDE.md)
- [Benchmarks](../BENCHMARKS.md)

## Contributing

Found an issue with an example? Want to add a new one?

Open an issue or pull request at:
https://github.com/hyperpolymath/stapeln-frontend
