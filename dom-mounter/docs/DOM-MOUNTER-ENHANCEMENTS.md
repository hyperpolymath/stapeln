# DOM Mounter Enhancement Plan

**Current Status**: Basic formally verified DOM mounting with Idris2 → Zig → ReScript
**Target**: Production-grade high-assurance mounting system

---

## 1. Dependability Enhancements

### A. Health Checks & Monitoring
```idris
-- Idris2 proof extensions
data HealthCheckResult = Healthy | Degraded String | Failed String

public export
data ContinuousValidation : Type where
  MonitoredMount : HealthCheckResult -> ContinuousValidation

-- Verify element remains valid after mount
export
healthCheck : String -> IO HealthCheckResult
```

```zig
// Zig FFI additions
pub const HealthStatus = enum(c_int) {
    Healthy = 0,
    Degraded = 1,
    Failed = 2,
};

export fn health_check(element_id: [*:0]const u8) c_int {
    // Check element exists, is in DOM, is visible, etc.
    if (!element_exists(element_id)) return @intFromEnum(HealthStatus.Failed);
    if (!is_visible(element_id)) return @intFromEnum(HealthStatus.Degraded);
    return @intFromEnum(HealthStatus.Healthy);
}

export fn is_visible(element_id: [*:0]const u8) bool {
    // Check if element is actually visible (not display:none, visibility:hidden)
}
```

### B. Recovery Mechanisms
```rescript
// ReScript additions
type recoveryStrategy =
  | Retry(int) // Retry N times
  | Fallback(string) // Use fallback element
  | CreateElement(string) // Create element if missing

let mountWithRecovery = (
  elementId: string,
  strategy: recoveryStrategy,
): mountResult => {
  switch mount(elementId) {
  | Ok() => Ok()
  | Error(msg) =>
      switch strategy {
      | Retry(n) if n > 0 => mountWithRecovery(elementId, Retry(n - 1))
      | Fallback(fallbackId) => mount(fallbackId)
      | CreateElement(tag) => {
          createElementAndMount(elementId, tag)
        }
      | _ => Error(msg)
      }
  }
}
```

### C. Lifecycle Hooks
```rescript
type lifecycle = {
  beforeMount: string => Result.t<unit, string>,
  afterMount: string => unit,
  beforeUnmount: string => unit,
  afterUnmount: string => unit,
  onError: string => unit,
}

let mountWithLifecycle = (
  elementId: string,
  hooks: lifecycle,
): mountResult => {
  switch hooks.beforeMount(elementId) {
  | Error(e) => {
      hooks.onError(e)
      Error(e)
    }
  | Ok() =>
      switch mount(elementId) {
      | Ok() => {
          hooks.afterMount(elementId)
          Ok()
        }
      | Error(e) => {
          hooks.onError(e)
          Error(e)
        }
      }
  }
}
```

---

## 2. Security Enhancements

### A. CSP (Content Security Policy) Validation
```idris
-- Prove CSP compliance at type level
data CSPCompliant : String -> Type where
  SafeElementId : (id : String) ->
                  (ValidChars id) ->
                  (NoScriptTags id) ->
                  CSPCompliant id

data ValidChars : String -> Type where
  AlphanumericDash : ValidChars id -- [a-zA-Z0-9-_]

data NoScriptTags : String -> Type where
  NoScript : NoScriptTags id -- No <script>, javascript:, etc.
```

```zig
// Zig CSP validation
pub const CSPResult = enum(c_int) {
    Valid = 0,
    InvalidChars = 1,
    ScriptDetected = 2,
    TooLong = 3,
};

export fn validate_csp(element_id: [*:0]const u8) c_int {
    const len = std.mem.len(element_id);
    if (len == 0 or len > 255) return @intFromEnum(CSPResult.TooLong);

    // Check for script injection attempts
    if (contains_script_tag(element_id)) {
        return @intFromEnum(CSPResult.ScriptDetected);
    }

    // Validate characters (alphanumeric + dash/underscore only)
    for (0..len) |i| {
        const c = element_id[i];
        if (!is_valid_char(c)) {
            return @intFromEnum(CSPResult.InvalidChars);
        }
    }

    return @intFromEnum(CSPResult.Valid);
}

fn is_valid_char(c: u8) bool {
    return (c >= 'a' and c <= 'z') or
           (c >= 'A' and c <= 'Z') or
           (c >= '0' and c <= '9') or
           c == '-' or c == '_';
}
```

### B. Audit Logging
```rescript
type auditLog = {
  timestamp: float,
  operation: string,
  elementId: string,
  result: Result.t<unit, string>,
  metadata: dict<string>,
}

let auditedMount = (elementId: string): mountResult => {
  let startTime = Js.Date.now()
  let result = mount(elementId)

  let log: auditLog = {
    timestamp: startTime,
    operation: "mount",
    elementId: elementId,
    result: result,
    metadata: Dict.fromArray([
      ("userAgent", WebAPI.getUserAgent()),
      ("documentTitle", WebAPI.getDocumentTitle()),
    ]),
  }

  sendAuditLog(log)
  result
}
```

### C. Sandboxing Support
```zig
// Mount within an iframe sandbox
export fn mount_sandboxed(
    element_id: [*:0]const u8,
    sandbox_flags: [*:0]const u8,
) c_int {
    // Create sandboxed iframe with specified flags
    // "allow-scripts allow-same-origin" etc.
}
```

---

## 3. Usability Enhancements

### A. Better Error Messages
```rescript
type mountError =
  | ElementNotFound(string) // "Element 'app-root' not found in DOM"
  | InvalidElementId(string) // "Element ID '' is empty"
  | ElementAlreadyMounted(string) // "Element 'app-root' already has content"
  | ParentNotFound(string) // "Parent element for 'app-root' not found"
  | PermissionDenied(string) // "Cannot mount to protected element"

let errorToUserMessage = (error: mountError): string => {
  switch error {
  | ElementNotFound(id) =>
      `Element with ID "${id}" not found. Check your HTML for <div id="${id}"></div>`
  | InvalidElementId(id) =>
      `Element ID "${id}" is invalid. IDs must be non-empty alphanumeric strings.`
  | ElementAlreadyMounted(id) =>
      `Element "${id}" already has content. Use unmount() first or choose a different element.`
  | ParentNotFound(id) =>
      `Cannot find parent element for "${id}". Ensure the element is in the document.`
  | PermissionDenied(id) =>
      `Permission denied mounting to "${id}". This may be a protected or restricted element.`
  }
}
```

### B. Developer Tools Integration
```rescript
// DevTools panel integration
module DevTools = {
  type mountStats = {
    totalMounts: int,
    activeMounts: int,
    failedMounts: int,
    avgMountTime: float,
  }

  let getStats = (): mountStats => {
    // Return mount statistics for DevTools panel
  }

  let inspectElement = (elementId: string): option<elementInfo> => {
    // Return detailed info about mounted element
  }

  let highlightMountedElements = (): unit => {
    // Add visual highlight to all mounted elements
  }
}
```

### C. TypeScript Definitions
```typescript
// dom_mounter.d.ts (auto-generated from Idris2)
export type MountResult<T> =
  | { tag: 'Ok', value: T }
  | { tag: 'Error', error: string };

export interface DomMounterAPI {
  /**
   * Mount to a DOM element with formal verification
   * @param elementId - Non-empty element ID (validated by Idris2 proofs)
   * @returns Result indicating success or detailed error
   */
  mount(elementId: string): MountResult<void>;

  /**
   * Unmount from a DOM element
   * @param elementId - Element ID to unmount from
   * @returns Result indicating success or error
   */
  unmount(elementId: string): MountResult<void>;

  /**
   * Check if element exists and is mountable
   * @param elementId - Element ID to check
   * @returns true if element exists and is valid
   */
  canMount(elementId: string): boolean;
}
```

---

## 4. Interoperability Enhancements

### A. Framework Adapters

#### React Integration
```rescript
// React hook for DOM mounter
let useDomMounter = (elementId: string) => {
  let (mounted, setMounted) = React.useState(() => false)
  let (error, setError) = React.useState(() => None)

  React.useEffect(() => {
    switch mount(elementId) {
    | Ok() => setMounted(_ => true)
    | Error(msg) => setError(_ => Some(msg))
    }

    // Cleanup on unmount
    Some(() => {
      let _ = unmount(elementId)
    })
  }, [elementId])

  (mounted, error)
}

@react.component
let make = () => {
  let (mounted, error) = useDomMounter("app-root")

  switch (mounted, error) {
  | (true, None) => <div>{"Mounted!"->React.string}</div>
  | (false, Some(err)) => <div>{err->React.string}</div>
  | _ => <div>{"Mounting..."->React.string}</div>
  }
}
```

#### Solid.js Integration
```typescript
// Solid.js primitive
import { createEffect, createSignal } from 'solid-js';
import { mount, unmount } from './dom_mounter';

export function createDomMounter(elementId: string) {
  const [mounted, setMounted] = createSignal(false);
  const [error, setError] = createSignal<string | null>(null);

  createEffect(() => {
    const result = mount(elementId);
    if (result.tag === 'Ok') {
      setMounted(true);
    } else {
      setError(result.error);
    }

    return () => unmount(elementId);
  });

  return { mounted, error };
}
```

### B. Web Components Support
```rescript
// Custom element with DOM mounter
let registerMountedComponent = (tagName: string) => {
  let customElement = %raw(`
    class MountedComponent extends HTMLElement {
      connectedCallback() {
        const elementId = this.getAttribute('element-id');
        if (elementId) {
          mount(elementId);
        }
      }

      disconnectedCallback() {
        const elementId = this.getAttribute('element-id');
        if (elementId) {
          unmount(elementId);
        }
      }
    }
  `)

  WebAPI.defineCustomElement(tagName, customElement)
}

// Usage: <mounted-component element-id="app-root"></mounted-component>
```

### C. SSR (Server-Side Rendering) Support
```rescript
// Detect SSR environment
let isSSR = (): bool => {
  %raw(`typeof window === 'undefined'`)
}

// SSR-safe mounting
let mountSSR = (elementId: string): mountResult => {
  if isSSR() {
    // In SSR, just validate the ID
    if String.length(elementId) == 0 {
      Error("Element ID cannot be empty")
    } else {
      // Mark for hydration on client
      Ok()
    }
  } else {
    // Client-side: actually mount
    mount(elementId)
  }
}
```

### D. Module Format Variations
```javascript
// UMD wrapper (for legacy compatibility)
(function (root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD
    define(['exports'], factory);
  } else if (typeof exports === 'object' && typeof exports.nodeName !== 'string') {
    // CommonJS
    factory(exports);
  } else {
    // Browser globals
    factory((root.DomMounter = {}));
  }
}(typeof self !== 'undefined' ? self : this, function (exports) {
  // Load WASM FFI
  const { mount, unmount } = loadWasmFFI();

  exports.mount = mount;
  exports.unmount = unmount;
}));
```

---

## 5. Functionality Enhancements

### A. Shadow DOM Support
```idris
-- Prove shadow DOM encapsulation
data ShadowMode = Open | Closed

data ShadowDOMSafe : ShadowMode -> Type where
  OpenShadow : ShadowDOMSafe Open
  ClosedShadow : ShadowDOMSafe Closed

export
mountToShadowDOM : (elementId : String) ->
                   (mode : ShadowMode) ->
                   (ShadowDOMSafe mode) ->
                   IO MountResult
```

```rescript
type shadowMode = Open | Closed

let mountToShadowRoot = (
  elementId: string,
  mode: shadowMode,
): mountResult => {
  let element = WebAPI.getElementById(elementId)

  switch element {
  | None => Error("Element not found: " ++ elementId)
  | Some(el) => {
      let shadowRoot = switch mode {
      | Open => WebAPI.attachShadow(el, {"mode": "open"})
      | Closed => WebAPI.attachShadow(el, {"mode": "closed"})
      }

      // Mount into shadow root instead of element directly
      mountToElement(shadowRoot)
    }
  }
}
```

### B. Multiple Element Mounting
```rescript
type batchMountResult = {
  successful: array<string>,
  failed: array<(string, string)>, // (elementId, error)
}

let mountBatch = (elementIds: array<string>): batchMountResult => {
  let results = Array.map(elementIds, id => {
    switch mount(id) {
    | Ok() => `Ok(id)
    | Error(msg) => `Error(id, msg)
    }
  })

  {
    successful: Array.keepMap(results, r => switch r {
    | `Ok(id) => Some(id)
    | _ => None
    }),
    failed: Array.keepMap(results, r => switch r {
    | `Error(id, msg) => Some(id, msg)
    | _ => None
    }),
  }
}
```

### C. Unmount Operations
```zig
// Proper cleanup and unmounting
export fn unmount_from_element(element_id: [*:0]const u8) c_int {
    const id_len = std.mem.len(element_id);
    if (id_len == 0) {
        return @intFromEnum(MountResult.InvalidId);
    }

    // 1. Remove event listeners
    remove_all_listeners(element_id);

    // 2. Clear content
    clear_element_content(element_id);

    // 3. Remove references
    remove_mount_record(element_id);

    return @intFromEnum(MountResult.Success);
}
```

### D. Event Handling Integration
```rescript
type eventHandler<'a> = 'a => unit

type mountOptions = {
  onClick: option<eventHandler<WebAPI.mouseEvent>>,
  onMount: option<unit => unit>,
  onUnmount: option<unit => unit>,
}

let mountWithEvents = (
  elementId: string,
  options: mountOptions,
): mountResult => {
  switch mount(elementId) {
  | Error(e) => Error(e)
  | Ok() => {
      // Attach event listeners
      switch options.onClick {
      | Some(handler) => WebAPI.addEventListener(elementId, "click", handler)
      | None => ()
      }

      // Call mount callback
      switch options.onMount {
      | Some(cb) => cb()
      | None => ()
      }

      Ok()
    }
  }
}
```

### E. Animation & Transition Hooks
```rescript
type animationConfig = {
  duration: float,
  easing: string,
  delay: float,
}

let mountWithAnimation = (
  elementId: string,
  animation: animationConfig,
): mountResult => {
  switch mount(elementId) {
  | Error(e) => Error(e)
  | Ok() => {
      // Apply entry animation
      let element = WebAPI.getElementById(elementId)
      switch element {
      | Some(el) => {
          WebAPI.setStyle(el, "opacity", "0")
          WebAPI.setStyle(el, "transition",
            `opacity ${Float.toString(animation.duration)}ms ${animation.easing}`)

          // Trigger reflow
          let _ = WebAPI.getComputedStyle(el)

          // Fade in
          WebAPI.setStyle(el, "opacity", "1")
        }
      | None => ()
      }
      Ok()
    }
  }
}
```

### F. ResizeObserver Integration
```rescript
type resizeCallback = {
  width: float,
  height: float,
} => unit

let mountWithResizeObserver = (
  elementId: string,
  onResize: resizeCallback,
): mountResult => {
  switch mount(elementId) {
  | Error(e) => Error(e)
  | Ok() => {
      let observer = WebAPI.makeResizeObserver(entries => {
        Array.forEach(entries, entry => {
          let rect = WebAPI.getContentRect(entry)
          onResize({width: rect.width, height: rect.height})
        })
      })

      let element = WebAPI.getElementById(elementId)
      switch element {
      | Some(el) => WebAPI.observeResize(observer, el)
      | None => ()
      }

      Ok()
    }
  }
}
```

---

## 6. Testing & Verification Enhancements

### A. Property-Based Testing
```rescript
// QuickCheck-style property tests
let propValidIdNeverFails = (id: string): bool => {
  if String.length(id) > 0 && isAlphanumeric(id) {
    switch mount(id) {
    | Ok() | Error("Element not found: " ++ _) => true
    | Error("Element ID cannot be empty") => false // Should never happen
    | _ => true
    }
  } else {
    true
  }
}
```

### B. Formal Verification Tests
```idris
-- Prove mount is idempotent
mountIdempotent : (id : String) ->
                  (mount id = mount (mount id))
mountIdempotent id = Refl

-- Prove unmount after mount restores state
mountUnmountIdentity : (id : String) ->
                       (unmount (mount id) = id)
```

---

## 7. Performance Enhancements

### A. Caching & Memoization
```rescript
// Cache element lookups
module MountCache = {
  let cache: ref<dict<bool>> = ref(Dict.empty())

  let checkCached = (elementId: string): option<bool> => {
    Dict.get(cache.contents, elementId)
  }

  let setCached = (elementId: string, exists: bool): unit => {
    cache := Dict.set(cache.contents, elementId, exists)
  }

  let invalidate = (elementId: string): unit => {
    cache := Dict.remove(cache.contents, elementId)
  }
}
```

### B. Lazy Mounting
```rescript
type lazyMount = {
  elementId: string,
  threshold: float, // IntersectionObserver threshold
}

let mountWhenVisible = (config: lazyMount): unit => {
  let observer = WebAPI.makeIntersectionObserver((entries, _observer) => {
    Array.forEach(entries, entry => {
      if WebAPI.isIntersecting(entry) {
        let _ = mount(config.elementId)
        WebAPI.unobserve(_observer, entry.target)
      }
    })
  }, {"threshold": config.threshold})

  let element = WebAPI.getElementById(config.elementId)
  switch element {
  | Some(el) => WebAPI.observe(observer, el)
  | None => ()
  }
}
```

---

## 8. Documentation & Examples

### A. Interactive Documentation
- Create Docusaurus site with live examples
- Add TypeScript playground integration
- Include Idris2 proof explanations
- Architecture decision records (ADRs)

### B. Migration Guides
- From plain DOM manipulation
- From React 18 root API
- From other mounting libraries
- From unsafe mounting approaches

### C. Best Practices Guide
- When to use shadow DOM
- CSP configuration
- Performance optimization
- Error handling patterns

---

## Implementation Priority

### Phase 1: Core Reliability (Week 1-2)
- [ ] Health checks and monitoring
- [ ] Recovery mechanisms
- [ ] Lifecycle hooks
- [ ] Better error messages

### Phase 2: Security Hardening (Week 3)
- [ ] CSP validation
- [ ] Audit logging
- [ ] Sandboxing support

### Phase 3: Developer Experience (Week 4)
- [ ] TypeScript definitions
- [ ] DevTools integration
- [ ] Framework adapters (React, Solid)

### Phase 4: Advanced Features (Week 5-6)
- [ ] Shadow DOM support
- [ ] Batch mounting
- [ ] Animation hooks
- [ ] ResizeObserver integration

### Phase 5: Interoperability (Week 7)
- [ ] Web Components
- [ ] SSR support
- [ ] Module format variations

### Phase 6: Documentation & Polish (Week 8)
- [ ] Interactive documentation
- [ ] Migration guides
- [ ] Performance benchmarks
- [ ] Security audit

---

## Success Metrics

- **Dependability**: 99.99% uptime, automatic recovery from 95%+ of failures
- **Security**: Zero XSS vulnerabilities, full CSP compliance, audit trail for all operations
- **Usability**: <5 minute onboarding, clear error messages, TypeScript support
- **Interoperability**: Works with React, Solid, Vue, Web Components, SSR
- **Functionality**: Shadow DOM, batch operations, animations, observers
- **Performance**: <1ms mount time, <100KB bundle size, zero memory leaks

---

## References

- **Formal Verification**: Idris2 dependent types documentation
- **Web Standards**: WHATWG DOM specification
- **Security**: OWASP DOM-based XSS prevention cheat sheet
- **Performance**: Web Vitals, Chrome DevTools Performance API
- **Accessibility**: WCAG 2.1 guidelines for dynamic content

---

**Status**: Enhancement plan ready for implementation
**Next Step**: Begin Phase 1 (Core Reliability) implementation
