// SPDX-License-Identifier: PMPL-1.0-or-later
// dom_mounter.d.ts - TypeScript definitions for DOM Mounter
// Auto-generated from Idris2 proofs and ReScript types

/**
 * Result type for operations that can fail
 */
export type Result<T, E> =
  | { tag: 'Ok'; value: T }
  | { tag: 'Error'; error: E };

// ============================================================================
// PHASE 1: CORE RELIABILITY
// ============================================================================

/**
 * Health status from formal Idris2 proofs
 */
export type HealthStatus = 'Healthy' | 'Degraded' | 'Failed';

/**
 * Lifecycle stages with proven transitions
 */
export type LifecycleStage = 'BeforeMount' | 'Mounted' | 'BeforeUnmount' | 'Unmounted';

/**
 * Recovery strategies with formal validation
 */
export type RecoveryStrategy =
  | { tag: 'Retry'; attempts: number }
  | { tag: 'Fallback'; elementId: string }
  | { tag: 'CreateIfMissing' };

/**
 * Lifecycle hooks for mount/unmount operations
 */
export interface LifecycleHooks {
  beforeMount?: (elementId: string) => Result<void, string>;
  afterMount?: (elementId: string) => void;
  beforeUnmount?: (elementId: string) => void;
  afterUnmount?: (elementId: string) => void;
  onError?: (error: string) => void;
}

/**
 * Mount configuration with all Phase 1 features
 */
export interface MountConfig {
  elementId: string;
  recovery?: RecoveryStrategy;
  lifecycle: LifecycleHooks;
  monitoring: boolean;
}

// ============================================================================
// PHASE 2: SECURITY HARDENING
// ============================================================================

/**
 * CSP validation results
 */
export type CSPResult = 'CSPValid' | 'InvalidChars' | 'ScriptDetected' | 'TooLong';

/**
 * Audit log severity levels
 */
export type AuditSeverity = 'Info' | 'Warning' | 'Error' | 'Critical';

/**
 * Sandbox modes for isolated mounting
 */
export type SandboxMode = 'NoSandbox' | 'IframeSandbox' | 'ShadowDOMSandbox';

/**
 * Security policy configuration
 */
export interface SecurityPolicy {
  requireCSP: boolean;
  enableAuditLog: boolean;
  sandboxMode: SandboxMode;
  maxElementIdLength: number;
}

/**
 * Audit log entry
 */
export interface AuditEntry {
  timestamp: number;
  operation: string;
  elementId: string;
  severity: AuditSeverity;
  message: string;
}

// ============================================================================
// CORE API
// ============================================================================

/**
 * Main DOM Mounter API with formal verification
 */
export interface DomMounterAPI {
  // Basic mounting
  /**
   * Mount to a DOM element with default recovery (retry 3 times)
   * @param elementId - Non-empty element ID (validated by Idris2 proofs)
   * @returns Result indicating success or detailed error
   * @example
   * const result = mount('app-root');
   * if (result.tag === 'Ok') {
   *   console.log('Mounted successfully');
   * }
   */
  mount(elementId: string): Result<void, string>;

  /**
   * Mount with custom lifecycle hooks
   * @param elementId - Element ID to mount to
   * @param hooks - Lifecycle hooks
   * @returns Result indicating success or error
   */
  mountWith(elementId: string, hooks: Partial<LifecycleHooks>): Result<void, string>;

  /**
   * Mount with full configuration (recovery, lifecycle, monitoring)
   * @param config - Complete mount configuration
   * @returns Result indicating success or error
   */
  mountEnhanced(config: MountConfig): Result<void, string>;

  // Health checks & monitoring
  /**
   * Perform health check on element ID
   * @param elementId - Element ID to check
   * @returns Tuple of health status and descriptive message
   */
  healthCheck(elementId: string): [HealthStatus, string];

  /**
   * Check if element is visible in viewport
   * @param elementId - Element ID to check
   * @returns true if visible, false otherwise
   */
  isElementVisible(elementId: string): boolean;

  /**
   * Get current lifecycle stage of element
   * @param elementId - Element ID to check
   * @returns Current lifecycle stage
   */
  getLifecycleStage(elementId: string): LifecycleStage;

  /**
   * Start continuous monitoring of element
   * @param elementId - Element ID to monitor
   * @returns Result indicating success or error
   */
  startMonitoring(elementId: string): Result<void, string>;

  /**
   * Stop monitoring element
   * @param elementId - Element ID to stop monitoring
   * @returns Result indicating success or error
   */
  stopMonitoring(elementId: string): Result<void, string>;

  // Recovery
  /**
   * Mount with automatic recovery on failure
   * @param elementId - Primary element ID
   * @param strategy - Recovery strategy to use
   * @returns Result with recovered element ID or error
   */
  mountWithRecovery(
    elementId: string,
    strategy: RecoveryStrategy
  ): Result<string, string>;

  // Lifecycle
  /**
   * Mount with lifecycle hooks
   * @param elementId - Element ID to mount to
   * @param hooks - Lifecycle hooks
   * @returns Result indicating success or error
   */
  mountWithLifecycle(elementId: string, hooks: LifecycleHooks): Result<void, string>;

  /**
   * Unmount with lifecycle hooks
   * @param elementId - Element ID to unmount from
   * @param hooks - Lifecycle hooks
   * @returns Result indicating success or error
   */
  unmountWithLifecycle(elementId: string, hooks: LifecycleHooks): Result<void, string>;

  // Security
  /**
   * Validate element ID against CSP rules
   * @param elementId - Element ID to validate
   * @returns CSP validation result
   */
  validateCSP(elementId: string): CSPResult;

  /**
   * Check if element ID is CSP compliant
   * @param elementId - Element ID to check
   * @returns true if compliant, false otherwise
   */
  isCSPValid(elementId: string): boolean;

  /**
   * Get human-readable CSP error message
   * @param result - CSP validation result
   * @returns Descriptive error message
   */
  cspErrorMessage(result: CSPResult): string;

  /**
   * Mount with security policy enforcement
   * @param elementId - Element ID to mount to
   * @param policy - Security policy to enforce
   * @returns Result indicating success or error
   */
  mountWithPolicy(elementId: string, policy: SecurityPolicy): Result<void, string>;

  /**
   * Mount in sandboxed environment
   * @param elementId - Element ID to mount to
   * @param mode - Sandbox mode
   * @returns Result indicating success or error
   */
  mountSandboxed(elementId: string, mode: SandboxMode): Result<void, string>;

  // Audit logging
  /**
   * Log audit entry
   * @param operation - Operation name
   * @param elementId - Element ID
   * @param severity - Log severity
   * @param message - Log message
   * @returns Result indicating success or error
   */
  logAudit(
    operation: string,
    elementId: string,
    severity: AuditSeverity,
    message: string
  ): Result<void, string>;

  /**
   * Get count of audit log entries
   * @returns Number of log entries
   */
  getAuditLogCount(): number;

  /**
   * Clear all audit log entries
   */
  clearAuditLog(): void;
}

// ============================================================================
// REACT INTEGRATION (Phase 3)
// ============================================================================

/**
 * React hook for DOM mounting with formal verification
 * @param elementId - Element ID to mount to
 * @returns [mounted, error] tuple
 * @example
 * function App() {
 *   const [mounted, error] = useDomMounter('app-root');
 *   if (error) return <div>Error: {error}</div>;
 *   if (!mounted) return <div>Mounting...</div>;
 *   return <div>Mounted!</div>;
 * }
 */
export function useDomMounter(elementId: string): [boolean, string | null];

/**
 * React hook with lifecycle callbacks
 * @param elementId - Element ID to mount to
 * @param hooks - Lifecycle hooks
 * @returns [mounted, error] tuple
 */
export function useDomMounterWithHooks(
  elementId: string,
  hooks: Partial<LifecycleHooks>
): [boolean, string | null];

/**
 * React hook with security policy
 * @param elementId - Element ID to mount to
 * @param policy - Security policy
 * @returns [mounted, error] tuple
 */
export function useDomMounterSecure(
  elementId: string,
  policy?: SecurityPolicy
): [boolean, string | null];

// ============================================================================
// SOLID.JS INTEGRATION (Phase 5)
// ============================================================================

/**
 * Solid.js primitive for DOM mounting
 * @param elementId - Element ID to mount to
 * @returns Object with mounted() and error() signals
 */
export function createDomMounter(elementId: string): {
  mounted: () => boolean;
  error: () => string | null;
};

// ============================================================================
// VUE INTEGRATION (Phase 5)
// ============================================================================

/**
 * Vue composable for DOM mounting
 * @param elementId - Element ID to mount to
 * @returns Reactive refs for mounted and error
 */
export function useDomMounterVue(elementId: string): {
  mounted: { value: boolean };
  error: { value: string | null };
};

// ============================================================================
// EXPORTS
// ============================================================================

declare const DomMounter: DomMounterAPI;
export default DomMounter;

// Named exports for tree-shaking
export {
  // Phase 1
  type HealthStatus,
  type LifecycleStage,
  type RecoveryStrategy,
  type LifecycleHooks,
  type MountConfig,
  // Phase 2
  type CSPResult,
  type AuditSeverity,
  type SandboxMode,
  type SecurityPolicy,
  type AuditEntry,
  // Core
  type Result,
};
