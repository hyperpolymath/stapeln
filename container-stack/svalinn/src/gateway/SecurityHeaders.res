// SPDX-License-Identifier: PMPL-1.0-or-later
// Security Headers Middleware for Svalinn Edge Gateway
//
// Implements OWASP security headers to defend against:
// - Clickjacking (X-Frame-Options)
// - MIME sniffing (X-Content-Type-Options)
// - XSS (Content-Security-Policy)
// - TLS downgrade (Strict-Transport-Security)

@@warning("-27") // Suppress unused variable warnings

// Context type from Hono.js
type context

@module("hono") @scope("Context")
external header: (context, string, string) => context = "header"

@module("hono") @scope("Context")
external next: context => Promise.t<context> = "next"

/**
 * Apply security headers to HTTP response
 *
 * Headers applied:
 * - Strict-Transport-Security: Enforce HTTPS for 1 year
 * - X-Frame-Options: Prevent clickjacking
 * - X-Content-Type-Options: Prevent MIME sniffing
 * - X-XSS-Protection: Enable XSS filter (legacy browsers)
 * - Content-Security-Policy: Strict CSP (self-only)
 * - Referrer-Policy: Privacy-preserving referrer
 * - Permissions-Policy: Disable unnecessary features
 */
let applySecurityHeaders = (c: context): context => {
  // HSTS: Enforce HTTPS for 1 year, include subdomains, enable preload
  let c = c->header(
    "Strict-Transport-Security",
    "max-age=31536000; includeSubDomains; preload",
  )

  // Clickjacking protection: Deny all framing
  let c = c->header("X-Frame-Options", "DENY")

  // MIME sniffing protection
  let c = c->header("X-Content-Type-Options", "nosniff")

  // XSS filter (legacy browsers - modern browsers use CSP)
  let c = c->header("X-XSS-Protection", "1; mode=block")

  // Content Security Policy: Strict self-only policy
  // - default-src 'self': Only load resources from same origin
  // - script-src 'self': Only execute scripts from same origin
  // - style-src 'self': Only load styles from same origin
  // - img-src 'self' data:: Allow images from same origin + data URIs
  // - font-src 'self': Only load fonts from same origin
  // - connect-src 'self': Only allow fetch/XHR to same origin
  // - frame-ancestors 'none': Prevent framing (redundant with X-Frame-Options)
  // - base-uri 'self': Prevent base tag injection
  // - form-action 'self': Prevent form submission to external sites
  let c = c->header(
    "Content-Security-Policy",
    "default-src 'self'; script-src 'self'; style-src 'self'; img-src 'self' data:; font-src 'self'; connect-src 'self'; frame-ancestors 'none'; base-uri 'self'; form-action 'self'",
  )

  // Referrer policy: Only send origin when navigating to less secure (HTTPS→HTTP)
  let c = c->header("Referrer-Policy", "strict-origin-when-cross-origin")

  // Permissions policy: Disable unnecessary features
  // Disables: geolocation, microphone, camera, payment, usb
  let c = c->header(
    "Permissions-Policy",
    "geolocation=(), microphone=(), camera=(), payment=(), usb=()",
  )

  c
}

/**
 * Middleware function for Hono.js
 *
 * Usage:
 * ```rescript
 * app->use(SecurityHeaders.middleware)
 * ```
 */
let middleware = async (c: context) => {
  // Apply security headers
  let c = applySecurityHeaders(c)

  // Continue to next middleware/handler
  await c->next
}

/**
 * CORS headers for API endpoints
 *
 * Configures:
 * - Access-Control-Allow-Origin: Specific origin only (not *)
 * - Access-Control-Allow-Methods: Limited to safe methods
 * - Access-Control-Allow-Headers: Limited to necessary headers
 * - Access-Control-Max-Age: Cache preflight for 1 hour
 */
let applyCorsHeaders = (c: context, ~allowedOrigin: string="https://svalinn.example.com"): context => {
  // Only allow specific origin (NOT wildcard *)
  let c = c->header("Access-Control-Allow-Origin", allowedOrigin)

  // Allow credentials (requires specific origin, not *)
  let c = c->header("Access-Control-Allow-Credentials", "true")

  // Limit HTTP methods
  let c = c->header("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")

  // Limit headers
  let c = c->header(
    "Access-Control-Allow-Headers",
    "Content-Type, Authorization, X-Request-ID",
  )

  // Cache preflight for 1 hour
  let c = c->header("Access-Control-Max-Age", "3600")

  c
}

/**
 * Rate limiting headers
 *
 * Implements standard rate limit headers:
 * - X-RateLimit-Limit: Maximum requests per window
 * - X-RateLimit-Remaining: Requests remaining in window
 * - X-RateLimit-Reset: Unix timestamp when window resets
 */
let applyRateLimitHeaders = (
  c: context,
  ~limit: int,
  ~remaining: int,
  ~resetAt: int,
): context => {
  let c = c->header("X-RateLimit-Limit", Int.toString(limit))
  let c = c->header("X-RateLimit-Remaining", Int.toString(remaining))
  let c = c->header("X-RateLimit-Reset", Int.toString(resetAt))
  c
}

/**
 * Security headers for error responses
 *
 * Ensures security headers are applied even on error pages
 */
let applyErrorHeaders = (c: context): context => {
  let c = applySecurityHeaders(c)

  // Add Cache-Control to prevent caching of error pages
  let c = c->header("Cache-Control", "no-store, no-cache, must-revalidate")
  let c = c->header("Pragma", "no-cache")
  let c = c->header("Expires", "0")

  c
}
