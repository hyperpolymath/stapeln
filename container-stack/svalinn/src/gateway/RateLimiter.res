// SPDX-License-Identifier: PMPL-1.0-or-later
// In-memory sliding window rate limiter for Svalinn Edge Gateway
//
// Tracks request counts per client IP within a configurable time window.
// No external dependencies — all state held in mutable Js.Dict.

type config = {
  windowMs: int,
  maxRequests: int,
}

type entry = {
  mutable count: int,
  mutable windowStart: float,
}

type t = {
  config: config,
  entries: Js.Dict.t<entry>,
}

let defaultConfig: config = {
  windowMs: 60000,
  maxRequests: 100,
}

let make = (~config: config=defaultConfig, ()): t => {
  config,
  entries: Js.Dict.empty(),
}

// Extract client IP from request headers (X-Forwarded-For or fallback)
let getClientIp = (req: Hono.Request.t): string => {
  switch Hono.Request.header(req, "X-Forwarded-For") {
  | Some(forwarded) =>
    switch Js.String2.split(forwarded, ",")->Belt.Array.get(0) {
    | Some(ip) => Js.String2.trim(ip)
    | None => "unknown"
    }
  | None =>
    switch Hono.Request.header(req, "X-Real-IP") {
    | Some(ip) => ip
    | None => "unknown"
    }
  }
}

// Check if a request should be allowed and update counters
let check = (limiter: t, clientIp: string): bool => {
  let now = Js.Date.now()
  let windowMs = Belt.Int.toFloat(limiter.config.windowMs)

  switch Js.Dict.get(limiter.entries, clientIp) {
  | Some(entry) =>
    if now -. entry.windowStart > windowMs {
      // Window expired — reset
      entry.count = 1
      entry.windowStart = now
      true
    } else if entry.count < limiter.config.maxRequests {
      entry.count = entry.count + 1
      true
    } else {
      false
    }
  | None =>
    Js.Dict.set(limiter.entries, clientIp, {count: 1, windowStart: now})
    true
  }
}

// Get remaining requests for a client
let remaining = (limiter: t, clientIp: string): int => {
  let now = Js.Date.now()
  let windowMs = Belt.Int.toFloat(limiter.config.windowMs)

  switch Js.Dict.get(limiter.entries, clientIp) {
  | Some(entry) =>
    if now -. entry.windowStart > windowMs {
      limiter.config.maxRequests
    } else {
      let rem = limiter.config.maxRequests - entry.count
      if rem < 0 { 0 } else { rem }
    }
  | None => limiter.config.maxRequests
  }
}

// Get window reset time in seconds
let retryAfter = (limiter: t, clientIp: string): int => {
  let now = Js.Date.now()
  let windowMs = Belt.Int.toFloat(limiter.config.windowMs)

  switch Js.Dict.get(limiter.entries, clientIp) {
  | Some(entry) =>
    let resetAt = entry.windowStart +. windowMs
    let secondsLeft = (resetAt -. now) /. 1000.0
    if secondsLeft > 0.0 {
      Belt.Float.toInt(Js.Math.ceil_float(secondsLeft))
    } else {
      0
    }
  | None => 0
  }
}

// Cleanup expired entries (call periodically)
let cleanup = (limiter: t): unit => {
  let now = Js.Date.now()
  let windowMs = Belt.Int.toFloat(limiter.config.windowMs)
  let keys = Js.Dict.keys(limiter.entries)

  Belt.Array.forEach(keys, key => {
    switch Js.Dict.get(limiter.entries, key) {
    | Some(entry) =>
      if now -. entry.windowStart > windowMs *. 2.0 {
        // Remove entries older than 2x the window
        %raw(`delete limiter.entries[key]`)
      }
    | None => ()
    }
  })
}

// Hono middleware factory
let middleware = (~config: config=defaultConfig, ()): Hono.middleware<'env, 'path> => {
  let limiter = make(~config, ())

  async (c, next) => {
    let req = Hono.Context.req(c)
    let clientIp = getClientIp(req)

    if check(limiter, clientIp) {
      // Set rate limit headers
      let rem = remaining(limiter, clientIp)
      Hono.Context.header(c, "X-RateLimit-Limit", Belt.Int.toString(config.maxRequests))
      Hono.Context.header(c, "X-RateLimit-Remaining", Belt.Int.toString(rem))

      await next()
    } else {
      // Rate limited — return 429
      Metrics.increment(Metrics.requestsErrorsTotal)
      let retry = retryAfter(limiter, clientIp)
      Hono.Context.header(c, "Retry-After", Belt.Int.toString(retry))
      Hono.Context.header(c, "X-RateLimit-Limit", Belt.Int.toString(config.maxRequests))
      Hono.Context.header(c, "X-RateLimit-Remaining", "0")

      let _ = Hono.Context.json(
        c,
        Js.Json.object_(Js.Dict.fromArray([
          ("error", Js.Json.string("Rate limit exceeded")),
          ("retryAfter", Js.Json.number(Belt.Int.toFloat(retry))),
        ])),
        ~status=429,
        ()
      )
    }
  }
}
