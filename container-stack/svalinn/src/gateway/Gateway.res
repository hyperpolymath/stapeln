// SPDX-License-Identifier: PMPL-1.0-or-later
// Svalinn Edge Gateway - Main HTTP server

// Configuration
module Config = {
  @scope(("Deno", "env")) @val external getEnv: string => option<string> = "get"

  let port = getEnv("SVALINN_PORT")
    ->Belt.Option.flatMap(Belt.Int.fromString)
    ->Belt.Option.getWithDefault(8000)

  let host = getEnv("SVALINN_HOST")->Belt.Option.getWithDefault("0.0.0.0")

  let vordrEndpoint = getEnv("VORDR_ENDPOINT")->Belt.Option.getWithDefault("http://localhost:8080")

  let specVersion = getEnv("SPEC_VERSION")->Belt.Option.getWithDefault("v0.1.0")

  let enableAuth = getEnv("AUTH_ENABLED") == Some("true")

  let logLevel = getEnv("LOG_LEVEL")->Belt.Option.getWithDefault("info")
}

// Logging
module Log = {
  type level = Debug | Info | Warn | Error

  let levelToString = (level: level): string => {
    switch level {
    | Debug => "DEBUG"
    | Info => "INFO"
    | Warn => "WARN"
    | Error => "ERROR"
    }
  }

  let shouldLog = (level: level): bool => {
    switch (Config.logLevel, level) {
    | ("debug", _) => true
    | ("info", Debug) => false
    | ("info", _) => true
    | ("warn", Debug | Info) => false
    | ("warn", _) => true
    | ("error", Error) => true
    | (_, _) => false
    }
  }

  let log = (level: level, message: string, ~metadata: option<Js.Json.t>=?, ()) => {
    if shouldLog(level) {
      let timestamp = %raw(`new Date().toISOString()`)
      let logObj = switch metadata {
      | Some(meta) =>
        Js.Json.object_(
          Js.Dict.fromArray([
            ("timestamp", Js.Json.string(timestamp)),
            ("level", Js.Json.string(levelToString(level))),
            ("message", Js.Json.string(message)),
            ("metadata", meta),
          ])
        )
      | None =>
        Js.Json.object_(
          Js.Dict.fromArray([
            ("timestamp", Js.Json.string(timestamp)),
            ("level", Js.Json.string(levelToString(level))),
            ("message", Js.Json.string(message)),
          ])
        )
      }
      Js.Console.log(Js.Json.stringify(logObj))
    }
  }

  let debug = (message: string, ~metadata: option<Js.Json.t>=?, ()) =>
    log(Debug, message, ~metadata?, ())

  let info = (message: string, ~metadata: option<Js.Json.t>=?, ()) =>
    log(Info, message, ~metadata?, ())

  let warn = (message: string, ~metadata: option<Js.Json.t>=?, ()) =>
    log(Warn, message, ~metadata?, ())

  let error = (message: string, ~metadata: option<Js.Json.t>=?, ()) =>
    log(Error, message, ~metadata?, ())
}

// Health check endpoint
module HealthCheck = {
  let handler = async (c: Hono.Context.t<'env, 'path>): Hono.Response.t => {
    // Check Vörðr connectivity
    let vordrConnected = try {
      let response = await Fetch.fetch(Config.vordrEndpoint ++ "/health", %raw(`{}`))
      Fetch.Response.ok(response)
    } catch {
    | _ => false
    }

    let status = if vordrConnected {"healthy"} else {"degraded"}

    Hono.Context.json(
      c,
      Js.Json.object_(
        Js.Dict.fromArray([
          ("status", Js.Json.string(status)),
          ("version", Js.Json.string("0.1.0")),
          ("vordrConnected", Js.Json.boolean(vordrConnected)),
          ("specVersion", Js.Json.string(Config.specVersion)),
          ("timestamp", Js.Json.string(%raw(`new Date().toISOString()`))),
        ])
      ),
      ~status=200,
      ()
    )
  }
}

// Readiness check endpoint
module ReadinessCheck = {
  let handler = async (c: Hono.Context.t<'env, 'path>): Hono.Response.t => {
    // Check if Vörðr is reachable
    let ready = try {
      let response = await Fetch.fetch(Config.vordrEndpoint ++ "/health", %raw(`{}`))
      Fetch.Response.ok(response)
    } catch {
    | _ => false
    }

    if ready {
      Hono.Context.json(
        c,
        Js.Json.object_(Js.Dict.fromArray([("ready", Js.Json.boolean(true))])),
        ~status=200,
        ()
      )
    } else {
      Hono.Context.json(
        c,
        Js.Json.object_(
          Js.Dict.fromArray([
            ("ready", Js.Json.boolean(false)),
            ("reason", Js.Json.string("Vörðr unavailable")),
          ])
        ),
        ~status=503,
        ()
      )
    }
  }
}

// Metrics endpoint
module Metrics = {
  let handler = async (c: Hono.Context.t<'env, 'path>): Hono.Response.t => {
    // Placeholder for Prometheus metrics
    // In production, collect actual metrics
    Hono.Context.text(
      c,
      "# HELP svalinn_requests_total Total HTTP requests
# TYPE svalinn_requests_total counter
svalinn_requests_total 0

# HELP svalinn_request_duration_seconds HTTP request duration
# TYPE svalinn_request_duration_seconds histogram
svalinn_request_duration_seconds_bucket{le=\"0.1\"} 0
svalinn_request_duration_seconds_bucket{le=\"0.5\"} 0
svalinn_request_duration_seconds_bucket{le=\"1\"} 0
svalinn_request_duration_seconds_bucket{le=\"+Inf\"} 0
svalinn_request_duration_seconds_sum 0
svalinn_request_duration_seconds_count 0
",
      ~status=200,
      ()
    )
  }
}

// Request logging middleware
let requestLogger = (): Hono.middleware<'env, 'path> => {
  async (c, next) => {
    let req = Hono.Context.req(c)
    let method = Hono.Request.method_(req)
    let url = Hono.Request.url(req)
    let start = Js.Date.now()

    Log.info(
      "Incoming request",
      ~metadata=Js.Json.object_(
        Js.Dict.fromArray([("method", Js.Json.string(method)), ("url", Js.Json.string(url))])
      ),
      ()
    )

    await next()

    let duration = Js.Date.now() -. start
    Log.info(
      "Request completed",
      ~metadata=Js.Json.object_(
        Js.Dict.fromArray([
          ("method", Js.Json.string(method)),
          ("url", Js.Json.string(url)),
          ("duration_ms", Js.Json.number(duration)),
        ])
      ),
      ()
    )
  }
}

// CORS middleware - SECURITY: Only allow whitelisted origins
let cors = (): Hono.middleware<'env, 'path> => {
  async (c, next) => {
    // Get allowed origins from environment (comma-separated list)
    let allowedOriginsStr = Deno.Env.get("ALLOWED_ORIGINS")
    let allowedOrigins = switch allowedOriginsStr {
    | Some(str) if str != "" => Js.String2.split(str, ",")
    | _ => [] // Default: no origins allowed (most secure)
    }

    let req = Hono.Context.req(c)
    let origin = Hono.Request.header(req, "Origin")

    // Only set CORS headers if origin is in whitelist
    switch origin {
    | Some(requestOrigin) =>
      if Belt.Array.some(allowedOrigins, allowed => allowed == requestOrigin) {
        Hono.Context.header(c, "Access-Control-Allow-Origin", requestOrigin)
        Hono.Context.header(c, "Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
        Hono.Context.header(c, "Access-Control-Allow-Headers", "Content-Type, Authorization, X-API-Key")
        Hono.Context.header(c, "Access-Control-Allow-Credentials", "true")
      } // else: Don't set CORS headers for untrusted origins
    | None => () // No origin header, likely same-origin request
    }

    switch Hono.Request.method_(req) {
    | "OPTIONS" => {
        let _ = Hono.Context.text(c, "", ~status=204, ())
        ()
      }
    | _ => await next()
    }
  }
}

// Error handler middleware
let errorHandler = (): Hono.middleware<'env, 'path> => {
  async (c, next) => {
    try {
      await next()
    } catch {
    | Js.Exn.Error(e) => {
        let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Internal server error")
        Log.error("Request error", ~metadata=Js.Json.object_(Js.Dict.fromArray([
          ("error", Js.Json.string(message))
        ])), ())

        let _ = Hono.Context.json(
          c,
          Js.Json.object_(
            Js.Dict.fromArray([
              ("error", Js.Json.string("Internal Server Error")),
              ("message", Js.Json.string(message)),
            ])
          ),
          ~status=500,
          ()
        )
      }
    }
  }
}

// Validation helper - validates request body and returns 400 on error
let validateRequest = (
  c: Hono.Context.t<'env, 'path>,
  validator: Validation.t,
  schemaId: string,
  body: Js.Json.t
): option<Hono.Response.t> => {
  let result = Validation.validate(validator, schemaId, body)

  if !result.valid {
    switch result.errors {
    | Some(errors) => {
        let formattedErrors = Validation.formatErrors(errors)
        Log.warn("Validation failed", ~metadata=Js.Json.object_(
          Js.Dict.fromArray([
            ("schema", Js.Json.string(schemaId)),
            ("errors", Js.Json.array(formattedErrors))
          ])
        ), ())

        Some(Hono.Context.json(
          c,
          Js.Json.object_(Js.Dict.fromArray([
            ("error", Js.Json.string("Validation failed")),
            ("details", Js.Json.array(formattedErrors))
          ])),
          ~status=400,
          ()
        ))
      }
    | None => {
        Some(Hono.Context.json(
          c,
          Js.Json.object_(Js.Dict.fromArray([
            ("error", Js.Json.string("Validation failed"))
          ])),
          ~status=400,
          ()
        ))
      }
    }
  } else {
    None
  }
}

// Create Hono app with validation
let createAppWithValidator = (validator: Validation.t): Hono.t<'env> => {
  let app = Hono.make()

  // Global middleware
  app->Hono.use(errorHandler())->ignore
  app->Hono.use(requestLogger())->ignore
  app->Hono.use(cors())->ignore

  // Health/readiness endpoints (no auth required)
  app->Hono.get("/health", HealthCheck.handler)->ignore
  app->Hono.get("/healthz", HealthCheck.handler)->ignore
  app->Hono.get("/ready", ReadinessCheck.handler)->ignore
  app->Hono.get("/readyz", ReadinessCheck.handler)->ignore
  app->Hono.get("/metrics", Metrics.handler)->ignore

  // Authentication middleware (applied to all routes below)
  if Config.enableAuth {
    let authConfig = Middleware.loadAuthConfigFromEnv()
    app->Hono.use(Middleware.authMiddleware(authConfig))->ignore
    Log.info("Authentication enabled", ())
  } else {
    Log.warn("Authentication DISABLED - not for production!", ())
  }

  // API routes - Connected to Vörðr via MCP
  let mcpConfig = McpClient.fromEnv()

  // Containers - List all containers
  app->Hono.get("/api/v1/containers", async c => {
    try {
      let result = await McpClient.Container.list(mcpConfig, ())
      Log.info("Listed containers", ())
      Hono.Context.json(c, result, ())
    } catch {
    | Js.Exn.Error(e) => {
        let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Failed to list containers")
        Log.error("Container list error", ~metadata=Js.Json.object_(
          Js.Dict.fromArray([("error", Js.Json.string(message))])
        ), ())
        Hono.Context.json(
          c,
          Js.Json.object_(Js.Dict.fromArray([("error", Js.Json.string(message))])),
          ~status=500,
          ()
        )
      }
    }
  })->ignore

  // Containers - Get specific container
  app->Hono.get("/api/v1/containers/:id", async c => {
    try {
      let req = Hono.Context.req(c)
      let id = Hono.Request.param(req, "id")->Belt.Option.getExn
      let result = await McpClient.Container.get(mcpConfig, id)
      Log.info("Got container", ~metadata=Js.Json.object_(
        Js.Dict.fromArray([("id", Js.Json.string(id))])
      ), ())
      Hono.Context.json(c, result, ())
    } catch {
    | Js.Exn.Error(e) => {
        let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Failed to get container")
        Log.error("Container get error", ~metadata=Js.Json.object_(
          Js.Dict.fromArray([("error", Js.Json.string(message))])
        ), ())
        Hono.Context.json(
          c,
          Js.Json.object_(Js.Dict.fromArray([("error", Js.Json.string(message))])),
          ~status=500,
          ()
        )
      }
    }
  })->ignore

  // Containers - Create container
  app->Hono.post("/api/v1/containers", async c => {
    try {
      let req = Hono.Context.req(c)
      let body = await Hono.Request.json(req)
      let image = Validation.getString(body, "image")->Belt.Option.getExn
      let name = Validation.getString(body, "name")
      let config = Validation.getObject(body, "config")->Belt.Option.map(Js.Json.object_)

      let result = switch (name, config) {
      | (Some(n), Some(c)) => await McpClient.Container.create(mcpConfig, ~image, ~name=n, ~containerConfig=c, ())
      | (Some(n), None) => await McpClient.Container.create(mcpConfig, ~image, ~name=n, ())
      | (None, Some(c)) => await McpClient.Container.create(mcpConfig, ~image, ~containerConfig=c, ())
      | (None, None) => await McpClient.Container.create(mcpConfig, ~image, ())
      }
      Log.info("Created container", ~metadata=Js.Json.object_(
        Js.Dict.fromArray([("image", Js.Json.string(image))])
      ), ())
      Hono.Context.json(c, result, ~status=201, ())
    } catch {
    | Js.Exn.Error(e) => {
        let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Failed to create container")
        Log.error("Container create error", ~metadata=Js.Json.object_(
          Js.Dict.fromArray([("error", Js.Json.string(message))])
        ), ())
        Hono.Context.json(
          c,
          Js.Json.object_(Js.Dict.fromArray([("error", Js.Json.string(message))])),
          ~status=500,
          ()
        )
      }
    }
  })->ignore

  // Containers - Start container
  app->Hono.post("/api/v1/containers/:id/start", async c => {
    try {
      let req = Hono.Context.req(c)
      let id = Hono.Request.param(req, "id")->Belt.Option.getExn
      let result = await McpClient.Container.start(mcpConfig, id)
      Log.info("Started container", ~metadata=Js.Json.object_(
        Js.Dict.fromArray([("id", Js.Json.string(id))])
      ), ())
      Hono.Context.json(c, result, ())
    } catch {
    | Js.Exn.Error(e) => {
        let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Failed to start container")
        Log.error("Container start error", ~metadata=Js.Json.object_(
          Js.Dict.fromArray([("error", Js.Json.string(message))])
        ), ())
        Hono.Context.json(
          c,
          Js.Json.object_(Js.Dict.fromArray([("error", Js.Json.string(message))])),
          ~status=500,
          ()
        )
      }
    }
  })->ignore

  // Containers - Stop container
  app->Hono.post("/api/v1/containers/:id/stop", async c => {
    try {
      let req = Hono.Context.req(c)
      let id = Hono.Request.param(req, "id")->Belt.Option.getExn
      let result = await McpClient.Container.stop(mcpConfig, id, ())
      Log.info("Stopped container", ~metadata=Js.Json.object_(
        Js.Dict.fromArray([("id", Js.Json.string(id))])
      ), ())
      Hono.Context.json(c, result, ())
    } catch {
    | Js.Exn.Error(e) => {
        let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Failed to stop container")
        Log.error("Container stop error", ~metadata=Js.Json.object_(
          Js.Dict.fromArray([("error", Js.Json.string(message))])
        ), ())
        Hono.Context.json(
          c,
          Js.Json.object_(Js.Dict.fromArray([("error", Js.Json.string(message))])),
          ~status=500,
          ()
        )
      }
    }
  })->ignore

  // Containers - Remove container
  app->Hono.delete("/api/v1/containers/:id", async c => {
    try {
      let req = Hono.Context.req(c)
      let id = Hono.Request.param(req, "id")->Belt.Option.getExn
      let result = await McpClient.Container.remove(mcpConfig, id, ())
      Log.info("Removed container", ~metadata=Js.Json.object_(
        Js.Dict.fromArray([("id", Js.Json.string(id))])
      ), ())
      Hono.Context.json(c, result, ())
    } catch {
    | Js.Exn.Error(e) => {
        let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Failed to remove container")
        Log.error("Container remove error", ~metadata=Js.Json.object_(
          Js.Dict.fromArray([("error", Js.Json.string(message))])
        ), ())
        Hono.Context.json(
          c,
          Js.Json.object_(Js.Dict.fromArray([("error", Js.Json.string(message))])),
          ~status=500,
          ()
        )
      }
    }
  })->ignore

  // Images - List images
  app->Hono.get("/api/v1/images", async c => {
    try {
      let result = await McpClient.Image.list(mcpConfig)
      Log.info("Listed images", ())
      Hono.Context.json(c, result, ())
    } catch {
    | Js.Exn.Error(e) => {
        let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Failed to list images")
        Log.error("Image list error", ~metadata=Js.Json.object_(
          Js.Dict.fromArray([("error", Js.Json.string(message))])
        ), ())
        Hono.Context.json(
          c,
          Js.Json.object_(Js.Dict.fromArray([("error", Js.Json.string(message))])),
          ~status=500,
          ()
        )
      }
    }
  })->ignore

  // Images - Pull image
  app->Hono.post("/api/v1/images/pull", async c => {
    try {
      let req = Hono.Context.req(c)
      let body = await Hono.Request.json(req)
      let image = Validation.getString(body, "image")->Belt.Option.getExn
      let result = await McpClient.Image.pull(mcpConfig, image)
      Log.info("Pulled image", ~metadata=Js.Json.object_(
        Js.Dict.fromArray([("image", Js.Json.string(image))])
      ), ())
      Hono.Context.json(c, result, ())
    } catch {
    | Js.Exn.Error(e) => {
        let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Failed to pull image")
        Log.error("Image pull error", ~metadata=Js.Json.object_(
          Js.Dict.fromArray([("error", Js.Json.string(message))])
        ), ())
        Hono.Context.json(
          c,
          Js.Json.object_(Js.Dict.fromArray([("error", Js.Json.string(message))])),
          ~status=500,
          ()
        )
      }
    }
  })->ignore

  // Images - Verify image (with policy enforcement)
  app->Hono.post("/api/v1/images/verify", async c => {
    try {
      let req = Hono.Context.req(c)
      let body = await Hono.Request.json(req)
      let digest = Validation.getString(body, "digest")->Belt.Option.getExn
      let policyJson = Validation.getObject(body, "policy")->Belt.Option.map(Js.Json.object_)

      // If policy provided, validate it first
      switch policyJson {
      | Some(pol) => {
          // Validate policy format
          let policyValidation = PolicyEngine.validatePolicy(validator, pol)
          if !policyValidation.valid {
            // Policy is malformed
            switch policyValidation.errors {
            | Some(errors) => {
                let formattedErrors = Validation.formatErrors(errors)
                Log.warn("Invalid policy format", ~metadata=Js.Json.object_(
                  Js.Dict.fromArray([
                    ("errors", Js.Json.array(formattedErrors))
                  ])
                ), ())

                Hono.Context.json(
                  c,
                  Js.Json.object_(Js.Dict.fromArray([
                    ("error", Js.Json.string("Invalid policy format")),
                    ("details", Js.Json.array(formattedErrors))
                  ])),
                  ~status=400,
                  ()
                )
              }
            | None => {
                Hono.Context.json(
                  c,
                  Js.Json.object_(Js.Dict.fromArray([
                    ("error", Js.Json.string("Invalid policy format"))
                  ])),
                  ~status=400,
                  ()
                )
              }
            }
          } else {
            // Policy is valid, send to Vörðr for enforcement
            let result = await McpClient.Image.verify(mcpConfig, digest, ~policy=pol, ())

            Log.info("Verified image with policy", ~metadata=Js.Json.object_(
              Js.Dict.fromArray([("digest", Js.Json.string(digest))])
            ), ())
            Hono.Context.json(c, result, ())
          }
        }
      | None => {
          // Verify without policy (use Vörðr's default policy)
          let result = await McpClient.Image.verify(mcpConfig, digest, ())
          Log.info("Verified image without policy", ~metadata=Js.Json.object_(
            Js.Dict.fromArray([("digest", Js.Json.string(digest))])
          ), ())
          Hono.Context.json(c, result, ())
        }
      }
    } catch {
    | Js.Exn.Error(e) => {
        let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Failed to verify image")
        Log.error("Image verify error", ~metadata=Js.Json.object_(
          Js.Dict.fromArray([("error", Js.Json.string(message))])
        ), ())
        Hono.Context.json(
          c,
          Js.Json.object_(Js.Dict.fromArray([("error", Js.Json.string(message))])),
          ~status=500,
          ()
        )
      }
    }
  })->ignore

  // Run container (with validation + policy)
  app->Hono.post("/api/v1/run", async c => {
    try {
      let req = Hono.Context.req(c)
      let body = await Hono.Request.json(req)

      // Validate request against schema
      switch validateRequest(c, validator, "gateway-run-request", body) {
      | Some(errorResponse) => errorResponse
      | None => {
          let image = Validation.getString(body, "image")->Belt.Option.getExn
          let name = Validation.getString(body, "name")
          let config = Validation.getObject(body, "config")->Belt.Option.map(Js.Json.object_)

          // Create container
          let createResult = switch (name, config) {
          | (Some(n), Some(c)) => await McpClient.Container.create(mcpConfig, ~image, ~name=n, ~containerConfig=c, ())
          | (Some(n), None) => await McpClient.Container.create(mcpConfig, ~image, ~name=n, ())
          | (None, Some(c)) => await McpClient.Container.create(mcpConfig, ~image, ~containerConfig=c, ())
          | (None, None) => await McpClient.Container.create(mcpConfig, ~image, ())
          }

          // Extract container ID from result
          let containerId = Validation.getString(createResult, "id")->Belt.Option.getExn

          // Start container
          let startResult = await McpClient.Container.start(mcpConfig, containerId)

          Log.info("Ran container", ~metadata=Js.Json.object_(
            Js.Dict.fromArray([
              ("image", Js.Json.string(image)),
              ("containerId", Js.Json.string(containerId))
            ])
          ), ())

          Hono.Context.json(c, startResult, ~status=201, ())
        }
      }
    } catch {
    | Js.Exn.Error(e) => {
        let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Failed to run container")
        Log.error("Container run error", ~metadata=Js.Json.object_(
          Js.Dict.fromArray([("error", Js.Json.string(message))])
        ), ())
        Hono.Context.json(
          c,
          Js.Json.object_(Js.Dict.fromArray([("error", Js.Json.string(message))])),
          ~status=500,
          ()
        )
      }
    }
  })->ignore

  // Verify bundle (Cerro Torre .ctp bundle verification)
  app->Hono.post("/api/v1/verify", async c => {
    try {
      let req = Hono.Context.req(c)
      let body = await Hono.Request.json(req)

      // Validate request against schema
      switch validateRequest(c, validator, "gateway-verify-request", body) {
      | Some(errorResponse) => errorResponse
      | None => {
          let digest = Validation.getString(body, "digest")->Belt.Option.getExn
          let policyJson = Validation.getObject(body, "policy")->Belt.Option.map(Js.Json.object_)

          // If policy provided, validate it first
          let policyError = switch policyJson {
          | Some(pol) => {
              let policyValidation = PolicyEngine.validatePolicy(validator, pol)
              if !policyValidation.valid {
                switch policyValidation.errors {
                | Some(errors) => {
                    let formattedErrors = Validation.formatErrors(errors)
                    Some(Hono.Context.json(
                      c,
                      Js.Json.object_(Js.Dict.fromArray([
                        ("error", Js.Json.string("Invalid policy format")),
                        ("details", Js.Json.array(formattedErrors))
                      ])),
                      ~status=400,
                      ()
                    ))
                  }
                | None => {
                    Some(Hono.Context.json(
                      c,
                      Js.Json.object_(Js.Dict.fromArray([
                        ("error", Js.Json.string("Invalid policy format"))
                      ])),
                      ~status=400,
                      ()
                    ))
                  }
                }
              } else {
                None
              }
            }
          | None => None
          }

          switch policyError {
          | Some(errorResponse) => errorResponse
          | None => {
              // Verify image (which includes .ctp bundle verification)
              let result = switch policyJson {
              | Some(pol) => await McpClient.Image.verify(mcpConfig, digest, ~policy=pol, ())
              | None => await McpClient.Image.verify(mcpConfig, digest, ())
              }

              Log.info("Verified bundle", ~metadata=Js.Json.object_(
                Js.Dict.fromArray([("digest", Js.Json.string(digest))])
              ), ())

              Hono.Context.json(c, result, ())
            }
          }
        }
      }
    } catch {
    | Js.Exn.Error(e) => {
        let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Failed to verify bundle")
        Log.error("Bundle verify error", ~metadata=Js.Json.object_(
          Js.Dict.fromArray([("error", Js.Json.string(message))])
        ), ())
        Hono.Context.json(
          c,
          Js.Json.object_(Js.Dict.fromArray([("error", Js.Json.string(message))])),
          ~status=500,
          ()
        )
      }
    }
  })->ignore

  // Policies - List default policies
  app->Hono.get("/api/v1/policies", async c => {
    try {
      let policies = Js.Json.object_(
        Js.Dict.fromArray([
          ("default", PolicyEngine.formatResult({
            allowed: true,
            mode: PolicyEngine.Strict,
            predicatesFound: PolicyEngine.defaultPolicy.requiredPredicates,
            missingPredicates: [],
            signersVerified: [],
            invalidSigners: [],
            logCount: 1,
            logQuorumMet: true,
            violations: [],
            warnings: [],
          })),
          ("permissive", PolicyEngine.formatResult({
            allowed: true,
            mode: PolicyEngine.Permissive,
            predicatesFound: [],
            missingPredicates: [],
            signersVerified: [],
            invalidSigners: [],
            logCount: 0,
            logQuorumMet: true,
            violations: [],
            warnings: [],
          })),
        ])
      )

      Log.info("Listed policies", ())
      Hono.Context.json(c, policies, ())
    } catch {
    | Js.Exn.Error(e) => {
        let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Failed to list policies")
        Log.error("Policy list error", ~metadata=Js.Json.object_(
          Js.Dict.fromArray([("error", Js.Json.string(message))])
        ), ())
        Hono.Context.json(
          c,
          Js.Json.object_(Js.Dict.fromArray([("error", Js.Json.string(message))])),
          ~status=500,
          ()
        )
      }
    }
  })->ignore

  // 404 handler
  app->Hono.all("*", async c => {
    let req = Hono.Context.req(c)
    let url = Hono.Request.url(req)

    Hono.Context.json(
      c,
      Js.Json.object_(
        Js.Dict.fromArray([
          ("error", Js.Json.string("Not Found")),
          ("path", Js.Json.string(url)),
        ])
      ),
      ~status=404,
      ()
    )
  })->ignore

  app
}

// Start server
let serve = async () => {
  // Load JSON schemas
  Log.info("Loading JSON schemas...", ())
  let validator = Validation.make()
  let validatorWithSchemas = await Validation.loadStandardSchemas(validator)
  Log.info("JSON schemas loaded", ())

  // Create app with validator
  let app = createAppWithValidator(validatorWithSchemas)

  Log.info(
    "Starting Svalinn Gateway",
    ~metadata=Js.Json.object_(
      Js.Dict.fromArray([
        ("port", Js.Json.number(Belt.Int.toFloat(Config.port))),
        ("host", Js.Json.string(Config.host)),
        ("vordrEndpoint", Js.Json.string(Config.vordrEndpoint)),
        ("authEnabled", Js.Json.boolean(Config.enableAuth)),
      ])
    ),
    ()
  )

  // Start the server with Deno.serve
  let handler = (req: Fetch.Request.t): promise<Fetch.Response.t> => {
    app->Hono.fetch(req, %raw(`{}`))
  }

  Deno.Http.serve(
    handler,
    {
      port: Config.port,
      hostname: Some(Config.host),
      signal: None,
    }
  )->ignore
}
