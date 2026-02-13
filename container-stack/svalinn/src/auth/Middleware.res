// SPDX-License-Identifier: PMPL-1.0-or-later
// Authentication middleware for Svalinn

open AuthTypes

// Deno environment variable access
@scope(("Deno", "env")) @val external getEnv: string => option<string> = "get"

// Try each authentication method until one succeeds
let rec tryAuthMethods = async (
  c: Hono.Context.t<'env, 'path>,
  config: authConfig,
  methods: array<authMethod>,
  index: int,
  result: ref<authResult>
) => {
  if index >= Belt.Array.length(methods) {
    ()
  } else {
    switch methods->Belt.Array.get(index) {
    | Some(method) => {
        let r: authResult = await tryAuthenticate(c, config, method)
        if r.authenticated {
          result := r
        } else {
          await tryAuthMethods(c, config, methods, index + 1, result)
        }
      }
    | None => () // Should never happen due to bounds check, but safe
    }
  }
}

// Create authentication middleware
and authMiddleware = (config: authConfig): Hono.middleware<'env, 'path> => {
  async (c, next) => {
    // Skip if auth disabled
    if !config.enabled {
      await next()
    } else {
      // Check excluded paths
      let req = Hono.Context.req(c)
      let url = Hono.Request.url(req)
      let urlObj = %raw(`new URL(url)`)
      let pathname = %raw(`urlObj.pathname`)

      let isExcluded = Belt.Array.some(config.excludePaths, p =>
        Js.String2.startsWith(pathname, p)
      )

      if isExcluded {
        await next()
      } else {
        // Try authentication methods in order
        let result = ref({
          authenticated: false,
          method: AuthTypes.None,
          subject: None,
          scopes: None,
          token: None,
          error: Some("No authentication provided"),
        })

        await tryAuthMethods(c, config, config.methods, 0, result)

        // Store result
        Hono.Context.set(c, "authResult", result.contents)

        if result.contents.authenticated {
          // Create user context
          let token = result.contents.token
          let user: userContext = {
            id: result.contents.subject->Belt.Option.getWithDefault("anonymous"),
            email: token->Belt.Option.flatMap(t => t.email),
            name: token->Belt.Option.flatMap(t => t.name),
            groups: token->Belt.Option.flatMap(t => t.groups)->Belt.Option.getWithDefault([]),
            scopes: result.contents.scopes->Belt.Option.getWithDefault([]),
            method: result.contents.method,
            issuedAt: token->Belt.Option.map(t => t.iat)->Belt.Option.getWithDefault(
              Js.Date.now() /. 1000.0 |> Belt.Float.toInt
            ),
            expiresAt: token->Belt.Option.flatMap(t => Some(t.exp)),
          }

          Hono.Context.set(c, "user", user)
          await next()
        } else {
          // Not authenticated - return 401
          let errorMsg = result.contents.error->Belt.Option.getWithDefault("Unauthorized")
          let _ = Hono.Context.json(
            c,
            Js.Json.object_(
              Js.Dict.fromArray([
                ("error", Js.Json.string("Unauthorized")),
                ("message", Js.Json.string(errorMsg)),
              ])
            ),
            ~status=401,
            ()
          )
        }
      }
    }
  }
}

// Try a specific authentication method
and tryAuthenticate = async (
  c: Hono.Context.t<'env, 'path>,
  config: authConfig,
  method: authMethod
): authResult => {
  switch method {
  | OAuth2 | OIDC => await authenticateBearerToken(c, config)
  | ApiKey => authenticateApiKey(c, config)
  | MTLS => authenticateMTLS(c)
  | AuthTypes.None => {
      authenticated: true,
      method: AuthTypes.None,
      subject: None,
      scopes: None,
      token: None,
      error: None,
    }
  }
}

// Authenticate via Bearer token (OAuth2/OIDC)
and authenticateBearerToken = async (
  c: Hono.Context.t<'env, 'path>,
  config: authConfig
): authResult => {
  let req = Hono.Context.req(c)
  let authHeader = Hono.Request.header(req, "Authorization")

  switch authHeader {
  | None => {
      authenticated: false,
      method: OIDC,
      subject: None,
      scopes: None,
      token: None,
      error: Some("No bearer token provided"),
    }
  | Some(auth) if !Js.String2.startsWith(auth, "Bearer ") => {
      authenticated: false,
      method: OIDC,
      subject: None,
      scopes: None,
      token: None,
      error: Some("No bearer token provided"),
    }
  | Some(auth) => {
      let token = Js.String2.sliceToEnd(auth, ~from=7)

      try {
        let payload = switch config.oidc {
        | Some(oidcConfig) => await JWT.verifyJWT(token, oidcConfig)
        | None => {
            // SECURITY: Never accept unverified tokens in production
            let env = getEnv("DENO_ENV")
            switch env {
            | Some("development") | Some("test") => {
                %raw(`console.warn("INSECURE: Using unverified JWT decode (dev/test only)")`)
                let (_, payload) = JWT.decodeJWT(token)
                payload
              }
            | _ => {
                // Production or unset - require OIDC config
                raise(Js.Exn.raiseError("OIDC configuration required in production - cannot verify JWT"))
              }
            }
          }
        }

        // Extract scopes
        let scopes = switch payload.scope {
        | Some(s) => Js.String2.split(s, " ")
        | None => []
        }

        {
          authenticated: true,
          method: OIDC,
          subject: Some(payload.sub),
          scopes: Some(scopes),
          token: Some(payload),
          error: None,
        }
      } catch {
      | Js.Exn.Error(e) => {
          let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Unknown error")
          {
            authenticated: false,
            method: OIDC,
            subject: None,
            scopes: None,
            token: None,
            error: Some(`Token verification failed: ${message}`),
          }
        }
      }
    }
  }
}

// Authenticate via API key
and authenticateApiKey = (c: Hono.Context.t<'env, 'path>, config: authConfig): authResult => {
  switch config.apiKey {
  | None => {
      authenticated: false,
      method: ApiKey,
      subject: None,
      scopes: None,
      token: None,
      error: Some("API key auth not configured"),
    }
  | Some(apiKeyConfig) => {
      let header = apiKeyConfig.header
      let req = Hono.Context.req(c)
      let apiKeyValue = Hono.Request.header(req, header)

      switch apiKeyValue {
      | None => {
          authenticated: false,
          method: ApiKey,
          subject: None,
          scopes: None,
          token: None,
          error: Some(`No API key in ${header} header`),
        }
      | Some(apiKey) => {
          // Remove prefix if configured
          let key = switch apiKeyConfig.prefix {
          | Some(prefix) if Js.String2.startsWith(apiKey, prefix) =>
            Js.String2.sliceToEnd(apiKey, ~from=Js.String2.length(prefix))
          | _ => apiKey
          }

          // Look up key
          switch Belt.Map.String.get(apiKeyConfig.keys, key) {
          | None => {
              authenticated: false,
              method: ApiKey,
              subject: None,
              scopes: None,
              token: None,
              error: Some("Invalid API key"),
            }
          | Some(keyInfo) => {
              // Check expiry
              let isExpired = switch keyInfo.expiresAt {
              | Some(expiresAt) => {
                  let expiryDate = %raw(`new Date(expiresAt)`)
                  let now = %raw(`new Date()`)
                  %raw(`expiryDate < now`)
                }
              | None => false
              }

              if isExpired {
                {
                  authenticated: false,
                  method: ApiKey,
                  subject: None,
                  scopes: None,
                  token: None,
                  error: Some("API key expired"),
                }
              } else {
                // Create token payload from key info
                let expiresAtTimestamp = switch keyInfo.expiresAt {
                | Some(exp) => {
                    let date = %raw(`new Date(exp)`)
                    %raw(`Math.floor(date.getTime() / 1000)`)
                  }
                | None => 0
                }

                let createdAtTimestamp = {
                  let date = %raw(`new Date(keyInfo.createdAt)`)
                  %raw(`Math.floor(date.getTime() / 1000)`)
                }

                let tokenPayload: tokenPayload = {
                  sub: keyInfo.id,
                  iss: "svalinn",
                  aud: Js.Json.string("svalinn"),
                  exp: expiresAtTimestamp,
                  iat: createdAtTimestamp,
                  scope: None,
                  email: None,
                  name: Some(keyInfo.name),
                  groups: None,
                  claims: Js.Dict.empty(),
                }

                {
                  authenticated: true,
                  method: ApiKey,
                  subject: Some(keyInfo.id),
                  scopes: Some(keyInfo.scopes),
                  token: Some(tokenPayload),
                  error: None,
                }
              }
            }
          }
        }
      }
    }
  }
}

// Authenticate via mTLS client certificate
and authenticateMTLS = (c: Hono.Context.t<'env, 'path>): authResult => {
  // Client certificate info would be set by reverse proxy
  let req = Hono.Context.req(c)
  let clientCert = Hono.Request.header(req, "X-Client-Cert-DN")
  let clientCertVerify = Hono.Request.header(req, "X-Client-Cert-Verify")

  switch (clientCert, clientCertVerify) {
  | (None, _) | (_, None) => {
      authenticated: false,
      method: MTLS,
      subject: None,
      scopes: None,
      token: None,
      error: Some("No valid client certificate"),
    }
  | (Some(_), Some(verify)) if verify != "SUCCESS" => {
      authenticated: false,
      method: MTLS,
      subject: None,
      scopes: None,
      token: None,
      error: Some("No valid client certificate"),
    }
  | (Some(certDN), Some(_)) => {
      // Parse CN from DN
      let cnRegex = %re("/CN=([^,]+)/")
      let matchResult: option<array<string>> = %raw(`certDN.match(cnRegex)`)
      let subject = switch matchResult {
      | Some(matches) => matches->Belt.Array.get(1)->Belt.Option.getWithDefault(certDN)
      | None => certDN
      }

      {
        authenticated: true,
        method: MTLS,
        subject: Some(subject),
        scopes: Some(["svalinn:read", "svalinn:write"]),
        token: None,
        error: None,
      }
    }
  }
}

// Require specific scopes middleware
let requireScopes = (requiredScopes: array<string>): Hono.middleware<'env, 'path> => {
  async (c, next) => {
    let user = Hono.Context.get(c, "user")

    switch user {
    | None => {
        let _ = Hono.Context.json(
          c,
          Js.Json.object_(Js.Dict.fromArray([("error", Js.Json.string("Not authenticated"))])),
          ~status=401,
          ()
        )
      }
    | Some(u) => {
        let missingScopes = Belt.Array.keep(requiredScopes, s =>
          !Belt.Array.some(u.scopes, us => us == s) && !Belt.Array.some(u.scopes, us => us == "svalinn:admin")
        )

        if Belt.Array.length(missingScopes) > 0 {
          let _ = Hono.Context.json(
            c,
            Js.Json.object_(
              Js.Dict.fromArray([
                ("error", Js.Json.string("Forbidden")),
                ("message", Js.Json.string("Insufficient scopes")),
                ("required", requiredScopes->Js.Json.stringArray),
                ("missing", missingScopes->Js.Json.stringArray),
              ])
            ),
            ~status=403,
            ()
          )
        } else {
          await next()
        }
      }
    }
  }
}

// Require specific groups middleware
let requireGroups = (requiredGroups: array<string>): Hono.middleware<'env, 'path> => {
  async (c, next) => {
    let user = Hono.Context.get(c, "user")

    switch user {
    | None => {
        let _ = Hono.Context.json(
          c,
          Js.Json.object_(Js.Dict.fromArray([("error", Js.Json.string("Not authenticated"))])),
          ~status=401,
          ()
        )
      }
    | Some(u) => {
        let hasGroup = Belt.Array.some(requiredGroups, g => Belt.Array.some(u.groups, ug => ug == g))

        if !hasGroup {
          let _ = Hono.Context.json(
            c,
            Js.Json.object_(
              Js.Dict.fromArray([
                ("error", Js.Json.string("Forbidden")),
                ("message", Js.Json.string("Not a member of required groups")),
                ("required", requiredGroups->Js.Json.stringArray),
              ])
            ),
            ~status=403,
            ()
          )
        } else {
          await next()
        }
      }
    }
  }
}

// Create default auth config
let createAuthConfig = (~options: option<authConfig>=?, ()): authConfig => {
  let defaults = {
    enabled: false,
    methods: [OIDC, ApiKey],
    oauth2: None,
    oidc: None,
    apiKey: Some({
      header: "X-API-Key",
      prefix: None,
      keys: Belt.Map.String.empty,
    }),
    mtls: None,
    excludePaths: ["/healthz", "/health", "/ready", "/metrics", "/.well-known/"],
  }

  switch options {
  | None => defaults
  | Some(opts) => opts
  }
}

// Load auth config from environment
let loadAuthConfigFromEnv = (): authConfig => {
  let enabled = getEnv("AUTH_ENABLED") == Some("true")

  let methods = switch getEnv("AUTH_METHODS") {
  | Some(methodsStr) =>
    Js.String2.split(methodsStr, ",")->Belt.Array.keepMap(authMethodFromString)
  | None => [OIDC, ApiKey]
  }

  let config = {
    enabled,
    methods,
    oauth2: None,
    oidc: None,
    apiKey: Some({
      header: "X-API-Key",
      prefix: None,
      keys: Belt.Map.String.empty,
    }),
    mtls: None,
    excludePaths: ["/healthz", "/health", "/ready", "/metrics", "/.well-known/"],
  }

  // Load OIDC config
  let oidcConfig = switch getEnv("OIDC_ISSUER") {
  | Some(issuer) => Some({
      issuer,
      clientId: getEnv("OIDC_CLIENT_ID")->Belt.Option.getWithDefault(""),
      clientSecret: getEnv("OIDC_CLIENT_SECRET")->Belt.Option.getWithDefault(""),
      authorizationEndpoint: getEnv("OIDC_AUTH_ENDPOINT")->Belt.Option.getWithDefault(""),
      tokenEndpoint: getEnv("OIDC_TOKEN_ENDPOINT")->Belt.Option.getWithDefault(""),
      userInfoEndpoint: getEnv("OIDC_USERINFO_ENDPOINT")->Belt.Option.getWithDefault(""),
      jwksUri: getEnv("OIDC_JWKS_URI")->Belt.Option.getWithDefault(""),
      redirectUri: getEnv("OIDC_REDIRECT_URI")->Belt.Option.getWithDefault(""),
      scopes: getEnv("OIDC_SCOPES")
        ->Belt.Option.getWithDefault("openid profile email")
        ->Js.String2.split(" "),
      endSessionEndpoint: getEnv("OIDC_END_SESSION_ENDPOINT"),
    })
  | None => None
  }

  // Load API keys from environment (comma-separated id:key:scopes format)
  let apiKeyConfig = switch getEnv("API_KEYS") {
  | Some(apiKeysStr) => {
      let keys = Js.String2.split(apiKeysStr, ",")->Belt.Array.reduce(Belt.Map.String.empty, (acc, entry) => {
        let parts = Js.String2.split(entry, ":")
        switch (parts->Belt.Array.get(0), parts->Belt.Array.get(1)) {
        | (Some(id), Some(key)) => {
            let scopes = switch parts->Belt.Array.get(2) {
            | Some(scopesStr) => Js.String2.split(scopesStr, "+")
            | None => ["svalinn:read"]
            }

            Belt.Map.String.set(acc, key, {
              id,
              name: id,
              scopes,
              createdAt: %raw(`new Date().toISOString()`),
              expiresAt: None,
              rateLimit: None,
            })
          }
        | _ => acc
        }
      })

      Some({
        header: "X-API-Key",
        prefix: None,
        keys,
      })
    }
  | None => config.apiKey
  }

  {...config, oidc: oidcConfig, apiKey: apiKeyConfig}
}
