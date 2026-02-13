// SPDX-License-Identifier: PMPL-1.0-or-later
// Authentication types for Svalinn

// Authentication method types
type authMethod =
  | OAuth2
  | OIDC
  | ApiKey
  | MTLS
  | None

// OAuth2 configuration
type oauth2Config = {
  clientId: string,
  clientSecret: string,
  authorizationEndpoint: string,
  tokenEndpoint: string,
  redirectUri: string,
  scopes: array<string>,
}

// OIDC configuration (extends OAuth2)
type oidcConfig = {
  clientId: string,
  clientSecret: string,
  authorizationEndpoint: string,
  tokenEndpoint: string,
  redirectUri: string,
  scopes: array<string>,
  issuer: string,
  userInfoEndpoint: string,
  jwksUri: string,
  endSessionEndpoint: option<string>,
}

// API key information
type apiKeyInfo = {
  id: string,
  name: string,
  scopes: array<string>,
  createdAt: string,
  expiresAt: option<string>,
  rateLimit: option<int>,
}

// API key configuration
type apiKeyConfig = {
  header: string,
  prefix: option<string>,
  keys: Belt.Map.String.t<apiKeyInfo>,
}

// mTLS configuration
type mtlsConfig = {
  caCert: string,
  requireClientCert: bool,
  verifyDepth: int,
}

// Authentication configuration
type authConfig = {
  enabled: bool,
  methods: array<authMethod>,
  oauth2: option<oauth2Config>,
  oidc: option<oidcConfig>,
  apiKey: option<apiKeyConfig>,
  mtls: option<mtlsConfig>,
  excludePaths: array<string>,
}

// Token payload (decoded JWT)
type tokenPayload = {
  sub: string,
  iss: string,
  aud: Js.Json.t, // string or array<string>
  exp: int,
  iat: int,
  scope: option<string>,
  email: option<string>,
  name: option<string>,
  groups: option<array<string>>,
  claims: Js.Dict.t<Js.Json.t>,
}

// Authentication result
type authResult = {
  authenticated: bool,
  method: authMethod,
  subject: option<string>,
  scopes: option<array<string>>,
  token: option<tokenPayload>,
  error: option<string>,
}

// User context attached to requests
type userContext = {
  id: string,
  email: option<string>,
  name: option<string>,
  groups: array<string>,
  scopes: array<string>,
  method: authMethod,
  issuedAt: int,
  expiresAt: option<int>,
}

// Authorization check result
type authzResult = {
  allowed: bool,
  reason: option<string>,
  requiredScopes: option<array<string>>,
  missingScopes: option<array<string>>,
}

// Permission action
type permissionAction =
  | Create
  | Read
  | Update
  | Delete
  | Execute

// Permission definition
type permission = {
  resource: string,
  actions: array<permissionAction>,
}

// RBAC role definition
type role = {
  name: string,
  permissions: array<permission>,
  description: option<string>,
}

// Convert permission action to string
let permissionActionToString = (action: permissionAction): string => {
  switch action {
  | Create => "create"
  | Read => "read"
  | Update => "update"
  | Delete => "delete"
  | Execute => "execute"
  }
}

// Convert string to permission action
let permissionActionFromString = (str: string): option<permissionAction> => {
  switch str {
  | "create" => Some(Create)
  | "read" => Some(Read)
  | "update" => Some(Update)
  | "delete" => Some(Delete)
  | "execute" => Some(Execute)
  | _ => None
  }
}

// Convert auth method to string
let authMethodToString = (method: authMethod): string => {
  switch method {
  | OAuth2 => "oauth2"
  | OIDC => "oidc"
  | ApiKey => "api-key"
  | MTLS => "mtls"
  | None => "none"
  }
}

// Convert string to auth method
let authMethodFromString = (str: string): option<authMethod> => {
  switch str {
  | "oauth2" => Some(OAuth2)
  | "oidc" => Some(OIDC)
  | "api-key" => Some(ApiKey)
  | "mtls" => Some(MTLS)
  | "none" => Some(None)
  | _ => None
  }
}

// Default roles
let defaultRoles: array<role> = [
  {
    name: "admin",
    description: Some("Full access to all resources"),
    permissions: [
      {
        resource: "*",
        actions: [Create, Read, Update, Delete, Execute],
      },
    ],
  },
  {
    name: "operator",
    description: Some("Can manage containers but not policies"),
    permissions: [
      {
        resource: "containers",
        actions: [Create, Read, Update, Delete, Execute],
      },
      {
        resource: "images",
        actions: [Read],
      },
      {
        resource: "policies",
        actions: [Read],
      },
    ],
  },
  {
    name: "viewer",
    description: Some("Read-only access"),
    permissions: [
      {
        resource: "containers",
        actions: [Read],
      },
      {
        resource: "images",
        actions: [Read],
      },
      {
        resource: "policies",
        actions: [Read],
      },
    ],
  },
  {
    name: "auditor",
    description: Some("Can view logs and audit trail"),
    permissions: [
      {
        resource: "containers",
        actions: [Read],
      },
      {
        resource: "logs",
        actions: [Read],
      },
      {
        resource: "audit",
        actions: [Read],
      },
    ],
  },
]

// Default scopes mapping
let defaultScopes: Belt.Map.String.t<string> = Belt.Map.String.fromArray([
  ("svalinn:read", "Read access to Svalinn resources"),
  ("svalinn:write", "Write access to Svalinn resources"),
  ("svalinn:admin", "Administrative access"),
  ("containers:create", "Create containers"),
  ("containers:read", "View containers"),
  ("containers:delete", "Delete containers"),
  ("images:verify", "Verify images"),
  ("policies:manage", "Manage policies"),
])
