// SPDX-License-Identifier: PMPL-1.0-or-later
// JWT verification for Svalinn

open AuthTypes

// JWKS key structure
type jwk = {
  kty: string,
  @as("use") use_: option<string>,
  alg: option<string>,
  kid: string,
  n: option<string>, // RSA modulus
  e: option<string>, // RSA exponent
  x: option<string>, // EC x coordinate
  y: option<string>, // EC y coordinate
  crv: option<string>, // EC curve name
}

// JWKS response
type jwks = {keys: array<jwk>}

// Cached JWKS with expiry
type cachedJwks = {
  jwks: jwks,
  expiresAt: float,
}

// JWKS cache (mutable Map)
let jwksCache: Js.Dict.t<cachedJwks> = Js.Dict.empty()
let jwksCacheTtl = 3600000.0 // 1 hour in milliseconds

// JWT header
type jwtHeader = {
  alg: string,
  typ: option<string>,
  kid: option<string>,
}

// Algorithm types for Web Crypto API
type algorithm =
  | RSA_PKCS1(string) // hash algorithm (SHA-256, SHA-384, SHA-512)
  | ECDSA(string, string) // curve, hash

// Fetch JWKS from issuer with caching
let fetchJWKS = async (jwksUri: string): jwks => {
  // Check cache
  switch Js.Dict.get(jwksCache, jwksUri) {
  | Some(cached) if cached.expiresAt > Js.Date.now() => cached.jwks
  | _ => {
      // Fetch JWKS
      let response = await Fetch.fetch(jwksUri, {"method": "GET"})
      if !Fetch.Response.ok(response) {
        let status = Fetch.Response.status(response)->Belt.Int.toString
        raise(Js.Exn.raiseError(`Failed to fetch JWKS: ${status}`))
      }

      let json = await Fetch.Response.json(response)
      let jwks = json->Js.Json.decodeObject
        ->Belt.Option.flatMap(obj => obj->Js.Dict.get("keys"))
        ->Belt.Option.flatMap(Js.Json.decodeArray)
        ->Belt.Option.getExn

      let keys = jwks->Belt.Array.map(keyJson => {
        let obj = keyJson->Js.Json.decodeObject->Belt.Option.getExn
        {
          kty: obj->Js.Dict.get("kty")->Belt.Option.flatMap(Js.Json.decodeString)->Belt.Option.getExn,
          use_: obj->Js.Dict.get("use")->Belt.Option.flatMap(Js.Json.decodeString),
          alg: obj->Js.Dict.get("alg")->Belt.Option.flatMap(Js.Json.decodeString),
          kid: obj->Js.Dict.get("kid")->Belt.Option.flatMap(Js.Json.decodeString)->Belt.Option.getExn,
          n: obj->Js.Dict.get("n")->Belt.Option.flatMap(Js.Json.decodeString),
          e: obj->Js.Dict.get("e")->Belt.Option.flatMap(Js.Json.decodeString),
          x: obj->Js.Dict.get("x")->Belt.Option.flatMap(Js.Json.decodeString),
          y: obj->Js.Dict.get("y")->Belt.Option.flatMap(Js.Json.decodeString),
          crv: obj->Js.Dict.get("crv")->Belt.Option.flatMap(Js.Json.decodeString),
        }
      })

      let jwksResult = {keys: keys}

      // Cache
      Js.Dict.set(jwksCache, jwksUri, {
        jwks: jwksResult,
        expiresAt: Js.Date.now() +. jwksCacheTtl,
      })

      jwksResult
    }
  }
}

// Base64 URL decode
let base64UrlDecode = (str: string): Js.TypedArray2.Uint8Array.t => {
  let base64 = str
    ->Js.String2.replaceByRe(%re("/-/g"), "+")
    ->Js.String2.replaceByRe(%re("/_/g"), "/")

  let padding = Js.String2.repeat("=", (4 - mod(Js.String2.length(base64), 4)) |> mod(_, 4))
  let binary = %raw(`atob(base64 + padding)`)

  let bytes = Js.TypedArray2.Uint8Array.fromLength(Js.String2.length(binary))
  for i in 0 to Js.String2.length(binary) - 1 {
    Js.TypedArray2.Uint8Array.unsafe_set(bytes, i, Js.String2.charCodeAt(binary, i)->Belt.Float.toInt)
  }
  bytes
}

// Base64 URL encode
let base64UrlEncode = (bytes: Js.TypedArray2.Uint8Array.t): string => {
  let len = Js.TypedArray2.Uint8Array.length(bytes)
  let binary = ref("")
  for i in 0 to len - 1 {
    binary := binary.contents ++ Js.String2.fromCharCode(Js.TypedArray2.Uint8Array.unsafe_get(bytes, i))
  }
  %raw(`btoa(binary.contents)`)
    ->Js.String2.replaceByRe(%re("/\\+/g"), "-")
    ->Js.String2.split("/")
    ->Js.Array2.joinWith("_")
    ->Js.String2.replaceByRe(%re("/=/g"), "")
}

// Decode JWT without verification (for header inspection)
let decodeJWT = (token: string): (jwtHeader, tokenPayload) => {
  let parts = Js.String2.split(token, ".")
  if Js.Array2.length(parts) != 3 {
    raise(Js.Exn.raiseError("Invalid JWT format: expected 3 parts"))
  }

  let headerStr = switch Belt.Array.get(parts, 0) {
  | Some(h) => base64UrlDecode(h)
  | None => raise(Js.Exn.raiseError("Invalid JWT: missing header"))
  }

  let payloadStr = switch Belt.Array.get(parts, 1) {
  | Some(p) => base64UrlDecode(p)
  | None => raise(Js.Exn.raiseError("Invalid JWT: missing payload"))
  }

  // Convert Uint8Array to string
  let headerLen = Js.TypedArray2.Uint8Array.length(headerStr)
  let headerString = ref("")
  for i in 0 to headerLen - 1 {
    headerString := headerString.contents ++ Js.String2.fromCharCode(Js.TypedArray2.Uint8Array.unsafe_get(headerStr, i))
  }
  let headerJson = Js.Json.parseExn(headerString.contents)

  let payloadLen = Js.TypedArray2.Uint8Array.length(payloadStr)
  let payloadString = ref("")
  for i in 0 to payloadLen - 1 {
    payloadString := payloadString.contents ++ Js.String2.fromCharCode(Js.TypedArray2.Uint8Array.unsafe_get(payloadStr, i))
  }
  let payloadJson = Js.Json.parseExn(payloadString.contents)

  // Parse header
  let headerObj = switch headerJson->Js.Json.decodeObject {
  | Some(obj) => obj
  | None => raise(Js.Exn.raiseError("Invalid JWT: header is not an object"))
  }

  let header = {
    alg: switch headerObj->Js.Dict.get("alg")->Belt.Option.flatMap(Js.Json.decodeString) {
    | Some(a) => a
    | None => raise(Js.Exn.raiseError("Invalid JWT: missing required 'alg' field in header"))
    },
    typ: headerObj->Js.Dict.get("typ")->Belt.Option.flatMap(Js.Json.decodeString),
    kid: headerObj->Js.Dict.get("kid")->Belt.Option.flatMap(Js.Json.decodeString),
  }

  // Parse payload
  let payloadObj = switch payloadJson->Js.Json.decodeObject {
  | Some(obj) => obj
  | None => raise(Js.Exn.raiseError("Invalid JWT: payload is not an object"))
  }

  let payload = {
    sub: switch payloadObj->Js.Dict.get("sub")->Belt.Option.flatMap(Js.Json.decodeString) {
    | Some(s) => s
    | None => raise(Js.Exn.raiseError("Invalid JWT: missing required 'sub' field"))
    },
    iss: switch payloadObj->Js.Dict.get("iss")->Belt.Option.flatMap(Js.Json.decodeString) {
    | Some(i) => i
    | None => raise(Js.Exn.raiseError("Invalid JWT: missing required 'iss' field"))
    },
    aud: switch payloadObj->Js.Dict.get("aud") {
    | Some(a) => a
    | None => raise(Js.Exn.raiseError("Invalid JWT: missing required 'aud' field"))
    },
    exp: switch payloadObj->Js.Dict.get("exp")->Belt.Option.flatMap(Js.Json.decodeNumber)->Belt.Option.map(Belt.Float.toInt) {
    | Some(e) => e
    | None => raise(Js.Exn.raiseError("Invalid JWT: missing required 'exp' field"))
    },
    iat: switch payloadObj->Js.Dict.get("iat")->Belt.Option.flatMap(Js.Json.decodeNumber)->Belt.Option.map(Belt.Float.toInt) {
    | Some(i) => i
    | None => raise(Js.Exn.raiseError("Invalid JWT: missing required 'iat' field"))
    },
    scope: payloadObj->Js.Dict.get("scope")->Belt.Option.flatMap(Js.Json.decodeString),
    email: payloadObj->Js.Dict.get("email")->Belt.Option.flatMap(Js.Json.decodeString),
    name: payloadObj->Js.Dict.get("name")->Belt.Option.flatMap(Js.Json.decodeString),
    groups: payloadObj->Js.Dict.get("groups")->Belt.Option.flatMap(Js.Json.decodeArray)->Belt.Option.map(arr =>
      arr->Belt.Array.keepMap(Js.Json.decodeString)
    ),
    claims: payloadObj,
  }

  (header, payload)
}

// Get algorithm parameters from alg string
let getAlgorithm = (alg: string): algorithm => {
  switch alg {
  | "RS256" => RSA_PKCS1("SHA-256")
  | "RS384" => RSA_PKCS1("SHA-384")
  | "RS512" => RSA_PKCS1("SHA-512")
  | "ES256" => ECDSA("P-256", "SHA-256")
  | "ES384" => ECDSA("P-384", "SHA-384")
  | "ES512" => ECDSA("P-521", "SHA-512")
  | _ => raise(Js.Exn.raiseError(`Unsupported algorithm: ${alg}`))
  }
}

// Import JWK to CryptoKey using Web Crypto API
@val external importKey: (
  string,
  'a,
  'b,
  bool,
  array<string>
) => promise<'cryptoKey> = "crypto.subtle.importKey"

@val external verify: (
  'algorithm,
  'cryptoKey,
  Js.TypedArray2.ArrayBuffer.t,
  Js.TypedArray2.ArrayBuffer.t
) => promise<bool> = "crypto.subtle.verify"

let importJWK = async (jwk: jwk, alg: string): 'cryptoKey => {
  let algorithm = getAlgorithm(alg)

  let algorithmObj = switch algorithm {
  | RSA_PKCS1(hash) => {
      "name": "RSASSA-PKCS1-v1_5",
      "hash": hash,
    }->Obj.magic
  | ECDSA(curve, _) => {
      "name": "ECDSA",
      "namedCurve": curve,
    }->Obj.magic
  }

  // Convert jwk to JS object for importKey
  let jwkObj = {
    "kty": jwk.kty,
    "kid": jwk.kid,
  }->Obj.magic

  // Add optional fields
  switch jwk.alg {
  | Some(a) => %raw(`jwkObj.alg = a`)
  | None => ()
  }
  switch jwk.n {
  | Some(n) => %raw(`jwkObj.n = n`)
  | None => ()
  }
  switch jwk.e {
  | Some(e) => %raw(`jwkObj.e = e`)
  | None => ()
  }
  switch jwk.x {
  | Some(x) => %raw(`jwkObj.x = x`)
  | None => ()
  }
  switch jwk.y {
  | Some(y) => %raw(`jwkObj.y = y`)
  | None => ()
  }
  switch jwk.crv {
  | Some(crv) => %raw(`jwkObj.crv = crv`)
  | None => ()
  }

  await importKey("jwk", jwkObj, algorithmObj, true, ["verify"])
}

// Verify JWT signature
let verifySignature = async (token: string, key: 'cryptoKey, algorithm: algorithm): bool => {
  let parts = Js.String2.split(token, ".")

  let part0 = switch Belt.Array.get(parts, 0) {
  | Some(p) => p
  | None => raise(Js.Exn.raiseError("Invalid JWT: missing header for signature verification"))
  }

  let part1 = switch Belt.Array.get(parts, 1) {
  | Some(p) => p
  | None => raise(Js.Exn.raiseError("Invalid JWT: missing payload for signature verification"))
  }

  let dataStr = part0 ++ "." ++ part1
  let data = %raw(`new TextEncoder().encode(dataStr)`)

  let signature = switch Belt.Array.get(parts, 2) {
  | Some(sig) => base64UrlDecode(sig)
  | None => raise(Js.Exn.raiseError("Invalid JWT: missing signature"))
  }

  let algorithmObj = switch algorithm {
  | RSA_PKCS1(hash) => {
      "name": "RSASSA-PKCS1-v1_5",
      "hash": hash,
    }->Obj.magic
  | ECDSA(_, hash) => {
      "name": "ECDSA",
      "hash": hash,
    }->Obj.magic
  }

  await verify(
    algorithmObj,
    key,
    Js.TypedArray2.Uint8Array.buffer(signature),
    Js.TypedArray2.Uint8Array.buffer(data)
  )
}

// Verify JWT signature using Web Crypto API
let verifyJWT = async (token: string, config: oidcConfig): tokenPayload => {
  let (header, payload) = decodeJWT(token)

  // Validate basic claims
  let now = Js.Date.now() /. 1000.0 |> Belt.Float.toInt

  if payload.exp < now {
    raise(Js.Exn.raiseError("Token expired"))
  }

  if payload.iat > now + 60 {
    raise(Js.Exn.raiseError("Token issued in the future"))
  }

  if payload.iss != config.issuer {
    raise(Js.Exn.raiseError(`Invalid issuer: expected ${config.issuer}, got ${payload.iss}`))
  }

  // Validate audience
  let audiences = switch payload.aud->Js.Json.decodeArray {
  | Some(arr) => arr->Belt.Array.keepMap(Js.Json.decodeString)
  | None => switch payload.aud->Js.Json.decodeString {
    | Some(s) => [s]
    | None => []
    }
  }

  if !Belt.Array.some(audiences, aud => aud == config.clientId) {
    raise(Js.Exn.raiseError(`Invalid audience: ${Js.Json.stringify(payload.aud)}`))
  }

  // Fetch JWKS and verify signature
  let jwks = await fetchJWKS(config.jwksUri)
  let kid = header.kid->Belt.Option.getExn
  let key = jwks.keys->Belt.Array.getBy(k => k.kid == kid)

  switch key {
  | None => raise(Js.Exn.raiseError(`Key not found: ${kid}`))
  | Some(k) => {
      // Import key and verify
      let cryptoKey = await importJWK(k, header.alg)
      let algorithm = getAlgorithm(header.alg)
      let valid = await verifySignature(token, cryptoKey, algorithm)

      if !valid {
        raise(Js.Exn.raiseError("Invalid signature"))
      }

      payload
    }
  }
}

// Discover OIDC configuration from issuer
let discoverOIDC = async (issuer: string): oidcConfig => {
  let wellKnown = issuer
    ->Js.String2.replaceByRe(%re("/\/$/"), "")
    ++ "/.well-known/openid-configuration"

  let response = await Fetch.fetch(wellKnown, {"method": "GET"})
  if !Fetch.Response.ok(response) {
    let status = Fetch.Response.status(response)->Belt.Int.toString
    raise(Js.Exn.raiseError(`OIDC discovery failed: ${status}`))
  }

  let config = await Fetch.Response.json(response)
  let obj = config->Js.Json.decodeObject->Belt.Option.getExn

  {
    clientId: "", // Must be provided by caller
    clientSecret: "", // Must be provided by caller
    issuer: obj->Js.Dict.get("issuer")->Belt.Option.flatMap(Js.Json.decodeString)->Belt.Option.getExn,
    authorizationEndpoint: obj->Js.Dict.get("authorization_endpoint")->Belt.Option.flatMap(Js.Json.decodeString)->Belt.Option.getExn,
    tokenEndpoint: obj->Js.Dict.get("token_endpoint")->Belt.Option.flatMap(Js.Json.decodeString)->Belt.Option.getExn,
    userInfoEndpoint: obj->Js.Dict.get("userinfo_endpoint")->Belt.Option.flatMap(Js.Json.decodeString)->Belt.Option.getExn,
    jwksUri: obj->Js.Dict.get("jwks_uri")->Belt.Option.flatMap(Js.Json.decodeString)->Belt.Option.getExn,
    redirectUri: "", // Must be provided by caller
    scopes: ["openid", "profile", "email"], // Default scopes
    endSessionEndpoint: obj->Js.Dict.get("end_session_endpoint")->Belt.Option.flatMap(Js.Json.decodeString),
  }
}
