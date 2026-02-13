// SPDX-License-Identifier: PMPL-1.0-or-later
// JSON Schema validation for Svalinn requests

// Ajv bindings (JSON Schema validator)
module Ajv = {
  type t
  type validator

  type errorObject = {
    keyword: string,
    dataPath: string,
    schemaPath: string,
    params: Js.Json.t,
    message: option<string>,
  }

  @module("ajv") @new
  external make: {..} => t = "default"

  @send
  external addSchema: (t, Js.Json.t, string) => t = "addSchema"

  @send
  external getSchema: (t, string) => option<validator> = "getSchema"

  @send
  external compile: (t, Js.Json.t) => validator = "compile"

  @send
  external validate: (validator, Js.Json.t) => bool = ""

  @get
  external errors: validator => option<array<errorObject>> = "errors"
}

// Validation result
type validationResult = {
  valid: bool,
  errors: option<array<Ajv.errorObject>>,
}

// Schema registry
type t = {
  ajv: Ajv.t,
  schemas: Belt.Map.String.t<Js.Json.t>,
}

// Create validator instance
let make = (): t => {
  let ajv = Ajv.make({
    "allErrors": true,
    "strict": false,
    "validateFormats": true,
  })

  {
    ajv,
    schemas: Belt.Map.String.empty,
  }
}

// Load schema from file
@scope("Deno") @val external readTextFile: string => promise<string> = "readTextFile"

let loadSchema = async (validator: t, schemaPath: string, schemaId: string): t => {
  try {
    let content = await readTextFile(schemaPath)
    let schema = Js.Json.parseExn(content)

    // Add to Ajv
    let _ = validator.ajv->Ajv.addSchema(schema, schemaId)

    // Store in map
    let schemas = Belt.Map.String.set(validator.schemas, schemaId, schema)

    {...validator, schemas}
  } catch {
  | Js.Exn.Error(e) => {
      let message = Js.Exn.message(e)->Belt.Option.getWithDefault("Unknown error")
      Js.Console.error("Failed to load schema " ++ schemaId ++ " from " ++ schemaPath ++ ": " ++ message)
      validator
    }
  }
}

// Load all standard schemas
let loadStandardSchemas = async (validator: t): t => {
  let schemaDir = "../spec/schemas"

  let schemas = [
    ("gateway-run-request.v1.json", "gateway-run-request"),
    ("gateway-verify-request.v1.json", "gateway-verify-request"),
    ("container-info.v1.json", "container-info"),
    ("error-response.v1.json", "error-response"),
    ("containers.v1.json", "containers"),
    ("images.v1.json", "images"),
    ("gatekeeper-policy.v1.json", "gatekeeper-policy"),
    ("compose.v1.json", "compose"),
    ("doctor-report.v1.json", "doctor-report"),
  ]

  let rec loadSchemas = async (v: t, remaining: array<(string, string)>, index: int): t => {
    if index >= Belt.Array.length(remaining) {
      v
    } else {
      let (filename, schemaId) = remaining->Belt.Array.get(index)->Belt.Option.getExn
      let path = schemaDir ++ "/" ++ filename
      let v2 = await loadSchema(v, path, schemaId)
      await loadSchemas(v2, remaining, index + 1)
    }
  }

  await loadSchemas(validator, schemas, 0)
}

// Validate data against schema
let validate = (validator: t, schemaId: string, data: Js.Json.t): validationResult => {
  switch validator.ajv->Ajv.getSchema(schemaId) {
  | None => {
      valid: false,
      errors: Some([
        {
          keyword: "schema",
          dataPath: "",
          schemaPath: "",
          params: Js.Json.null,
          message: Some("Schema '" ++ schemaId ++ "' not found"),
        },
      ]),
    }
  | Some(validateFn) => {
      let valid = validateFn->Ajv.validate(data)
      let errors = if !valid {validateFn->Ajv.errors} else {None}

      {valid, errors}
    }
  }
}

// Format validation errors for response
let formatErrors = (errors: array<Ajv.errorObject>): array<Js.Json.t> => {
  Belt.Array.map(errors, error => {
    Js.Json.object_(
      Js.Dict.fromArray([
        ("keyword", Js.Json.string(error.keyword)),
        ("dataPath", Js.Json.string(error.dataPath)),
        ("schemaPath", Js.Json.string(error.schemaPath)),
        (
          "message",
          Js.Json.string(error.message->Belt.Option.getWithDefault("Validation failed")),
        ),
        ("params", error.params),
      ])
    )
  })
}

// Validate gateway run request
let validateRunRequest = (validator: t, data: Js.Json.t): validationResult => {
  validate(validator, "gateway-run-request", data)
}

// Validate gateway verify request
let validateVerifyRequest = (validator: t, data: Js.Json.t): validationResult => {
  validate(validator, "gateway-verify-request", data)
}

// Validate gatekeeper policy
let validatePolicy = (validator: t, data: Js.Json.t): validationResult => {
  validate(validator, "gatekeeper-policy", data)
}

// Validate compose file
let validateCompose = (validator: t, data: Js.Json.t): validationResult => {
  validate(validator, "compose", data)
}

// Check if data has required fields
let hasRequiredFields = (data: Js.Json.t, fields: array<string>): bool => {
  switch Js.Json.decodeObject(data) {
  | None => false
  | Some(obj) =>
    Belt.Array.every(fields, field => {
      Js.Dict.get(obj, field)->Belt.Option.isSome
    })
  }
}

// Get field from JSON object
let getField = (data: Js.Json.t, field: string): option<Js.Json.t> => {
  data->Js.Json.decodeObject->Belt.Option.flatMap(obj => Js.Dict.get(obj, field))
}

// Get string field from JSON object
let getString = (data: Js.Json.t, field: string): option<string> => {
  getField(data, field)->Belt.Option.flatMap(Js.Json.decodeString)
}

// Get boolean field from JSON object
let getBool = (data: Js.Json.t, field: string): option<bool> => {
  getField(data, field)->Belt.Option.flatMap(Js.Json.decodeBoolean)
}

// Get number field from JSON object
let getNumber = (data: Js.Json.t, field: string): option<float> => {
  getField(data, field)->Belt.Option.flatMap(Js.Json.decodeNumber)
}

// Get array field from JSON object
let getArray = (data: Js.Json.t, field: string): option<array<Js.Json.t>> => {
  getField(data, field)->Belt.Option.flatMap(Js.Json.decodeArray)
}

// Get object field from JSON object
let getObject = (data: Js.Json.t, field: string): option<Js.Dict.t<Js.Json.t>> => {
  getField(data, field)->Belt.Option.flatMap(Js.Json.decodeObject)
}

// Policy validation (stub - to be implemented)
type policy = {
  allowedRegistries: array<string>,
  deniedImages: array<string>,
}

let defaultPolicy: policy = {
  allowedRegistries: ["docker.io", "ghcr.io", "quay.io"],
  deniedImages: [],
}

let isAllowedRegistry = (image: string, policy: policy): bool => {
  Belt.Array.length(policy.allowedRegistries) == 0 ||
  Belt.Array.some(policy.allowedRegistries, registry => Js.String2.includes(image, registry))
}

let isDeniedImage = (image: string, policy: policy): bool => {
  Belt.Array.some(policy.deniedImages, denied => image == denied)
}
