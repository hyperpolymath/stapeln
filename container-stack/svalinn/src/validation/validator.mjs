// SPDX-License-Identifier: PMPL-1.0-or-later
// JSON Schema validator for Svalinn

import Ajv from "ajv";
import addFormats from "ajv-formats";

const ajv = new Ajv({ allErrors: true });
addFormats(ajv);

const schemas = new Map();
let schemasLoaded = false;

/**
 * Load all schemas from spec/schemas/
 * @param {string} basePath - Base path to spec/schemas/
 */
export async function loadSchemas(basePath = "../spec/schemas/") {
  if (schemasLoaded) return;

  const schemaFiles = [
    "gateway-run-request.v1.json",
    "gateway-verify-request.v1.json",
    "container-info.v1.json",
    "containers.v1.json",
    "doctor-report.v1.json",
    "error-response.v1.json",
    "gatekeeper-policy.v1.json",
    "images.v1.json",
  ];

  for (const file of schemaFiles) {
    try {
      const url = new URL(file, new URL(basePath, import.meta.url));
      const text = await Deno.readTextFile(url);
      const schema = JSON.parse(text);
      schemas.set(file, schema);
      ajv.addSchema(schema, file);
    } catch (e) {
      console.warn(`Could not load schema ${file}: ${e}`);
    }
  }

  schemasLoaded = true;
}

/**
 * Validate data against a schema
 * @param {string} schemaName - Name of the schema file
 * @param {unknown} data - Data to validate
 * @returns {Promise<{Valid: undefined} | {Invalid: Array<{path: string, message: string, keyword: string}>}>}
 */
export async function validate(schemaName, data) {
  if (!schemasLoaded) {
    await loadSchemas();
  }

  const validateFn = ajv.getSchema(schemaName);
  if (!validateFn) {
    return {
      Invalid: [{ path: "", message: `Schema not found: ${schemaName}`, keyword: "schema" }],
    };
  }

  const valid = validateFn(data);
  if (valid) {
    return { Valid: undefined };
  }

  const errors = (validateFn.errors || []).map((err) => ({
    path: err.instancePath || "",
    message: err.message || "Unknown error",
    keyword: err.keyword || "unknown",
  }));

  return { Invalid: errors };
}
