#!/usr/bin/env -S deno run --allow-read --allow-run=openssl
// SPDX-License-Identifier: PMPL-1.0-or-later
// Svalinn MVP policy gate — verify container bundles against policy and trust store
// Replaces svalinn_gate.py (Python is a banned language per hyperpolymath standards)

import { parseArgs } from "jsr:@std/cli@1/parse-args";
import { encodeBase64, decodeBase64 } from "jsr:@std/encoding@1/base64";

const PAYLOAD_TYPE = "application/vnd.in-toto+json";

/** Canonical JSON encoding (sorted keys, no whitespace). */
function canonicalJsonBytes(value) {
  return new TextEncoder().encode(
    JSON.stringify(value, Object.keys(value).sort(), 0)
      .replace(/\s+/g, "")
  );
}

/** Deterministic JSON serialisation matching Python's sort_keys+compact. */
function sortedCompactJson(value) {
  if (value === null || typeof value !== "object") return JSON.stringify(value);
  if (Array.isArray(value)) {
    return "[" + value.map(sortedCompactJson).join(",") + "]";
  }
  const keys = Object.keys(value).sort();
  return "{" + keys.map((k) => JSON.stringify(k) + ":" + sortedCompactJson(value[k])).join(",") + "}";
}

function canonicalBytes(value) {
  return new TextEncoder().encode(sortedCompactJson(value));
}

/** DSSE Pre-Authentication Encoding. */
function dssePae(payloadType, payloadB64) {
  const enc = (part) => {
    const bytes = new TextEncoder().encode(part);
    return new TextEncoder().encode(`${bytes.length} `);
  };
  const ptBytes = new TextEncoder().encode(payloadType);
  const pbBytes = new TextEncoder().encode(payloadB64);
  const header = new TextEncoder().encode("DSSEv1 ");
  const ptLen = new TextEncoder().encode(`${ptBytes.length} `);
  const pbLen = new TextEncoder().encode(`${pbBytes.length} `);
  const out = new Uint8Array(header.length + ptLen.length + ptBytes.length + 1 + pbLen.length + pbBytes.length);
  let offset = 0;
  out.set(header, offset); offset += header.length;
  out.set(ptLen, offset); offset += ptLen.length;
  out.set(ptBytes, offset); offset += ptBytes.length;
  out.set(new TextEncoder().encode(" "), offset); offset += 1;
  out.set(pbLen, offset); offset += pbLen.length;
  out.set(pbBytes, offset);
  return out;
}

/** Verify openssl is available. */
async function requireOpenssl() {
  try {
    const cmd = new Deno.Command("openssl", { args: ["version"], stdout: "null", stderr: "null" });
    const status = await cmd.output();
    if (!status.success) throw new Error("openssl failed");
  } catch {
    console.error("openssl not found; required for MVP signature verification");
    Deno.exit(1);
  }
}

/** Run openssl with given args and optional stdin. */
async function runOpenssl(args, inputBytes) {
  const cmd = new Deno.Command("openssl", {
    args,
    stdin: inputBytes ? "piped" : "null",
    stdout: "piped",
    stderr: "piped",
  });
  const child = cmd.spawn();
  if (inputBytes) {
    const writer = child.stdin.getWriter();
    await writer.write(inputBytes);
    await writer.close();
  }
  const output = await child.output();
  if (!output.success) {
    const errText = new TextDecoder().decode(output.stderr);
    throw new Error(`openssl error: ${errText}`);
  }
  return output.stdout;
}

/** Load and parse a JSON file. */
async function loadJson(path) {
  const text = await Deno.readTextFile(path);
  return JSON.parse(text);
}

class GateError extends Error {
  constructor(message, errorId, details = {}) {
    super(message);
    this.name = "GateError";
    this.errorId = errorId;
    this.details = details;
  }
}

/** Extract public keys from trust store, keyed by ID. */
function trustStoreKeys(trustStore) {
  const keys = {};
  for (const roleKeys of Object.values(trustStore.keys ?? {})) {
    for (const entry of roleKeys) {
      if (entry.id && entry.publicKey) {
        keys[entry.id] = entry.publicKey;
      }
    }
  }
  return keys;
}

/** Verify a DSSE signature using openssl. */
async function verifySignature(pubKeyB64, payloadB64, signatureB64) {
  const payloadBytes = dssePae(PAYLOAD_TYPE, payloadB64);
  const pubDer = decodeBase64(pubKeyB64);
  const signature = decodeBase64(signatureB64);

  const tmpDir = await Deno.makeTempDir();
  try {
    const pubPath = `${tmpDir}/pub.der`;
    const sigPath = `${tmpDir}/sig.bin`;
    await Deno.writeFile(pubPath, pubDer);
    await Deno.writeFile(sigPath, signature);
    await runOpenssl(
      ["pkeyutl", "-verify", "-pubin", "-inkey", pubPath, "-inform", "DER", "-rawin", "-sigfile", sigPath],
      payloadBytes,
    );
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
}

/** Decode an in-toto statement from an attestation envelope. */
function decodeStatement(attestation) {
  const payload = new TextDecoder().decode(decodeBase64(attestation.payload));
  return JSON.parse(payload);
}

/** Basic bundle structure validation. */
function sanityCheckBundle(bundle) {
  if (bundle.mediaType !== "application/vnd.verified-container.bundle+json") {
    throw new GateError("invalid mediaType", "ERR_POLICY_DENIED", { field: "mediaType" });
  }
  if (bundle.version !== "0.1.0") {
    throw new GateError("unsupported bundle version", "ERR_POLICY_DENIED", { field: "version" });
  }
  if (!Array.isArray(bundle.attestations) || bundle.attestations.length === 0) {
    throw new GateError("attestations required", "ERR_POLICY_DENIED", { field: "attestations" });
  }
  if (!Array.isArray(bundle.logEntries) || bundle.logEntries.length === 0) {
    throw new GateError("logEntries required", "ERR_POLICY_DENIED", { field: "logEntries" });
  }
}

/** Verify bundle against policy and trust store. */
async function verifyPolicy(bundle, policy, trustStore, imageDigest) {
  const requiredPredicates = new Set(policy.requiredPredicates);
  const allowedSigners = new Set(policy.allowedSigners);
  const logQuorum = policy.logQuorum ?? 1;

  sanityCheckBundle(bundle);

  const keys = trustStoreKeys(trustStore);
  const logIds = new Set((bundle.logEntries ?? []).map((e) => e.logId));
  if (logIds.size < logQuorum) {
    throw new GateError("log quorum not satisfied", "ERR_POLICY_DENIED", {
      required: logQuorum,
      observed: logIds.size,
    });
  }
  for (const logId of logIds) {
    if (!(logId in (trustStore.logs ?? {}))) {
      throw new GateError("unknown log operator", "ERR_POLICY_DENIED", { logId });
    }
  }

  const seenPredicates = new Set();
  const seenSigners = new Set();
  const missingSubjects = [];

  for (const attestation of bundle.attestations ?? []) {
    const statement = decodeStatement(attestation);
    seenPredicates.add(statement.predicateType);

    for (const subject of statement.subject ?? []) {
      const digest = subject.digest?.sha256;
      if (digest && `sha256:${digest}` !== imageDigest) {
        missingSubjects.push(digest);
      }
    }

    let validSignature = false;
    for (const signature of attestation.signatures ?? []) {
      const keyId = signature.keyid;
      if (keyId) seenSigners.add(keyId);
      if (allowedSigners.has(keyId) && keyId in keys) {
        await verifySignature(keys[keyId], attestation.payload, signature.sig);
        validSignature = true;
        break;
      }
    }
    if (!validSignature) {
      throw new GateError("no valid signature for allowed signer", "ERR_POLICY_DENIED", {
        allowed: [...allowedSigners].sort(),
        seen: [...seenSigners].sort(),
      });
    }
  }

  if (missingSubjects.length > 0) {
    throw new GateError("subject digest mismatch", "ERR_POLICY_DENIED", {
      expected: imageDigest,
      observed: missingSubjects,
    });
  }

  const missingPredicates = [...requiredPredicates].filter((p) => !seenPredicates.has(p));
  if (missingPredicates.length > 0) {
    throw new GateError("missing required predicates", "ERR_POLICY_DENIED", {
      missing: missingPredicates.sort(),
    });
  }

  return {
    predicates: [...seenPredicates].sort(),
    signers: [...seenSigners].sort(),
    logIds: [...logIds].sort(),
  };
}

async function main() {
  const args = parseArgs(Deno.args, {
    string: ["bundle", "trust-store", "policy", "image-digest"],
    boolean: ["json", "help"],
    alias: { h: "help" },
  });

  if (args.help || args._.length === 0) {
    console.log(`Usage: svalinn_gate.js verify --bundle <path> --trust-store <path> --policy <path> --image-digest <digest> [--json]`);
    Deno.exit(0);
  }

  const command = args._[0];
  if (command !== "verify") {
    console.error(`Unknown command: ${command}`);
    Deno.exit(1);
  }

  if (!args.bundle || !args["trust-store"] || !args.policy || !args["image-digest"]) {
    console.error("Missing required arguments: --bundle, --trust-store, --policy, --image-digest");
    Deno.exit(1);
  }

  await requireOpenssl();

  try {
    const bundle = await loadJson(args.bundle);
    const trustStore = await loadJson(args["trust-store"]);
    const policy = await loadJson(args.policy);
    const report = await verifyPolicy(bundle, policy, trustStore, args["image-digest"]);

    if (args.json) {
      console.log(JSON.stringify({ ok: true, report }, null, 2));
    } else {
      console.log("ok");
    }
  } catch (err) {
    if (err instanceof GateError) {
      const payload = {
        ok: false,
        error: { id: err.errorId, message: err.message, details: err.details },
      };
      if (args.json) {
        console.log(JSON.stringify(payload, null, 2));
      } else {
        console.error(err.message);
      }
      Deno.exit(1);
    }
    throw err;
  }
}

main();
