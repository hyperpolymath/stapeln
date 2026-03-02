// SPDX-License-Identifier: PMPL-1.0-or-later
// Example external policy adapter contract for Rokur.
// Replace this with an Ephapax runner invocation when ready.

const decoder = new TextDecoder();
const encoder = new TextEncoder();

async function readStdinJson() {
  const chunks = [];
  for await (const chunk of Deno.stdin.readable) {
    chunks.push(chunk);
  }
  const merged = chunks.length === 1
    ? chunks[0]
    : Uint8Array.from(chunks.flatMap((chunk) => Array.from(chunk)));
  const text = decoder.decode(merged).trim();
  return text.length > 0 ? JSON.parse(text) : {};
}

function evaluate(payload) {
  const requiredSecrets = Array.isArray(payload.requiredSecrets) ? payload.requiredSecrets : [];
  const missingSecretCount = 0;
  const allowed = missingSecretCount === 0;

  return {
    allowed,
    policy: allowed ? "allow" : "deny",
    code: allowed ? "AUTHORIZED" : "POLICY_DENIED",
    requiredSecretCount: requiredSecrets.length,
    missingSecretCount,
  };
}

const input = await readStdinJson();
const decision = evaluate(input);
await Deno.stdout.write(encoder.encode(JSON.stringify(decision)));
