// SPDX-License-Identifier: PMPL-1.0-or-later
// Rokur policy engine boundary: builtin evaluator now, external command hook for Ephapax.

const VALID_BACKENDS = new Set(["builtin", "external", "auto"]);
const DEFAULT_EXTERNAL_TIMEOUT_MS = 1500;

function parseIntegerEnv(name, defaultValue, min, max) {
  const raw = Deno.env.get(name);
  if (!raw || raw.trim().length === 0) {
    return defaultValue;
  }

  const parsed = Number(raw);
  if (!Number.isInteger(parsed)) {
    throw new Error(`Invalid ${name}: "${raw}". Expected integer.`);
  }
  if (parsed < min || parsed > max) {
    throw new Error(
      `Invalid ${name}: "${raw}". Expected integer between ${min} and ${max}.`,
    );
  }

  return parsed;
}

function parseCommandArgs(rawValue) {
  if (!rawValue || rawValue.trim().length === 0) {
    return [];
  }

  const trimmed = rawValue.trim();
  if (trimmed.startsWith("[")) {
    let parsed;
    try {
      parsed = JSON.parse(trimmed);
    } catch (error) {
      throw new Error(
        `Invalid ROKUR_POLICY_COMMAND_ARGS JSON: ${
          error instanceof Error ? error.message : String(error)
        }`,
      );
    }
    if (
      !Array.isArray(parsed) ||
      !parsed.every((value) => typeof value === "string")
    ) {
      throw new Error(
        "Invalid ROKUR_POLICY_COMMAND_ARGS JSON: expected array of strings.",
      );
    }
    return parsed;
  }

  return trimmed.split(",").map((value) => value.trim()).filter((value) =>
    value.length > 0
  );
}

function resolveBackend(requestedBackend, externalCommand) {
  if (!VALID_BACKENDS.has(requestedBackend)) {
    throw new Error(
      `Invalid ROKUR_POLICY_BACKEND: "${requestedBackend}". Use builtin, external, or auto.`,
    );
  }

  if (requestedBackend === "auto") {
    return externalCommand ? "external" : "builtin";
  }

  return requestedBackend;
}

function createDeniedDecision({
  requiredSecretCount,
  code = "POLICY_DENIED",
  missingSecretCount = requiredSecretCount,
  engine,
}) {
  return {
    allowed: false,
    policy: "deny",
    code,
    requiredSecretCount,
    missingSecretCount,
    engine,
  };
}

function evaluateBuiltin(requiredSecrets, secretEnvName) {
  let missingSecretCount = 0;

  for (const secretName of requiredSecrets) {
    const envName = secretEnvName(secretName);
    const value = Deno.env.get(envName);
    if (!value || value.trim().length === 0) {
      missingSecretCount += 1;
    }
  }

  const allowed = missingSecretCount === 0;
  return {
    allowed,
    policy: allowed ? "allow" : "deny",
    code: allowed ? "AUTHORIZED" : "REQUIRED_SECRETS_MISSING",
    requiredSecretCount: requiredSecrets.length,
    missingSecretCount,
    engine: "builtin",
  };
}

function normalizeExternalDecision(rawDecision, requiredSecretCount) {
  if (!rawDecision || typeof rawDecision !== "object") {
    return createDeniedDecision({
      requiredSecretCount,
      code: "POLICY_ENGINE_INVALID_RESPONSE",
      engine: "external",
    });
  }

  const allowed = rawDecision.allowed === true;
  const policy = typeof rawDecision.policy === "string"
    ? rawDecision.policy
    : (allowed ? "allow" : "deny");
  const code = typeof rawDecision.code === "string"
    ? rawDecision.code
    : (allowed ? "AUTHORIZED" : "POLICY_DENIED");

  const missingSecretCount = Number.isInteger(rawDecision.missingSecretCount) &&
      rawDecision.missingSecretCount >= 0
    ? rawDecision.missingSecretCount
    : (allowed ? 0 : requiredSecretCount);

  const resolvedRequiredSecretCount =
    Number.isInteger(rawDecision.requiredSecretCount) &&
      rawDecision.requiredSecretCount >= 0
      ? rawDecision.requiredSecretCount
      : requiredSecretCount;

  return {
    allowed,
    policy,
    code,
    requiredSecretCount: resolvedRequiredSecretCount,
    missingSecretCount,
    engine: "external",
  };
}

async function evaluateExternal(config, input, requiredSecrets) {
  const encoder = new TextEncoder();
  const decoder = new TextDecoder();

  const requestPayload = {
    version: "1",
    timestamp: new Date().toISOString(),
    requiredSecrets,
    input: {
      image: typeof input.image === "string" ? input.image : null,
      name: typeof input.name === "string" ? input.name : null,
    },
  };

  let child;
  let timeoutId;
  try {
    const command = new Deno.Command(config.externalCommand, {
      args: config.externalCommandArgs,
      stdin: "piped",
      stdout: "piped",
      stderr: "piped",
    });

    child = command.spawn();

    const writer = child.stdin.getWriter();
    await writer.write(encoder.encode(JSON.stringify(requestPayload)));
    await writer.close();

    const timeoutPromise = new Promise((_, reject) => {
      timeoutId = setTimeout(() => {
        try {
          child.kill("SIGTERM");
        } catch {
          // Ignore: process may have exited.
        }
        reject(
          new Error(
            `External policy command timed out after ${config.externalTimeoutMs}ms.`,
          ),
        );
      }, config.externalTimeoutMs);
    });

    const output = await Promise.race([child.output(), timeoutPromise]);
    clearTimeout(timeoutId);

    if (output.code !== 0) {
      return createDeniedDecision({
        requiredSecretCount: requiredSecrets.length,
        code: "POLICY_ENGINE_ERROR",
        engine: "external",
      });
    }

    const stdoutText = decoder.decode(output.stdout).trim();
    if (!stdoutText) {
      return createDeniedDecision({
        requiredSecretCount: requiredSecrets.length,
        code: "POLICY_ENGINE_EMPTY_RESPONSE",
        engine: "external",
      });
    }

    let parsedResponse;
    try {
      parsedResponse = JSON.parse(stdoutText);
    } catch {
      return createDeniedDecision({
        requiredSecretCount: requiredSecrets.length,
        code: "POLICY_ENGINE_INVALID_JSON",
        engine: "external",
      });
    }

    return normalizeExternalDecision(parsedResponse, requiredSecrets.length);
  } catch {
    if (timeoutId) {
      clearTimeout(timeoutId);
    }
    return createDeniedDecision({
      requiredSecretCount: requiredSecrets.length,
      code: "POLICY_ENGINE_UNAVAILABLE",
      engine: "external",
    });
  }
}

export function createPolicyEvaluator({ requiredSecrets, secretEnvName }) {
  const requestedBackend = (Deno.env.get("ROKUR_POLICY_BACKEND") ?? "builtin")
    .trim().toLowerCase();
  const externalCommand = (Deno.env.get("ROKUR_POLICY_COMMAND") ?? "").trim();
  const externalCommandArgs = parseCommandArgs(
    Deno.env.get("ROKUR_POLICY_COMMAND_ARGS") ?? "",
  );
  const externalTimeoutMs = parseIntegerEnv(
    "ROKUR_POLICY_TIMEOUT_MS",
    DEFAULT_EXTERNAL_TIMEOUT_MS,
    100,
    60000,
  );

  const resolvedBackend = resolveBackend(requestedBackend, externalCommand);
  if (resolvedBackend === "external" && externalCommand.length === 0) {
    throw new Error(
      "ROKUR_POLICY_BACKEND=external requires ROKUR_POLICY_COMMAND.",
    );
  }

  return {
    config: {
      requestedBackend,
      resolvedBackend,
      externalCommandConfigured: externalCommand.length > 0,
      externalTimeoutMs,
    },
    evaluate: (input) => {
      if (resolvedBackend === "external") {
        return evaluateExternal(
          {
            externalCommand,
            externalCommandArgs,
            externalTimeoutMs,
          },
          input,
          requiredSecrets,
        );
      }

      return Promise.resolve(evaluateBuiltin(requiredSecrets, secretEnvName));
    },
  };
}
