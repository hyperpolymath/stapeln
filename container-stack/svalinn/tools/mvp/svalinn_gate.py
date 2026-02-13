#!/usr/bin/env python3
# SPDX-License-Identifier: PMPL-1.0 OR PMPL-1.0-or-later
import argparse
import base64
import json
import os
import subprocess
import tempfile
from typing import Any, Dict, List
import sys


PAYLOAD_TYPE = "application/vnd.in-toto+json"


def canonical_json_bytes(value):
    return json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=True).encode("utf-8")


def dsse_pae(payload_type, payload_b64):
    def enc(part):
        return str(len(part)).encode("ascii") + b" " + part

    pt = payload_type.encode("utf-8")
    pb = payload_b64.encode("utf-8")
    return b"DSSEv1 " + enc(pt) + b" " + enc(pb)


def require_openssl():
    try:
        subprocess.run(["openssl", "version"], check=True, stdout=subprocess.DEVNULL)
    except (subprocess.CalledProcessError, FileNotFoundError):
        raise SystemExit("openssl not found; required for MVP signature verification")


def run_openssl(args, input_bytes=None):
    return subprocess.run(
        ["openssl"] + args,
        input=input_bytes,
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    ).stdout


def load_json(path):
    with open(path, "r", encoding="utf-8") as handle:
        return json.load(handle)


class GateError(Exception):
    def __init__(self, message: str, error_id: str, details: Dict[str, Any] | None = None):
        super().__init__(message)
        self.error_id = error_id
        self.details = details or {}


def trust_store_keys(trust_store):
    keys = {}
    for role_keys in trust_store.get("keys", {}).values():
        for entry in role_keys:
            key_id = entry.get("id")
            if key_id and "publicKey" in entry:
                keys[key_id] = entry["publicKey"]
    return keys


def verify_signature(pub_key_b64, payload_b64, signature_b64):
    payload_bytes = dsse_pae(PAYLOAD_TYPE, payload_b64)
    pub_der = base64.b64decode(pub_key_b64)
    signature = base64.b64decode(signature_b64)
    with tempfile.TemporaryDirectory() as temp_dir:
        pub_path = os.path.join(temp_dir, "pub.der")
        sig_path = os.path.join(temp_dir, "sig.bin")
        with open(pub_path, "wb") as handle:
            handle.write(pub_der)
        with open(sig_path, "wb") as handle:
            handle.write(signature)
        run_openssl(
            [
                "pkeyutl",
                "-verify",
                "-pubin",
                "-inkey",
                pub_path,
                "-inform",
                "DER",
                "-rawin",
                "-sigfile",
                sig_path,
            ],
            input_bytes=payload_bytes,
        )


def decode_statement(attestation):
    payload = base64.b64decode(attestation["payload"]).decode("utf-8")
    return json.loads(payload)


def sanity_check_bundle(bundle):
    if bundle.get("mediaType") != "application/vnd.verified-container.bundle+json":
        raise GateError("invalid mediaType", "ERR_POLICY_DENIED", {"field": "mediaType"})
    if bundle.get("version") != "0.1.0":
        raise GateError("unsupported bundle version", "ERR_POLICY_DENIED", {"field": "version"})
    if not isinstance(bundle.get("attestations"), list) or not bundle["attestations"]:
        raise GateError("attestations required", "ERR_POLICY_DENIED", {"field": "attestations"})
    if not isinstance(bundle.get("logEntries"), list) or not bundle["logEntries"]:
        raise GateError("logEntries required", "ERR_POLICY_DENIED", {"field": "logEntries"})


def verify_policy(bundle, policy, trust_store, image_digest):
    required_predicates = set(policy["requiredPredicates"])
    allowed_signers = set(policy["allowedSigners"])
    log_quorum = policy.get("logQuorum", 1)

    sanity_check_bundle(bundle)

    keys = trust_store_keys(trust_store)
    log_ids = {entry["logId"] for entry in bundle.get("logEntries", [])}
    if len(log_ids) < log_quorum:
        raise GateError(
            "log quorum not satisfied",
            "ERR_POLICY_DENIED",
            {"required": log_quorum, "observed": len(log_ids)},
        )
    for log_id in log_ids:
        if log_id not in trust_store.get("logs", {}):
            raise GateError(
                "unknown log operator",
                "ERR_POLICY_DENIED",
                {"logId": log_id},
            )

    seen_predicates = set()
    seen_signers = set()
    missing_subjects: List[str] = []
    for attestation in bundle.get("attestations", []):
        statement = decode_statement(attestation)
        seen_predicates.add(statement.get("predicateType"))
        for subject in statement.get("subject", []):
            digest = subject.get("digest", {}).get("sha256")
            if digest and f"sha256:{digest}" != image_digest:
                missing_subjects.append(digest)

        valid_signature = False
        for signature in attestation.get("signatures", []):
            key_id = signature.get("keyid")
            if key_id:
                seen_signers.add(key_id)
            if key_id in allowed_signers and key_id in keys:
                verify_signature(keys[key_id], attestation["payload"], signature["sig"])
                valid_signature = True
                break
        if not valid_signature:
            raise GateError(
                "no valid signature for allowed signer",
                "ERR_POLICY_DENIED",
                {"allowed": sorted(allowed_signers), "seen": sorted(seen_signers)},
            )

    if missing_subjects:
        raise GateError(
            "subject digest mismatch",
            "ERR_POLICY_DENIED",
            {"expected": image_digest, "observed": missing_subjects},
        )

    missing_predicates = required_predicates - seen_predicates
    if missing_predicates:
        raise GateError(
            "missing required predicates",
            "ERR_POLICY_DENIED",
            {"missing": sorted(missing_predicates)},
        )

    return {
        "predicates": sorted(seen_predicates),
        "signers": sorted(seen_signers),
        "logIds": sorted(log_ids),
    }


def command_verify(args):
    require_openssl()
    bundle = load_json(args.bundle)
    trust_store = load_json(args.trust_store)
    policy = load_json(args.policy)
    report = verify_policy(bundle, policy, trust_store, args.image_digest)
    if args.json:
        print(json.dumps({"ok": True, "report": report}, indent=2))
        return
    print("ok")


def build_parser():
    parser = argparse.ArgumentParser(description="Svalinn MVP policy gate")
    sub = parser.add_subparsers(dest="command", required=True)

    verify = sub.add_parser("verify", help="Verify bundle against policy and trust store")
    verify.add_argument("--bundle", required=True)
    verify.add_argument("--trust-store", required=True)
    verify.add_argument("--policy", required=True)
    verify.add_argument("--image-digest", required=True)
    verify.add_argument("--json", action="store_true")
    verify.set_defaults(func=command_verify)

    return parser


def main():
    parser = build_parser()
    args = parser.parse_args()
    try:
        args.func(args)
    except GateError as exc:
        payload = {"ok": False, "error": {"id": exc.error_id, "message": str(exc), "details": exc.details}}
        if getattr(args, "json", False):
            print(json.dumps(payload, indent=2))
        else:
            print(str(exc), file=sys.stderr)
        raise SystemExit(1) from exc


if __name__ == "__main__":
    main()
