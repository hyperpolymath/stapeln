#!/usr/bin/env python3
# SPDX-License-Identifier: PMPL-1.0 OR PMPL-1.0-or-later
import argparse
import base64
import datetime as dt
import hashlib
import json
import os
import subprocess
import sys


PAYLOAD_TYPE = "application/vnd.in-toto+json"


def canonical_json_bytes(value):
    return json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=True).encode("utf-8")


def sha256_hex(data):
    return hashlib.sha256(data).hexdigest()


def require_openssl():
    try:
        subprocess.run(["openssl", "version"], check=True, stdout=subprocess.DEVNULL)
    except (subprocess.CalledProcessError, FileNotFoundError):
        raise SystemExit("openssl not found; required for MVP signing")


def run_openssl(args, input_bytes=None):
    return subprocess.run(
        ["openssl"] + args,
        input=input_bytes,
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    ).stdout


def dsse_pae(payload_type, payload_b64):
    def enc(part):
        return str(len(part)).encode("ascii") + b" " + part

    pt = payload_type.encode("utf-8")
    pb = payload_b64.encode("utf-8")
    return b"DSSEv1 " + enc(pt) + b" " + enc(pb)


def ed25519_sign(priv_key_path, message_bytes):
    return run_openssl(
        ["pkeyutl", "-sign", "-inkey", priv_key_path, "-rawin"],
        input_bytes=message_bytes,
    )


def public_key_der(pub_key_path):
    return run_openssl(["pkey", "-pubin", "-in", pub_key_path, "-outform", "DER"])


def key_id_from_pub(pub_key_path):
    return "sha256:" + sha256_hex(public_key_der(pub_key_path))


def generate_keypair(out_dir, prefix):
    priv_path = os.path.join(out_dir, f"{prefix}.key")
    pub_path = os.path.join(out_dir, f"{prefix}.pub")
    run_openssl(["genpkey", "-algorithm", "ED25519", "-out", priv_path])
    run_openssl(["pkey", "-in", priv_path, "-pubout", "-out", pub_path])
    return priv_path, pub_path


def build_statement(subject_name, subject_digest, predicate_type, predicate):
    return {
        "_type": "https://in-toto.io/Statement/v1",
        "subject": [
            {
                "name": subject_name,
                "digest": {"sha256": subject_digest.replace("sha256:", "")},
            }
        ],
        "predicateType": predicate_type,
        "predicate": predicate,
    }


def dsse_envelope(statement, signer_key_id, signer_priv_key):
    payload = base64.b64encode(canonical_json_bytes(statement)).decode("ascii")
    pae = dsse_pae(PAYLOAD_TYPE, payload)
    signature = ed25519_sign(signer_priv_key, pae)
    return {
        "payloadType": PAYLOAD_TYPE,
        "payload": payload,
        "signatures": [
            {
                "keyid": signer_key_id,
                "sig": base64.b64encode(signature).decode("ascii"),
            }
        ],
    }


def log_entry_for_attestation(attestation_digest, subject_digest, predicate_type, log_key_path, log_id):
    timestamp = dt.datetime.utcnow().replace(microsecond=0).isoformat() + "Z"
    entry = {
        "version": 1,
        "timestamp": timestamp,
        "entryType": "attestation",
        "body": {
            "attestationDigest": attestation_digest,
            "subjectDigest": subject_digest,
            "predicateType": predicate_type,
        },
    }
    entry_bytes = canonical_json_bytes(entry)
    leaf_hash = sha256_hex(b"\x00" + entry_bytes)
    signed_entry_timestamp = ed25519_sign(log_key_path, entry_bytes)
    return {
        "logId": log_id,
        "logIndex": 0,
        "integratedTime": timestamp,
        "inclusionProof": {
            "logIndex": 0,
            "rootHash": leaf_hash,
            "treeSize": 1,
            "hashes": [],
        },
        "signedEntryTimestamp": base64.b64encode(signed_entry_timestamp).decode("ascii"),
    }


def command_keygen(args):
    require_openssl()
    os.makedirs(args.out_dir, exist_ok=True)
    signer_priv, signer_pub = generate_keypair(args.out_dir, args.signer_prefix)
    log_priv, log_pub = generate_keypair(args.out_dir, args.log_prefix)
    output = {
        "signer": {
            "privateKey": signer_priv,
            "publicKey": signer_pub,
            "keyId": key_id_from_pub(signer_pub),
        },
        "log": {
            "privateKey": log_priv,
            "publicKey": log_pub,
            "keyId": key_id_from_pub(log_pub),
        },
    }
    print(json.dumps(output, indent=2))


def command_trust_store(args):
    signer_key_id = key_id_from_pub(args.signer_pub)
    log_key_id = key_id_from_pub(args.log_pub)
    trust_store = {
        "$schema": "https://verified-container.org/schema/trust-store-v1.json",
        "version": 1,
        "id": args.store_id,
        "updated": dt.datetime.utcnow().replace(microsecond=0).isoformat() + "Z",
        "keys": {
            "builders": [
                {
                    "id": signer_key_id,
                    "algorithm": "ed25519",
                    "publicKey": base64.b64encode(public_key_der(args.signer_pub)).decode("ascii"),
                    "validFrom": args.valid_from,
                    "metadata": {"owner": args.signer_owner},
                }
            ]
        },
        "thresholds": {},
        "logs": {
            args.log_id: {
                "operator": args.log_operator,
                "publicKey": base64.b64encode(public_key_der(args.log_pub)).decode("ascii"),
                "url": args.log_url,
                "algorithm": "ed25519",
            }
        },
        "metadata": {
            "mvp": True,
            "logKeyId": log_key_id,
        },
    }
    with open(args.out, "w", encoding="utf-8") as handle:
        json.dump(trust_store, handle, indent=2)


def command_bundle(args):
    require_openssl()
    signer_key_id = key_id_from_pub(args.signer_pub)
    slsa_statement = build_statement(
        args.image_name,
        args.image_digest,
        "https://slsa.dev/provenance/v1",
        {
            "buildType": "https://cerro-torre.org/build/mvp",
            "builder": {"id": "cerro-torre-mvp"},
            "invocation": {},
        },
    )
    sbom_statement = build_statement(
        args.image_name,
        args.image_digest,
        "https://spdx.dev/Document",
        {"spdxVersion": "SPDX-2.3", "name": "mvp-sbom"},
    )
    slsa_dsse = dsse_envelope(slsa_statement, signer_key_id, args.signer_key)
    sbom_dsse = dsse_envelope(sbom_statement, signer_key_id, args.signer_key)
    slsa_digest = "sha256:" + sha256_hex(canonical_json_bytes(slsa_dsse))
    sbom_digest = "sha256:" + sha256_hex(canonical_json_bytes(sbom_dsse))
    log_entries = [
        log_entry_for_attestation(
            slsa_digest,
            args.image_digest,
            slsa_statement["predicateType"],
            args.log_key,
            args.log_id,
        ),
        log_entry_for_attestation(
            sbom_digest,
            args.image_digest,
            sbom_statement["predicateType"],
            args.log_key,
            args.log_id,
        ),
    ]
    bundle = {
        "mediaType": "application/vnd.verified-container.bundle+json",
        "version": "0.1.0",
        "attestations": [slsa_dsse, sbom_dsse],
        "logEntries": log_entries,
    }
    with open(args.out, "w", encoding="utf-8") as handle:
        json.dump(bundle, handle, indent=2)


def build_parser():
    parser = argparse.ArgumentParser(description="Cerro Torre MVP bundle helper")
    sub = parser.add_subparsers(dest="command", required=True)

    keygen = sub.add_parser("keygen", help="Generate signer and log keys")
    keygen.add_argument("--out-dir", required=True)
    keygen.add_argument("--signer-prefix", default="signer")
    keygen.add_argument("--log-prefix", default="log")
    keygen.set_defaults(func=command_keygen)

    trust = sub.add_parser("trust-store", help="Create trust store JSON")
    trust.add_argument("--signer-pub", required=True)
    trust.add_argument("--log-pub", required=True)
    trust.add_argument("--log-id", required=True)
    trust.add_argument("--log-url", required=True)
    trust.add_argument("--log-operator", default="Cerro Torre MVP Log")
    trust.add_argument("--store-id", default="cerro-torre-mvp")
    trust.add_argument("--signer-owner", default="cerro-torre-mvp")
    trust.add_argument("--valid-from", default="2024-01-01T00:00:00Z")
    trust.add_argument("--out", required=True)
    trust.set_defaults(func=command_trust_store)

    bundle = sub.add_parser("bundle", help="Create attestation bundle JSON")
    bundle.add_argument("--image-name", required=True)
    bundle.add_argument("--image-digest", required=True)
    bundle.add_argument("--signer-key", required=True)
    bundle.add_argument("--signer-pub", required=True)
    bundle.add_argument("--log-key", required=True)
    bundle.add_argument("--log-id", required=True)
    bundle.add_argument("--out", required=True)
    bundle.set_defaults(func=command_bundle)

    return parser


def main():
    parser = build_parser()
    args = parser.parse_args()
    args.func(args)


if __name__ == "__main__":
    main()
