#!/usr/bin/env python3
# SPDX-License-Identifier: PMPL-1.0 OR PMPL-1.0-or-later
import argparse
import json
import os
import subprocess
import sys


def run(cmd):
    subprocess.run(cmd, check=True)


def get_digest(image_ref):
    result = subprocess.run(
        [
            "oras",
            "manifest",
            "fetch",
            "--descriptor",
            image_ref,
        ],
        check=True,
        stdout=subprocess.PIPE,
    )
    payload = json.loads(result.stdout.decode("utf-8"))
    digest = payload.get("digest", "")
    if not digest:
        raise SystemExit("oras did not return an image digest")
    return digest


def main():
    parser = argparse.ArgumentParser(description="Cerro Torre MVP pack (wrap existing OCI image)")
    parser.add_argument("--image", required=True, help="OCI image reference")
    parser.add_argument("--out-dir", required=True)
    parser.add_argument("--image-digest", default="")
    parser.add_argument("--log-id", default="verified-container-log-mvp")
    parser.add_argument("--log-url", default="https://logs.mvp.local")
    parser.add_argument("--log-operator", default="Cerro Torre MVP Log")
    parser.add_argument("--store-id", default="cerro-torre-mvp")
    parser.add_argument("--signer-owner", default="cerro-torre-mvp")
    parser.add_argument("--attach", action="store_true")
    args = parser.parse_args()

    os.makedirs(args.out_dir, exist_ok=True)
    keys_dir = os.path.join(args.out_dir, "keys")
    os.makedirs(keys_dir, exist_ok=True)

    image_digest = args.image_digest or get_digest(args.image)

    run(["python3", "tools/mvp/ct_mvp.py", "keygen", "--out-dir", keys_dir])
    run(
        [
            "python3",
            "tools/mvp/ct_mvp.py",
            "trust-store",
            "--signer-pub",
            os.path.join(keys_dir, "signer.pub"),
            "--log-pub",
            os.path.join(keys_dir, "log.pub"),
            "--log-id",
            args.log_id,
            "--log-url",
            args.log_url,
            "--log-operator",
            args.log_operator,
            "--store-id",
            args.store_id,
            "--signer-owner",
            args.signer_owner,
            "--out",
            os.path.join(args.out_dir, "trust-store.json"),
        ]
    )
    run(
        [
            "python3",
            "tools/mvp/ct_mvp.py",
            "bundle",
            "--image-name",
            args.image,
            "--image-digest",
            image_digest,
            "--signer-key",
            os.path.join(keys_dir, "signer.key"),
            "--signer-pub",
            os.path.join(keys_dir, "signer.pub"),
            "--log-key",
            os.path.join(keys_dir, "log.key"),
            "--log-id",
            args.log_id,
            "--out",
            os.path.join(args.out_dir, "bundle.json"),
        ]
    )

    if args.attach:
        run(
            [
                "tools/mvp/publish_bundle.sh",
                args.image,
                os.path.join(args.out_dir, "bundle.json"),
                "application/vnd.verified-container.bundle+json",
            ]
        )

    summary = {
        "image": args.image,
        "digest": image_digest,
        "bundle": os.path.join(args.out_dir, "bundle.json"),
        "trustStore": os.path.join(args.out_dir, "trust-store.json"),
        "keysDir": keys_dir,
        "attached": bool(args.attach),
    }
    with open(os.path.join(args.out_dir, "summary.json"), "w", encoding="utf-8") as handle:
        json.dump(summary, handle, indent=2)


if __name__ == "__main__":
    main()
