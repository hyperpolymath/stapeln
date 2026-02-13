#!/usr/bin/env python3
# SPDX-License-Identifier: PMPL-1.0 OR PMPL-1.0-or-later
import argparse
import hashlib
import json
import os
import shutil
import tarfile
import tempfile
import urllib.request


def sha256_hex(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def sha256_file(path: str) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


def parse_ctp(path: str) -> dict:
    current = None
    data: dict[str, dict[str, str]] = {}
    with open(path, "r", encoding="utf-8") as handle:
        for raw in handle:
            line = raw.strip()
            if not line or line.startswith("#"):
                continue
            if line.startswith("[") and line.endswith("]"):
                current = line[1:-1]
                data[current] = {}
                continue
            if current and "=" in line:
                key, value = line.split("=", 1)
                key = key.strip()
                value = value.strip()
                if value.startswith('"') and value.endswith('"'):
                    value = value[1:-1]
                data[current][key] = value
    return data


def download(url: str, dest: str):
    with urllib.request.urlopen(url) as response, open(dest, "wb") as handle:
        shutil.copyfileobj(response, handle)


def write_oci_layout(rootfs_dir: str, out_dir: str, name: str, version: str):
    os.makedirs(out_dir, exist_ok=True)
    blobs_dir = os.path.join(out_dir, "blobs", "sha256")
    os.makedirs(blobs_dir, exist_ok=True)

    layer_path = os.path.join(out_dir, "layer.tar")
    with tarfile.open(layer_path, "w") as tar:
        tar.add(rootfs_dir, arcname="")

    diff_id = sha256_file(layer_path)
    with open(layer_path, "rb") as handle:
        layer_bytes = handle.read()
    layer_digest = sha256_hex(layer_bytes)
    layer_size = len(layer_bytes)

    config = {
        "created": "2025-01-01T00:00:00Z",
        "architecture": "amd64",
        "os": "linux",
        "config": {"Labels": {"org.opencontainers.image.ref.name": f"{name}:{version}"}},
        "rootfs": {"type": "layers", "diff_ids": [f"sha256:{diff_id}"]},
        "history": [{"created": "2025-01-01T00:00:00Z", "created_by": "ct_build"}],
    }
    config_bytes = json.dumps(config, separators=(",", ":")).encode("utf-8")
    config_digest = sha256_hex(config_bytes)

    with open(os.path.join(blobs_dir, config_digest), "wb") as handle:
        handle.write(config_bytes)
    with open(os.path.join(blobs_dir, layer_digest), "wb") as handle:
        handle.write(layer_bytes)

    manifest = {
        "schemaVersion": 2,
        "mediaType": "application/vnd.oci.image.manifest.v1+json",
        "config": {
            "mediaType": "application/vnd.oci.image.config.v1+json",
            "digest": f"sha256:{config_digest}",
            "size": len(config_bytes),
        },
        "layers": [
            {
                "mediaType": "application/vnd.oci.image.layer.v1.tar",
                "digest": f"sha256:{layer_digest}",
                "size": layer_size,
            }
        ],
    }
    manifest_bytes = json.dumps(manifest, separators=(",", ":")).encode("utf-8")
    manifest_digest = sha256_hex(manifest_bytes)
    with open(os.path.join(blobs_dir, manifest_digest), "wb") as handle:
        handle.write(manifest_bytes)

    index = {
        "schemaVersion": 2,
        "manifests": [
            {
                "mediaType": "application/vnd.oci.image.manifest.v1+json",
                "digest": f"sha256:{manifest_digest}",
                "size": len(manifest_bytes),
                "annotations": {"org.opencontainers.image.ref.name": f"{name}:{version}"},
            }
        ],
    }
    with open(os.path.join(out_dir, "index.json"), "w", encoding="utf-8") as handle:
        json.dump(index, handle, indent=2)
    with open(os.path.join(out_dir, "oci-layout"), "w", encoding="utf-8") as handle:
        json.dump({"imageLayoutVersion": "1.0.0"}, handle, indent=2)

    return f"sha256:{manifest_digest}"


def main():
    parser = argparse.ArgumentParser(description="Cerro Torre MVP build from .ctp manifest")
    parser.add_argument("--manifest", required=True)
    parser.add_argument("--out-dir", required=True)
    parser.add_argument("--attach", action="store_true")
    parser.add_argument("--log-id", default="verified-container-log-mvp")
    parser.add_argument("--log-url", default="https://logs.mvp.local")
    parser.add_argument("--log-operator", default="Cerro Torre MVP Log")
    parser.add_argument("--store-id", default="cerro-torre-mvp")
    parser.add_argument("--signer-owner", default="cerro-torre-mvp")
    args = parser.parse_args()

    data = parse_ctp(args.manifest)
    metadata = data.get("metadata", {})
    provenance = data.get("provenance", {})
    name = metadata.get("name", "ct-image")
    version = metadata.get("version", "0.0.0")
    upstream = provenance.get("upstream", "")
    upstream_hash = provenance.get("upstream_hash", "")

    if not upstream or not upstream_hash.startswith("sha256:"):
        raise SystemExit("manifest requires provenance.upstream and sha256 upstream_hash")

    os.makedirs(args.out_dir, exist_ok=True)
    keys_dir = os.path.join(args.out_dir, "keys")
    os.makedirs(keys_dir, exist_ok=True)

    with tempfile.TemporaryDirectory() as temp_dir:
        archive_path = os.path.join(temp_dir, "source.tar.gz")
        download(upstream, archive_path)
        digest = sha256_file(archive_path)
        if digest != upstream_hash.replace("sha256:", ""):
            raise SystemExit("upstream hash mismatch")

        rootfs_dir = os.path.join(args.out_dir, "rootfs")
        if os.path.exists(rootfs_dir):
            shutil.rmtree(rootfs_dir)
        os.makedirs(rootfs_dir, exist_ok=True)
        with tarfile.open(archive_path, "r:*") as tar:
            tar.extractall(path=rootfs_dir)

        oci_dir = os.path.join(args.out_dir, "oci")
        if os.path.exists(oci_dir):
            shutil.rmtree(oci_dir)
        os.makedirs(oci_dir, exist_ok=True)
        image_digest = write_oci_layout(rootfs_dir, oci_dir, name, version)

    subprocess = __import__("subprocess")
    subprocess.run(["python3", "tools/mvp/ct_mvp.py", "keygen", "--out-dir", keys_dir], check=True)
    subprocess.run(
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
        ],
        check=True,
    )
    subprocess.run(
        [
            "python3",
            "tools/mvp/ct_mvp.py",
            "bundle",
            "--image-name",
            f"{name}:{version}",
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
        ],
        check=True,
    )

    if args.attach:
        subprocess.run(
            [
                "tools/mvp/publish_bundle.sh",
                f"{name}:{version}",
                os.path.join(args.out_dir, "bundle.json"),
                "application/vnd.verified-container.bundle+json",
            ],
            check=True,
        )

    summary = {
        "name": name,
        "version": version,
        "imageDigest": image_digest,
        "rootfs": os.path.join(args.out_dir, "rootfs"),
        "ociLayout": os.path.join(args.out_dir, "oci"),
        "bundle": os.path.join(args.out_dir, "bundle.json"),
        "trustStore": os.path.join(args.out_dir, "trust-store.json"),
    }
    with open(os.path.join(args.out_dir, "summary.json"), "w", encoding="utf-8") as handle:
        json.dump(summary, handle, indent=2)


if __name__ == "__main__":
    main()
