#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0 OR PMPL-1.0-or-later
set -euo pipefail

if [[ $# -lt 3 ]]; then
  echo "usage: publish_bundle.sh <image-ref> <bundle.json> <artifact-type>" >&2
  exit 2
fi

image_ref="$1"
bundle_path="$2"
artifact_type="$3"

if ! command -v oras >/dev/null 2>&1; then
  echo "oras is required to attach bundle via OCI referrers" >&2
  exit 1
fi

oras attach \
  --artifact-type "${artifact_type}" \
  "${image_ref}" \
  "${bundle_path}:application/vnd.verified-container.bundle+json"
