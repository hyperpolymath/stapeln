#!/usr/bin/env python3
# SPDX-License-Identifier: PMPL-1.0 OR PMPL-1.0-or-later
import json
import os
from http.server import BaseHTTPRequestHandler, HTTPServer


class Handler(BaseHTTPRequestHandler):
    def _send(self, status, payload):
        body = json.dumps(payload, indent=2).encode("utf-8")
        self.send_response(status)
        self.send_header("content-type", "application/json")
        self.send_header("content-length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        if self.path == "/healthz":
            return self._send(200, {"status": "ok"})
        if self.path == "/v1/images":
            source = os.environ.get("CT_PLUGIN_IMAGES", "tools/mvp/images.json")
            try:
                with open(source, "r", encoding="utf-8") as handle:
                    payload = json.load(handle)
            except FileNotFoundError:
                payload = {"images": []}
            return self._send(200, payload)
        return self._send(404, {"error": "not found"})


def main():
    host = os.environ.get("CT_PLUGIN_HOST", "0.0.0.0")
    port = int(os.environ.get("CT_PLUGIN_PORT", "8081"))
    httpd = HTTPServer((host, port), Handler)
    print(f"ct_plugin listening on http://{host}:{port}")
    httpd.serve_forever()


if __name__ == "__main__":
    main()
