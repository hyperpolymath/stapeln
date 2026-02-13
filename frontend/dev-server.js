#!/usr/bin/env -S deno run --allow-net --allow-read --allow-env
// SPDX-License-Identifier: PMPL-1.0-or-later
// dev-server.js - Deno development server with hot reload

import { serve } from "https://deno.land/std@0.224.0/http/server.ts";
import { serveFile } from "https://deno.land/std@0.224.0/http/file_server.ts";
import { extname } from "https://deno.land/std@0.224.0/path/mod.ts";

const PORT = 8080;
const ROOT_DIR = Deno.cwd();

const MIME_TYPES = {
  ".html": "text/html",
  ".js": "application/javascript",
  ".mjs": "application/javascript",
  ".css": "text/css",
  ".json": "application/json",
  ".png": "image/png",
  ".jpg": "image/jpeg",
  ".jpeg": "image/jpeg",
  ".gif": "image/gif",
  ".svg": "image/svg+xml",
  ".ico": "image/x-icon",
  ".wasm": "application/wasm",
};

function getMimeType(path) {
  const ext = extname(path);
  return MIME_TYPES[ext] || "application/octet-stream";
}

const HOT_RELOAD_SCRIPT = `
<script>
  (function() {
    const ws = new WebSocket('ws://localhost:${PORT}/ws');
    ws.onmessage = (event) => {
      if (event.data === 'reload') {
        console.log('Hot reload triggered');
        window.location.reload();
      }
    };
    ws.onerror = () => console.log('Hot reload disconnected');
    ws.onclose = () => {
      console.log('Hot reload connection closed - reconnecting in 1s...');
      setTimeout(() => window.location.reload(), 1000);
    };
    console.log('Hot reload enabled');
  })();
</script>
`;

const wsClients = new Set();

async function watchFiles() {
  const watcher = Deno.watchFs([
    `${ROOT_DIR}/src`,
    `${ROOT_DIR}/lib`,
    `${ROOT_DIR}/index.html`,
  ]);

  console.log("Watching for file changes...");

  for await (const event of watcher) {
    if (event.kind === "modify" || event.kind === "create") {
      const paths = event.paths.map((p) => p.replace(ROOT_DIR, ""));
      console.log(`File changed: ${paths.join(", ")}`);

      wsClients.forEach((client) => {
        try {
          client.send("reload");
        } catch (err) {
          console.error("Failed to send reload signal:", err);
          wsClients.delete(client);
        }
      });
    }
  }
}

async function handler(req) {
  const url = new URL(req.url);
  let pathname = url.pathname;

  if (pathname === "/ws") {
    const upgrade = req.headers.get("upgrade") || "";
    if (upgrade.toLowerCase() !== "websocket") {
      return new Response("Expected websocket", { status: 400 });
    }

    const { socket, response } = Deno.upgradeWebSocket(req);
    wsClients.add(socket);

    socket.onclose = () => {
      wsClients.delete(socket);
    };

    socket.onerror = () => {
      wsClients.delete(socket);
    };

    return response;
  }

  if (pathname === "/") {
    pathname = "/index.html";
  }

  const filePath = `${ROOT_DIR}${pathname}`;

  try {
    const fileInfo = await Deno.stat(filePath);

    if (fileInfo.isFile) {
      const response = await serveFile(req, filePath);

      if (pathname.endsWith(".html")) {
        const content = await Deno.readTextFile(filePath);
        const withHotReload = content.replace(
          "</body>",
          `${HOT_RELOAD_SCRIPT}</body>`
        );

        return new Response(withHotReload, {
          status: 200,
          headers: {
            "content-type": "text/html; charset=utf-8",
            "cache-control": "no-cache",
          },
        });
      }

      const headers = new Headers(response.headers);
      headers.set("cache-control", "no-cache");

      return new Response(response.body, {
        status: response.status,
        headers,
      });
    }
  } catch (err) {
    if (err instanceof Deno.errors.NotFound) {
      try {
        const indexPath = `${ROOT_DIR}/index.html`;
        const content = await Deno.readTextFile(indexPath);
        const withHotReload = content.replace(
          "</body>",
          `${HOT_RELOAD_SCRIPT}</body>`
        );

        return new Response(withHotReload, {
          status: 200,
          headers: {
            "content-type": "text/html; charset=utf-8",
            "cache-control": "no-cache",
          },
        });
      } catch {
        return new Response("404 Not Found", { status: 404 });
      }
    }

    console.error("Error serving file:", err);
    return new Response("500 Internal Server Error", { status: 500 });
  }

  return new Response("404 Not Found", { status: 404 });
}

console.log(`stapeln dev server running at http://localhost:${PORT}`);
console.log("Hot reload: ENABLED");

watchFiles().catch((err) => {
  console.error("File watcher error:", err);
});

await serve(handler, { port: PORT });
