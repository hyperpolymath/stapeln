#!/usr/bin/env -S deno run --allow-net --allow-read --allow-env
// SPDX-License-Identifier: PMPL-1.0-or-later
// dev-server.ts - Deno development server with hot reload

import { serve } from "https://deno.land/std@0.224.0/http/server.ts";
import { serveFile } from "https://deno.land/std@0.224.0/http/file_server.ts";
import { extname } from "https://deno.land/std@0.224.0/path/mod.ts";

const PORT = 8080;
const ROOT_DIR = Deno.cwd();

// MIME types
const MIME_TYPES: Record<string, string> = {
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

// Get MIME type from file extension
function getMimeType(path: string): string {
  const ext = extname(path);
  return MIME_TYPES[ext] || "application/octet-stream";
}

// Hot reload script injection
const HOT_RELOAD_SCRIPT = `
<script>
  // Hot reload WebSocket client
  (function() {
    const ws = new WebSocket('ws://localhost:${PORT}/ws');
    ws.onmessage = (event) => {
      if (event.data === 'reload') {
        console.log('🔄 Hot reload triggered - reloading page...');
        window.location.reload();
      }
    };
    ws.onerror = () => console.log('⚠️  Hot reload disconnected');
    ws.onclose = () => {
      console.log('🔌 Hot reload connection closed - reconnecting in 1s...');
      setTimeout(() => window.location.reload(), 1000);
    };
    console.log('✓ Hot reload enabled');
  })();
</script>
`;

// Active WebSocket connections for hot reload
const wsClients = new Set<WebSocket>();

// File watcher for hot reload
async function watchFiles() {
  const watcher = Deno.watchFs([
    `${ROOT_DIR}/src`,
    `${ROOT_DIR}/lib`,
    `${ROOT_DIR}/index.html`,
  ]);

  console.log("👁️  Watching for file changes...");

  for await (const event of watcher) {
    if (event.kind === "modify" || event.kind === "create") {
      const paths = event.paths.map((p) => p.replace(ROOT_DIR, ""));
      console.log(`🔄 File changed: ${paths.join(", ")}`);

      // Notify all connected clients
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

// Request handler
async function handler(req: Request): Promise<Response> {
  const url = new URL(req.url);
  let pathname = url.pathname;

  // Handle WebSocket upgrade for hot reload
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

  // Serve index.html for root
  if (pathname === "/") {
    pathname = "/index.html";
  }

  // Construct file path
  const filePath = `${ROOT_DIR}${pathname}`;

  try {
    // Check if file exists
    const fileInfo = await Deno.stat(filePath);

    if (fileInfo.isFile) {
      // Serve file
      const response = await serveFile(req, filePath);

      // Inject hot reload script into HTML files
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

      // Add no-cache headers for development
      const headers = new Headers(response.headers);
      headers.set("cache-control", "no-cache");

      return new Response(response.body, {
        status: response.status,
        headers,
      });
    }
  } catch (err) {
    // File not found
    if (err instanceof Deno.errors.NotFound) {
      // Try to serve index.html for SPA routing
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

// Start server
console.log(`
╔════════════════════════════════════════════════════════════╗
║                                                            ║
║  🚀 stapeln Development Server                             ║
║  ⚡ Powered by Deno                                        ║
║                                                            ║
╚════════════════════════════════════════════════════════════╝

📍 Server running at: http://localhost:${PORT}
👁️  Hot reload: ENABLED
🔄 File watcher: ACTIVE

Features:
  ✓ Automatic page reload on file changes
  ✓ SPA routing fallback
  ✓ No-cache headers for development
  ✓ WebSocket-based hot reload

Press Ctrl+C to stop the server
`);

// Start file watcher in background
watchFiles().catch((err) => {
  console.error("File watcher error:", err);
});

// Start HTTP server
await serve(handler, { port: PORT });
