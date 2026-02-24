// Vörðr MCP Bootstrap
// Robust Deno script to start the ReScript MCP server

import { handler } from "./HttpMain.res.js";

const PORT = 8081;
const HOST = "::";

console.log(`🚀 Bootstrapping Vörðr on http://[${HOST}]:${PORT}`);

Deno.serve({
  port: PORT,
  hostname: HOST,
  handler: handler,
  onListen({ port, hostname }) {
    console.log(`✅ Vörðr is active and guarding on ${hostname}:${port}`);
  },
});
