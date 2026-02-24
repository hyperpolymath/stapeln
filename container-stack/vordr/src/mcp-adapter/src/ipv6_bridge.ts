// Vörðr MCP IPv6 Bridge
import { handler } from "./HttpMain.res.js";

const listener = Deno.listen({ port: 8081, hostname: "::" });
console.log("🛡️ Vörðr Guardian (IPv6) listening on [::]:8081");

for await (const conn of listener) {
  serveHttp(conn);
}

async function serveHttp(conn: Deno.Conn) {
  const httpConn = Deno.serveHttp(conn);
  for await (const requestEvent of httpConn) {
    const response = await handler(requestEvent.request);
    requestEvent.respondWith(response);
  }
}
