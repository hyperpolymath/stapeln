// SPDX-License-Identifier: PMPL-1.0-or-later
// server.ts - Deno development server

import { serve } from "https://deno.land/std@0.224.0/http/server.ts";
import { serveDir } from "https://deno.land/std@0.224.0/http/file_server.ts";

const PORT = 8000;

console.log(`🏔️ stapeln development server`);
console.log(`Listening on http://localhost:${PORT}`);

serve(
  (req) => {
    const url = new URL(req.url);

    // Serve static files
    return serveDir(req, {
      fsRoot: ".",
      urlRoot: "",
      enableCors: true,
    });
  },
  { port: PORT }
);
