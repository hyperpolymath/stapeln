// SPDX-License-Identifier: PMPL-1.0-or-later
// Vörðr MCP HTTP Adapter - Deno TypeScript implementation
// LSP-style architecture: HTTP transport wrapping MCP protocol

const PORT = 8080;
const HOST = "0.0.0.0";

// JSON-RPC types
interface JsonRpcRequest {
  jsonrpc: string;
  id?: number | string | null;
  method: string;
  params?: unknown;
}

interface JsonRpcResponse {
  jsonrpc: string;
  id?: number | string | null;
  result?: unknown;
  error?: {
    code: number;
    message: string;
    data?: unknown;
  };
}

// MCP Protocol Handler
class McpProtocol {
  private initialized = false;

  handleRequest(request: JsonRpcRequest): JsonRpcResponse {
    console.log(`→ ${request.method}`, request.params || "");

    switch (request.method) {
      case "initialize":
        return this.handleInitialize(request);
      case "initialized":
        return this.success(request.id, {});
      case "tools/list":
        return this.handleToolsList(request);
      case "tools/call":
        return this.handleToolsCall(request);
      case "ping":
        return this.success(request.id, { pong: true });

      // Direct tool calls (Svalinn compatibility)
      case "containers/list":
      case "containers/create":
      case "containers/start":
      case "containers/stop":
      case "containers/get":
      case "containers/remove":
      case "containers/logs":
      case "containers/exec":
      case "images/list":
      case "images/pull":
      case "images/remove":
      case "images/verify":
      case "health":
      case "version":
        return this.handleDirectToolCall(request);

      default:
        return this.error(request.id, -32601, `Method not found: ${request.method}`);
    }
  }

  private handleDirectToolCall(request: JsonRpcRequest): JsonRpcResponse {
    const toolName = request.method;
    const params = request.params as Record<string, unknown>;

    console.log(`  ⚙️  Direct tool call: ${toolName}`);

    // Mock implementation - will integrate with Elixir orchestrator
    const result = {
      success: true,
      tool: toolName,
      params: params,
      message: `Tool ${toolName} executed successfully (mock response)`,
    };

    return this.success(request.id, result);
  }

  private handleInitialize(request: JsonRpcRequest): JsonRpcResponse {
    this.initialized = true;
    return this.success(request.id, {
      protocolVersion: "2024-11-05",
      capabilities: {
        tools: { listChanged: false },
        resources: { listChanged: false },
      },
      serverInfo: {
        name: "vordr-mcp-adapter",
        version: "0.1.0",
      },
    });
  }

  private handleToolsList(request: JsonRpcRequest): JsonRpcResponse {
    const tools = [
      {
        name: "containers/list",
        description: "List all containers",
        inputSchema: { type: "object", properties: {} },
      },
      {
        name: "containers/create",
        description: "Create a container",
        inputSchema: {
          type: "object",
          properties: {
            image: { type: "string" },
            name: { type: "string" },
          },
          required: ["image"],
        },
      },
      {
        name: "containers/start",
        description: "Start a container",
        inputSchema: {
          type: "object",
          properties: { id: { type: "string" } },
          required: ["id"],
        },
      },
      {
        name: "containers/stop",
        description: "Stop a container",
        inputSchema: {
          type: "object",
          properties: { id: { type: "string" } },
          required: ["id"],
        },
      },
      {
        name: "images/verify",
        description: "Verify a container image (.ctp bundle)",
        inputSchema: {
          type: "object",
          properties: {
            digest: { type: "string" },
            policy: { type: "object" },
          },
          required: ["digest"],
        },
      },
    ];

    return this.success(request.id, { tools });
  }

  private handleToolsCall(request: JsonRpcRequest): JsonRpcResponse {
    const params = request.params as { name?: string; arguments?: unknown };
    const toolName = params?.name;

    if (!toolName) {
      return this.error(request.id, -32602, "Missing tool name");
    }

    console.log(`  ⚙️  Calling tool: ${toolName}`);

    // Mock responses for now - will integrate with Elixir orchestrator
    const result = {
      content: [
        {
          type: "text",
          text: `Tool ${toolName} executed successfully (mock)`,
        },
      ],
    };

    return this.success(request.id, result);
  }

  private success(id: number | string | null | undefined, result: unknown): JsonRpcResponse {
    return { jsonrpc: "2.0", id, result };
  }

  private error(
    id: number | string | null | undefined,
    code: number,
    message: string,
    data?: unknown
  ): JsonRpcResponse {
    return { jsonrpc: "2.0", id, error: { code, message, data } };
  }
}

// HTTP Server
const mcp = new McpProtocol();

const corsHeaders = {
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Methods": "POST, OPTIONS",
  "Access-Control-Allow-Headers": "Content-Type",
  "Content-Type": "application/json",
};

async function handler(req: Request): Promise<Response> {
  // CORS preflight
  if (req.method === "OPTIONS") {
    return new Response("", { status: 200, headers: corsHeaders });
  }

  // Only accept POST
  if (req.method !== "POST") {
    return new Response("Method not allowed", { status: 405, headers: corsHeaders });
  }

  try {
    // Parse JSON-RPC request
    const body = await req.text();
    const request = JSON.parse(body) as JsonRpcRequest;

    // Validate JSON-RPC
    if (request.jsonrpc !== "2.0") {
      const error = mcp["error"](request.id, -32600, "Invalid JSON-RPC version");
      return new Response(JSON.stringify(error), { status: 200, headers: corsHeaders });
    }

    // Handle request through MCP protocol
    const response = mcp.handleRequest(request);

    return new Response(JSON.stringify(response), { status: 200, headers: corsHeaders });
  } catch (error) {
    console.error("Error:", error);
    const errorResponse = {
      jsonrpc: "2.0",
      id: null,
      error: {
        code: -32700,
        message: "Parse error",
        data: error instanceof Error ? error.message : String(error),
      },
    };
    return new Response(JSON.stringify(errorResponse), { status: 500, headers: corsHeaders });
  }
}

// Start server
console.log("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
console.log("  Vörðr MCP Adapter v0.1.0");
console.log("  Transport: HTTP (LSP-style)");
console.log(`  Listening: http://${HOST}:${PORT}`);
console.log("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

Deno.serve({ port: PORT, hostname: HOST }, handler);
