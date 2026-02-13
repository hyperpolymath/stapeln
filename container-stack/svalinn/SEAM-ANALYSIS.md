# Svalinn Integration Seam Analysis

## Module Inventory

### Newly Implemented (Phase 2 - ReScript)

#### Authentication (auth/)
- ✅ **Types.res** - Auth types, RBAC roles (270 lines)
- ✅ **JWT.res** - JWT verification, JWKS caching (370+ lines)
- ✅ **OAuth2.res** - OAuth2 flows (230+ lines)
- ✅ **Middleware.res** - Hono auth middleware (430+ lines)

#### Bindings (bindings/)
- ✅ **Hono.res** - Hono HTTP framework bindings (90 lines)
- ✅ **Deno.res** - Deno runtime bindings (existing)
- ✅ **Fetch.res** - Fetch API bindings (existing)

#### Gateway (gateway/)
- ✅ **Gateway.res** - NEW: Complete HTTP server with Hono (400+ lines)
- ⚠️ **Handlers.res** - EXISTING: Old handler stubs referencing VordrClient
- ⚠️ **Server.res** - EXISTING: Old server referencing Handlers
- ⚠️ **Types.res** - EXISTING: Old type definitions

#### MCP Client (mcp/)
- ✅ **McpClient.res** - NEW: Vörðr MCP client (330+ lines)
- ⚠️ **Server.res** - EXISTING: MCP server for edge tools
- ⚠️ **Tools.res** - EXISTING: MCP tool definitions
- ⚠️ **Types.res** - EXISTING: MCP protocol types

#### Policy (policy/)
- ✅ **PolicyEngine.res** - NEW: Gatekeeper policy evaluation (330+ lines)

#### Validation (validation/)
- ✅ **Validation.res** - NEW: JSON Schema validation (230+ lines)
- ⚠️ **Schema.res** - EXISTING: Old schema loader

### Legacy TypeScript (to be replaced)
- auth/*.ts - OAuth2/JWT in TypeScript (should delete after ReScript works)
- gateway/*.ts - Gateway stubs (should delete)

## Seam Issues Identified

### 1. **Gateway Duplication**
**Problem:** Two gateway implementations exist
- **Gateway.res** (NEW) - Complete Hono-based server with auth
- **Server.res** (OLD) - References non-existent Handlers module

**Resolution:** Use Gateway.res, deprecate Server.res and Handlers.res

### 2. **MCP Client Naming Conflict**
**Problem:** Handlers.res expects `VordrClient` module
- References: `@module("../vordr/Client.res.mjs")`
- But we created: `McpClient.res` in mcp/ directory

**Resolution:** Either:
- A) Rename McpClient → VordrClient
- B) Update Handlers.res to use McpClient (RECOMMENDED)

### 3. **Missing vordr/ Directory**
**Problem:** Handlers.res imports from `../vordr/Client.res.mjs`
- No vordr/ directory exists
- Should use mcp/McpClient.res instead

**Resolution:** Create vordr/Client.res as re-export of McpClient

### 4. **Gateway Types Mismatch**
**Problem:** gateway/Types.res defines types that overlap with:
- gateway/Gateway.res (has its own Config module)
- Validation schemas (JSON schema types)

**Resolution:** Consolidate types into gateway/Types.res

### 5. **Validation Module Split**
**Problem:** Two validation files:
- Validation.res (NEW) - Complete Ajv-based validator
- Schema.res (OLD) - Old schema loader

**Resolution:** Merge or use only Validation.res

## Integration Gaps

### Missing Connections

1. **Gateway → McpClient**
   - Gateway.res has stub routes (501 Not Implemented)
   - Need to wire routes to McpClient calls

2. **Gateway → Validation**
   - No request validation in Gateway routes
   - Need to call Validation.validateRunRequest etc.

3. **Gateway → PolicyEngine**
   - No policy enforcement in Gateway
   - Need to evaluate policies before allowing operations

4. **Auth → Gateway**
   - Auth middleware integrated but not tested
   - Need to verify scope/group checks work

5. **McpClient → Vörðr**
   - MCP client implemented but not tested against real Vörðr
   - Need integration test with Vörðr MCP server

## Smoothing Plan

### Phase 1: Consolidation (Remove Duplicates)
1. Delete or rename conflicting files
2. Consolidate type definitions
3. Create compatibility shims where needed

### Phase 2: Wiring (Connect Modules)
1. Wire Gateway routes to McpClient
2. Add validation to request handlers
3. Add policy enforcement to verify/run operations
4. Test auth middleware integration

### Phase 3: Testing (Integration Tests)
1. Unit tests for each module
2. Integration tests for Gateway ↔ McpClient
3. End-to-end tests with mock Vörðr
4. Authentication flow tests

### Phase 4: Polish (Error Handling & Logging)
1. Consistent error responses
2. Structured logging throughout
3. Metrics collection
4. Documentation

## Recommendation: File Actions

### Keep (Production Code)
- ✅ auth/*.res (all 4 files)
- ✅ bindings/Hono.res
- ✅ gateway/Gateway.res
- ✅ mcp/McpClient.res
- ✅ policy/PolicyEngine.res
- ✅ validation/Validation.res

### Refactor/Integrate
- ⚠️ gateway/Types.res → Merge into Gateway.res or use as shared types
- ⚠️ mcp/Types.res → Keep for MCP protocol definitions

### Deprecate/Delete
- ❌ gateway/Handlers.res → Functionality moved to Gateway.res
- ❌ gateway/Server.res → Replaced by Gateway.res
- ❌ validation/Schema.res → Replaced by Validation.res
- ❌ mcp/Server.res → Not needed for client-only implementation
- ❌ mcp/Tools.res → Not needed for client-only implementation
- ❌ auth/*.ts → TypeScript originals (keep until ReScript verified)

## Next Steps

1. Create integration test suite
2. Wire Gateway.res routes to McpClient
3. Add validation and policy enforcement
4. Test auth middleware
5. Clean up deprecated files
6. Document final architecture
