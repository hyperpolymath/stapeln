# stackur Implementation Plan

## Phase 1: Frontend Foundation (Week 1-2)

### 1.1 ReScript-TEA Setup
- [x] Create ReScript project structure
- [x] Configure Deno runtime
- [x] Set up import maps for dependencies
- [ ] Install/vendor cadre-tea-router
- [ ] Create TEA Model (State management)
- [ ] Create TEA Msg (Events)
- [ ] Create TEA Update (State transitions)
- [ ] Create TEA View (UI rendering)

### 1.2 Paragon-Style UI
- [x] Design component block layout (vertical stack)
- [x] Implement WCAG 2.3 AAA color palette
- [x] Create dark/light mode toggle
- [ ] Detect system theme preference
- [ ] Implement keyboard navigation
- [ ] Add ARIA labels and roles
- [ ] Add Braille annotations
- [ ] Create semantic HTML + XML metadata

### 1.3 Component Library
- [ ] Create component palette sidebar
- [ ] Implement drag-from-palette
- [ ] Add component icons/visuals
- [ ] Create component configuration panel
- [ ] Support for all component types:
  - Cerro Torre
  - Svalinn
  - selur
  - Vörðr
  - Podman/Docker/nerdctl
  - Volumes
  - Networks

## Phase 2: Drag-and-Drop (Week 3)

### 2.1 Canvas Implementation
- [ ] SVG canvas with pan/zoom
- [ ] Grid snapping
- [ ] Component positioning
- [ ] Drag state management
- [ ] Drop zones
- [ ] Visual feedback (hover, active)

### 2.2 Connection Lines
- [ ] Click-to-connect mode
- [ ] Bezier curve rendering
- [ ] Connection validation (type compatibility)
- [ ] Connection removal
- [ ] Connection labels (data flow direction)

### 2.3 Accessibility
- [ ] Keyboard-only drag-and-drop
- [ ] Screen reader announcements for all actions
- [ ] Focus management
- [ ] Skip links
- [ ] Reduced motion support

## Phase 3: Backend API (Week 4-5)

### 3.1 Phoenix Setup
- [ ] Create Phoenix project
- [ ] Set up PostgreSQL database
- [ ] Create Ecto schemas (Stack, Component, Connection)
- [ ] GraphQL endpoint (Absinthe)
- [ ] WebSocket channel for real-time updates
- [ ] Authentication (JWT)

### 3.2 GraphQL Schema
- [x] Define types (Component, Stack, Connection)
- [x] Define queries (stack, stacks, validateStack)
- [x] Define mutations (createStack, addComponent, etc.)
- [x] Define subscriptions (stackUpdated, validationResult)
- [ ] Implement resolvers
- [ ] Add pagination
- [ ] Add filtering/sorting

### 3.3 WebSocket Channels
- [ ] Stack channel (real-time updates)
- [ ] Presence tracking (who's editing)
- [ ] Collaborative editing (OT or CRDT)
- [ ] Conflict resolution

## Phase 4: Validation Engine (Week 6-7)

### 4.1 Idris2 Proofs
- [ ] Create Idris2 project structure
- [ ] Define Stack type
- [ ] Prove: No circular dependencies (acyclic graph)
- [ ] Prove: Valid dependency ordering (topological sort)
- [ ] Prove: Type safety (component compatibility)
- [ ] Prove: Resource uniqueness (ports, volumes)
- [ ] Compile to executable
- [ ] Port integration from Elixir

### 4.2 Ephapax Linear Types (or Rust MVP)
- [ ] **Option A (Ephapax)**: Linear type validation
  - Volume consumed exactly once
  - Port bindings unique
  - Network scopes isolated
- [ ] **Option B (Rust MVP)**: Affine types
  - Rust struct with consume-once semantics
  - Port conflict detection
  - Resource constraint checking

### 4.3 Validation API
- [ ] REST endpoint: POST /api/stacks/:id/validate
- [ ] GraphQL mutation: validateStack
- [ ] Async validation (Task queue)
- [ ] Caching validation results
- [ ] Incremental validation (only changed components)

## Phase 5: Code Generation (Week 8)

### 5.1 Rust Codegen NIF
- [ ] Create Rust crate (stackur_codegen)
- [ ] Elixir NIF wrapper (Rustler)
- [ ] Parse Stack JSON
- [ ] Generate compose.toml (TOML library)
- [ ] Generate docker-compose.yml (YAML library)
- [ ] Generate podman-compose.yml
- [ ] Validate against verified-container-spec schemas

### 5.2 Export Functionality
- [ ] REST endpoint: POST /api/stacks/:id/export
- [ ] GraphQL mutation: exportStack
- [ ] Format selection (selur/docker/podman)
- [ ] File download (streaming for large files)
- [ ] Pretty-printing
- [ ] Comments with metadata

## Phase 6: Runtime Integration (Week 9-10)

### 6.1 Podman API Client
- [ ] HTTP client (Tesla/Req)
- [ ] Authentication
- [ ] Container operations (create, start, stop)
- [ ] Image operations (pull, push)
- [ ] Network operations
- [ ] Volume operations
- [ ] Logs streaming

### 6.2 Docker API Client
- [ ] HTTP client
- [ ] Docker socket connection
- [ ] Same operations as Podman

### 6.3 nerdctl CLI Wrapper
- [ ] Command builder
- [ ] Process spawning (Erlang ports)
- [ ] Output parsing
- [ ] Error handling

### 6.4 Live Deployment
- [ ] "Deploy" button in UI
- [ ] Progress tracking
- [ ] Rollback on failure
- [ ] Health checks
- [ ] Logs viewer

## Phase 7: Testing & QA (Week 11)

### 7.1 Accessibility Testing
- [ ] Automated: axe-core, Lighthouse
- [ ] Manual: NVDA, JAWS, VoiceOver, TalkBack
- [ ] Keyboard-only testing
- [ ] Braille display testing
- [ ] Screen magnification testing (400% zoom)
- [ ] Color blindness simulation

### 7.2 Unit Testing
- [ ] ReScript tests (Tea.Test)
- [ ] Elixir tests (ExUnit)
- [ ] Idris2 tests
- [ ] Rust tests (Cargo test)

### 7.3 Integration Testing
- [ ] Frontend <-> Backend API tests
- [ ] Validation engine tests
- [ ] Codegen tests (verify output files)
- [ ] Runtime adapter tests

### 7.4 End-to-End Testing
- [ ] Playwright/Cypress tests
- [ ] Full user workflows
- [ ] Accessibility automation
- [ ] Performance benchmarks

## Phase 8: Documentation (Week 12)

### 8.1 User Documentation
- [ ] Getting started guide
- [ ] Component reference
- [ ] Keyboard shortcuts
- [ ] Accessibility features
- [ ] Troubleshooting

### 8.2 Developer Documentation
- [ ] Architecture overview
- [ ] API reference
- [ ] GraphQL schema docs
- [ ] Contributing guide
- [ ] Code of conduct

### 8.3 Video Tutorials
- [ ] Basic usage
- [ ] Advanced features
- [ ] Accessibility features demo

## Technology Stack Summary

| Layer | Technology | Purpose |
|-------|-----------|---------|
| **Frontend** | ReScript-TEA | UI state management (Elm Architecture) |
| | Deno | Secure JavaScript runtime |
| | cadre-tea-router | Routing for TEA |
| | SVG | Canvas for drag-and-drop |
| **Backend** | Elixir + Phoenix | API server, WebSocket |
| | Absinthe | GraphQL implementation |
| | PostgreSQL | Data persistence |
| **Validation** | Idris2 | Formal proofs (dependency graphs) |
| | Ephapax (or Rust) | Linear type checking |
| **Codegen** | Rust (NIF) | Generate compose files |
| **Runtime** | Podman API | Container operations |
| | Docker API | Container operations |
| | nerdctl CLI | Container operations |

## Milestones

- **M1 (Week 2)**: Frontend prototype (static UI, no backend)
- **M2 (Week 3)**: Drag-and-drop working
- **M3 (Week 5)**: Backend API + GraphQL
- **M4 (Week 7)**: Validation engine integrated
- **M5 (Week 8)**: Code generation working
- **M6 (Week 10)**: Runtime integration complete
- **M7 (Week 11)**: All tests passing
- **M8 (Week 12)**: Documentation complete
- **MVP (Week 12)**: Public release

## Success Metrics

- [ ] WCAG 2.3 AAA compliance (100%)
- [ ] Lighthouse accessibility score: 100/100
- [ ] Test coverage: >90%
- [ ] GraphQL API response time: <100ms (p95)
- [ ] Validation latency: <500ms (p95)
- [ ] Codegen latency: <1s (p95)
- [ ] Support for 100+ component stacks
- [ ] Zero keyboard navigation blockers
- [ ] Zero screen reader issues

## Risks & Mitigations

| Risk | Mitigation |
|------|------------|
| Ephapax not production-ready | Use Rust affine types as MVP |
| AffineScript compiler unavailable | Implement constraint checking in Rust |
| cadre-tea-router not on JSR/npm | Vendor locally or fork |
| Idris2 compilation slow | Cache validation results aggressively |
| WebSocket scaling issues | Use Phoenix Presence + PubSub |
| Braille testing requires hardware | Partner with accessibility org for testing |

## Next Steps

1. **Immediate**: Set up ReScript + Deno development environment
2. **This week**: Complete Phase 1 (Frontend foundation)
3. **Next week**: Implement drag-and-drop (Phase 2)
4. **Two weeks**: Backend setup (Phase 3)

---

**Status**: 📍 Phase 1 in progress (10% complete)
**Last updated**: 2026-02-05
