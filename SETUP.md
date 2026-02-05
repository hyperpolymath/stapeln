# stapeln Setup Guide

## Installation (No npm/Node.js!)

stapeln follows hyperpolymath language policy: **Deno only, no npm/Node.js**.

### Prerequisites

1. **Deno 2.0+**
```bash
# Install Deno (if not already installed)
curl -fsSL https://deno.land/install.sh | sh

# Add to PATH (add to ~/.bashrc)
export PATH="$HOME/.deno/bin:$PATH"
```

2. **ReScript Compiler via Deno**
```bash
# Install ReScript using Deno's npm compatibility
deno install --allow-read --allow-write --allow-env --allow-run \
  -n rescript \
  npm:rescript@11

# Verify installation
rescript -version
```

3. **Elixir + Phoenix (for backend)**
```bash
# Fedora
sudo dnf install elixir

# Install Phoenix
mix archive.install hex phx_new
```

4. **Idris2 (for formal proofs)**
```bash
# Fedora
sudo dnf install idris2
```

5. **Rust (for codegen NIFs)**
```bash
# Already installed on your system
rustc --version
```

## Frontend Setup (ReScript + Deno)

```bash
cd ~/Documents/hyperpolymath-repos/stapeln/frontend

# Compile ReScript to JavaScript
rescript build

# Start Deno dev server
deno task dev

# Open browser
xdg-open http://localhost:8000
```

### Development Workflow

```bash
# Watch mode (recompile on save)
rescript build -w &

# In another terminal, run dev server
deno task dev
```

### Deno Permissions

stapeln requires these Deno permissions:
- `--allow-read` - Read frontend files
- `--allow-write` - Write compiled output
- `--allow-net` - HTTP server
- `--allow-env` - Environment variables

These are declared in `deno.json` tasks.

## Backend Setup (Elixir + Phoenix)

```bash
cd ~/Documents/hyperpolymath-repos/stapeln/backend

# Create Phoenix project (if not exists)
mix phx.new . --app stapeln --no-html --no-webpack --binary-id

# Install dependencies
mix deps.get

# Create database
mix ecto.create

# Run migrations
mix ecto.migrate

# Start server
mix phx.server

# API available at http://localhost:4000
```

## Validation Engine (Idris2)

```bash
cd ~/Documents/hyperpolymath-repos/stapeln/validation

# Compile Idris2 proofs
idris2 --build validation.ipkg

# Run tests
idris2 --repl src/Proofs.idr
```

## Tauri Desktop App (Future)

```bash
# Install Tauri CLI
cargo install tauri-cli

# Development build
cargo tauri dev

# Production build
cargo tauri build
```

## Troubleshooting

### "rescript: command not found"

The Deno install didn't add to PATH. Add to `~/.bashrc`:
```bash
export PATH="$HOME/.deno/bin:$PATH"
```

### "Cannot find module 'npm:@rescript/core'"

Deno's npm compatibility needs the import map. Check `frontend/import_map.json`:
```json
{
  "imports": {
    "@rescript/core": "https://esm.sh/@rescript/core@1.0.0"
  }
}
```

### ReScript compilation errors

Check `frontend/rescript.json` for correct configuration:
- `"module": "es6"`
- `"suffix": ".res.js"`
- `"in-source": true`

## Environment Variables

Create `frontend/.env`:
```bash
# Backend API
STAPELN_API_URL=http://localhost:4000

# GraphQL endpoint
STAPELN_GRAPHQL_URL=http://localhost:4000/graphql

# WebSocket
STAPELN_WS_URL=ws://localhost:4000/socket
```

## Editor Setup

### VSCode
```bash
# Install ReScript VSCode extension
code --install-extension chenglou92.rescript-vscode
```

### Vim/Neovim
```vim
" Install rescript-vim plugin
Plug 'rescript-lang/vim-rescript'
```

## Testing

### Frontend Tests
```bash
cd frontend
deno test --allow-read --allow-env
```

### Backend Tests
```bash
cd backend
mix test
```

## Production Build

### Frontend
```bash
cd frontend
rescript build
deno bundle src/Main.res.js dist/bundle.js
```

### Backend
```bash
cd backend
MIX_ENV=prod mix release
```

## No npm/Node.js Required! ✅

stapeln is 100% Deno-based for JavaScript runtime. We use:
- ✅ Deno for runtime
- ✅ Deno's npm: specifier for ReScript compiler
- ✅ Deno's import maps for dependencies
- ❌ No package.json
- ❌ No node_modules/
- ❌ No npm install

This follows hyperpolymath standards and ensures security-first execution.
