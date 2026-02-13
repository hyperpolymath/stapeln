# Vörðr CLI Reference

Complete reference for all 16 Vörðr CLI commands.

---

## Global Flags

```bash
vordr [FLAGS] [COMMAND]
```

**Flags:**
- `-v, --verbose` - Enable verbose output
- `--db-path <PATH>` - Path to state database (default: `/var/lib/vordr/vordr.db`)
- `--runtime <RUNTIME>` - Container runtime (default: `youki`)
- `--root <PATH>` - Root directory for container state (default: `/var/lib/vordr`)
- `--help` - Print help information
- `--version` - Print version information

**Environment Variables:**
- `VORDR_DB` - Overrides `--db-path`
- `VORDR_RUNTIME` - Overrides `--runtime`
- `VORDR_ROOT` - Overrides `--root`

---

## Container Commands

### `run` - Run a container

```bash
vordr run [OPTIONS] IMAGE [COMMAND] [ARGS...]
```

**Options:**
- `-d, --detach` - Run container in background
- `--name <NAME>` - Assign a name to the container
- `-e, --env <KEY=VALUE>` - Set environment variables
- `-v, --volume <SRC:DEST>` - Bind mount a volume
- `-p, --publish <HOST:CONTAINER>` - Publish container port
- `--network <NETWORK>` - Connect to a network
- `--rm` - Automatically remove container when it exits

**Examples:**
```bash
# Run interactive shell
vordr run -it alpine:latest /bin/sh

# Run detached with name
vordr run -d --name web nginx:latest

# Run with volume and port mapping
vordr run -d -v /data:/app/data -p 8080:80 myapp:latest
```

---

### `exec` - Execute command in running container

```bash
vordr exec [OPTIONS] CONTAINER COMMAND [ARGS...]
```

**Options:**
- `-i, --interactive` - Keep STDIN open
- `-t, --tty` - Allocate a pseudo-TTY
- `-e, --env <KEY=VALUE>` - Set environment variables
- `-w, --workdir <DIR>` - Working directory

**Examples:**
```bash
# Run shell in container
vordr exec -it mycontainer /bin/sh

# Run command as specific user
vordr exec --user 1000 mycontainer whoami
```

---

### `ps` - List containers

```bash
vordr ps [OPTIONS]
```

**Options:**
- `-a, --all` - Show all containers (default shows running only)
- `-q, --quiet` - Only display container IDs
- `-n, --last <N>` - Show last N created containers
- `--no-trunc` - Don't truncate output

**Examples:**
```bash
# List running containers
vordr ps

# List all containers with IDs only
vordr ps -aq

# List last 5 containers
vordr ps -n 5
```

---

### `inspect` - Display detailed container information

```bash
vordr inspect CONTAINER [CONTAINER...]
```

**Output:** JSON-formatted container details

**Examples:**
```bash
# Inspect single container
vordr inspect mycontainer

# Inspect multiple containers
vordr inspect web db cache
```

---

### `start` - Start stopped container

```bash
vordr start CONTAINER [CONTAINER...]
```

**Examples:**
```bash
vordr start mycontainer
vordr start web db  # Start multiple
```

---

### `stop` - Stop running container

```bash
vordr stop [OPTIONS] CONTAINER [CONTAINER...]
```

**Options:**
- `-t, --timeout <SECONDS>` - Seconds to wait before killing (default: 10)

**Examples:**
```bash
vordr stop mycontainer
vordr stop --timeout 30 web  # Wait 30s before killing
```

---

### `rm` - Remove container

```bash
vordr rm [OPTIONS] CONTAINER [CONTAINER...]
```

**Options:**
- `-f, --force` - Force remove running container

**Examples:**
```bash
vordr rm mycontainer
vordr rm -f web  # Force remove even if running
```

---

## Image Commands

### `image ls` - List images

```bash
vordr image ls [OPTIONS]
```

**Options:**
- `-q, --quiet` - Only show image IDs
- `--no-trunc` - Don't truncate output

**Examples:**
```bash
vordr image ls
vordr image ls -q  # IDs only
```

---

### `image pull` - Pull image from registry

```bash
vordr image pull IMAGE[:TAG]
```

**Examples:**
```bash
vordr image pull alpine:latest
vordr image pull ghcr.io/myorg/myapp:v1.0
```

---

### `image rm` - Remove image

```bash
vordr image rm [OPTIONS] IMAGE [IMAGE...]
```

**Options:**
- `-f, --force` - Force removal

**Examples:**
```bash
vordr image rm alpine:latest
vordr image rm -f myapp:old  # Force remove
```

---

### `image inspect` - Display image details

```bash
vordr image inspect IMAGE [IMAGE...]
```

**Examples:**
```bash
vordr image inspect alpine:latest
```

---

### `pull` - Pull image (shorthand)

```bash
vordr pull IMAGE[:TAG]
```

Alias for `vordr image pull`.

---

## Network Commands

### `network ls` - List networks

```bash
vordr network ls [OPTIONS]
```

**Options:**
- `-q, --quiet` - Only show network IDs

---

### `network create` - Create network

```bash
vordr network create [OPTIONS] NETWORK
```

**Options:**
- `--driver <DRIVER>` - Network driver (default: bridge)
- `--subnet <CIDR>` - Subnet in CIDR format
- `--gateway <IP>` - Gateway address

**Examples:**
```bash
vordr network create mynet
vordr network create --subnet 172.20.0.0/16 --gateway 172.20.0.1 custom-net
```

---

### `network rm` - Remove network

```bash
vordr network rm NETWORK [NETWORK...]
```

---

### `network inspect` - Display network details

```bash
vordr network inspect NETWORK [NETWORK...]
```

---

## Volume Commands

### `volume ls` - List volumes

```bash
vordr volume ls [OPTIONS]
```

**Options:**
- `-q, --quiet` - Only show volume names

---

### `volume create` - Create volume

```bash
vordr volume create [OPTIONS] VOLUME
```

**Options:**
- `--driver <DRIVER>` - Volume driver (default: local)

---

### `volume rm` - Remove volume

```bash
vordr volume rm VOLUME [VOLUME...]
```

---

### `volume inspect` - Display volume details

```bash
vordr volume inspect VOLUME [VOLUME...]
```

---

## System Commands

### `info` - Display system information

```bash
vordr info
```

Shows:
- Runtime information
- Storage driver
- Container count
- Image count

---

### `version` - Show version

```bash
vordr --version
```

---

### `system df` - Show disk usage

```bash
vordr system df
```

Shows storage used by containers, images, and volumes.

---

### `system prune` - Remove unused data

```bash
vordr system prune [OPTIONS]
```

**Options:**
- `-a, --all` - Remove all unused images, not just dangling
- `-f, --force` - Do not prompt for confirmation

**Examples:**
```bash
vordr system prune  # Removes stopped containers, unused networks
vordr system prune -a  # Also removes unused images
```

---

## Monitoring Commands (eBPF)

### `monitor start` - Start eBPF monitoring

```bash
vordr monitor start [OPTIONS]
```

**Options:**
- `-c, --container <ID>` - Monitor specific containers
- `-p, --policy <POLICY>` - Security policy (strict, minimal-audit)
- `-w, --webhook <URL>` - Webhook URL for alerts
- `--sensitivity <0.0-1.0>` - Anomaly detection sensitivity (default: 0.8)
- `-d, --daemon` - Run in background

**Examples:**
```bash
# Monitor all containers with strict policy
vordr monitor start --policy strict

# Monitor specific container with alerts
vordr monitor start \
  --container myapp \
  --webhook https://alerts.example.com/hooks \
  --sensitivity 0.9
```

---

### `monitor stop` - Stop monitoring

```bash
vordr monitor stop
```

---

### `monitor status` - Show monitoring status

```bash
vordr monitor status
```

Shows:
- Status (running, stopped)
- Active policy
- Monitored containers
- Event counts

---

### `monitor events` - View captured events

```bash
vordr monitor events [OPTIONS]
```

**Options:**
- `-f, --follow` - Follow events in real-time
- `-c, --container <ID>` - Filter by container
- `-a, --anomalies` - Show only anomalies
- `-n, --limit <N>` - Maximum events to show (default: 100)

---

### `monitor policies` - List available policies

```bash
vordr monitor policies
```

Shows all monitoring policies with syscall groups.

---

### `monitor stats` - Show statistics

```bash
vordr monitor stats [OPTIONS]
```

**Options:**
- `-c, --container <ID>` - Stats for specific container

---

### `monitor check` - Check eBPF support

```bash
vordr monitor check
```

Checks:
- Kernel version
- BPF filesystem
- BTF support
- Required capabilities

---

## Utility Commands

### `doctor` - Check system prerequisites

```bash
vordr doctor [OPTIONS]
```

**Options:**
- `--verbose` - Show detailed checks

Checks:
- Runtime (youki/runc) availability
- Kernel version
- Required tools (netavark, etc.)
- Permissions

**Examples:**
```bash
vordr doctor
vordr doctor --verbose
```

---

### `completion` - Generate shell completions

```bash
vordr completion <SHELL>
```

**Shells:** bash, zsh, fish, powershell

**Examples:**
```bash
# Bash
vordr completion bash > /etc/bash_completion.d/vordr

# Zsh
vordr completion zsh > /usr/local/share/zsh/site-functions/_vordr

# Fish
vordr completion fish > ~/.config/fish/completions/vordr.fish
```

---

### `profile` - Manage security profiles

```bash
vordr profile [SUBCOMMAND]
```

**Subcommands:**
- `list` - List available profiles
- `show <PROFILE>` - Show profile details
- `apply <PROFILE>` - Apply profile

**Profiles:**
- **strict** - Maximum security, blocks dangerous operations
- **balanced** - Balance between security and usability
- **dev** - Development mode, minimal restrictions

---

### `explain` - Explain policy decisions

```bash
vordr explain [OPTIONS]
```

Explains why a container was blocked or allowed.

---

### `auth` - Manage registry authentication

```bash
vordr auth [SUBCOMMAND]
```

**Subcommands:**
- `list` - List stored credentials
- `login <REGISTRY>` - Log in to registry
- `logout <REGISTRY>` - Log out from registry

---

### `login` - Log in to registry

```bash
vordr login [OPTIONS] REGISTRY
```

**Options:**
- `-u, --username <USER>` - Username
- `-p, --password <PASSWORD>` - Password (not recommended, use prompt)
- `--password-stdin` - Read password from stdin

---

### `logout` - Log out from registry

```bash
vordr logout REGISTRY
```

---

### `compose` - Multi-container applications

```bash
vordr compose [SUBCOMMAND]
```

**Subcommands:**
- `up` - Create and start containers
- `down` - Stop and remove containers
- `ps` - List compose containers
- `logs` - View container logs

*Note: Compose support is in development*

---

### `serve` - Start MCP server

```bash
vordr serve [OPTIONS]
```

**Options:**
- `--host <HOST>` - Host to bind to (default: 0.0.0.0)
- `-p, --port <PORT>` - Port to listen on (default: 8080)

**Environment Variables:**
- `VORDR_SERVE_HOST`
- `VORDR_SERVE_PORT`

**Examples:**
```bash
vordr serve
vordr serve --host 127.0.0.1 --port 9000
```

**Endpoints:**
- `POST /` - JSON-RPC 2.0
- `GET /health` - Health check
- `GET /tools` - List available MCP tools

---

## Exit Codes

- `0` - Success
- `1` - General error
- `2` - Misuse of command (invalid arguments)
- `125` - Container failed to run
- `126` - Command cannot be invoked
- `127` - Command not found
- `130` - Terminated by Ctrl+C

---

## Configuration Files

**Database:**
- Location: `$VORDR_ROOT/vordr.db` (default: `/var/lib/vordr/vordr.db`)
- Format: SQLite3

**Container State:**
- Location: `$VORDR_ROOT/<container-id>/`
- Files: `config.json`, `state.json`, `rootfs/`

**Auth Credentials:**
- Location: `$HOME/.config/vordr/auth.json`
- Format: JSON

---

## Examples

### Basic Workflow

```bash
# 1. Check system
vordr doctor

# 2. Pull image
vordr pull alpine:latest

# 3. Run container
vordr run -d --name myapp alpine:latest sleep infinity

# 4. Exec into container
vordr exec -it myapp /bin/sh

# 5. Monitor runtime
vordr monitor start --policy strict

# 6. Check status
vordr ps
vordr monitor status

# 7. Stop and remove
vordr stop myapp
vordr rm myapp
```

### Advanced Workflow

```bash
# Run with full configuration
vordr run -d \
  --name webapp \
  --network app-net \
  -v /data/app:/app/data \
  -e DB_HOST=postgres \
  -e DB_PORT=5432 \
  -p 8080:80 \
  --restart=always \
  myapp:latest

# Monitor with webhook alerts
vordr monitor start \
  --container webapp \
  --policy strict \
  --webhook https://alerts.example.com/vordr \
  --sensitivity 0.95 \
  --daemon

# View live events
vordr monitor events --follow --container webapp
```

---

**Last Updated:** 2026-01-25
**Version:** 0.5.0-dev
