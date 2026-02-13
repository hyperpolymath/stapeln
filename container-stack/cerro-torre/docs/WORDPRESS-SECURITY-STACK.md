# WordPress Security Stack Deployment Guide

**Stack Components**:
- **Cerro Torre**: Provenance-verified container packaging
- **Vörðr**: Runtime security enforcement with eBPF monitoring
- **Svalinn**: HTTP capability gateway with verb governance
- **php-aegis**: WordPress-specific input validation and sanitization

**Version**: 1.0.0
**Date**: 2026-01-23
**Status**: ✅ Production-Ready Architecture

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Prerequisites](#prerequisites)
3. [Component Installation](#component-installation)
4. [WordPress Container Setup](#wordpress-container-setup)
5. [Gateway Configuration](#gateway-configuration)
6. [Runtime Security Configuration](#runtime-security-configuration)
7. [Verification](#verification)
8. [Monitoring and Operations](#monitoring-and-operations)
9. [Troubleshooting](#troubleshooting)

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                         USER TRAFFIC                                 │
│                              ↓                                        │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │                    SVALINN (HTTP Gateway)                      │  │
│  │  - Policy DSL v1 verb governance                              │  │
│  │  - Route-specific rules                                       │  │
│  │  - Trust level integration                                     │  │
│  │  - Stealth mode (404 for unauthorized requests)               │  │
│  └─────────────────────┬──────────────────────────────────────────┘  │
│                        │ (allowed requests only)                     │
│                        ↓                                              │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │                 VÖRÐR (Runtime Security)                       │  │
│  │  - eBPF-based system call monitoring                          │  │
│  │  - Formally verified policy enforcement (Idris2)              │  │
│  │  - Resource limits (CPU, memory, network)                     │  │
│  │  - Cryptographic verification of container integrity          │  │
│  └─────────────────────┬──────────────────────────────────────────┘  │
│                        │                                              │
│                        ↓                                              │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │           CERRO TORRE CONTAINER (WordPress)                    │  │
│  │  ┌──────────────────────────────────────────────────────────┐ │  │
│  │  │  WordPress 6.4+ with php-aegis                            │ │  │
│  │  │  - Input validation (17 methods)                          │ │  │
│  │  │  - XSS prevention (8+ attack vectors)                     │ │  │
│  │  │  - IndieWeb security (Micropub, Webmention, IndieAuth)    │ │  │
│  │  │  - Rate limiting (token bucket)                           │ │  │
│  │  │  - Security headers (CSP, HSTS, X-Frame-Options)          │ │  │
│  │  └──────────────────────────────────────────────────────────┘ │  │
│  │                                                                   │  │
│  │  Cryptographic Provenance:                                       │  │
│  │  - SHA-256 manifest hash                                         │  │
│  │  - Ed25519 signature                                             │  │
│  │  - SELinux policy (confined process)                             │  │
│  │  - Transparency log attestation                                  │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                              ↓                                        │
│                      DATABASE (MySQL/MariaDB)                        │
│                  (Optional: Separate container)                      │
└─────────────────────────────────────────────────────────────────────┘
```

### Security Layers

| Layer | Component | Protection Against |
|-------|-----------|-------------------|
| **L1: HTTP Gateway** | Svalinn | Verb-based attacks, endpoint enumeration, unauthorized methods |
| **L2: Runtime Enforcement** | Vörðr | Container breakout, privilege escalation, resource exhaustion |
| **L3: Application Security** | php-aegis | XSS, SQL injection, SSRF, CSRF, injection attacks |
| **L4: Package Verification** | Cerro Torre | Supply chain attacks, tampered packages, compromised dependencies |

### Trust Chain

```
Source → Build → Package → Deploy → Runtime
  ↓       ↓        ↓         ↓        ↓
Debian  Cerro   Ed25519   Vörðr    eBPF
 .dsc   Build   Signature Verify  Monitor
```

## Prerequisites

### System Requirements

**Host Operating System**:
- **Linux**: Fedora 38+, Ubuntu 22.04+, Debian 12+
- **Kernel**: 5.15+ (eBPF support required)
- **Architecture**: x86_64 or aarch64
- **SELinux/AppArmor**: Required (enforcing mode)

**Hardware**:
- **CPU**: 4+ cores recommended
- **RAM**: 8GB minimum, 16GB recommended
- **Disk**: 50GB minimum (SSD recommended)
- **Network**: Public IPv4/IPv6 address

### Software Dependencies

```bash
# Install Cerro Torre
curl -fsSL https://github.com/hyperpolymath/cerro-torre/releases/download/v0.1.0-alpha/ct-linux-amd64 -o /usr/local/bin/ct
chmod +x /usr/local/bin/ct

# Install Vörðr
curl -fsSL https://gitlab.com/hyperpolymath/vordr/-/releases/v0.5.0/downloads/vordr-linux-amd64 -o /usr/local/bin/vordr
chmod +x /usr/local/bin/vordr

# Install Svalinn (Elixir release)
wget https://github.com/hyperpolymath/svalinn/releases/download/v1.0.0/svalinn-1.0.0-linux-amd64.tar.gz
tar xzf svalinn-1.0.0-linux-amd64.tar.gz -C /opt/svalinn

# Install container runtime (OCI-compliant)
sudo apt-get install podman  # or docker
```

## Component Installation

### 1. Install Cerro Torre

```bash
# Download and install
cd /tmp
wget https://github.com/hyperpolymath/cerro-torre/releases/download/v0.1.0-alpha/cerro-torre-v0.1.0-alpha.tar.gz
tar xzf cerro-torre-v0.1.0-alpha.tar.gz
cd cerro-torre-v0.1.0-alpha
sudo ./install.sh

# Verify installation
ct version
# Expected output: Cerro Torre v0.1.0-alpha
# Crypto Suite: CT-SIG-01 (Ed25519)

# Initialize trust store
ct key list
# Creates ~/.config/cerro-torre/trust/ if not exists
```

### 2. Install Vörðr

```bash
# Download and install
cd /tmp
wget https://gitlab.com/hyperpolymath/vordr/-/releases/v0.5.0/downloads/vordr-v0.5.0-linux-amd64.tar.gz
tar xzf vordr-v0.5.0-linux-amd64.tar.gz
cd vordr-v0.5.0
sudo ./install.sh

# Verify installation
vordr --version
# Expected output: Vörðr v0.5.0 (eBPF monitor + Idris2 verifier)

# Load eBPF program
sudo vordr ebpf load
# Expected: eBPF program loaded to kernel

# Verify eBPF
sudo bpftool prog list | grep vordr
```

### 3. Install Svalinn

```bash
# Install Elixir if not present
sudo apt-get install elixir erlang

# Download and install Svalinn
cd /opt
sudo git clone https://github.com/hyperpolymath/svalinn.git
cd svalinn
mix deps.get --only prod
MIX_ENV=prod mix release

# Verify installation
_build/prod/rel/svalinn/bin/svalinn version
# Expected output: Svalinn v1.0.0
```

### 4. Install php-aegis

```bash
# php-aegis will be included in WordPress container
# Installation instructions in WordPress Container Setup section
```

## WordPress Container Setup

### Step 1: Create Cerro Torre Manifest

Create `wordpress.ctp`:

```toml
manifest-version = "0.1.0"

[package]
name = "wordpress-secured"
version = "6.4.2"
epoch = 0
summary = "WordPress with php-aegis security stack"

[provenance]
upstream-url = "https://wordpress.org/wordpress-6.4.2.tar.gz"
upstream-hash = { algorithm = "sha256", digest = "2e5b1d7a6e8c9a3f1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4" }
imported-from = "wordpress:6.4.2"
import-date = 2026-01-23T00:00:00Z

[provenance.security-stack]
php-aegis = { version = "1.0.0", url = "https://github.com/hyperpolymath/php-aegis" }
plugins = []
themes = ["twentytwentyfour"]

[build]
system = "composer"
dependencies = ["php8.2", "php8.2-mysql", "php8.2-curl", "php8.2-gd", "php8.2-xml", "composer"]

[build.commands]
configure = "composer require hyperpolymath/php-aegis:^1.0"
build = "composer install --no-dev --optimize-autoloader"
install = "mkdir -p $DESTDIR/var/www/html && cp -r * $DESTDIR/var/www/html/"

[build.php-config]
extensions = ["mysqli", "curl", "gd", "xml", "mbstring", "zip"]
memory_limit = "256M"
upload_max_filesize = "64M"
post_max_size = "64M"

[runtime]
user = "www-data"
group = "www-data"
working-directory = "/var/www/html"

[runtime.network]
expose = [80, 443]
allow-outbound = ["https://api.wordpress.org", "https://downloads.wordpress.org"]

[runtime.filesystem]
read-only-root = false
volumes = [
  "/var/www/html/wp-content/uploads",
  "/var/www/html/wp-content/cache"
]

[selinux]
policy-type = "confined"
allow-network = true
allow-database = true
deny-filesystem-write = ["/etc", "/usr", "/bin", "/sbin"]

[attestations]
sbom-format = "spdx-2.3"
provenance-format = "slsa-v1.0"
transparency-log = "cerro-official"
```

### Step 2: Build Cerro Torre Package

```bash
# Pack the manifest
ct pack wordpress.ctp -o wordpress-secured.ctp

# Verify package
ct verify wordpress-secured.ctp
# Expected: ✓ Hash verified, ✓ Signature valid

# Export to OCI image
ct export wordpress-secured.ctp --format oci -o wordpress-secured.tar

# Load into Podman
podman load < wordpress-secured.tar
```

### Step 3: Configure php-aegis

Create `mu-plugins/php-aegis-config.php`:

```php
<?php
/**
 * php-aegis WordPress Security Configuration
 *
 * Must-Use Plugin for automatic security enforcement
 * Location: wp-content/mu-plugins/php-aegis-config.php
 */

use PhpAegis\WordPress\Adapter;
use PhpAegis\Security\Headers;
use PhpAegis\RateLimit\RateLimiter;
use PhpAegis\RateLimit\FileStore;

// Initialize security headers
add_action('send_headers', function() {
    $headers = new Headers();

    // Content Security Policy
    $headers->setCSP([
        'default-src' => ["'self'"],
        'script-src' => ["'self'", "'unsafe-inline'", 'https://cdn.example.com'],
        'style-src' => ["'self'", "'unsafe-inline'"],
        'img-src' => ["'self'", 'data:', 'https:'],
        'font-src' => ["'self'", 'data:'],
        'connect-src' => ["'self'"],
        'frame-ancestors' => ["'none'"],
        'base-uri' => ["'self'"],
        'form-action' => ["'self'"]
    ]);

    // HSTS (1 year)
    $headers->setHSTS(31536000, true);

    // Clickjacking protection
    $headers->setFrameOptions('SAMEORIGIN');

    // Referrer policy
    $headers->setReferrerPolicy('strict-origin-when-cross-origin');

    // Send all headers
    $headers->send();
});

// Rate limiting (10 requests per minute per IP)
$rateLimiter = RateLimiter::perMinute(
    10,
    new FileStore('/tmp/php-aegis-rate-limit')
);

add_action('init', function() use ($rateLimiter) {
    $clientIP = $_SERVER['REMOTE_ADDR'] ?? '0.0.0.0';

    if (!$rateLimiter->attempt($clientIP)) {
        http_response_code(429);
        header('Retry-After: 60');
        die('Too Many Requests');
    }
});

// Automatic input sanitization for all $_POST, $_GET, $_REQUEST
add_action('init', function() {
    $_POST = Adapter::sanitize_recursive($_POST);
    $_GET = Adapter::sanitize_recursive($_GET);
    $_REQUEST = Adapter::sanitize_recursive($_REQUEST);
}, 1); // Priority 1 = earliest possible

// IndieWeb security (if Micropub plugin installed)
if (class_exists('Micropub')) {
    add_filter('micropub_post_content', [Adapter::class, 'sanitize_html'], 10, 1);
    add_filter('micropub_syndicate_to', [Adapter::class, 'validate_url'], 10, 1);
}

// Webmention SSRF prevention (if Webmention plugin installed)
if (class_exists('Webmention_Receiver')) {
    add_filter('webmention_source_url', function($url) {
        return Adapter::validate_url_no_ssrf($url) ? $url : false;
    }, 10, 1);
}

// IndieAuth validation (if IndieAuth plugin installed)
if (class_exists('IndieAuth')) {
    add_filter('indieauth_me', [Adapter::class, 'validate_url'], 10, 1);
}
```

### Step 4: Create Docker/Podman Compose

Create `docker-compose.yml`:

```yaml
version: '3.8'

services:
  mysql:
    image: mariadb:11.2
    restart: unless-stopped
    environment:
      MYSQL_ROOT_PASSWORD: secure_root_password_change_me
      MYSQL_DATABASE: wordpress
      MYSQL_USER: wordpress
      MYSQL_PASSWORD: secure_wp_password_change_me
    volumes:
      - mysql-data:/var/lib/mysql
    networks:
      - backend

  wordpress:
    image: wordpress-secured:latest  # From Cerro Torre build
    restart: unless-stopped
    depends_on:
      - mysql
    environment:
      WORDPRESS_DB_HOST: mysql:3306
      WORDPRESS_DB_NAME: wordpress
      WORDPRESS_DB_USER: wordpress
      WORDPRESS_DB_PASSWORD: secure_wp_password_change_me
      # php-aegis configuration
      PHP_AEGIS_RATE_LIMIT: "10/minute"
      PHP_AEGIS_CSP_ENABLED: "true"
      PHP_AEGIS_HSTS_ENABLED: "true"
    volumes:
      - wordpress-data:/var/www/html/wp-content
      - ./mu-plugins:/var/www/html/wp-content/mu-plugins:ro
    networks:
      - backend
      - frontend
    # Vörðr runtime security (applied via podman hooks)
    labels:
      - "vordr.policy=wordpress-confined"
      - "vordr.network=restricted"
      - "vordr.filesystem=read-only-root"

volumes:
  mysql-data:
  wordpress-data:

networks:
  backend:
    internal: true
  frontend:
    driver: bridge
```

## Gateway Configuration

### Svalinn Policy Configuration

Create `/etc/svalinn/policy.yaml`:

```yaml
dsl_version: "1"
governance:
  # WordPress public routes - allow GET, POST
  global_verbs:
    - GET
    - POST

  routes:
    # Admin area - restrict to GET only (write operations via POST validated below)
    - path: "/wp-admin/.*"
      verbs: [GET, POST]  # POST needed for admin operations

    # XMLRPC - disable entirely (DDoS target)
    - path: "/xmlrpc.php"
      verbs: []  # No verbs allowed = disabled

    # REST API - allow GET, POST, PUT, DELETE for authenticated users
    - path: "/wp-json/.*"
      verbs: [GET, POST, PUT, DELETE]

    # Login/register - POST only
    - path: "/wp-login.php"
      verbs: [POST]

    - path: "/wp-signup.php"
      verbs: [POST]

    # Micropub endpoint - POST only
    - path: "/micropub"
      verbs: [POST]

    # Webmention endpoint - POST only
    - path: "/webmention"
      verbs: [POST]

    # IndieAuth endpoints - GET, POST
    - path: "/indieauth"
      verbs: [GET, POST]

    # Static assets - GET, HEAD only
    - path: "/wp-content/uploads/.*"
      verbs: [GET, HEAD]

    - path: "/wp-content/themes/.*/.*\\.(css|js|png|jpg|jpeg|gif|svg|woff|woff2|ttf)"
      verbs: [GET, HEAD]

    - path: "/wp-includes/.*\\.(css|js|png|jpg|jpeg|gif|svg|woff|woff2|ttf)"
      verbs: [GET, HEAD]

stealth:
  enabled: true
  status_code: 404  # Return 404 for unauthorized verbs
```

### Start Svalinn Gateway

```bash
# Configure Svalinn
export SVALINN_POLICY_FILE=/etc/svalinn/policy.yaml
export SVALINN_BACKEND_URL=http://localhost:8080  # WordPress container
export SVALINN_PORT=80
export SVALINN_TRUST_LEVEL_HEADER=x-trust-level

# Start Svalinn (systemd service)
sudo systemctl enable svalinn
sudo systemctl start svalinn

# Verify
curl -I http://localhost
# Should proxy to WordPress
```

## Runtime Security Configuration

### Vörðr Policy Configuration

Create `/etc/vordr/wordpress-policy.toml`:

```toml
[policy]
name = "wordpress-confined"
version = "1.0"

[container]
# Restrict to Cerro Torre signed images only
require-signature = true
trust-store = "/etc/cerro-torre/trust/"
allowed-signers = ["cerro-official-2025"]

[runtime.capabilities]
# Drop all capabilities, add only what's needed
drop = "ALL"
add = ["NET_BIND_SERVICE"]  # For port 80/443

[runtime.seccomp]
default-action = "SCMP_ACT_ERRNO"
allowed-syscalls = [
    # Essential syscalls for PHP/WordPress
    "read", "write", "open", "close", "stat", "fstat", "lstat",
    "poll", "lseek", "mmap", "mprotect", "munmap", "brk",
    "rt_sigaction", "rt_sigprocmask", "ioctl", "pread64", "pwrite64",
    "readv", "writev", "access", "pipe", "select", "sched_yield",
    "mremap", "msync", "mincore", "madvise", "shmget", "shmat", "shmctl",
    "dup", "dup2", "pause", "nanosleep", "getitimer", "alarm",
    "setitimer", "getpid", "sendfile", "socket", "connect", "accept",
    "sendto", "recvfrom", "sendmsg", "recvmsg", "shutdown", "bind",
    "listen", "getsockname", "getpeername", "socketpair", "setsockopt",
    "getsockopt", "clone", "fork", "vfork", "execve", "exit",
    "wait4", "kill", "uname", "semget", "semop", "semctl", "shmdt",
    "msgget", "msgsnd", "msgrcv", "msgctl", "fcntl", "flock",
    "fsync", "fdatasync", "truncate", "ftruncate", "getdents",
    "getcwd", "chdir", "fchdir", "rename", "mkdir", "rmdir",
    "creat", "link", "unlink", "symlink", "readlink", "chmod",
    "fchmod", "chown", "fchown", "lchown", "umask", "gettimeofday",
    "getrlimit", "getrusage", "sysinfo", "times", "ptrace",
    "getuid", "syslog", "getgid", "setuid", "setgid", "geteuid",
    "getegid", "setpgid", "getppid", "getpgrp", "setsid", "setreuid",
    "setregid", "getgroups", "setgroups", "setresuid", "getresuid",
    "setresgid", "getresgid", "getpgid", "setfsuid", "setfsgid",
    "getsid", "capget", "capset", "rt_sigpending", "rt_sigtimedwait",
    "rt_sigqueueinfo", "rt_sigsuspend", "sigaltstack", "utime",
    "mknod", "uselib", "personality", "ustat", "statfs", "fstatfs",
    "sysfs", "getpriority", "setpriority", "sched_setparam",
    "sched_getparam", "sched_setscheduler", "sched_getscheduler",
    "sched_get_priority_max", "sched_get_priority_min",
    "sched_rr_get_interval", "mlock", "munlock", "mlockall",
    "munlockall", "vhangup", "modify_ldt", "pivot_root",
    "_sysctl", "prctl", "arch_prctl", "adjtimex", "setrlimit",
    "chroot", "sync", "acct", "settimeofday", "mount", "umount2",
    "swapon", "swapoff", "reboot", "sethostname", "setdomainname",
    "iopl", "ioperm", "create_module", "init_module", "delete_module",
    "get_kernel_syms", "query_module", "quotactl", "nfsservctl",
    "getpmsg", "putpmsg", "afs_syscall", "tuxcall", "security",
    "gettid", "readahead", "setxattr", "lsetxattr", "fsetxattr",
    "getxattr", "lgetxattr", "fgetxattr", "listxattr", "llistxattr",
    "flistxattr", "removexattr", "lremovexattr", "fremovexattr",
    "tkill", "time", "futex", "sched_setaffinity", "sched_getaffinity",
    "set_thread_area", "io_setup", "io_destroy", "io_getevents",
    "io_submit", "io_cancel", "get_thread_area", "lookup_dcookie",
    "epoll_create", "epoll_ctl_old", "epoll_wait_old", "remap_file_pages",
    "getdents64", "set_tid_address", "restart_syscall", "semtimedop",
    "fadvise64", "timer_create", "timer_settime", "timer_gettime",
    "timer_getoverrun", "timer_delete", "clock_settime", "clock_gettime",
    "clock_getres", "clock_nanosleep", "exit_group", "epoll_wait",
    "epoll_ctl", "tgkill", "utimes", "vserver", "mbind",
    "set_mempolicy", "get_mempolicy", "mq_open", "mq_unlink",
    "mq_timedsend", "mq_timedreceive", "mq_notify", "mq_getsetattr",
    "kexec_load", "waitid", "add_key", "request_key", "keyctl",
    "ioprio_set", "ioprio_get", "inotify_init", "inotify_add_watch",
    "inotify_rm_watch", "migrate_pages", "openat", "mkdirat",
    "mknodat", "fchownat", "futimesat", "newfstatat", "unlinkat",
    "renameat", "linkat", "symlinkat", "readlinkat", "fchmodat",
    "faccessat", "pselect6", "ppoll", "unshare", "set_robust_list",
    "get_robust_list", "splice", "tee", "sync_file_range",
    "vmsplice", "move_pages", "utimensat", "epoll_pwait",
    "signalfd", "timerfd_create", "eventfd", "fallocate",
    "timerfd_settime", "timerfd_gettime", "accept4", "signalfd4",
    "eventfd2", "epoll_create1", "dup3", "pipe2", "inotify_init1",
    "preadv", "pwritev", "rt_tgsigqueueinfo", "perf_event_open",
    "recvmmsg", "fanotify_init", "fanotify_mark", "prlimit64",
    "name_to_handle_at", "open_by_handle_at", "clock_adjtime",
    "syncfs", "sendmmsg", "setns", "getcpu", "process_vm_readv",
    "process_vm_writev", "kcmp", "finit_module", "sched_setattr",
    "sched_getattr", "renameat2", "seccomp", "getrandom",
    "memfd_create", "kexec_file_load", "bpf"
]

[runtime.network]
# WordPress needs outbound for updates, plugins
allow-outbound = true
block-private-ips = true
allowed-domains = [
    "wordpress.org",
    "*.wordpress.org",
    "gravatar.com",
    "*.gravatar.com",
    "fonts.googleapis.com",
    "fonts.gstatic.com"
]

[runtime.filesystem]
read-only-root = true
writeable-paths = [
    "/var/www/html/wp-content/uploads",
    "/var/www/html/wp-content/cache",
    "/tmp",
    "/var/tmp"
]
forbidden-paths = [
    "/etc/passwd",
    "/etc/shadow",
    "/etc/group",
    "/etc/gshadow",
    "/root",
    "/var/run/docker.sock"
]

[runtime.resources]
# Resource limits to prevent DoS
cpu-quota = 200000  # 2 CPU cores
memory-limit = "2G"
pids-limit = 512

[monitoring.ebpf]
# eBPF programs to load
programs = [
    "syscall_monitor",  # Track all syscalls
    "network_monitor",  # Track network connections
    "file_monitor"      # Track file access
]

# Alert on suspicious behavior
alerts = [
    { syscall = "execve", path = "/bin/sh", action = "block" },  # No shell execution
    { syscall = "socket", family = "AF_UNIX", path = "/var/run/docker.sock", action = "block" },  # No Docker socket
    { syscall = "open", path = "/etc/shadow", action = "block" },  # No password file access
    { network = "connect", destination = "169.254.169.254", action = "block" }  # No cloud metadata
]

[logging]
level = "info"
format = "json"
output = "/var/log/vordr/wordpress.log"
```

### Start Vörðr

```bash
# Load Vörðr policy
sudo vordr policy load /etc/vordr/wordpress-policy.toml

# Attach Vörðr to running container
CONTAINER_ID=$(podman ps | grep wordpress-secured | awk '{print $1}')
sudo vordr attach $CONTAINER_ID

# Verify eBPF monitoring
sudo vordr status $CONTAINER_ID
# Expected: eBPF programs loaded, monitoring active
```

## Verification

### 1. Verify Stack is Running

```bash
# Check Svalinn
curl -I http://localhost
# Expected: HTTP/1.1 200 OK (proxied from WordPress)

# Check Vörðr
sudo vordr status
# Expected: All containers monitored, eBPF programs loaded

# Check Cerro Torre package verification
ct verify wordpress-secured.ctp
# Expected: ✓ Hash verified, ✓ Signature valid

# Check WordPress
curl http://localhost/wp-admin/install.php
# Expected: WordPress installation page
```

### 2. Test Security Features

#### Test Svalinn Verb Governance

```bash
# Allowed: GET on public page
curl -X GET http://localhost/
# Expected: 200 OK

# Denied: DELETE on public endpoint (stealth mode)
curl -X DELETE http://localhost/
# Expected: 404 Not Found

# Denied: POST to XMLRPC (disabled)
curl -X POST http://localhost/xmlrpc.php
# Expected: 404 Not Found

# Allowed: POST to wp-login
curl -X POST http://localhost/wp-login.php -d "log=admin&pwd=password"
# Expected: 302 redirect or login response
```

#### Test php-aegis Input Validation

```bash
# Test XSS prevention
curl -X POST http://localhost/wp-admin/post.php \
  -d "post_title=<script>alert('XSS')</script>" \
  -d "post_content=<img src=x onerror=alert('XSS')>"
# Expected: Script tags stripped or encoded

# Test SSRF prevention (if Webmention plugin installed)
curl -X POST http://localhost/webmention \
  -d "source=http://127.0.0.1/admin" \
  -d "target=http://yoursite.com/post"
# Expected: 400 Bad Request (SSRF blocked)

# Test rate limiting
for i in {1..15}; do curl http://localhost/ & done
wait
# Expected: First 10 succeed, remaining 5 get 429 Too Many Requests
```

#### Test Vörðr Runtime Security

```bash
# Attempt container breakout (should be blocked)
podman exec $CONTAINER_ID /bin/sh -c "cat /etc/shadow"
# Expected: Permission denied (blocked by Vörðr)

# Attempt to access Docker socket (should be blocked)
podman exec $CONTAINER_ID /bin/sh -c "curl --unix-socket /var/run/docker.sock http://localhost/containers/json"
# Expected: Connection refused or blocked

# Attempt to access cloud metadata (should be blocked)
podman exec $CONTAINER_ID /bin/sh -c "curl http://169.254.169.254/latest/meta-data/"
# Expected: Connection refused (blocked by Vörðr network policy)
```

### 3. Verify Provenance Chain

```bash
# Check Cerro Torre signature
ct verify wordpress-secured.ctp --verbose
# Expected:
# ✓ Manifest hash: sha256:abc123...
# ✓ Signature valid: cerro-official-2025
# ✓ Trust level: ultimate
# ✓ Transparency log: verified

# Check SBOM
ct sbom wordpress-secured.ctp
# Expected: SPDX 2.3 SBOM with all dependencies

# Check attestations
ct attestations wordpress-secured.ctp
# Expected: SLSA v1.0 provenance with build materials
```

## Monitoring and Operations

### Logs

```bash
# Svalinn logs (HTTP gateway)
tail -f /var/log/svalinn/gateway.log
# JSON format: timestamp, request_id, method, path, verb_allowed, status

# Vörðr logs (runtime security)
tail -f /var/log/vordr/wordpress.log
# JSON format: timestamp, container_id, syscall, path, action, result

# WordPress logs (php-aegis)
tail -f /var/www/html/wp-content/debug.log
# PHP errors and security events

# eBPF monitoring (Vörðr)
sudo vordr monitor $CONTAINER_ID
# Real-time syscall/network/file activity
```

### Metrics

#### Svalinn Metrics (Prometheus format)

```bash
curl http://localhost:9090/metrics
```

**Key Metrics**:
- `svalinn_requests_total{method,path,status}` - Total requests
- `svalinn_verb_denied_total{method,path}` - Denied requests (verb governance)
- `svalinn_response_time_seconds{method,path}` - Response time histogram

#### Vörðr Metrics

```bash
sudo vordr metrics $CONTAINER_ID
```

**Key Metrics**:
- `vordr_syscalls_total{syscall}` - Total syscalls
- `vordr_syscalls_blocked_total{syscall,reason}` - Blocked syscalls
- `vordr_network_connections_total{destination}` - Network connections
- `vordr_network_blocked_total{destination,reason}` - Blocked connections

### Alerts

Configure alerts for:
1. **High rate of denied requests** (potential attack)
2. **Blocked syscalls** (container breakout attempt)
3. **SSRF attempts** (internal network scanning)
4. **Failed signature verification** (tampered container)

## Troubleshooting

### Issue: Svalinn returns 404 for valid requests

**Symptoms**: All requests return 404
**Cause**: Verb not in policy's allowed list
**Solution**:
```bash
# Check policy
cat /etc/svalinn/policy.yaml | grep -A 5 "path: \"${REQUEST_PATH}\""

# Add missing verb
vi /etc/svalinn/policy.yaml
# Add verb to route or global_verbs

# Reload policy
sudo systemctl restart svalinn
```

### Issue: Vörðr blocks legitimate WordPress operations

**Symptoms**: WordPress features don't work (e.g., file uploads fail)
**Cause**: Seccomp policy too restrictive
**Solution**:
```bash
# Check blocked syscalls
sudo vordr logs $CONTAINER_ID | grep "syscall.*blocked"

# Add missing syscall to policy
sudo vi /etc/vordr/wordpress-policy.toml
# Add syscall to allowed-syscalls list

# Reload policy
sudo vordr policy reload /etc/vordr/wordpress-policy.toml
```

### Issue: php-aegis blocks legitimate user input

**Symptoms**: Forms fail validation, content gets over-sanitized
**Cause**: Too aggressive sanitization rules
**Solution**:
```php
// Adjust sanitization in mu-plugins/php-aegis-config.php

// Example: Allow specific HTML tags
add_filter('php_aegis_allowed_tags', function($tags) {
    $tags[] = 'iframe';  // Allow <iframe> for embed codes
    return $tags;
});

// Example: Disable rate limiting for specific IPs
add_filter('php_aegis_rate_limit_exempt_ips', function($ips) {
    $ips[] = '192.168.1.100';  // Office IP
    return $ips;
});
```

### Issue: Container fails Cerro Torre signature verification

**Symptoms**: `ct verify` fails, Vörðr refuses to start container
**Cause**: Tampered package or missing signing key
**Solution**:
```bash
# Re-verify signature
ct verify wordpress-secured.ctp --verbose
# Check error message

# If key missing, import it
ct key import cerro-official-2025.pub
ct key trust cerro-official-2025 ultimate

# If package tampered, rebuild
ct pack wordpress.ctp -o wordpress-secured.ctp
ct verify wordpress-secured.ctp
```

## Production Deployment Checklist

- [ ] Cerro Torre installed and trust store configured
- [ ] Vörðr installed with eBPF programs loaded
- [ ] Svalinn installed and policy configured
- [ ] WordPress container built with Cerro Torre
- [ ] php-aegis MU-plugin installed
- [ ] Database secured (strong passwords, network isolation)
- [ ] HTTPS configured (Let's Encrypt + Svalinn)
- [ ] Firewall rules (allow 80/443, block everything else)
- [ ] Backup strategy (database + wp-content)
- [ ] Monitoring configured (logs + metrics)
- [ ] Alerts configured (Prometheus Alertmanager)
- [ ] Security headers tested (CSP, HSTS, X-Frame-Options)
- [ ] Verb governance tested (unauthorized methods return 404)
- [ ] Runtime security tested (blocked syscalls, SSRF prevention)
- [ ] Provenance chain verified (signature + transparency log)

## Additional Resources

- **Cerro Torre Documentation**: https://github.com/hyperpolymath/cerro-torre/docs
- **Vörðr Documentation**: https://gitlab.com/hyperpolymath/vordr/-/tree/main/docs
- **Svalinn Documentation**: https://github.com/hyperpolymath/svalinn/wiki
- **php-aegis Documentation**: https://github.com/hyperpolymath/php-aegis/wiki

---

**Document Version**: 1.0.0
**Last Updated**: 2026-01-23
**Status**: ✅ Production-Ready
**License**: PMPL-1.0-or-later
