// SPDX-License-Identifier: PMPL-1.0-or-later
//! Command-line interface for Vordr

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use clap_complete::Shell;
use tracing::info;

pub mod auth;
pub mod completion;
pub mod compose;
pub mod doctor;
pub mod exec;
pub mod explain;
pub mod images;
pub mod inspect;
pub mod monitor;
pub mod network;
pub mod profile;
pub mod ps;
pub mod run;
pub mod system;
pub mod validation;
pub mod volume;

/// Vordr - High-Assurance Daemonless Container Engine
#[derive(Parser, Debug)]
#[command(
    name = "vordr",
    author = "Svalinn Project",
    version,
    about = "High-assurance daemonless container engine with formally verified security",
    long_about = None
)]
pub struct Cli {
    /// Enable verbose output
    #[arg(short, long, global = true)]
    pub verbose: bool,

    /// Path to state database
    #[arg(
        long,
        global = true,
        default_value = "/var/lib/vordr/vordr.db",
        env = "VORDR_DB"
    )]
    pub db_path: String,

    /// Container runtime path (youki or runc)
    #[arg(
        long,
        global = true,
        default_value = "youki",
        env = "VORDR_RUNTIME"
    )]
    pub runtime: String,

    /// Root directory for container state
    #[arg(
        long,
        global = true,
        default_value = "/var/lib/vordr",
        env = "VORDR_ROOT"
    )]
    pub root: String,

    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    /// Run a container from an image
    Run(run::RunArgs),

    /// Execute a command in a running container
    Exec(exec::ExecArgs),

    /// List containers
    Ps(ps::PsArgs),

    /// Display detailed information on a container
    Inspect(inspect::InspectArgs),

    /// Start a stopped container
    Start {
        /// Container ID or name
        container: String,
    },

    /// Stop a running container
    Stop {
        /// Container ID or name
        container: String,

        /// Seconds to wait before killing
        #[arg(short, long, default_value = "10")]
        timeout: u32,
    },

    /// Remove a container
    Rm {
        /// Container ID or name
        container: String,

        /// Force remove running container
        #[arg(short, long)]
        force: bool,
    },

    /// Manage images
    #[command(subcommand)]
    Image(images::ImageCommands),

    /// Manage networks
    #[command(subcommand)]
    Network(network::NetworkCommands),

    /// Manage volumes
    #[command(subcommand)]
    Volume(volume::VolumeCommands),

    /// Pull an image from a registry
    Pull {
        /// Image reference (e.g., alpine:latest)
        image: String,
    },

    /// Display system information
    Info,

    /// Show Vordr version
    Version,

    // === New commands ===

    /// Check system prerequisites and configuration
    Doctor(doctor::DoctorArgs),

    /// System management (df, prune, reset)
    System(system::SystemArgs),

    /// Multi-container applications with Compose
    Compose(compose::ComposeArgs),

    /// Log in to a container registry
    Login(auth::LoginArgs),

    /// Log out from a container registry
    Logout(auth::LogoutArgs),

    /// Manage registry authentication
    Auth(auth::AuthArgs),

    /// Generate shell completions
    Completion {
        /// Shell to generate completions for
        #[arg(value_enum)]
        shell: Shell,
    },

    /// Manage security profiles (strict/balanced/dev)
    Profile(profile::ProfileArgs),

    /// Explain why a policy blocked an action
    Explain(explain::ExplainArgs),

    /// eBPF-based runtime monitoring
    Monitor(monitor::MonitorArgs),

    /// Start the MCP HTTP server (for Svalinn integration)
    Serve {
        /// Host to bind to
        #[arg(long, default_value = "0.0.0.0", env = "VORDR_SERVE_HOST")]
        host: String,

        /// Port to listen on
        #[arg(short, long, default_value = "8080", env = "VORDR_SERVE_PORT")]
        port: u16,
    },
}

/// Execute a CLI command
pub async fn execute(mut cli: Cli) -> Result<()> {
    // Take command out of cli to allow borrowing cli afterwards
    let command = std::mem::replace(&mut cli.command, Commands::Version);
    match command {
        Commands::Run(args) => run::execute(args, &cli).await,
        Commands::Exec(args) => exec::execute(args, &cli).await,
        Commands::Ps(args) => ps::execute(args, &cli).await,
        Commands::Inspect(args) => inspect::execute(args, &cli).await,
        Commands::Start { container } => start_container(&container, &cli).await,
        Commands::Stop { container, timeout } => stop_container(&container, timeout, &cli).await,
        Commands::Rm { container, force } => remove_container(&container, force, &cli).await,
        Commands::Image(cmd) => images::execute(cmd, &cli).await,
        Commands::Network(cmd) => network::execute(cmd, &cli).await,
        Commands::Volume(cmd) => volume::execute(cmd, &cli).await,
        Commands::Pull { image } => pull_image(&image, &cli).await.map(|_| ()),
        Commands::Info => show_info(&cli).await,
        Commands::Version => show_version(),
        // New commands
        Commands::Doctor(args) => doctor::execute(args, &cli).await,
        Commands::System(args) => system::execute(args, &cli).await,
        Commands::Compose(args) => compose::execute(args, &cli).await,
        Commands::Login(args) => auth::login(args, &cli).await,
        Commands::Logout(args) => auth::logout(args, &cli).await,
        Commands::Auth(args) => auth::execute_auth(args, &cli).await,
        Commands::Completion { shell } => completion::execute(completion::CompletionArgs { shell }),
        Commands::Profile(args) => profile::execute(args, &cli).await,
        Commands::Explain(args) => explain::execute(args, &cli).await,
        Commands::Monitor(args) => monitor::execute(args, &cli).await,
        Commands::Serve { host, port } => start_mcp_server(host, port, &cli).await,
    }
}

async fn start_mcp_server(host: String, port: u16, cli: &Cli) -> Result<()> {
    use crate::mcp::{start_server, McpServerConfig};

    let config = McpServerConfig {
        host,
        port,
        db_path: cli.db_path.clone(),
        root_dir: cli.root.clone(),
        runtime: cli.runtime.clone(),
    };

    println!("Starting Vörðr MCP server on {}:{}", config.host, config.port);
    println!("Endpoints:");
    println!("  POST /         - JSON-RPC 2.0 (tools/call, tools/list)");
    println!("  GET  /health   - Health check");
    println!("  GET  /tools    - List available tools");

    start_server(config).await.map_err(|e| anyhow::anyhow!("Server error: {}", e))
}

async fn start_container(container: &str, cli: &Cli) -> Result<()> {
    use std::path::Path;
    use crate::engine::ContainerLifecycle;

    let lifecycle = ContainerLifecycle::new(
        Path::new(&cli.db_path),
        Path::new(&cli.root),
        &cli.runtime,
    )?;

    let info = lifecycle.get(container)?;
    println!("Starting container: {} ({})", info.name, info.id);

    let pid = lifecycle.start(&info.id).await?;
    println!("Container started (PID: {})", pid);

    Ok(())
}

async fn stop_container(container: &str, timeout: u32, cli: &Cli) -> Result<()> {
    use std::path::Path;
    use crate::engine::ContainerLifecycle;

    let lifecycle = ContainerLifecycle::new(
        Path::new(&cli.db_path),
        Path::new(&cli.root),
        &cli.runtime,
    )?;

    let info = lifecycle.get(container)?;
    println!("Stopping container: {} (timeout: {}s)", info.name, timeout);

    lifecycle.stop(&info.id, timeout).await?;
    println!("Container stopped");

    Ok(())
}

async fn remove_container(container: &str, force: bool, cli: &Cli) -> Result<()> {
    use std::path::Path;
    use crate::engine::ContainerLifecycle;

    let lifecycle = ContainerLifecycle::new(
        Path::new(&cli.db_path),
        Path::new(&cli.root),
        &cli.runtime,
    )?;

    let info = lifecycle.get(container)?;

    if force {
        println!("Force removing container: {}", info.name);
    } else {
        println!("Removing container: {}", info.name);
    }

    lifecycle.delete(&info.id, force)?;
    println!("Container removed");

    Ok(())
}

pub async fn pull_image(image: &str, cli: &Cli) -> Result<String> {
    use sha2::{Sha256, Digest};

    println!("Pulling image: {}", image);

    // Parse image reference (registry/repo:tag format)
    let (registry, repository, tag) = parse_image_ref(image);
    info!("Registry: {}, Repository: {}, Tag: {}", registry, repository, tag);

    // Resolve manifest via OCI Distribution API
    let client = reqwest::Client::builder()
        .user_agent(format!("vordr/{}", env!("CARGO_PKG_VERSION")))
        .timeout(std::time::Duration::from_secs(120))
        .build()
        .context("Failed to create HTTP client")?;

    // Load auth credentials if available
    let auth_header = crate::cli::auth::resolve_auth_header(&registry);

    // Fetch manifest
    let manifest_url = format!(
        "https://{}/v2/{}/manifests/{}",
        registry, repository, tag
    );
    let mut req = client.get(&manifest_url)
        .header("Accept", "application/vnd.oci.image.manifest.v1+json")
        .header("Accept", "application/vnd.docker.distribution.manifest.v2+json");
    if let Some(auth) = &auth_header {
        req = req.header("Authorization", auth);
    }

    let manifest_resp = req.send().await
        .context("Failed to fetch manifest")?;

    if !manifest_resp.status().is_success() {
        anyhow::bail!(
            "Registry returned {} for manifest {}:{}",
            manifest_resp.status(), repository, tag
        );
    }

    let manifest_bytes = manifest_resp.bytes().await
        .context("Failed to read manifest body")?;

    // Compute image ID from manifest digest
    let mut hasher = Sha256::new();
    hasher.update(&manifest_bytes);
    let image_id = format!("sha256:{}", hex::encode(hasher.finalize()));

    // Store manifest in image directory
    let image_dir = std::path::Path::new(&cli.root)
        .join("images")
        .join(&image_id);
    std::fs::create_dir_all(&image_dir)
        .context("Failed to create image directory")?;
    std::fs::write(image_dir.join("manifest.json"), &manifest_bytes)
        .context("Failed to write manifest")?;

    // Pull layers referenced in manifest
    let manifest: serde_json::Value = serde_json::from_slice(&manifest_bytes)
        .context("Failed to parse manifest JSON")?;

    if let Some(layers) = manifest.get("layers").and_then(|v: &serde_json::Value| v.as_array()) {
        let layer_dir = image_dir.join("layers");
        std::fs::create_dir_all(&layer_dir)
            .context("Failed to create layers directory")?;

        for (i, layer) in layers.iter().enumerate() {
            let digest = layer.get("digest")
                .and_then(|v: &serde_json::Value| v.as_str())
                .unwrap_or("unknown");
            let size = layer.get("size")
                .and_then(|v: &serde_json::Value| v.as_u64())
                .unwrap_or(0);

            println!("  Pulling layer {}/{}: {} ({} bytes)", i + 1, layers.len(), digest, size);

            let blob_url = format!(
                "https://{}/v2/{}/blobs/{}",
                registry, repository, digest
            );
            let mut blob_req = client.get(&blob_url);
            if let Some(auth) = &auth_header {
                blob_req = blob_req.header("Authorization", auth);
            }

            let blob_resp = blob_req.send().await
                .with_context(|| format!("Failed to fetch layer {}", digest))?;

            if blob_resp.status().is_success() {
                let blob_bytes = blob_resp.bytes().await
                    .with_context(|| format!("Failed to read layer {}", digest))?;
                let safe_name = digest.replace(':', "_");
                std::fs::write(layer_dir.join(&safe_name), &blob_bytes)
                    .with_context(|| format!("Failed to write layer {}", digest))?;
            } else {
                tracing::warn!("Layer {} returned {}, skipping", digest, blob_resp.status());
            }
        }
    }

    println!("Image {} pulled successfully ({})", image, &image_id[..19]);
    Ok(image_id)
}

/// Parse an image reference into (registry, repository, tag).
pub fn parse_image_ref(image: &str) -> (String, String, String) {
    let (name, tag) = if let Some((n, t)) = image.rsplit_once(':') {
        // Avoid treating "registry.io:5000/repo" as repo="registry.io" tag="5000/repo"
        if t.contains('/') {
            (image, "latest")
        } else {
            (n, t)
        }
    } else {
        (image, "latest")
    };

    let parts: Vec<&str> = name.splitn(2, '/').collect();
    if parts.len() == 2 && (parts[0].contains('.') || parts[0].contains(':')) {
        // Explicit registry: registry.example.com/repo
        (parts[0].to_string(), parts[1].to_string(), tag.to_string())
    } else if parts.len() == 2 {
        // Docker Hub: user/repo
        ("registry-1.docker.io".to_string(), name.to_string(), tag.to_string())
    } else {
        // Docker Hub official image: just "nginx" → library/nginx
        ("registry-1.docker.io".to_string(), format!("library/{}", name), tag.to_string())
    }
}

async fn show_info(_cli: &Cli) -> Result<()> {
    println!("Vordr Container Engine");
    println!("Version: {}", env!("CARGO_PKG_VERSION"));
    println!("Gatekeeper: {}", crate::ffi::gatekeeper_version());
    println!("Runtime: youki (default)");

    #[cfg(target_os = "linux")]
    {
        // Show kernel info
        if let Ok(output) = std::process::Command::new("uname")
            .args(["-r"])
            .output()
        {
            let kernel = String::from_utf8_lossy(&output.stdout);
            println!("Kernel: {}", kernel.trim());
        }
    }

    Ok(())
}

fn show_version() -> Result<()> {
    println!("vordr version {}", env!("CARGO_PKG_VERSION"));
    println!("gatekeeper version {}", crate::ffi::gatekeeper_version());
    Ok(())
}
