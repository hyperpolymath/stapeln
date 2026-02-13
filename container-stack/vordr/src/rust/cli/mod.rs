// SPDX-License-Identifier: PMPL-1.0-or-later
//! Command-line interface for Vordr

use anyhow::Result;
use clap::{Parser, Subcommand};
use clap_complete::Shell;

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
        Commands::Pull { image } => pull_image(&image, &cli).await,
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

async fn pull_image(image: &str, _cli: &Cli) -> Result<()> {
    println!("Pulling image: {}", image);
    // TODO: Implement registry client
    Ok(())
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
