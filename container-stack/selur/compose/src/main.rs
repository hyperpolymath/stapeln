// SPDX-License-Identifier: PMPL-1.0-or-later
//! selur-compose - Multi-container orchestration for verified containers
//!
//! This is the main CLI entry point. It coordinates:
//! - Cerro Torre (ct) for .ctp bundle packing/verification
//! - Svalinn for gateway and policy enforcement
//! - selur for zero-copy IPC
//! - Vörðr for container orchestration

use anyhow::Result;
use clap::{Parser, Subcommand};
use std::path::PathBuf;

mod commands;
mod compose;
mod ct;
mod graph;
mod svalinn;
mod vordr;
mod vordr_mcp;

#[derive(Parser)]
#[command(name = "selur-compose")]
#[command(version, about, long_about = None)]
#[command(author = "Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>")]
struct Cli {
    /// Path to compose file
    #[arg(short = 'f', long, default_value = "compose.toml", env = "SELUR_COMPOSE_FILE")]
    file: PathBuf,

    /// Project name (default: directory name)
    #[arg(short = 'p', long, env = "SELUR_PROJECT_NAME")]
    project: Option<String>,

    /// Enable verbose logging
    #[arg(short, long, action = clap::ArgAction::Count)]
    verbose: u8,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Start all services
    Up {
        /// Run in detached mode
        #[arg(short, long)]
        detach: bool,

        /// Build images before starting
        #[arg(long)]
        build: bool,

        /// Services to start (default: all)
        services: Vec<String>,
    },

    /// Stop and remove all services
    Down {
        /// Remove volumes
        #[arg(short = 'v', long)]
        volumes: bool,

        /// Remove images
        #[arg(long)]
        rmi: Option<String>,
    },

    /// Start stopped services
    Start {
        /// Services to start
        services: Vec<String>,
    },

    /// Stop running services
    Stop {
        /// Services to stop
        services: Vec<String>,

        /// Timeout before killing (seconds)
        #[arg(short = 't', long, default_value = "10")]
        timeout: u64,
    },

    /// Restart services
    Restart {
        /// Services to restart
        services: Vec<String>,

        /// Timeout before killing (seconds)
        #[arg(short = 't', long, default_value = "10")]
        timeout: u64,
    },

    /// List services
    Ps {
        /// Show all services (default: running only)
        #[arg(short = 'a', long)]
        all: bool,

        /// Output format (table, json)
        #[arg(long, default_value = "table")]
        format: String,
    },

    /// View logs
    Logs {
        /// Follow log output
        #[arg(short = 'f', long)]
        follow: bool,

        /// Number of lines to show from the end
        #[arg(long)]
        tail: Option<usize>,

        /// Services to show logs for
        services: Vec<String>,
    },

    /// Build .ctp bundles
    Build {
        /// Services to build
        services: Vec<String>,

        /// No cache
        #[arg(long)]
        no_cache: bool,
    },

    /// Pull .ctp bundles from registry
    Pull {
        /// Services to pull
        services: Vec<String>,
    },

    /// Push .ctp bundles to registry
    Push {
        /// Services to push
        services: Vec<String>,
    },

    /// Execute command in service
    Exec {
        /// Service name
        service: String,

        /// Command to execute
        command: Vec<String>,
    },

    /// Run one-off command
    Run {
        /// Service name
        service: String,

        /// Command to execute
        command: Vec<String>,

        /// Remove container after run
        #[arg(long)]
        rm: bool,
    },

    /// Scale service to N replicas
    Scale {
        /// Service=replicas pairs (e.g., api=3)
        services: Vec<String>,
    },

    /// Verify all .ctp bundle signatures
    Verify {
        /// Services to verify (default: all)
        services: Vec<String>,
    },

    /// Check policy compliance
    Policy {
        /// Policy file
        #[arg(short = 'f', long)]
        file: Option<PathBuf>,
    },

    /// Show SBOM for service
    Sbom {
        /// Service name
        service: String,

        /// Output format (json, cyclonedx, spdx)
        #[arg(long, default_value = "json")]
        format: String,
    },

    /// Show build provenance
    Provenance {
        /// Service name
        service: String,
    },

    /// Validate and view merged config
    Config {
        /// Output format (toml, json)
        #[arg(long, default_value = "toml")]
        format: String,
    },

    /// Display running processes
    Top {
        /// Services to show (default: all)
        services: Vec<String>,
    },

    /// Stream real-time events
    Events,

    /// View service details
    Inspect {
        /// Service name
        service: String,

        /// Output format (json, pretty)
        #[arg(long, default_value = "pretty")]
        format: String,
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    // Initialize logging
    let log_level = match cli.verbose {
        0 => "info",
        1 => "debug",
        _ => "trace",
    };
    tracing_subscriber::fmt()
        .with_env_filter(format!("selur_compose={}", log_level))
        .init();

    tracing::info!("selur-compose v{}", env!("CARGO_PKG_VERSION"));

    // Load compose file
    let compose = compose::ComposeFile::load(&cli.file)?;
    let project_name = cli
        .project
        .unwrap_or_else(|| compose.default_project_name());

    tracing::debug!("Project: {}", project_name);
    tracing::debug!("Services: {}", compose.services.len());

    // Execute command
    match cli.command {
        Commands::Up {
            detach,
            build,
            services,
        } => {
            commands::up(&compose, &project_name, detach, build, services).await?;
        }

        Commands::Down { volumes, rmi } => {
            commands::down(&compose, &project_name, volumes, rmi).await?;
        }

        Commands::Ps { all, format } => {
            commands::ps(&project_name, all, &format).await?;
        }

        Commands::Verify { services } => {
            commands::verify(&compose, services).await?;
        }

        Commands::Logs { follow, tail, services } => {
            commands::logs(&compose, &project_name, follow, tail, services).await?;
        }

        Commands::Exec { service, command } => {
            commands::exec(&compose, &project_name, service, command).await?;
        }

        Commands::Scale { services } => {
            commands::scale(&compose, &project_name, services).await?;
        }

        Commands::Build { services, no_cache } => {
            commands::build(&compose, &project_name, services, no_cache).await?;
        }

        Commands::Start { services } => {
            commands::start(&compose, &project_name, services).await?;
        }

        Commands::Stop { services, timeout } => {
            commands::stop(&compose, &project_name, services, timeout).await?;
        }

        Commands::Restart { services, timeout } => {
            commands::restart(&compose, &project_name, services, timeout).await?;
        }

        Commands::Run { service, command, rm } => {
            commands::run(&compose, &project_name, service, command, rm).await?;
        }

        Commands::Top { services } => {
            commands::top(&compose, &project_name, services).await?;
        }

        Commands::Events => {
            commands::events(&project_name).await?;
        }

        Commands::Inspect { service, format } => {
            commands::inspect(&compose, &project_name, service, &format).await?;
        }

        Commands::Pull { services } => {
            commands::pull(&compose, &project_name, services).await?;
        }

        Commands::Push { services } => {
            commands::push(&compose, &project_name, services).await?;
        }

        Commands::Config { format } => {
            tracing::info!("Validating compose file...");
            match format.as_str() {
                "json" => println!("{}", serde_json::to_string_pretty(&compose)?),
                "toml" => println!("{}", toml::to_string_pretty(&compose)?),
                _ => anyhow::bail!("Unknown format: {}", format),
            }
        }

        _ => {
            println!("Command not yet implemented");
        }
    }

    Ok(())
}
