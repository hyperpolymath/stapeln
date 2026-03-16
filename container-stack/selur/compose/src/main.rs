// SPDX-License-Identifier: PMPL-1.0-or-later
//! selur-compose - Multi-container orchestration for verified containers
//!
//! This is the main CLI entry point. It coordinates:
//! - Cerro Torre (ct) for .ctp bundle packing/verification
//! - Svalinn for gateway and policy enforcement
//! - selur for zero-copy IPC
//! - Vordr for container orchestration

#![forbid(unsafe_code)]
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
#[command(author = "Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>")]
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
        #[arg(long)]
        policy_file: Option<PathBuf>,
    },

    /// Show SBOM for service (via Cerro Torre)
    Sbom {
        /// Service name
        service: String,

        /// Output format (json, cyclonedx, spdx)
        #[arg(long, default_value = "json")]
        format: String,
    },

    /// Show build provenance chain (via Cerro Torre)
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

    /// Manage networks
    Network {
        #[command(subcommand)]
        action: NetworkAction,
    },

    /// Manage volumes
    Volume {
        #[command(subcommand)]
        action: VolumeAction,
    },

    /// Watch build contexts and rebuild on changes (dev mode)
    Watch {
        /// Services to watch (default: all with build config)
        services: Vec<String>,

        /// Poll interval in seconds
        #[arg(long, default_value = "2")]
        interval: u64,
    },

    /// Copy files to/from containers (SERVICE:/path syntax)
    Cp {
        /// Source path (host path or SERVICE:/path)
        src: String,

        /// Destination path (host path or SERVICE:/path)
        dest: String,
    },

    /// Pause running containers
    Pause {
        /// Services to pause (default: all)
        services: Vec<String>,
    },

    /// Unpause paused containers
    Unpause {
        /// Services to unpause (default: all)
        services: Vec<String>,
    },

    /// Create containers without starting them
    Create {
        /// Services to create (default: all)
        services: Vec<String>,
    },

    /// List images used by compose services
    Images,

    /// Wait for containers to stop, return exit codes
    Wait {
        /// Services to wait for (default: all)
        services: Vec<String>,
    },

    /// Show health check status for services
    Health {
        /// Services to check (default: all)
        services: Vec<String>,
    },
}

/// Subcommands for `selur-compose network`
#[derive(Subcommand)]
enum NetworkAction {
    /// List all networks
    Ls,

    /// Create a network
    Create {
        /// Network name
        name: String,

        /// Network driver (selur, bridge)
        #[arg(long, default_value = "selur")]
        driver: String,

        /// Subnet CIDR (e.g. 172.28.0.0/16)
        #[arg(long)]
        subnet: Option<String>,
    },

    /// Remove a network
    Rm {
        /// Network name
        name: String,
    },
}

/// Subcommands for `selur-compose volume`
#[derive(Subcommand)]
enum VolumeAction {
    /// List all volumes
    Ls,

    /// Create a volume
    Create {
        /// Volume name
        name: String,

        /// Volume driver
        #[arg(long, default_value = "local")]
        driver: String,
    },

    /// Remove a volume
    Rm {
        /// Volume name
        name: String,
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

        Commands::Logs {
            follow,
            tail,
            services,
        } => {
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

        Commands::Run {
            service,
            command,
            rm,
        } => {
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
            commands::config(&compose, &format).await?;
        }

        Commands::Sbom { service, format } => {
            commands::sbom(&compose, &service, &format).await?;
        }

        Commands::Provenance { service } => {
            commands::provenance(&compose, &service).await?;
        }

        Commands::Policy { policy_file } => {
            commands::policy(&compose, &project_name, policy_file).await?;
        }

        Commands::Network { action } => match action {
            NetworkAction::Ls => {
                commands::network_ls().await?;
            }
            NetworkAction::Create {
                name,
                driver,
                subnet,
            } => {
                commands::network_create(&name, &driver, subnet).await?;
            }
            NetworkAction::Rm { name } => {
                commands::network_rm(&name).await?;
            }
        },

        Commands::Volume { action } => match action {
            VolumeAction::Ls => {
                commands::volume_ls().await?;
            }
            VolumeAction::Create { name, driver } => {
                commands::volume_create(&name, &driver).await?;
            }
            VolumeAction::Rm { name } => {
                commands::volume_rm(&name).await?;
            }
        },

        Commands::Watch {
            services,
            interval,
        } => {
            commands::watch(&compose, &project_name, services, interval).await?;
        }

        Commands::Cp { src, dest } => {
            commands::cp(&compose, &project_name, &src, &dest).await?;
        }

        Commands::Pause { services } => {
            commands::pause(&compose, &project_name, services).await?;
        }

        Commands::Unpause { services } => {
            commands::unpause(&compose, &project_name, services).await?;
        }

        Commands::Create { services } => {
            commands::create(&compose, &project_name, services).await?;
        }

        Commands::Images => {
            commands::images(&compose).await?;
        }

        Commands::Wait { services } => {
            commands::wait_cmd(&compose, &project_name, services).await?;
        }

        Commands::Health { services } => {
            commands::health(&compose, &project_name, services).await?;
        }
    }

    Ok(())
}
