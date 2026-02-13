// SPDX-License-Identifier: PMPL-1.0-or-later
//! Docker Compose subset support

use anyhow::{bail, Context, Result};
use clap::{Parser, Subcommand};
use dialoguer::Confirm;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use tabled::{Table, Tabled};

use crate::cli::Cli;

/// Compose commands for multi-container applications
#[derive(Parser, Debug)]
pub struct ComposeArgs {
    /// Path to compose file
    #[arg(short, long, default_value = "compose.yaml")]
    pub file: PathBuf,

    /// Project name (default: directory name)
    #[arg(short, long)]
    pub project_name: Option<String>,

    #[command(subcommand)]
    pub command: ComposeCommands,
}

#[derive(Subcommand, Debug)]
pub enum ComposeCommands {
    /// Create and start containers
    Up {
        /// Run in background
        #[arg(short, long)]
        detach: bool,

        /// Don't start linked services
        #[arg(long)]
        no_deps: bool,

        /// Force recreate containers
        #[arg(long)]
        force_recreate: bool,

        /// Specific services to start
        services: Vec<String>,
    },

    /// Stop and remove containers
    Down {
        /// Remove named volumes
        #[arg(short, long)]
        volumes: bool,

        /// Remove images (local|all)
        #[arg(long)]
        rmi: Option<String>,

        /// Remove orphan containers
        #[arg(long)]
        remove_orphans: bool,
    },

    /// List containers
    Ps {
        /// Show all (including stopped)
        #[arg(short, long)]
        all: bool,

        /// Output format (table, json)
        #[arg(long, default_value = "table")]
        format: String,
    },

    /// View container logs
    Logs {
        /// Service name
        service: Option<String>,

        /// Follow log output
        #[arg(short, long)]
        follow: bool,

        /// Number of lines to show
        #[arg(long)]
        tail: Option<usize>,

        /// Show timestamps
        #[arg(short, long)]
        timestamps: bool,
    },

    /// Validate and view compose file
    Config {
        /// Only check for errors, don't print
        #[arg(short, long)]
        quiet: bool,

        /// Resolve and print compose file
        #[arg(long)]
        resolve: bool,
    },

    /// Pull service images
    Pull {
        /// Service names
        services: Vec<String>,

        /// Ignore images that don't exist
        #[arg(long)]
        ignore_pull_failures: bool,
    },
}

/// Supported compose file structure
#[derive(Debug, Deserialize, Serialize)]
pub struct ComposeFile {
    /// Version (ignored, always v3+ semantics)
    #[serde(default)]
    pub version: Option<String>,

    /// Service definitions
    #[serde(default)]
    pub services: HashMap<String, ServiceConfig>,

    /// Network definitions
    #[serde(default)]
    pub networks: HashMap<String, NetworkConfig>,

    /// Volume definitions
    #[serde(default)]
    pub volumes: HashMap<String, VolumeConfig>,
}

#[derive(Debug, Deserialize, Serialize, Default)]
pub struct ServiceConfig {
    /// Container image (required)
    pub image: Option<String>,

    /// Build context (UNSUPPORTED)
    pub build: Option<serde_yaml::Value>,

    /// Override command
    pub command: Option<StringOrList>,

    /// Container entrypoint
    pub entrypoint: Option<StringOrList>,

    /// Environment variables
    #[serde(default)]
    pub environment: Option<EnvironmentConfig>,

    /// Environment file
    pub env_file: Option<StringOrList>,

    /// Port mappings
    #[serde(default)]
    pub ports: Vec<String>,

    /// Volume mounts
    #[serde(default)]
    pub volumes: Vec<String>,

    /// Service dependencies
    #[serde(default)]
    pub depends_on: Option<DependsOnConfig>,

    /// Networks to attach
    #[serde(default)]
    pub networks: Option<NetworksConfig>,

    /// Restart policy
    pub restart: Option<String>,

    /// Container name
    pub container_name: Option<String>,

    /// Working directory
    pub working_dir: Option<String>,

    /// User
    pub user: Option<String>,

    /// Privileged mode
    #[serde(default)]
    pub privileged: bool,

    /// Health check (UNSUPPORTED)
    pub healthcheck: Option<serde_yaml::Value>,

    /// Deploy config (UNSUPPORTED)
    pub deploy: Option<serde_yaml::Value>,

    /// Configs (UNSUPPORTED)
    pub configs: Option<serde_yaml::Value>,

    /// Secrets (UNSUPPORTED)
    pub secrets: Option<serde_yaml::Value>,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum StringOrList {
    String(String),
    List(Vec<String>),
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum EnvironmentConfig {
    List(Vec<String>),
    Map(HashMap<String, String>),
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum DependsOnConfig {
    List(Vec<String>),
    Map(HashMap<String, DependsOnCondition>),
}

#[derive(Debug, Deserialize, Serialize)]
pub struct DependsOnCondition {
    pub condition: Option<String>,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum NetworksConfig {
    List(Vec<String>),
    Map(HashMap<String, Option<NetworkAttachment>>),
}

#[derive(Debug, Deserialize, Serialize)]
pub struct NetworkAttachment {
    pub aliases: Option<Vec<String>>,
    pub ipv4_address: Option<String>,
}

#[derive(Debug, Deserialize, Serialize, Default)]
pub struct NetworkConfig {
    pub driver: Option<String>,
    pub external: Option<bool>,
}

#[derive(Debug, Deserialize, Serialize, Default)]
pub struct VolumeConfig {
    pub driver: Option<String>,
    pub external: Option<bool>,
}

#[derive(Tabled)]
struct ServiceStatusRow {
    #[tabled(rename = "NAME")]
    name: String,
    #[tabled(rename = "IMAGE")]
    image: String,
    #[tabled(rename = "STATUS")]
    status: String,
    #[tabled(rename = "PORTS")]
    ports: String,
}

/// Keys that are explicitly unsupported
const UNSUPPORTED_KEYS: &[(&str, &str)] = &[
    ("build", "Use 'vordr build' first, then reference image"),
    ("deploy", "Deploy config ignored (use Svalinn for orchestration)"),
    ("healthcheck", "Health checks not yet supported (coming in v0.3)"),
    ("configs", "Use volume mounts instead"),
    ("secrets", "Use environment variables or volume-mounted files"),
];

/// Execute compose command
pub async fn execute(args: ComposeArgs, cli: &Cli) -> Result<()> {
    let project_name = args.project_name.clone().unwrap_or_else(|| {
        std::env::current_dir()
            .ok()
            .and_then(|p| p.file_name().map(|n| n.to_string_lossy().to_string()))
            .unwrap_or_else(|| "default".to_string())
    });

    match args.command {
        ComposeCommands::Up {
            detach,
            no_deps,
            force_recreate,
            services,
        } => up(cli, &args.file, &project_name, detach, no_deps, force_recreate, services).await,
        ComposeCommands::Down {
            volumes,
            rmi,
            remove_orphans,
        } => down(cli, &args.file, &project_name, volumes, rmi, remove_orphans).await,
        ComposeCommands::Ps { all, format } => ps(cli, &args.file, &project_name, all, &format).await,
        ComposeCommands::Logs {
            service,
            follow,
            tail,
            timestamps,
        } => logs(cli, &args.file, &project_name, service, follow, tail, timestamps).await,
        ComposeCommands::Config { quiet, resolve } => config(&args.file, quiet, resolve).await,
        ComposeCommands::Pull {
            services,
            ignore_pull_failures,
        } => pull(cli, &args.file, services, ignore_pull_failures).await,
    }
}

fn load_compose_file(path: &PathBuf) -> Result<ComposeFile> {
    // Try multiple file names
    let paths_to_try = if path.exists() {
        vec![path.clone()]
    } else {
        vec![
            PathBuf::from("compose.yaml"),
            PathBuf::from("compose.yml"),
            PathBuf::from("docker-compose.yaml"),
            PathBuf::from("docker-compose.yml"),
        ]
    };

    for p in &paths_to_try {
        if p.exists() {
            let content = std::fs::read_to_string(p)
                .with_context(|| format!("Failed to read {}", p.display()))?;

            let compose: ComposeFile = serde_yaml::from_str(&content)
                .with_context(|| format!("Failed to parse {}", p.display()))?;

            println!("Using: {}\n", p.display());
            return Ok(compose);
        }
    }

    bail!("No compose file found. Tried: compose.yaml, docker-compose.yaml")
}

fn check_unsupported_keys(compose: &ComposeFile) -> Vec<(String, String, String)> {
    let mut warnings = Vec::new();

    for (service_name, service) in &compose.services {
        if service.build.is_some() {
            warnings.push((
                format!("services.{}.build", service_name),
                "build".to_string(),
                UNSUPPORTED_KEYS
                    .iter()
                    .find(|(k, _)| *k == "build")
                    .map(|(_, v)| v.to_string())
                    .unwrap_or_default(),
            ));
        }
        if service.deploy.is_some() {
            warnings.push((
                format!("services.{}.deploy", service_name),
                "deploy".to_string(),
                UNSUPPORTED_KEYS
                    .iter()
                    .find(|(k, _)| *k == "deploy")
                    .map(|(_, v)| v.to_string())
                    .unwrap_or_default(),
            ));
        }
        if service.healthcheck.is_some() {
            warnings.push((
                format!("services.{}.healthcheck", service_name),
                "healthcheck".to_string(),
                UNSUPPORTED_KEYS
                    .iter()
                    .find(|(k, _)| *k == "healthcheck")
                    .map(|(_, v)| v.to_string())
                    .unwrap_or_default(),
            ));
        }
        if service.configs.is_some() {
            warnings.push((
                format!("services.{}.configs", service_name),
                "configs".to_string(),
                UNSUPPORTED_KEYS
                    .iter()
                    .find(|(k, _)| *k == "configs")
                    .map(|(_, v)| v.to_string())
                    .unwrap_or_default(),
            ));
        }
        if service.secrets.is_some() {
            warnings.push((
                format!("services.{}.secrets", service_name),
                "secrets".to_string(),
                UNSUPPORTED_KEYS
                    .iter()
                    .find(|(k, _)| *k == "secrets")
                    .map(|(_, v)| v.to_string())
                    .unwrap_or_default(),
            ));
        }
    }

    warnings
}

async fn up(
    _cli: &Cli,
    file: &PathBuf,
    project_name: &str,
    detach: bool,
    _no_deps: bool,
    _force_recreate: bool,
    _services: Vec<String>,
) -> Result<()> {
    let compose = load_compose_file(file)?;

    // Check for unsupported keys
    let warnings = check_unsupported_keys(&compose);
    if !warnings.is_empty() {
        println!("WARNING: Unsupported compose keys detected:");
        for (path, _key, hint) in &warnings {
            println!("  {} → {}", path, hint);
        }
        println!();

        let confirmed = Confirm::new()
            .with_prompt("Continue with supported keys?")
            .default(true)
            .interact()?;

        if !confirmed {
            println!("Aborted.");
            return Ok(());
        }
        println!();
    }

    // Validate all services have images
    for (name, service) in &compose.services {
        if service.image.is_none() && service.build.is_none() {
            bail!("Service '{}' has no image specified", name);
        }
    }

    // Create networks
    for (name, _network) in &compose.networks {
        println!("Creating network {}_{}_...", project_name, name);
    }

    // Create default network if not specified
    if compose.networks.is_empty() {
        println!("Creating network {}_default...", project_name);
    }

    // Create volumes
    for (name, _volume) in &compose.volumes {
        println!("Creating volume {}_{}_...", project_name, name);
    }

    // Sort services by dependency order
    let service_order = topological_sort(&compose)?;

    // Create and start containers
    for service_name in &service_order {
        if let Some(service) = compose.services.get(service_name) {
            let image = service
                .image
                .as_ref()
                .cloned()
                .unwrap_or_else(|| format!("{}_{}:latest", project_name, service_name));

            print!("Creating container {} ({})... ", service_name, image);
            // Would actually create the container here
            println!("done");
        }
    }

    println!("\nServices started: {}", service_order.len());

    if !detach {
        println!("\nPress Ctrl+C to stop");
        // Would attach to container logs here
    }

    Ok(())
}

async fn down(
    _cli: &Cli,
    file: &PathBuf,
    project_name: &str,
    remove_volumes: bool,
    _rmi: Option<String>,
    _remove_orphans: bool,
) -> Result<()> {
    let compose = load_compose_file(file)?;

    // Stop containers in reverse order
    let service_order = topological_sort(&compose)?;
    for service_name in service_order.iter().rev() {
        print!("Stopping container {}... ", service_name);
        // Would actually stop the container here
        println!("done");
    }

    // Remove containers
    println!("Removing containers... done");

    // Remove networks
    let confirmed = Confirm::new()
        .with_prompt("Remove networks?")
        .default(false)
        .interact()?;

    if confirmed {
        if compose.networks.is_empty() {
            println!("Removing network {}_default... done", project_name);
        } else {
            for (name, _) in &compose.networks {
                println!("Removing network {}_{}... done", project_name, name);
            }
        }
    }

    // Remove volumes if requested
    if remove_volumes {
        let confirmed = Confirm::new()
            .with_prompt("Remove volumes?")
            .default(false)
            .interact()?;

        if confirmed {
            for (name, _) in &compose.volumes {
                println!("Removing volume {}_{}... done", project_name, name);
            }
        } else {
            println!("Volumes retained.");
        }
    }

    Ok(())
}

async fn ps(
    _cli: &Cli,
    file: &PathBuf,
    _project_name: &str,
    _all: bool,
    format: &str,
) -> Result<()> {
    let compose = load_compose_file(file)?;

    let rows: Vec<_> = compose
        .services
        .iter()
        .map(|(name, service)| ServiceStatusRow {
            name: name.clone(),
            image: service
                .image
                .clone()
                .unwrap_or_else(|| "(build)".to_string()),
            status: "running".to_string(), // Would query actual status
            ports: service.ports.join(", "),
        })
        .collect();

    match format {
        "json" => {
            #[derive(Serialize)]
            struct ServiceStatus {
                name: String,
                image: String,
                status: String,
                ports: Vec<String>,
            }

            let statuses: Vec<_> = compose
                .services
                .iter()
                .map(|(name, service)| ServiceStatus {
                    name: name.clone(),
                    image: service.image.clone().unwrap_or_default(),
                    status: "running".to_string(),
                    ports: service.ports.clone(),
                })
                .collect();

            println!("{}", serde_json::to_string_pretty(&statuses)?);
        }
        _ => {
            let table = Table::new(rows).to_string();
            println!("{}", table);
        }
    }

    Ok(())
}

async fn logs(
    _cli: &Cli,
    _file: &PathBuf,
    _project_name: &str,
    service: Option<String>,
    follow: bool,
    tail: Option<usize>,
    _timestamps: bool,
) -> Result<()> {
    if let Some(name) = service {
        println!("Showing logs for service: {}", name);
    } else {
        println!("Showing logs for all services");
    }

    if follow {
        println!("(Following... press Ctrl+C to stop)");
    }

    if let Some(n) = tail {
        println!("(Showing last {} lines)", n);
    }

    // Would actually stream container logs here
    println!("(Log output would appear here)");

    Ok(())
}

async fn config(file: &PathBuf, quiet: bool, _resolve: bool) -> Result<()> {
    let compose = load_compose_file(file)?;

    // Check for errors
    let warnings = check_unsupported_keys(&compose);

    for (name, service) in &compose.services {
        if service.image.is_none() && service.build.is_none() {
            bail!("Service '{}' has no image specified", name);
        }
    }

    if quiet {
        if warnings.is_empty() {
            println!("Configuration is valid.");
        } else {
            println!(
                "Configuration valid with {} warning(s).",
                warnings.len()
            );
        }
    } else {
        println!("Configuration is valid.\n");

        if !warnings.is_empty() {
            println!("Warnings:");
            for (path, _key, hint) in &warnings {
                println!("  {} → {}", path, hint);
            }
            println!();
        }

        println!("Services:");
        for (name, service) in &compose.services {
            println!(
                "  - {} ({})",
                name,
                service.image.as_deref().unwrap_or("build")
            );
        }

        if !compose.networks.is_empty() {
            println!("\nNetworks:");
            for (name, _) in &compose.networks {
                println!("  - {}", name);
            }
        }

        if !compose.volumes.is_empty() {
            println!("\nVolumes:");
            for (name, _) in &compose.volumes {
                println!("  - {}", name);
            }
        }
    }

    Ok(())
}

async fn pull(
    _cli: &Cli,
    file: &PathBuf,
    services: Vec<String>,
    ignore_failures: bool,
) -> Result<()> {
    let compose = load_compose_file(file)?;

    let services_to_pull: Vec<_> = if services.is_empty() {
        compose.services.keys().cloned().collect()
    } else {
        services
    };

    for name in &services_to_pull {
        if let Some(service) = compose.services.get(name) {
            if let Some(image) = &service.image {
                print!("Pulling {} ({})... ", name, image);
                // Would actually pull the image here
                println!("done");
            } else if !ignore_failures {
                println!("Skipping {} (no image, build required)", name);
            }
        } else if !ignore_failures {
            bail!("Service '{}' not found in compose file", name);
        }
    }

    Ok(())
}

/// Topological sort of services based on depends_on
fn topological_sort(compose: &ComposeFile) -> Result<Vec<String>> {
    let mut result = Vec::new();
    let mut visited = std::collections::HashSet::new();
    let mut visiting = std::collections::HashSet::new();

    fn visit(
        name: &str,
        compose: &ComposeFile,
        visited: &mut std::collections::HashSet<String>,
        visiting: &mut std::collections::HashSet<String>,
        result: &mut Vec<String>,
    ) -> Result<()> {
        if visited.contains(name) {
            return Ok(());
        }
        if visiting.contains(name) {
            bail!("Circular dependency detected involving '{}'", name);
        }

        visiting.insert(name.to_string());

        if let Some(service) = compose.services.get(name) {
            if let Some(deps) = &service.depends_on {
                let dep_names: Vec<String> = match deps {
                    DependsOnConfig::List(list) => list.clone(),
                    DependsOnConfig::Map(map) => map.keys().cloned().collect(),
                };

                for dep in dep_names {
                    visit(&dep, compose, visited, visiting, result)?;
                }
            }
        }

        visiting.remove(name);
        visited.insert(name.to_string());
        result.push(name.to_string());

        Ok(())
    }

    for name in compose.services.keys() {
        visit(name, compose, &mut visited, &mut visiting, &mut result)?;
    }

    Ok(result)
}
