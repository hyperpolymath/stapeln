// SPDX-License-Identifier: PMPL-1.0-or-later
//! `vordr run` command implementation

use anyhow::{Context, Result};
use clap::Args;
use std::path::Path;
use tracing::info;

use crate::cli::{Cli, pull_image};
use crate::engine::{ContainerState, OciConfigBuilder, StateManager};
use crate::ffi::{ConfigValidator, NetworkMode};
use crate::runtime::DirectRuntime;

/// Arguments for the `run` command
#[derive(Args, Debug)]
pub struct RunArgs {
    /// Image to run
    pub image: String,

    /// Assign a name to the container
    #[arg(long)]
    pub name: Option<String>,

    /// Run container in the background
    #[arg(short, long)]
    pub detach: bool,

    /// Run as privileged container (bypasses security checks)
    #[arg(long)]
    pub privileged: bool,

    /// Set user ID
    #[arg(short, long)]
    pub user: Option<String>,

    /// Enable user namespace (default: true)
    #[arg(long, default_value = "true")]
    pub userns: bool,

    /// Add a capability
    #[arg(long = "cap-add", action = clap::ArgAction::Append)]
    pub cap_add: Vec<String>,

    /// Drop a capability
    #[arg(long = "cap-drop", action = clap::ArgAction::Append)]
    pub cap_drop: Vec<String>,

    /// Mount a volume
    #[arg(short = 'v', long = "volume", action = clap::ArgAction::Append)]
    pub volumes: Vec<String>,

    /// Set environment variables
    #[arg(short = 'e', long = "env", action = clap::ArgAction::Append)]
    pub env: Vec<String>,

    /// Working directory inside the container
    #[arg(short = 'w', long)]
    pub workdir: Option<String>,

    /// Connect to a network
    #[arg(long)]
    pub network: Option<String>,

    /// Publish a port (host:container)
    #[arg(short = 'p', long = "publish", action = clap::ArgAction::Append)]
    pub ports: Vec<String>,

    /// Read-only root filesystem
    #[arg(long)]
    pub read_only: bool,

    /// Disable network
    #[arg(long)]
    pub no_network: bool,

    /// Memory limit (e.g., 512M, 2G)
    #[arg(long)]
    pub memory_limit: Option<String>,

    /// CPU limit (percentage of one core, e.g., 50 for 50%)
    #[arg(long)]
    pub cpu_limit: Option<u32>,

    /// Command and arguments to run
    #[arg(trailing_var_arg = true)]
    pub command: Vec<String>,
}

pub async fn execute(args: RunArgs, cli: &Cli) -> Result<()> {
    info!("Running container from image: {}", args.image);

    // Parse user ID
    let user_id = if let Some(ref user) = args.user {
        user.parse::<u32>()
            .context("Invalid user ID")?
    } else {
        1000 // Default non-root user
    };

    // Build and validate configuration through the gatekeeper
    let network_mode = if args.no_network {
        NetworkMode::Unprivileged
    } else if args.privileged {
        NetworkMode::Admin
    } else {
        NetworkMode::Restricted
    };

    let mut validator = ConfigValidator::new()
        .privileged(args.privileged)
        .user_namespace(args.userns)
        .user_id(user_id)
        .network_mode(network_mode)
        .readonly_rootfs(args.read_only);

    // Add capabilities
    for cap in &args.cap_add {
        validator = validator.add_capability(cap);
    }

    // Validate through the formally verified gatekeeper
    let validated_config = validator.validate()
        .context("Security validation failed")?;

    info!("Configuration validated by gatekeeper");

    // Generate container ID and name
    let container_id = generate_container_id();
    let container_name = args.name.unwrap_or_else(|| generate_container_name());

    // Ensure state directory exists
    let root_path = Path::new(&cli.root);
    std::fs::create_dir_all(root_path)
        .context("Failed to create root directory")?;

    // Ensure database directory exists
    let db_path = Path::new(&cli.db_path);
    if let Some(parent) = db_path.parent() {
        std::fs::create_dir_all(parent)
            .context("Failed to create database directory")?;
    }

    // Open state database
    let state = StateManager::open(db_path)
        .context("Failed to open state database")?;

    // Create bundle directory
    let bundle_path = root_path.join("containers").join(&container_id);
    std::fs::create_dir_all(&bundle_path)
        .context("Failed to create bundle directory")?;

    // Resolve image: check local state first, pull via registry if absent.
    let image_id = match state.find_image_by_tag(&args.image) {
        Ok(existing) => {
            info!("Image {} found locally ({})", args.image, existing.id);
            existing.id
        }
        Err(_) => {
            info!("Image {} not found locally, pulling...", args.image);
            let pulled_id = pull_image(&args.image, cli).await
                .context("Failed to pull image")?;
            let digest = format!("sha256:{}", hex::encode(pulled_id.as_bytes()));
            let image_path = root_path.join("images").join(&pulled_id);
            std::fs::create_dir_all(&image_path)
                .context("Failed to create image directory")?;
            state.create_image(
                &pulled_id,
                &digest,
                Some(&args.image),
                &[args.image.clone()],
                0,
                Some(image_path.to_str().unwrap_or("")),
            )?;
            info!("Image {} stored as {}", args.image, pulled_id);
            pulled_id
        }
    };

    // Create container record
    let config_json = serde_json::json!({
        "image": args.image,
        "command": args.command,
        "env": args.env,
        "volumes": args.volumes,
        "ports": args.ports,
        "privileged": validated_config.privileged,
        "user": user_id,
        "userns": validated_config.user_namespace,
    });

    state.create_container(
        &container_id,
        &container_name,
        &image_id,
        bundle_path.to_str()
            .ok_or_else(|| anyhow::anyhow!("Invalid bundle path - contains invalid UTF-8"))?,
        Some(&config_json.to_string()),
    ).context("Failed to create container record")?;

    info!("Created container {} ({})", container_name, container_id);

    // Generate proper OCI config.json in bundle directory
    let rootfs_path = bundle_path.join("rootfs");
    std::fs::create_dir_all(&rootfs_path)
        .context("Failed to create rootfs directory")?;

    // Build OCI spec from validated config
    let mut oci_builder = OciConfigBuilder::from_validated(&validated_config)
        .rootfs("rootfs")
        .hostname(&container_name)
        .terminal(!args.detach);

    // Only set explicit uid if not using user namespace
    // (user namespace already sets uid=0 which maps to host user)
    if !args.userns {
        oci_builder = oci_builder.uid(user_id);
    }

    if !args.command.is_empty() {
        oci_builder = oci_builder.command(args.command.clone());
    }

    // Add environment variables
    oci_builder = oci_builder.env(args.env.clone());

    // Add capabilities
    for cap in &args.cap_add {
        oci_builder = oci_builder.add_capability(cap);
    }
    for cap in &args.cap_drop {
        oci_builder = oci_builder.drop_capability(cap);
    }

    // Set read-only rootfs
    if args.read_only {
        oci_builder = oci_builder.readonly_rootfs(true);
    }

    // Set resource limits
    if let Some(ref memory) = args.memory_limit {
        oci_builder = oci_builder.memory_limit(memory);
    }
    if let Some(cpu) = args.cpu_limit {
        oci_builder = oci_builder.cpu_limit(cpu);
    }

    // Write OCI config.json
    let config_path = bundle_path.join("config.json");
    oci_builder.write_to_file(&config_path)
        .context("Failed to write OCI config")?;

    info!("Generated OCI config at {}", config_path.display());

    // Resolve the OCI runtime (crun or youki) — try DirectRuntime first,
    // fall back to legacy ShimClient if the configured runtime is unavailable.
    let bundle_str = bundle_path.to_str()
        .ok_or_else(|| anyhow::anyhow!("Invalid bundle path - contains invalid UTF-8"))?;

    let direct_rt = DirectRuntime::with_runtime(&cli.runtime);

    if args.detach {
        // Detached mode: create + start, print container ID, return.
        let start_result = match &direct_rt {
            Ok(rt) => {
                info!("Using DirectRuntime ({}) for container {}", rt.name(), container_id);
                rt.create_and_start(&container_id, bundle_str).await
            }
            Err(e) => {
                info!("DirectRuntime unavailable ({}), falling back to ShimClient", e);
                let shim = crate::runtime::ShimClient::new(&cli.runtime, bundle_str);
                shim.create_and_start(&container_id)
                    .await
                    .map_err(|e| anyhow::anyhow!("{}", e))
            }
        };

        match start_result {
            Ok(pid) => {
                state.set_container_state(&container_id, ContainerState::Running, Some(pid as i32))?;
                println!("{}", container_id);
            }
            Err(e) => {
                state.set_container_state(&container_id, ContainerState::Stopped, None)?;
                return Err(anyhow::anyhow!("Failed to start container: {}", e));
            }
        }
    } else {
        // Foreground mode: create + start, wait for exit.
        let start_result = match &direct_rt {
            Ok(rt) => {
                info!("Using DirectRuntime ({}) for container {}", rt.name(), container_id);
                rt.create_and_start(&container_id, bundle_str).await
            }
            Err(e) => {
                info!("DirectRuntime unavailable ({}), falling back to ShimClient", e);
                let shim = crate::runtime::ShimClient::new(&cli.runtime, bundle_str);
                shim.create_and_start(&container_id)
                    .await
                    .map_err(|e| anyhow::anyhow!("{}", e))
            }
        };

        match start_result {
            Ok(pid) => {
                state.set_container_state(&container_id, ContainerState::Running, Some(pid as i32))?;
                info!("Container {} started (PID: {})", container_name, pid);

                // Wait for container to exit.
                // DirectRuntime: poll `state` until stopped.
                // ShimClient fallback: use its `wait` method.
                let exit_code = match &direct_rt {
                    Ok(rt) => {
                        wait_for_exit(rt, &container_id).await
                            .context("Failed to wait for container")?
                    }
                    Err(_) => {
                        let shim = crate::runtime::ShimClient::new(&cli.runtime, bundle_str);
                        shim.wait(&container_id).await
                            .map_err(|e| anyhow::anyhow!("{}", e))
                            .context("Failed to wait for container")?
                    }
                };

                state.set_container_state(&container_id, ContainerState::Stopped, None)?;
                info!("Container {} exited with code {}", container_name, exit_code);

                if exit_code != 0 {
                    std::process::exit(exit_code);
                }
            }
            Err(e) => {
                state.set_container_state(&container_id, ContainerState::Stopped, None)?;
                return Err(anyhow::anyhow!("Failed to start container: {}", e));
            }
        }
    }

    Ok(())
}

/// Poll the DirectRuntime until the container exits, returning the exit code.
///
/// The OCI runtime `state` command returns a JSON blob with a `status` field.
/// We poll every 100ms until the status is "stopped" (or the container
/// disappears).
async fn wait_for_exit(rt: &DirectRuntime, container_id: &str) -> Result<i32> {
    loop {
        match rt.get_status(container_id).await {
            Ok(status) => {
                if status == "stopped" {
                    // Try to extract exit code from state JSON
                    if let Ok(json_str) = rt.state(container_id).await {
                        if let Ok(value) = serde_json::from_str::<serde_json::Value>(&json_str) {
                            // OCI state doesn't mandate an exit code field, but
                            // some runtimes include it.  Default to 0 for
                            // "stopped" without an explicit code.
                            let code = value
                                .get("exit_code")
                                .or_else(|| value.get("exitCode"))
                                .and_then(|v| v.as_i64())
                                .unwrap_or(0) as i32;
                            return Ok(code);
                        }
                    }
                    return Ok(0);
                }
            }
            Err(e) => {
                // If the container no longer exists the runtime returns an
                // error — treat as clean exit.
                let msg = e.to_string();
                if msg.contains("does not exist") || msg.contains("not found") {
                    return Ok(0);
                }
                return Err(e);
            }
        }

        tokio::time::sleep(std::time::Duration::from_millis(100)).await;
    }
}

/// Generate a unique container ID (64 hex characters)
fn generate_container_id() -> String {
    use sha2::{Sha256, Digest};

    let mut hasher = Sha256::new();
    hasher.update(uuid::Uuid::new_v4().as_bytes());
    hasher.update(std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos()
        .to_le_bytes());

    hex::encode(hasher.finalize())
}

/// Generate a random container name (adjective_noun format)
fn generate_container_name() -> String {
    let adjectives = [
        "brave", "calm", "eager", "fair", "gentle",
        "happy", "jolly", "kind", "lively", "merry",
        "nice", "proud", "quick", "sharp", "witty",
    ];
    let nouns = [
        "bear", "crane", "deer", "eagle", "falcon",
        "goose", "heron", "ibis", "jay", "kite",
        "lark", "myna", "owl", "panda", "quail",
    ];

    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let mut hasher = DefaultHasher::new();
    std::time::SystemTime::now().hash(&mut hasher);
    let hash = hasher.finish();

    let adj = adjectives[(hash % adjectives.len() as u64) as usize];
    let noun = nouns[((hash >> 8) % nouns.len() as u64) as usize];

    format!("{}_{}", adj, noun)
}
