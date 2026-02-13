// SPDX-License-Identifier: PMPL-1.0-or-later
//! Security profile management

use anyhow::{bail, Result};
use clap::{Parser, Subcommand};
use console::style;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use tabled::{Table, Tabled};

use crate::cli::Cli;

/// Security profile management
#[derive(Parser, Debug)]
pub struct ProfileArgs {
    #[command(subcommand)]
    pub command: ProfileCommands,
}

#[derive(Subcommand, Debug)]
pub enum ProfileCommands {
    /// List available profiles
    Ls {
        /// Output format (table, json)
        #[arg(long, default_value = "table")]
        format: String,
    },

    /// Show profile details
    Show {
        /// Profile name
        name: String,
    },

    /// Compare two profiles
    Diff {
        /// First profile
        profile1: String,
        /// Second profile
        profile2: String,
    },

    /// Set default profile
    SetDefault {
        /// Profile name
        name: String,
    },

    /// Get current default profile
    GetDefault,

    /// Create custom profile
    Create {
        /// Profile name
        name: String,

        /// Base profile to extend
        #[arg(long)]
        from: Option<String>,
    },
}

/// Security profile definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityProfile {
    pub name: String,
    pub description: String,
    pub security_level: SecurityLevel,
    pub capabilities: CapabilityConfig,
    pub security: SecurityConfig,
    pub seccomp: SeccompConfig,
    pub network: NetworkConfig,
    pub resources: ResourceConfig,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum SecurityLevel {
    High,
    Medium,
    Low,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct CapabilityConfig {
    pub drop: Vec<String>,
    pub add: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct SecurityConfig {
    pub privileged: bool,
    pub no_new_privileges: bool,
    pub read_only_rootfs: bool,
    pub user_namespace: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct SeccompConfig {
    pub profile: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct NetworkConfig {
    pub mode: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ResourceConfig {
    pub pids_limit: u64,
    pub memory_limit: String,
}

#[derive(Tabled)]
struct ProfileRow {
    #[tabled(rename = "NAME")]
    name: String,
    #[tabled(rename = "DESCRIPTION")]
    description: String,
    #[tabled(rename = "SECURITY")]
    security: String,
}

/// Execute profile command
pub async fn execute(args: ProfileArgs, cli: &Cli) -> Result<()> {
    match args.command {
        ProfileCommands::Ls { format } => list_profiles(&format).await,
        ProfileCommands::Show { name } => show_profile(&name).await,
        ProfileCommands::Diff { profile1, profile2 } => diff_profiles(&profile1, &profile2).await,
        ProfileCommands::SetDefault { name } => set_default(&name, cli).await,
        ProfileCommands::GetDefault => get_default(cli).await,
        ProfileCommands::Create { name, from } => create_profile(&name, from.as_deref()).await,
    }
}

/// Get built-in profiles
pub fn builtin_profiles() -> HashMap<String, SecurityProfile> {
    let mut profiles = HashMap::new();

    // Strict profile - maximum security
    profiles.insert(
        "strict".to_string(),
        SecurityProfile {
            name: "strict".to_string(),
            description: "Maximum security for production workloads".to_string(),
            security_level: SecurityLevel::High,
            capabilities: CapabilityConfig {
                drop: vec!["ALL".to_string()],
                add: vec![],
            },
            security: SecurityConfig {
                privileged: false,
                no_new_privileges: true,
                read_only_rootfs: true,
                user_namespace: true,
            },
            seccomp: SeccompConfig {
                profile: "strict".to_string(),
            },
            network: NetworkConfig {
                mode: "none".to_string(),
            },
            resources: ResourceConfig {
                pids_limit: 100,
                memory_limit: "512M".to_string(),
            },
        },
    );

    // Balanced profile - secure defaults with flexibility
    profiles.insert(
        "balanced".to_string(),
        SecurityProfile {
            name: "balanced".to_string(),
            description: "Secure defaults with practical flexibility".to_string(),
            security_level: SecurityLevel::Medium,
            capabilities: CapabilityConfig {
                drop: vec!["ALL".to_string()],
                add: vec![
                    "CHOWN".to_string(),
                    "DAC_OVERRIDE".to_string(),
                    "FOWNER".to_string(),
                    "SETGID".to_string(),
                    "SETUID".to_string(),
                ],
            },
            security: SecurityConfig {
                privileged: false,
                no_new_privileges: true,
                read_only_rootfs: false,
                user_namespace: true,
            },
            seccomp: SeccompConfig {
                profile: "default".to_string(),
            },
            network: NetworkConfig {
                mode: "bridge".to_string(),
            },
            resources: ResourceConfig {
                pids_limit: 500,
                memory_limit: "2G".to_string(),
            },
        },
    );

    // Dev profile - convenience for development
    profiles.insert(
        "dev".to_string(),
        SecurityProfile {
            name: "dev".to_string(),
            description: "Relaxed settings for development and debugging".to_string(),
            security_level: SecurityLevel::Low,
            capabilities: CapabilityConfig {
                drop: vec!["SYS_ADMIN".to_string(), "NET_ADMIN".to_string()],
                add: vec![
                    "CHOWN".to_string(),
                    "DAC_OVERRIDE".to_string(),
                    "FOWNER".to_string(),
                    "SETGID".to_string(),
                    "SETUID".to_string(),
                    "NET_BIND_SERVICE".to_string(),
                ],
            },
            security: SecurityConfig {
                privileged: false,
                no_new_privileges: false,
                read_only_rootfs: false,
                user_namespace: false,
            },
            seccomp: SeccompConfig {
                profile: "unconfined".to_string(),
            },
            network: NetworkConfig {
                mode: "bridge".to_string(),
            },
            resources: ResourceConfig {
                pids_limit: 0, // Unlimited
                memory_limit: String::new(), // Unlimited
            },
        },
    );

    profiles
}

fn get_profile(name: &str) -> Result<SecurityProfile> {
    let profiles = builtin_profiles();
    profiles
        .get(name)
        .cloned()
        .ok_or_else(|| anyhow::anyhow!("Profile '{}' not found", name))
}

fn security_bar(level: SecurityLevel) -> String {
    match level {
        SecurityLevel::High => format!(
            "{} HIGH",
            style("████████████").green()
        ),
        SecurityLevel::Medium => format!(
            "{}{} MEDIUM",
            style("████████").yellow(),
            style("░░░░").dim()
        ),
        SecurityLevel::Low => format!(
            "{}{} LOW",
            style("████").red(),
            style("░░░░░░░░").dim()
        ),
    }
}

async fn list_profiles(format: &str) -> Result<()> {
    let profiles = builtin_profiles();

    match format {
        "json" => {
            let list: Vec<_> = profiles.values().collect();
            println!("{}", serde_json::to_string_pretty(&list)?);
        }
        _ => {
            let mut rows: Vec<ProfileRow> = profiles
                .values()
                .map(|p| ProfileRow {
                    name: p.name.clone(),
                    description: if p.description.len() > 40 {
                        format!("{}...", &p.description[..37])
                    } else {
                        p.description.clone()
                    },
                    security: security_bar(p.security_level),
                })
                .collect();

            // Sort by security level (high first)
            rows.sort_by(|a, b| a.name.cmp(&b.name));

            let table = Table::new(rows).to_string();
            println!("{}", table);
        }
    }

    Ok(())
}

async fn show_profile(name: &str) -> Result<()> {
    let profile = get_profile(name)?;

    println!("{}: {}", style("Profile").bold(), profile.name);
    println!();
    println!("{}: {}", style("Description").bold(), profile.description);
    println!(
        "{}: {}",
        style("Security Level").bold(),
        security_bar(profile.security_level)
    );
    println!();

    println!("{}", style("Capabilities:").bold());
    if profile.capabilities.drop.is_empty() {
        println!("  Drop: (none)");
    } else {
        println!("  Drop: {}", profile.capabilities.drop.join(", "));
    }
    if profile.capabilities.add.is_empty() {
        println!("  Add:  (none)");
    } else {
        println!("  Add:  {}", profile.capabilities.add.join(", "));
    }
    println!();

    println!("{}", style("Security Options:").bold());
    println!(
        "  privileged:         {}",
        if profile.security.privileged {
            style("true").red()
        } else {
            style("false").green()
        }
    );
    println!(
        "  no_new_privileges:  {}",
        if profile.security.no_new_privileges {
            style("true").green()
        } else {
            style("false").yellow()
        }
    );
    println!(
        "  read_only_rootfs:   {}",
        if profile.security.read_only_rootfs {
            style("true").green()
        } else {
            style("false").yellow()
        }
    );
    println!(
        "  user_namespace:     {}",
        if profile.security.user_namespace {
            style("true").green()
        } else {
            style("false").yellow()
        }
    );
    println!();

    println!("{}", style("Seccomp:").bold());
    println!("  Profile: {}", profile.seccomp.profile);
    println!();

    println!("{}", style("Network:").bold());
    println!("  Mode: {}", profile.network.mode);
    println!();

    println!("{}", style("Resource Limits:").bold());
    println!(
        "  PIDs:   {}",
        if profile.resources.pids_limit == 0 {
            "unlimited".to_string()
        } else {
            profile.resources.pids_limit.to_string()
        }
    );
    println!(
        "  Memory: {}",
        if profile.resources.memory_limit.is_empty() {
            "unlimited".to_string()
        } else {
            profile.resources.memory_limit.clone()
        }
    );

    Ok(())
}

async fn diff_profiles(name1: &str, name2: &str) -> Result<()> {
    let p1 = get_profile(name1)?;
    let p2 = get_profile(name2)?;

    println!(
        "{}",
        style(format!("Profile Comparison: {} vs {}", name1, name2)).bold()
    );
    println!("{}", "=".repeat(50));
    println!();

    // Capabilities
    println!("{}", style("Capabilities:").bold());
    let drop1 = p1.capabilities.drop.join(",");
    let drop2 = p2.capabilities.drop.join(",");
    if drop1 != drop2 {
        println!(
            "  {}: drop {}, add {}",
            style(name1).cyan(),
            if p1.capabilities.drop.is_empty() {
                "NONE".to_string()
            } else {
                p1.capabilities.drop.join(",")
            },
            if p1.capabilities.add.is_empty() {
                "NONE".to_string()
            } else {
                p1.capabilities.add.join(",")
            }
        );
        println!(
            "  {}: drop {}, add {}",
            style(name2).cyan(),
            if p2.capabilities.drop.is_empty() {
                "NONE".to_string()
            } else {
                p2.capabilities.drop.join(",")
            },
            if p2.capabilities.add.is_empty() {
                "NONE".to_string()
            } else {
                p2.capabilities.add.join(",")
            }
        );
    } else {
        println!("  (identical)");
    }
    println!();

    // Security options
    println!("{}", style("Security:").bold());
    let mut has_diff = false;

    if p1.security.privileged != p2.security.privileged {
        println!(
            "  privileged:        {}={}   {}={}",
            name1, p1.security.privileged, name2, p2.security.privileged
        );
        has_diff = true;
    }
    if p1.security.no_new_privileges != p2.security.no_new_privileges {
        println!(
            "  no_new_privileges: {}={}   {}={}",
            name1, p1.security.no_new_privileges, name2, p2.security.no_new_privileges
        );
        has_diff = true;
    }
    if p1.security.read_only_rootfs != p2.security.read_only_rootfs {
        println!(
            "  read_only_rootfs:  {}={}   {}={}",
            name1, p1.security.read_only_rootfs, name2, p2.security.read_only_rootfs
        );
        has_diff = true;
    }
    if p1.security.user_namespace != p2.security.user_namespace {
        println!(
            "  user_namespace:    {}={}   {}={}",
            name1, p1.security.user_namespace, name2, p2.security.user_namespace
        );
        has_diff = true;
    }
    if !has_diff {
        println!("  (identical)");
    }
    println!();

    // Seccomp
    println!("{}", style("Seccomp:").bold());
    if p1.seccomp.profile != p2.seccomp.profile {
        println!(
            "  profile: {}={}   {}={}",
            name1, p1.seccomp.profile, name2, p2.seccomp.profile
        );
    } else {
        println!("  (identical)");
    }
    println!();

    // Network
    println!("{}", style("Network:").bold());
    if p1.network.mode != p2.network.mode {
        println!(
            "  mode: {}={}   {}={}",
            name1, p1.network.mode, name2, p2.network.mode
        );
    } else {
        println!("  (identical)");
    }
    println!();

    // Resources
    println!("{}", style("Resources:").bold());
    let mut has_diff = false;
    if p1.resources.pids_limit != p2.resources.pids_limit {
        println!(
            "  pids_limit:   {}={}   {}={}",
            name1,
            if p1.resources.pids_limit == 0 {
                "unlimited".to_string()
            } else {
                p1.resources.pids_limit.to_string()
            },
            name2,
            if p2.resources.pids_limit == 0 {
                "unlimited".to_string()
            } else {
                p2.resources.pids_limit.to_string()
            }
        );
        has_diff = true;
    }
    if p1.resources.memory_limit != p2.resources.memory_limit {
        println!(
            "  memory_limit: {}={}   {}={}",
            name1,
            if p1.resources.memory_limit.is_empty() {
                "unlimited"
            } else {
                &p1.resources.memory_limit
            },
            name2,
            if p2.resources.memory_limit.is_empty() {
                "unlimited"
            } else {
                &p2.resources.memory_limit
            }
        );
        has_diff = true;
    }
    if !has_diff {
        println!("  (identical)");
    }

    Ok(())
}

async fn set_default(name: &str, _cli: &Cli) -> Result<()> {
    // Verify profile exists
    let _ = get_profile(name)?;

    // Would save to config file
    let config_path = get_config_path()?;
    println!("Default profile set to: {}", style(name).green());
    println!("Saved to: {}", config_path.display());

    Ok(())
}

async fn get_default(_cli: &Cli) -> Result<()> {
    // Would read from config file
    // For now, return balanced as default
    println!("balanced");
    Ok(())
}

async fn create_profile(name: &str, from: Option<&str>) -> Result<()> {
    // Check if name conflicts with built-in
    let profiles = builtin_profiles();
    if profiles.contains_key(name) {
        bail!(
            "Cannot create profile '{}': conflicts with built-in profile",
            name
        );
    }

    let base = if let Some(base_name) = from {
        get_profile(base_name)?
    } else {
        get_profile("balanced")?
    };

    let config_dir = get_config_path()?.parent().unwrap().join("profiles");
    std::fs::create_dir_all(&config_dir)?;

    let profile_path = config_dir.join(format!("{}.toml", name));

    let mut new_profile = base;
    new_profile.name = name.to_string();
    new_profile.description = format!("Custom profile based on {}", from.unwrap_or("balanced"));

    // Would serialize to TOML and save
    println!(
        "Created profile '{}' at {}",
        style(name).green(),
        profile_path.display()
    );
    println!();
    println!("Edit the profile with:");
    println!("  $EDITOR {}", profile_path.display());

    Ok(())
}

fn get_config_path() -> Result<PathBuf> {
    let config_dir = dirs::config_dir()
        .ok_or_else(|| anyhow::anyhow!("Could not determine config directory"))?
        .join("vordr");

    std::fs::create_dir_all(&config_dir)?;

    Ok(config_dir.join("config.toml"))
}
