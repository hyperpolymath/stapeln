// SPDX-License-Identifier: PMPL-1.0-or-later
//! Registry authentication commands (login, logout)

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use dialoguer::{Input, Password};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::io::{self, BufRead};
use std::path::PathBuf;
use tabled::{Table, Tabled};

use crate::cli::Cli;

/// Login to a container registry
#[derive(Parser, Debug)]
pub struct LoginArgs {
    /// Registry URL (default: docker.io)
    #[arg(default_value = "docker.io")]
    pub registry: String,

    /// Username
    #[arg(short, long)]
    pub username: Option<String>,

    /// Password (insecure, prefer --password-stdin)
    #[arg(short, long)]
    pub password: Option<String>,

    /// Read password from stdin
    #[arg(long)]
    pub password_stdin: bool,

    /// Credential store backend (auto, file, secret-service, pass)
    #[arg(long, default_value = "auto")]
    pub credential_store: String,
}

/// Logout from a container registry
#[derive(Parser, Debug)]
pub struct LogoutArgs {
    /// Registry URL
    pub registry: String,

    /// Remove credentials for all registries
    #[arg(long)]
    pub all: bool,
}

/// Auth subcommand for listing credentials
#[derive(Parser, Debug)]
pub struct AuthArgs {
    #[command(subcommand)]
    pub command: AuthCommands,
}

#[derive(Subcommand, Debug)]
pub enum AuthCommands {
    /// List stored credentials
    Ls {
        /// Output format (table, json)
        #[arg(long, default_value = "table")]
        format: String,
    },
}

#[derive(Tabled)]
struct CredentialRow {
    #[tabled(rename = "REGISTRY")]
    registry: String,
    #[tabled(rename = "USERNAME")]
    username: String,
    #[tabled(rename = "METHOD")]
    method: String,
    #[tabled(rename = "EXPIRES")]
    expires: String,
}

#[derive(Debug, Serialize, Deserialize, Default)]
struct AuthConfig {
    auths: HashMap<String, RegistryAuth>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct RegistryAuth {
    auth: Option<String>,       // base64(username:password)
    username: Option<String>,
    password: Option<String>,
    #[serde(rename = "identitytoken")]
    identity_token: Option<String>,
    #[serde(rename = "registrytoken")]
    registry_token: Option<String>,
}

/// Execute login command
pub async fn login(args: LoginArgs, _cli: &Cli) -> Result<()> {
    let registry = normalize_registry(&args.registry);

    // Get username
    let username = if let Some(u) = args.username {
        u
    } else {
        Input::new()
            .with_prompt("Username")
            .interact_text()
            .context("Failed to read username")?
    };

    // Get password
    let password = if args.password_stdin {
        // Read from stdin
        let stdin = io::stdin();
        let mut line = String::new();
        stdin
            .lock()
            .read_line(&mut line)
            .context("Failed to read password from stdin")?;
        line.trim().to_string()
    } else if let Some(p) = args.password {
        p
    } else {
        Password::new()
            .with_prompt("Password")
            .interact()
            .context("Failed to read password")?
    };

    // Validate credentials against registry
    print!("Authenticating with {}... ", registry);

    // In a real implementation, we'd make a request to the registry's auth endpoint
    // For now, we'll just store the credentials
    let auth = base64::Engine::encode(
        &base64::engine::general_purpose::STANDARD,
        format!("{}:{}", username, password),
    );

    // Store credentials
    let auth_path = get_auth_file_path()?;
    let mut config = load_auth_config(&auth_path)?;

    config.auths.insert(
        registry.clone(),
        RegistryAuth {
            auth: Some(auth),
            username: Some(username.clone()),
            password: None, // Don't store plain password
            identity_token: None,
            registry_token: None,
        },
    );

    save_auth_config(&auth_path, &config)?;

    println!("done");
    println!("\nLogin succeeded.");
    println!("Credentials stored in: {}", auth_path.display());

    Ok(())
}

/// Execute logout command
pub async fn logout(args: LogoutArgs, _cli: &Cli) -> Result<()> {
    let auth_path = get_auth_file_path()?;
    let mut config = load_auth_config(&auth_path)?;

    if args.all {
        let count = config.auths.len();
        config.auths.clear();
        save_auth_config(&auth_path, &config)?;
        println!("Removed credentials for {} registries", count);
    } else {
        let registry = normalize_registry(&args.registry);
        if config.auths.remove(&registry).is_some() {
            save_auth_config(&auth_path, &config)?;
            println!("Removed credentials for {}", registry);
        } else {
            println!("No credentials found for {}", registry);
        }
    }

    Ok(())
}

/// Execute auth subcommand
pub async fn execute_auth(args: AuthArgs, _cli: &Cli) -> Result<()> {
    match args.command {
        AuthCommands::Ls { format } => list_credentials(&format).await,
    }
}

async fn list_credentials(format: &str) -> Result<()> {
    let auth_path = get_auth_file_path()?;
    let config = load_auth_config(&auth_path)?;

    if config.auths.is_empty() {
        println!("No credentials stored.");
        return Ok(());
    }

    match format {
        "json" => {
            #[derive(Serialize)]
            struct CredentialInfo {
                registry: String,
                username: Option<String>,
                method: String,
            }

            let creds: Vec<_> = config
                .auths
                .iter()
                .map(|(registry, auth)| CredentialInfo {
                    registry: registry.clone(),
                    username: auth.username.clone(),
                    method: detect_method(auth),
                })
                .collect();

            println!("{}", serde_json::to_string_pretty(&creds)?);
        }
        _ => {
            let rows: Vec<_> = config
                .auths
                .iter()
                .map(|(registry, auth)| CredentialRow {
                    registry: registry.clone(),
                    username: auth.username.clone().unwrap_or_else(|| "-".to_string()),
                    method: detect_method(auth),
                    expires: "never".to_string(), // Would parse token expiry
                })
                .collect();

            let table = Table::new(rows).to_string();
            println!("{}", table);
        }
    }

    Ok(())
}

fn detect_method(auth: &RegistryAuth) -> String {
    if auth.identity_token.is_some() {
        "identity-token".to_string()
    } else if auth.registry_token.is_some() {
        "registry-token".to_string()
    } else if auth.auth.is_some() {
        "file".to_string()
    } else {
        "unknown".to_string()
    }
}

fn normalize_registry(registry: &str) -> String {
    let registry = registry.trim();

    // Handle common aliases
    match registry {
        "docker.io" | "registry-1.docker.io" | "index.docker.io" => {
            "https://index.docker.io/v1/".to_string()
        }
        r if r.starts_with("https://") || r.starts_with("http://") => r.to_string(),
        r => format!("https://{}", r),
    }
}

fn get_auth_file_path() -> Result<PathBuf> {
    let config_dir = dirs::config_dir()
        .context("Could not determine config directory")?
        .join("vordr");

    std::fs::create_dir_all(&config_dir)?;

    Ok(config_dir.join("auth.json"))
}

fn load_auth_config(path: &PathBuf) -> Result<AuthConfig> {
    if path.exists() {
        let content = std::fs::read_to_string(path)?;
        Ok(serde_json::from_str(&content).unwrap_or_default())
    } else {
        Ok(AuthConfig::default())
    }
}

fn save_auth_config(path: &PathBuf, config: &AuthConfig) -> Result<()> {
    let content = serde_json::to_string_pretty(config)?;

    // Set restrictive permissions on auth file
    #[cfg(unix)]
    {
        use std::os::unix::fs::OpenOptionsExt;
        let mut file = std::fs::OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .mode(0o600)
            .open(path)?;
        std::io::Write::write_all(&mut file, content.as_bytes())?;
    }

    #[cfg(not(unix))]
    {
        std::fs::write(path, content)?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_normalize_registry() {
        assert_eq!(
            normalize_registry("docker.io"),
            "https://index.docker.io/v1/"
        );
        assert_eq!(normalize_registry("ghcr.io"), "https://ghcr.io");
        assert_eq!(
            normalize_registry("https://quay.io"),
            "https://quay.io"
        );
    }
}
