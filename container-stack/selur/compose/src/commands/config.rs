// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose config` command implementation
//!
//! Validates the compose file and displays the resolved (merged) configuration.

use anyhow::Result;

use crate::compose::ComposeFile;
use crate::graph::build_dependency_graph;

/// Run `config` command - validate and display resolved compose file
pub async fn config(compose: &ComposeFile, output_format: &str) -> Result<()> {
    tracing::info!("Validating compose file...");

    // Validate dependency graph (catches circular dependencies)
    let deployment_order = build_dependency_graph(compose)?;

    // Display based on format
    match output_format {
        "json" => {
            let json = serde_json::to_string_pretty(compose)?;
            println!("{}", json);
        }
        "toml" => {
            let toml_str = toml::to_string_pretty(compose)?;
            println!("{}", toml_str);
        }
        _ => {
            anyhow::bail!("Unknown output format: {} (expected 'toml' or 'json')", output_format);
        }
    }

    // Print validation summary to stderr so it does not pollute piped output
    eprintln!();
    eprintln!("Compose file is valid");
    eprintln!("  Version:    {}", compose.version);
    eprintln!("  Services:   {}", compose.services.len());
    eprintln!("  Networks:   {}", compose.networks.len());
    eprintln!("  Volumes:    {}", compose.volumes.len());
    eprintln!("  Secrets:    {}", compose.secrets.len());
    eprintln!("  Deploy order: {}", deployment_order.join(" -> "));

    // Additional diagnostics
    let services_with_build: Vec<&String> = compose
        .services
        .iter()
        .filter(|(_, s)| s.build.is_some())
        .map(|(name, _)| name)
        .collect();

    let services_with_healthcheck: Vec<&String> = compose
        .services
        .iter()
        .filter(|(_, s)| s.healthcheck.is_some())
        .map(|(name, _)| name)
        .collect();

    if !services_with_build.is_empty() {
        eprintln!("  Buildable:  {:?}", services_with_build);
    }
    if !services_with_healthcheck.is_empty() {
        eprintln!("  Healthchecked: {:?}", services_with_healthcheck);
    }

    Ok(())
}
