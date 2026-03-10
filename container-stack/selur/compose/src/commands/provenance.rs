// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose provenance` command implementation
//!
//! Shows the SLSA provenance chain for service images using Cerro Torre.

use anyhow::{Context, Result};

use crate::compose::ComposeFile;
use crate::ct::CtClient;

/// Run `provenance` command - show provenance chain for a service image
pub async fn provenance(
    compose: &ComposeFile,
    service_name: &str,
) -> Result<()> {
    tracing::info!("Showing provenance for service '{}'", service_name);

    // Validate service exists
    let service = compose
        .services
        .get(service_name)
        .ok_or_else(|| anyhow::anyhow!("Unknown service: {}", service_name))?;

    let ct_client = CtClient::new();

    // Fetch provenance via Cerro Torre
    let provenance_json = ct_client
        .provenance(&service.image)
        .await
        .with_context(|| format!("Failed to get provenance for {}", service_name))?;

    // Parse and pretty-print the provenance
    let provenance: serde_json::Value = serde_json::from_str(&provenance_json)
        .with_context(|| "Failed to parse provenance JSON")?;

    println!("Provenance for {} ({})", service_name, service.image);
    println!("=".repeat(60));
    println!();

    // Display builder info if present
    if let Some(builder) = provenance.get("builder") {
        println!("Builder:");
        if let Some(id) = builder.get("id").and_then(|v| v.as_str()) {
            println!("  ID: {}", id);
        }
    }

    // Display build type
    if let Some(build_type) = provenance.get("buildType").and_then(|v| v.as_str()) {
        println!("Build Type: {}", build_type);
    }

    // Display invocation (entry point, parameters)
    if let Some(invocation) = provenance.get("invocation") {
        println!();
        println!("Invocation:");
        if let Some(config_source) = invocation.get("configSource") {
            if let Some(uri) = config_source.get("uri").and_then(|v| v.as_str()) {
                println!("  Source: {}", uri);
            }
            if let Some(digest) = config_source.get("digest") {
                if let Some(sha256) = digest.get("sha256").and_then(|v| v.as_str()) {
                    println!("  Digest: sha256:{}", sha256);
                }
            }
            if let Some(entry_point) = config_source.get("entryPoint").and_then(|v| v.as_str()) {
                println!("  Entry Point: {}", entry_point);
            }
        }
    }

    // Display materials (dependencies)
    if let Some(materials) = provenance.get("materials").and_then(|v| v.as_array()) {
        println!();
        println!("Materials ({}):", materials.len());
        for material in materials {
            if let Some(uri) = material.get("uri").and_then(|v| v.as_str()) {
                print!("  - {}", uri);
                if let Some(digest) = material.get("digest") {
                    if let Some(sha256) = digest.get("sha256").and_then(|v| v.as_str()) {
                        print!(" (sha256:{}...)", &sha256[..std::cmp::min(12, sha256.len())]);
                    }
                }
                println!();
            }
        }
    }

    // Display metadata (build timestamps)
    if let Some(metadata) = provenance.get("metadata") {
        println!();
        println!("Metadata:");
        if let Some(started) = metadata.get("buildStartedOn").and_then(|v| v.as_str()) {
            println!("  Build Started:  {}", started);
        }
        if let Some(finished) = metadata.get("buildFinishedOn").and_then(|v| v.as_str()) {
            println!("  Build Finished: {}", finished);
        }
        if let Some(reproducible) = metadata.get("reproducible").and_then(|v| v.as_bool()) {
            println!("  Reproducible:   {}", reproducible);
        }
    }

    // Also list all attestations for completeness
    println!();
    println!("Attestations:");
    match ct_client.list_attestations(&service.image).await {
        Ok(attestations_json) => {
            let attestations: serde_json::Value =
                serde_json::from_str(&attestations_json).unwrap_or(serde_json::Value::Null);

            if let Some(arr) = attestations.as_array() {
                for att in arr {
                    if let Some(att_type) = att.get("type").and_then(|v| v.as_str()) {
                        println!("  - {}", att_type);
                    }
                }
            } else {
                println!("  (none found or unable to parse)");
            }
        }
        Err(e) => {
            tracing::debug!("Failed to list attestations: {}", e);
            println!("  (unable to fetch attestations)");
        }
    }

    println!();

    Ok(())
}
