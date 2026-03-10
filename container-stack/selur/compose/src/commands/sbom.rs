// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose sbom` command implementation
//!
//! Generates or displays SBOMs (Software Bill of Materials) for compose services
//! by delegating to Cerro Torre.

use anyhow::{Context, Result};

use crate::compose::ComposeFile;
use crate::ct::CtClient;

/// Run `sbom` command - generate/display SBOM for a service
pub async fn sbom(
    compose: &ComposeFile,
    service_name: &str,
    output_format: &str,
) -> Result<()> {
    tracing::info!("Generating SBOM for service '{}'", service_name);

    // Validate service exists
    let service = compose
        .services
        .get(service_name)
        .ok_or_else(|| anyhow::anyhow!("Unknown service: {}", service_name))?;

    let ct_client = CtClient::new();

    // Generate SBOM via Cerro Torre
    let sbom_json = ct_client
        .sbom(&service.image)
        .await
        .with_context(|| format!("Failed to generate SBOM for {}", service_name))?;

    // Output based on requested format
    match output_format {
        "json" => {
            // Raw JSON output
            println!("{}", sbom_json);
        }
        "cyclonedx" => {
            // CycloneDX format (ct already produces CycloneDX by default)
            println!("{}", sbom_json);
        }
        "spdx" => {
            // SPDX format - request conversion via ct
            let ct_client_spdx = CtClient::new();
            let args = vec![
                "sbom".to_string(),
                "--format".to_string(),
                "spdx".to_string(),
                service.image.clone(),
            ];
            match ct_client_spdx.run_command(&args).await {
                Ok(_) => {}
                Err(e) => {
                    tracing::warn!("SPDX conversion failed, falling back to CycloneDX: {}", e);
                    println!("{}", sbom_json);
                }
            }
        }
        _ => {
            anyhow::bail!(
                "Unknown SBOM format: {} (expected 'json', 'cyclonedx', or 'spdx')",
                output_format
            );
        }
    }

    eprintln!();
    eprintln!("SBOM generated for {} ({})", service_name, service.image);

    Ok(())
}

/// Generate SBOMs for all services in the compose file
pub async fn sbom_all(compose: &ComposeFile, output_format: &str) -> Result<()> {
    tracing::info!("Generating SBOMs for all services");

    let ct_client = CtClient::new();

    let mut succeeded = 0;
    let mut failed = 0;

    for (service_name, service) in &compose.services {
        eprint!("Generating SBOM for {} ... ", service_name);

        match ct_client.sbom(&service.image).await {
            Ok(sbom_json) => {
                eprintln!("done");

                match output_format {
                    "json" => {
                        println!("--- {} ---", service_name);
                        println!("{}", sbom_json);
                        println!();
                    }
                    _ => {
                        println!("--- {} ---", service_name);
                        println!("{}", sbom_json);
                        println!();
                    }
                }

                succeeded += 1;
            }
            Err(e) => {
                eprintln!("FAILED: {}", e);
                failed += 1;
            }
        }
    }

    eprintln!();
    eprintln!("SBOM generation complete: {} succeeded, {} failed", succeeded, failed);

    if failed > 0 {
        anyhow::bail!("{} service(s) failed SBOM generation", failed);
    }

    Ok(())
}
