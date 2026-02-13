// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose pull` command implementation

use anyhow::Result;

use crate::compose::ComposeFile;
use crate::ct::CtClient;

/// Run `pull` command - pull .ctp bundles from registry
pub async fn pull(
    compose: &ComposeFile,
    _project_name: &str,
    services: Vec<String>,
) -> Result<()> {
    tracing::info!("Pulling .ctp bundles from registry");

    // Determine which services to pull
    let target_services: Vec<String> = if services.is_empty() {
        compose.services.keys().cloned().collect()
    } else {
        // Validate that all requested services exist
        for service in &services {
            if !compose.services.contains_key(service) {
                anyhow::bail!("Unknown service: {}", service);
            }
        }
        services
    };

    tracing::info!("Pulling {} service(s)", target_services.len());

    // Initialize ct client
    let ct_client = CtClient::new();

    // Pull each service
    let mut pulled = 0;
    let mut failed = 0;

    for service_name in &target_services {
        let service = &compose.services[service_name];

        println!();
        println!("Pulling {} from {}...", service_name, service.image);

        // Determine output path (image reference becomes local .ctp file)
        let output_path = format!("{}.ctp", service_name);

        match ct_client.pull(&service.image, &output_path).await {
            Ok(_) => {
                println!("✓ {} → {}", service.image, output_path);
                pulled += 1;
            }
            Err(e) => {
                eprintln!("✗ {} failed: {}", service_name, e);
                failed += 1;
            }
        }
    }

    // Summary
    println!();
    if failed == 0 {
        println!("✓ All {} bundle(s) pulled successfully", pulled);
    } else {
        println!("⚠ {} succeeded, {} failed", pulled, failed);
        anyhow::bail!("Pull failed for {} service(s)", failed);
    }

    Ok(())
}
