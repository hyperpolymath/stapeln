// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose push` command implementation

use anyhow::Result;

use crate::compose::ComposeFile;
use crate::ct::CtClient;

/// Run `push` command - push .ctp bundles to registry
pub async fn push(
    compose: &ComposeFile,
    _project_name: &str,
    services: Vec<String>,
) -> Result<()> {
    tracing::info!("Pushing .ctp bundles to registry");

    // Determine which services to push
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

    tracing::info!("Pushing {} service(s)", target_services.len());

    // Initialize ct client
    let ct_client = CtClient::new();

    // Push each service
    let mut pushed = 0;
    let mut failed = 0;

    for service_name in &target_services {
        let service = &compose.services[service_name];

        // Local .ctp bundle path (convention: service_name.ctp)
        let bundle_path = format!("{}.ctp", service_name);

        // Check if bundle exists locally
        if !std::path::Path::new(&bundle_path).exists() {
            eprintln!("✗ {} not found (run 'selur-compose build {}' first)", bundle_path, service_name);
            failed += 1;
            continue;
        }

        println!();
        println!("Pushing {} to {}...", bundle_path, service.image);

        match ct_client.push(&bundle_path, &service.image).await {
            Ok(_) => {
                println!("✓ {} → {}", bundle_path, service.image);
                pushed += 1;
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
        println!("✓ All {} bundle(s) pushed successfully", pushed);
    } else {
        println!("⚠ {} succeeded, {} failed", pushed, failed);
        anyhow::bail!("Push failed for {} service(s)", failed);
    }

    Ok(())
}
