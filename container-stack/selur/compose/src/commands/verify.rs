// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose verify` command implementation

use anyhow::Result;

use crate::compose::ComposeFile;
use crate::ct::CtClient;

/// Run `verify` command - verify all .ctp bundle signatures
pub async fn verify(
    compose: &ComposeFile,
    services: Vec<String>,
) -> Result<()> {
    tracing::info!("Verifying .ctp bundle signatures");

    // Initialize ct client
    let ct_client = CtClient::new();

    // Filter services if specific ones requested
    let target_services: Vec<String> = if services.is_empty() {
        compose.services.keys().cloned().collect()
    } else {
        services
    };

    let mut verified = 0;
    let mut failed = 0;

    // Verify each service
    for service_name in &target_services {
        if let Some(service) = compose.services.get(service_name) {
            print!("Verifying {} ... ", service.image);

            match ct_client.verify(&service.image).await {
                Ok(_) => {
                    println!("✓ VALID");
                    verified += 1;
                }
                Err(e) => {
                    println!("✗ FAILED");
                    tracing::error!("  Error: {}", e);
                    failed += 1;
                }
            }
        } else {
            tracing::warn!("Service '{}' not found in compose file", service_name);
        }
    }

    // Summary
    println!();
    if failed == 0 {
        println!("✅ All bundles verified successfully!");
        println!("   Verified: {}", verified);
        Ok(())
    } else {
        println!("❌ Verification failed!");
        println!("   Verified: {}", verified);
        println!("   Failed: {}", failed);
        anyhow::bail!("{} bundle(s) failed verification", failed);
    }
}
