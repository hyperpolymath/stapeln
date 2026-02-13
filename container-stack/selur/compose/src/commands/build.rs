// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose build` command implementation

use anyhow::Result;

use crate::compose::ComposeFile;
use crate::ct::CtClient;

/// Run `build` command - build .ctp bundles from source
pub async fn build(
    compose: &ComposeFile,
    _project_name: &str,
    services: Vec<String>,
    no_cache: bool,
) -> Result<()> {
    tracing::info!("Building .ctp bundles");

    // Determine which services to build
    let target_services: Vec<String> = if services.is_empty() {
        // Build all services that have a build context
        compose
            .services
            .iter()
            .filter(|(_, service)| service.build.is_some())
            .map(|(name, _)| name.clone())
            .collect()
    } else {
        // Validate that all requested services exist
        for service in &services {
            if !compose.services.contains_key(service) {
                anyhow::bail!("Unknown service: {}", service);
            }
            if compose.services[service].build.is_none() {
                anyhow::bail!("Service '{}' has no build configuration", service);
            }
        }
        services
    };

    if target_services.is_empty() {
        println!("No services with build configuration found");
        return Ok(());
    }

    tracing::info!("Building {} service(s)", target_services.len());

    // Initialize ct client
    let ct_client = CtClient::new();

    // Build each service
    let mut built = 0;
    let mut failed = 0;

    for service_name in &target_services {
        let service = &compose.services[service_name];
        let build_config = service.build.as_ref().unwrap();

        println!();
        println!("Building {} from {}...", service_name, build_config.context);

        // Determine output path
        let output_path = format!("{}.ctp", service_name);

        // Build arguments
        let mut args = vec!["pack".to_string()];

        if no_cache {
            args.push("--no-cache".to_string());
        }

        if let Some(dockerfile) = &build_config.dockerfile {
            args.push("--dockerfile".to_string());
            args.push(dockerfile.clone());
        }

        // Add build args as environment variables
        for (key, value) in &build_config.args {
            args.push("--build-arg".to_string());
            args.push(format!("{}={}", key, value));
        }

        // Add target if specified
        if let Some(target) = &build_config.target {
            args.push("--target".to_string());
            args.push(target.clone());
        }

        args.push("--context".to_string());
        args.push(build_config.context.clone());
        args.push("--output".to_string());
        args.push(output_path.clone());

        tracing::debug!("Running ct with args: {:?}", args);

        match ct_client.run_command(&args).await {
            Ok(_) => {
                println!("✓ {} → {}", service_name, output_path);
                built += 1;
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
        println!("✓ All {} service(s) built successfully", built);
    } else {
        println!("⚠ {} succeeded, {} failed", built, failed);
        anyhow::bail!("Build failed for {} service(s)", failed);
    }

    Ok(())
}
