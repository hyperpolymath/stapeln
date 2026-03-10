// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose watch` command implementation
//!
//! Watches service build contexts for file changes and automatically
//! rebuilds and restarts services on change. Uses a poll-based approach
//! to avoid additional dependencies.

use anyhow::{Context, Result};
use std::collections::HashMap;
use std::path::Path;
use std::time::{Duration, SystemTime};
use tokio::time::sleep;

use crate::commands;
use crate::compose::ComposeFile;

/// Run `watch` command - watch build contexts and rebuild on changes
pub async fn watch(
    compose: &ComposeFile,
    project_name: &str,
    services: Vec<String>,
    poll_interval: u64,
) -> Result<()> {
    tracing::info!("Starting file watcher for project '{}'", project_name);

    // Determine which services to watch (must have build config)
    let target_services: Vec<String> = if services.is_empty() {
        compose
            .services
            .iter()
            .filter(|(_, s)| s.build.is_some())
            .map(|(name, _)| name.clone())
            .collect()
    } else {
        for service in &services {
            if !compose.services.contains_key(service) {
                anyhow::bail!("Unknown service: {}", service);
            }
            if compose.services[service].build.is_none() {
                anyhow::bail!("Service '{}' has no build configuration to watch", service);
            }
        }
        services
    };

    if target_services.is_empty() {
        println!("No services with build configuration found to watch");
        return Ok(());
    }

    println!("Watching {} service(s) for changes (poll every {}s)...", target_services.len(), poll_interval);
    for service_name in &target_services {
        let service = &compose.services[service_name];
        if let Some(build_config) = &service.build {
            println!("  {} -> {}", service_name, build_config.context);
        }
    }
    println!();
    println!("Press Ctrl+C to stop watching");
    println!();

    // Build initial snapshot of file modification times
    let mut snapshots: HashMap<String, HashMap<String, SystemTime>> = HashMap::new();

    for service_name in &target_services {
        let service = &compose.services[service_name];
        if let Some(build_config) = &service.build {
            let snapshot = scan_directory(&build_config.context)?;
            snapshots.insert(service_name.clone(), snapshot);
        }
    }

    // Poll loop
    loop {
        sleep(Duration::from_secs(poll_interval)).await;

        for service_name in &target_services {
            let service = &compose.services[service_name];
            if let Some(build_config) = &service.build {
                let current = scan_directory(&build_config.context)?;

                if let Some(previous) = snapshots.get(service_name) {
                    let changed_files = detect_changes(previous, &current);

                    if !changed_files.is_empty() {
                        println!();
                        println!("Changes detected in {} ({} file(s)):",
                            service_name, changed_files.len());
                        for f in changed_files.iter().take(5) {
                            println!("  {}", f);
                        }
                        if changed_files.len() > 5 {
                            println!("  ... and {} more", changed_files.len() - 5);
                        }

                        // Rebuild the service
                        println!();
                        println!("Rebuilding {}...", service_name);
                        match commands::build(
                            compose,
                            project_name,
                            vec![service_name.clone()],
                            false,
                        )
                        .await
                        {
                            Ok(_) => {
                                println!("Restarting {}...", service_name);
                                match commands::restart(
                                    compose,
                                    project_name,
                                    vec![service_name.clone()],
                                    10,
                                )
                                .await
                                {
                                    Ok(_) => {
                                        println!("Service {} restarted successfully", service_name);
                                    }
                                    Err(e) => {
                                        eprintln!(
                                            "Failed to restart {}: {}",
                                            service_name, e
                                        );
                                    }
                                }
                            }
                            Err(e) => {
                                eprintln!("Build failed for {}: {}", service_name, e);
                            }
                        }

                        println!();
                        println!("Watching for changes...");
                    }
                }

                // Update snapshot
                snapshots.insert(service_name.clone(), current);
            }
        }
    }
}

/// Scan a directory recursively and return a map of file_path -> modification_time
fn scan_directory(dir_path: &str) -> Result<HashMap<String, SystemTime>> {
    let mut files = HashMap::new();
    let path = Path::new(dir_path);

    if !path.exists() {
        anyhow::bail!("Build context directory does not exist: {}", dir_path);
    }

    for entry in walkdir::WalkDir::new(path)
        .follow_links(false)
        .into_iter()
        .filter_entry(|e| {
            // Skip hidden directories and common non-source directories
            let name = e.file_name().to_string_lossy();
            !name.starts_with('.')
                && name != "node_modules"
                && name != "target"
                && name != "_build"
                && name != "__pycache__"
        })
    {
        let entry = entry.with_context(|| format!("Failed to walk directory {}", dir_path))?;

        if entry.file_type().is_file() {
            if let Ok(metadata) = entry.metadata() {
                if let Ok(modified) = metadata.modified() {
                    files.insert(
                        entry.path().to_string_lossy().to_string(),
                        modified,
                    );
                }
            }
        }
    }

    Ok(files)
}

/// Compare two snapshots and return list of changed file paths
fn detect_changes(
    previous: &HashMap<String, SystemTime>,
    current: &HashMap<String, SystemTime>,
) -> Vec<String> {
    let mut changed = Vec::new();

    // Check for new or modified files
    for (path, current_time) in current {
        match previous.get(path) {
            Some(prev_time) => {
                if current_time != prev_time {
                    changed.push(path.clone());
                }
            }
            None => {
                // New file
                changed.push(path.clone());
            }
        }
    }

    // Check for deleted files
    for path in previous.keys() {
        if !current.contains_key(path) {
            changed.push(format!("{} (deleted)", path));
        }
    }

    changed
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_detect_changes_no_change() {
        let mut snapshot = HashMap::new();
        let time = SystemTime::now();
        snapshot.insert("file1.rs".to_string(), time);
        snapshot.insert("file2.rs".to_string(), time);

        let changes = detect_changes(&snapshot, &snapshot);
        assert!(changes.is_empty());
    }

    #[test]
    fn test_detect_changes_new_file() {
        let time = SystemTime::now();
        let mut prev = HashMap::new();
        prev.insert("file1.rs".to_string(), time);

        let mut curr = HashMap::new();
        curr.insert("file1.rs".to_string(), time);
        curr.insert("file2.rs".to_string(), time);

        let changes = detect_changes(&prev, &curr);
        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0], "file2.rs");
    }
}
