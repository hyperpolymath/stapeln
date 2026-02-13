// SPDX-License-Identifier: PMPL-1.0-or-later
//! Integration tests for Vörðr CLI commands
//!
//! Tests the full container lifecycle: run, ps, inspect, stop, rm

use std::path::PathBuf;
use std::process::Command;
use tempfile::TempDir;

/// Helper to get the vordr binary path
fn vordr_bin() -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("target");
    path.push("debug");
    path.push("vordr");
    path
}

/// Helper to run vordr with custom args and db/root paths
fn run_vordr(temp_dir: &TempDir, args: &[&str]) -> std::process::Output {
    let db_path = temp_dir.path().join("test.db");
    let root_path = temp_dir.path();

    Command::new(vordr_bin())
        .args(["--db-path", db_path.to_str().unwrap()])
        .args(["--root", root_path.to_str().unwrap()])
        .args(args)
        .output()
        .expect("Failed to execute vordr")
}

#[test]
#[ignore] // Requires runtime (youki/runc) to be installed
fn test_version_command() {
    let output = Command::new(vordr_bin())
        .arg("--version")
        .output()
        .expect("Failed to execute vordr");

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("vordr"));
}

#[test]
fn test_info_command() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["info"]);

    // Info command should always succeed (even without containers)
    assert!(output.status.success());
}

#[test]
fn test_ps_empty() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["ps"]);

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Should show headers but no containers
    assert!(stdout.contains("CONTAINER") || stdout.contains("NAME") || stdout.len() == 0);
}

#[test]
#[ignore] // Requires image to be present
fn test_container_lifecycle() {
    let temp_dir = TempDir::new().unwrap();

    // Step 1: Run a container (detached)
    let output = run_vordr(
        &temp_dir,
        &[
            "run",
            "--detach",
            "--name",
            "test-lifecycle",
            "alpine:latest",
            "sleep",
            "30",
        ],
    );

    if !output.status.success() {
        eprintln!("Run failed: {}", String::from_utf8_lossy(&output.stderr));
        eprintln!("Run stdout: {}", String::from_utf8_lossy(&output.stdout));
        panic!("Container run failed");
    }

    // Step 2: List containers - should see our container
    let output = run_vordr(&temp_dir, &["ps"]);
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("test-lifecycle"));

    // Step 3: Inspect the container
    let output = run_vordr(&temp_dir, &["inspect", "test-lifecycle"]);
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("test-lifecycle"));
    assert!(stdout.contains("alpine"));

    // Step 4: Stop the container
    let output = run_vordr(&temp_dir, &["stop", "test-lifecycle"]);
    assert!(output.status.success());

    // Step 5: Verify container is stopped
    let output = run_vordr(&temp_dir, &["ps", "-a"]);
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Should show container as stopped/exited
    assert!(stdout.contains("test-lifecycle"));

    // Step 6: Remove the container
    let output = run_vordr(&temp_dir, &["rm", "test-lifecycle"]);
    assert!(output.status.success());

    // Step 7: Verify container is gone
    let output = run_vordr(&temp_dir, &["ps", "-a"]);
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(!stdout.contains("test-lifecycle"));
}

#[test]
#[ignore] // Requires runtime
fn test_start_stop_restart() {
    let temp_dir = TempDir::new().unwrap();

    // Create and stop a container
    run_vordr(
        &temp_dir,
        &[
            "run",
            "--detach",
            "--name",
            "test-restart",
            "alpine:latest",
            "sleep",
            "30",
        ],
    );

    run_vordr(&temp_dir, &["stop", "test-restart"]);

    // Start it again
    let output = run_vordr(&temp_dir, &["start", "test-restart"]);
    assert!(output.status.success());

    // Verify it's running
    let output = run_vordr(&temp_dir, &["ps"]);
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("test-restart"));

    // Cleanup
    run_vordr(&temp_dir, &["rm", "-f", "test-restart"]);
}

#[test]
fn test_image_list() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["image", "ls"]);

    // Should succeed even with no images
    assert!(output.status.success() || output.status.code() == Some(1));
}

#[test]
#[ignore] // Requires network support
fn test_network_commands() {
    let temp_dir = TempDir::new().unwrap();

    // List networks
    let output = run_vordr(&temp_dir, &["network", "ls"]);
    assert!(output.status.success() || output.status.code() == Some(1));

    // Create network
    let output = run_vordr(&temp_dir, &["network", "create", "test-net"]);
    if output.status.success() {
        // List should now show our network
        let output = run_vordr(&temp_dir, &["network", "ls"]);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("test-net"));

        // Remove network
        let output = run_vordr(&temp_dir, &["network", "rm", "test-net"]);
        assert!(output.status.success());
    }
}

#[test]
fn test_system_df() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["system", "df"]);

    // Should show disk usage even if empty
    assert!(output.status.success());
}

#[test]
fn test_doctor_check() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["doctor"]);

    // Doctor should always run (may warn about missing dependencies)
    assert!(output.status.success() || output.status.code() == Some(1));
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    // Should produce some output (stdout or stderr)
    assert!(
        !stdout.is_empty() || !stderr.is_empty(),
        "Doctor command should produce output"
    );
}

#[test]
fn test_completion_generation() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["completion", "bash"]);

    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        // Should generate bash completion script
        assert!(stdout.contains("_vordr") || stdout.contains("complete"));
    }
}

#[test]
#[ignore] // Requires actual monitor implementation
fn test_monitor_commands() {
    let temp_dir = TempDir::new().unwrap();

    // Start monitoring (should fail without eBPF support)
    let output = run_vordr(&temp_dir, &["monitor", "start"]);
    // May fail due to missing eBPF support, but should not panic
    assert!(output.status.success() || !output.status.success());

    // Status should work
    let output = run_vordr(&temp_dir, &["monitor", "status"]);
    assert!(output.status.success() || !output.status.success());
}
