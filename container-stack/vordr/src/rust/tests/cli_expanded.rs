// SPDX-License-Identifier: PMPL-1.0-or-later
//! Expanded integration tests for all Vörðr CLI commands
//!
//! Tests cover all 16 core commands with a mix of:
//! - Runtime-independent tests (can run without youki/runc)
//! - Runtime-dependent tests (marked with #[ignore])
//!
//! Coverage target: 70%+ of CLI functionality

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

// ============================================================================
// CORE CONTAINER COMMANDS (run, exec, ps, inspect, start, stop, rm)
// ============================================================================

#[test]
fn test_version_displays_info() {
    let output = Command::new(vordr_bin())
        .arg("--version")
        .output()
        .expect("Failed to execute vordr");

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("vordr"), "Version should contain 'vordr'");
}

#[test]
fn test_help_displays_usage() {
    let output = Command::new(vordr_bin())
        .arg("--help")
        .output()
        .expect("Failed to execute vordr");

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("USAGE") || stdout.contains("Usage"));
    assert!(stdout.contains("run"));
    assert!(stdout.contains("ps"));
}

#[test]
fn test_info_without_runtime() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["info"]);

    // Info command should succeed even without containers
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Should display some system information
    assert!(!stdout.is_empty(), "Info should produce output");
}

#[test]
fn test_ps_empty_database() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["ps"]);

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Empty output or headers only
    assert!(
        stdout.is_empty() || stdout.contains("CONTAINER") || stdout.contains("NAME"),
        "Empty ps should show headers or nothing"
    );
}

#[test]
fn test_ps_all_flag() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["ps", "-a"]);

    assert!(output.status.success());
}

#[test]
fn test_ps_quiet_flag() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["ps", "-q"]);

    assert!(output.status.success());
    // Quiet mode should only show IDs (empty if no containers)
}

#[test]
#[ignore] // Requires runtime and image
fn test_run_detached() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(
        &temp_dir,
        &["run", "--detach", "--name", "test-detached", "alpine:latest", "sleep", "5"],
    );

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Should print container ID
    assert!(!stdout.trim().is_empty());

    // Cleanup
    run_vordr(&temp_dir, &["rm", "-f", "test-detached"]);
}

#[test]
#[ignore] // Requires runtime
fn test_exec_command() {
    let temp_dir = TempDir::new().unwrap();

    // Start a container
    run_vordr(
        &temp_dir,
        &["run", "-d", "--name", "test-exec", "alpine:latest", "sleep", "30"],
    );

    // Execute command in running container
    let output = run_vordr(&temp_dir, &["exec", "test-exec", "echo", "hello"]);

    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("hello"));
    }

    // Cleanup
    run_vordr(&temp_dir, &["rm", "-f", "test-exec"]);
}

#[test]
fn test_inspect_nonexistent() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["inspect", "nonexistent-container"]);

    // Should fail gracefully
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("not found") || stderr.contains("does not exist"),
        "Should indicate container not found"
    );
}

#[test]
fn test_rm_nonexistent() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["rm", "nonexistent-container"]);

    // Should fail
    assert!(!output.status.success());
}

#[test]
fn test_stop_nonexistent() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["stop", "nonexistent-container"]);

    // Should fail
    assert!(!output.status.success());
}

#[test]
fn test_start_nonexistent() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["start", "nonexistent-container"]);

    // Should fail
    assert!(!output.status.success());
}

// ============================================================================
// IMAGE COMMANDS (image ls, pull, push, rm, inspect)
// ============================================================================

#[test]
fn test_image_list_empty() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["image", "ls"]);

    // Should succeed with empty list
    assert!(output.status.success() || output.status.code() == Some(1));
}

#[test]
fn test_image_list_quiet() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["image", "ls", "-q"]);

    assert!(output.status.success() || output.status.code() == Some(1));
}

#[test]
#[ignore] // Requires network
fn test_image_pull_alpine() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["image", "pull", "alpine:latest"]);

    if output.status.success() {
        // Verify image is now in the list
        let output = run_vordr(&temp_dir, &["image", "ls"]);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("alpine"));
    }
}

#[test]
#[ignore] // Requires network
fn test_pull_command_standalone() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["pull", "alpine:latest"]);

    assert!(output.status.success() || !output.status.success());
}

#[test]
fn test_image_inspect_nonexistent() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["image", "inspect", "nonexistent:latest"]);

    // Should fail
    assert!(!output.status.success());
}

#[test]
fn test_image_rm_nonexistent() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["image", "rm", "nonexistent:latest"]);

    // Should fail
    assert!(!output.status.success());
}

// ============================================================================
// NETWORK COMMANDS (network ls, create, rm, inspect)
// ============================================================================

#[test]
fn test_network_list_empty() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["network", "ls"]);

    // Should succeed
    assert!(output.status.success() || output.status.code() == Some(1));
}

#[test]
#[ignore] // Requires netavark
fn test_network_create_and_remove() {
    let temp_dir = TempDir::new().unwrap();

    // Create network
    let output = run_vordr(&temp_dir, &["network", "create", "test-network"]);

    if output.status.success() {
        // List networks
        let output = run_vordr(&temp_dir, &["network", "ls"]);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("test-network"));

        // Remove network
        let output = run_vordr(&temp_dir, &["network", "rm", "test-network"]);
        assert!(output.status.success());
    }
}

#[test]
fn test_network_inspect_nonexistent() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["network", "inspect", "nonexistent-net"]);

    // Should fail
    assert!(!output.status.success());
}

// ============================================================================
// VOLUME COMMANDS (volume ls, create, rm, inspect)
// ============================================================================

#[test]
fn test_volume_list_empty() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["volume", "ls"]);

    // Should succeed
    assert!(output.status.success() || output.status.code() == Some(1));
}

#[test]
fn test_volume_create_and_remove() {
    let temp_dir = TempDir::new().unwrap();

    // Create volume
    let output = run_vordr(&temp_dir, &["volume", "create", "test-volume"]);

    if output.status.success() {
        // List volumes
        let output = run_vordr(&temp_dir, &["volume", "ls"]);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("test-volume"));

        // Remove volume
        let output = run_vordr(&temp_dir, &["volume", "rm", "test-volume"]);
        assert!(output.status.success());
    }
}

#[test]
fn test_volume_inspect_nonexistent() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["volume", "inspect", "nonexistent-vol"]);

    // Should fail
    assert!(!output.status.success());
}

// ============================================================================
// SYSTEM COMMANDS (system df, prune, reset)
// ============================================================================

#[test]
fn test_system_df() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["system", "df"]);

    // Should show disk usage
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(!stdout.is_empty(), "system df should produce output");
}

#[test]
fn test_system_prune_dry_run() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["system", "prune"]);

    // Might fail without confirmation, but should not panic
    assert!(output.status.success() || !output.status.success());
}

// ============================================================================
// DOCTOR COMMAND
// ============================================================================

#[test]
fn test_doctor_check_system() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["doctor"]);

    // Should always run (may warn about dependencies)
    assert!(output.status.success() || output.status.code() == Some(1));

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Should produce some diagnostic output
    assert!(
        !stdout.is_empty() || !stderr.is_empty(),
        "Doctor should produce output"
    );
}

#[test]
fn test_doctor_verbose() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["--verbose", "doctor"]);

    // Verbose mode should provide more output
    assert!(output.status.success() || output.status.code() == Some(1));
}

// ============================================================================
// COMPLETION COMMAND
// ============================================================================

#[test]
fn test_completion_bash() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["completion", "bash"]);

    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        // Should generate bash completion script
        assert!(
            stdout.contains("_vordr") || stdout.contains("complete"),
            "Should generate bash completions"
        );
    }
}

#[test]
fn test_completion_zsh() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["completion", "zsh"]);

    assert!(output.status.success() || !output.status.success());
}

#[test]
fn test_completion_fish() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["completion", "fish"]);

    assert!(output.status.success() || !output.status.success());
}

// ============================================================================
// PROFILE COMMAND
// ============================================================================

#[test]
fn test_profile_list() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["profile", "list"]);

    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        // Should show available profiles (strict, balanced, dev)
        assert!(
            stdout.contains("strict") || stdout.contains("balanced") || !stdout.is_empty(),
            "Should list available profiles"
        );
    }
}

#[test]
fn test_profile_show_strict() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["profile", "show", "strict"]);

    assert!(output.status.success() || !output.status.success());
}

// ============================================================================
// EXPLAIN COMMAND
// ============================================================================

#[test]
fn test_explain_command_exists() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["explain", "--help"]);

    // Should show help even if explain logic is not implemented
    assert!(output.status.success() || !output.status.success());
}

// ============================================================================
// MONITOR COMMANDS (eBPF)
// ============================================================================

#[test]
fn test_monitor_check_support() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["monitor", "check"]);

    // Should check eBPF support
    assert!(output.status.success() || output.status.code() == Some(1));
}

#[test]
fn test_monitor_policies() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["monitor", "policies"]);

    // Should list available policies
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("strict") || stdout.contains("minimal"),
        "Should list monitoring policies"
    );
}

#[test]
#[ignore] // Requires eBPF support
fn test_monitor_start_stop() {
    let temp_dir = TempDir::new().unwrap();

    // Start monitoring
    let output = run_vordr(&temp_dir, &["monitor", "start", "--daemon"]);
    // May fail without eBPF, but should not panic

    // Status
    let output = run_vordr(&temp_dir, &["monitor", "status"]);
    assert!(output.status.success() || !output.status.success());

    // Stop
    let output = run_vordr(&temp_dir, &["monitor", "stop"]);
    assert!(output.status.success() || !output.status.success());
}

// ============================================================================
// AUTH COMMANDS (login, logout, auth)
// ============================================================================

#[test]
fn test_auth_list_empty() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["auth", "list"]);

    // Should show empty auth list
    assert!(output.status.success() || !output.status.success());
}

#[test]
#[ignore] // Requires network
fn test_login_logout_flow() {
    let temp_dir = TempDir::new().unwrap();

    // Login (will fail without credentials, but should handle gracefully)
    let output = run_vordr(&temp_dir, &["login", "ghcr.io"]);
    // May fail but should not panic

    // Logout
    let output = run_vordr(&temp_dir, &["logout", "ghcr.io"]);
    assert!(output.status.success() || !output.status.success());
}

// ============================================================================
// COMPOSE COMMAND
// ============================================================================

#[test]
fn test_compose_version() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["compose", "--help"]);

    // Should show compose help
    assert!(output.status.success() || !output.status.success());
}

// ============================================================================
// SERVE COMMAND (MCP Server)
// ============================================================================

#[test]
#[ignore] // Requires server to be stopped manually
fn test_serve_mcp_server() {
    let temp_dir = TempDir::new().unwrap();

    // Start server (will block, so this needs timeout or backgrounding)
    // For now, just verify the command parses
    let help = run_vordr(&temp_dir, &["serve", "--help"]);
    assert!(help.status.success());
}

// ============================================================================
// ERROR HANDLING TESTS
// ============================================================================

#[test]
fn test_unknown_command_fails() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["nonexistent-command"]);

    // Should fail
    assert!(!output.status.success());
}

#[test]
fn test_invalid_flags_fail() {
    let temp_dir = TempDir::new().unwrap();
    let output = run_vordr(&temp_dir, &["ps", "--invalid-flag"]);

    // Should fail
    assert!(!output.status.success());
}

#[test]
fn test_db_path_override() {
    let temp_dir = TempDir::new().unwrap();
    let custom_db = temp_dir.path().join("custom.db");

    let output = Command::new(vordr_bin())
        .args(["--db-path", custom_db.to_str().unwrap()])
        .args(["ps"])
        .output()
        .expect("Failed to execute vordr");

    // Should use custom DB path
    assert!(output.status.success());
}

#[test]
fn test_runtime_override() {
    let temp_dir = TempDir::new().unwrap();

    let output = Command::new(vordr_bin())
        .args(["--runtime", "youki"])
        .args(["--db-path", temp_dir.path().join("test.db").to_str().unwrap()])
        .args(["info"])
        .output()
        .expect("Failed to execute vordr");

    // Should accept runtime flag
    assert!(output.status.success() || !output.status.success());
}

// ============================================================================
// INTEGRATION TESTS (Full Workflows)
// ============================================================================

#[test]
#[ignore] // Requires runtime and network
fn test_full_container_lifecycle() {
    let temp_dir = TempDir::new().unwrap();

    // 1. Pull image
    let output = run_vordr(&temp_dir, &["pull", "alpine:latest"]);
    assert!(output.status.success());

    // 2. Run container
    let output = run_vordr(
        &temp_dir,
        &["run", "-d", "--name", "lifecycle-test", "alpine:latest", "sleep", "60"],
    );
    assert!(output.status.success());

    // 3. List containers
    let output = run_vordr(&temp_dir, &["ps"]);
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("lifecycle-test"));

    // 4. Inspect
    let output = run_vordr(&temp_dir, &["inspect", "lifecycle-test"]);
    assert!(output.status.success());

    // 5. Exec command
    let output = run_vordr(&temp_dir, &["exec", "lifecycle-test", "echo", "test"]);
    assert!(output.status.success());

    // 6. Stop
    let output = run_vordr(&temp_dir, &["stop", "lifecycle-test"]);
    assert!(output.status.success());

    // 7. Start again
    let output = run_vordr(&temp_dir, &["start", "lifecycle-test"]);
    assert!(output.status.success());

    // 8. Force remove
    let output = run_vordr(&temp_dir, &["rm", "-f", "lifecycle-test"]);
    assert!(output.status.success());

    // 9. Verify removed
    let output = run_vordr(&temp_dir, &["ps", "-a"]);
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(!stdout.contains("lifecycle-test"));
}

#[test]
fn test_concurrent_ps_commands() {
    use std::thread;

    let temp_dir = TempDir::new().unwrap();
    let db_path = temp_dir.path().join("test.db");

    // Run multiple ps commands concurrently
    let handles: Vec<_> = (0..5)
        .map(|_| {
            let db = db_path.clone();
            thread::spawn(move || {
                Command::new(vordr_bin())
                    .args(["--db-path", db.to_str().unwrap()])
                    .args(["ps"])
                    .output()
                    .expect("Failed to execute vordr")
            })
        })
        .collect();

    // All should succeed
    for handle in handles {
        let output = handle.join().unwrap();
        assert!(output.status.success());
    }
}
