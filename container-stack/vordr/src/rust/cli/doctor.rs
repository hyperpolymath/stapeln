// SPDX-License-Identifier: PMPL-1.0-or-later
//! System doctor command for validating prerequisites

use anyhow::Result;
use clap::{Parser, ValueEnum};
use console::{style, Emoji};
use serde::Serialize;
use std::path::Path;
use std::process::Command;

use crate::cli::Cli;

static CHECK: Emoji<'_, '_> = Emoji("✓ ", "+ ");
static CROSS: Emoji<'_, '_> = Emoji("✗ ", "x ");
static WARN: Emoji<'_, '_> = Emoji("⚠ ", "! ");

/// Check system prerequisites and configuration
#[derive(Parser, Debug)]
pub struct DoctorArgs {
    /// Show all checks, not just failures
    #[arg(long)]
    pub all: bool,

    /// Output format
    #[arg(long, value_enum, default_value = "human")]
    pub format: OutputFormat,

    /// Attempt to automatically fix issues
    #[arg(long)]
    pub fix: bool,
}

#[derive(Debug, Clone, Copy, ValueEnum, Default)]
pub enum OutputFormat {
    #[default]
    Human,
    Json,
}

#[derive(Debug, Clone, Serialize)]
pub struct CheckResult {
    pub category: String,
    pub name: String,
    pub status: CheckStatus,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fix: Option<FixCommand>,
}

#[derive(Debug, Clone, Copy, Serialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum CheckStatus {
    Pass,
    Warn,
    Fail,
}

#[derive(Debug, Clone, Serialize)]
pub struct FixCommand {
    pub command: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub persist: Option<String>,
}

#[derive(Debug, Serialize)]
struct DoctorReport {
    version: String,
    timestamp: String,
    checks: Vec<CheckResult>,
    summary: Summary,
    next_steps: Vec<String>,
}

#[derive(Debug, Serialize)]
struct Summary {
    passed: usize,
    warnings: usize,
    errors: usize,
}

/// Execute doctor command
pub async fn execute(args: DoctorArgs, cli: &Cli) -> Result<()> {
    let mut checks = Vec::new();

    // Runtime checks
    checks.extend(check_runtime(cli));

    // Networking checks
    checks.extend(check_networking());

    // Kernel/rootless checks
    checks.extend(check_kernel());

    // State database checks
    checks.extend(check_state(cli));

    // Gatekeeper checks
    checks.extend(check_gatekeeper());

    // Calculate summary
    let passed = checks.iter().filter(|c| c.status == CheckStatus::Pass).count();
    let warnings = checks.iter().filter(|c| c.status == CheckStatus::Warn).count();
    let errors = checks.iter().filter(|c| c.status == CheckStatus::Fail).count();

    // Collect next steps
    let next_steps: Vec<_> = checks
        .iter()
        .filter(|c| c.status == CheckStatus::Fail)
        .filter_map(|c| c.fix.as_ref().map(|f| f.command.clone()))
        .collect();

    match args.format {
        OutputFormat::Json => {
            let filtered_checks: Vec<_> = if args.all {
                checks.clone()
            } else {
                checks
                    .iter()
                    .filter(|c| c.status != CheckStatus::Pass)
                    .cloned()
                    .collect()
            };
            let report = DoctorReport {
                version: env!("CARGO_PKG_VERSION").to_string(),
                timestamp: chrono_lite_now(),
                checks: filtered_checks,
                summary: Summary {
                    passed,
                    warnings,
                    errors,
                },
                next_steps: next_steps.clone(),
            };
            println!("{}", serde_json::to_string_pretty(&report)?);
        }
        OutputFormat::Human => {
            println!("{}", style("VORDR SYSTEM CHECK").bold());
            println!("{}", style("==================").dim());
            println!();

            let categories = ["Runtime", "Networking", "Kernel/Rootless", "State Database", "Gatekeeper"];

            for category in categories {
                let cat_checks: Vec<_> = checks
                    .iter()
                    .filter(|c| c.category == category)
                    .collect();

                if cat_checks.is_empty() {
                    continue;
                }

                // Skip category if all pass and not showing all
                if !args.all && cat_checks.iter().all(|c| c.status == CheckStatus::Pass) {
                    continue;
                }

                println!("{}", style(category).bold());

                for check in cat_checks {
                    if !args.all && check.status == CheckStatus::Pass {
                        continue;
                    }

                    let (emoji, color) = match check.status {
                        CheckStatus::Pass => (CHECK, console::Color::Green),
                        CheckStatus::Warn => (WARN, console::Color::Yellow),
                        CheckStatus::Fail => (CROSS, console::Color::Red),
                    };

                    println!(
                        "  {} {}",
                        style(format!("{}", emoji)).fg(color),
                        check.message
                    );

                    if let Some(fix) = &check.fix {
                        println!("    → {}", style(&fix.command).cyan());
                        if let Some(persist) = &fix.persist {
                            println!("    → Persist: {}", style(persist).dim());
                        }
                    }
                }
                println!();
            }

            // Summary line
            println!("{}", style("─".repeat(45)).dim());
            print!("SUMMARY: ");
            if errors > 0 {
                print!("{} ", style(format!("{} error(s)", errors)).red());
            }
            if warnings > 0 {
                print!("{} ", style(format!("{} warning(s)", warnings)).yellow());
            }
            println!("{}", style(format!("{} passed", passed)).green());

            // Next steps
            if !next_steps.is_empty() {
                println!();
                println!("{}", style("DO THIS NEXT:").bold().yellow());
                for step in &next_steps {
                    println!("  {}", style(step).cyan());
                }
            }
        }
    }

    // If fix mode, attempt fixes
    if args.fix {
        println!();
        println!("{}", style("Attempting automatic fixes...").bold());

        for check in &checks {
            if check.status == CheckStatus::Fail {
                if let Some(fix) = &check.fix {
                    // Only auto-fix safe operations
                    if is_safe_fix(&fix.command) {
                        print!("  Running: {} ... ", &fix.command);
                        // Would actually run the command here
                        println!("{}", style("(skipped - manual review required)").dim());
                    }
                }
            }
        }
    }

    Ok(())
}

fn check_runtime(cli: &Cli) -> Vec<CheckResult> {
    let mut results = Vec::new();

    // Check for youki
    match which::which("youki") {
        Ok(path) => {
            let version = get_command_version("youki", &["--version"]);
            results.push(CheckResult {
                category: "Runtime".to_string(),
                name: "youki_binary".to_string(),
                status: CheckStatus::Pass,
                message: format!("youki {} found at {}", version, path.display()),
                fix: None,
            });
        }
        Err(_) => {
            // Check for runc as fallback
            match which::which("runc") {
                Ok(path) => {
                    let version = get_command_version("runc", &["--version"]);
                    results.push(CheckResult {
                        category: "Runtime".to_string(),
                        name: "runc_binary".to_string(),
                        status: CheckStatus::Pass,
                        message: format!("runc {} found at {} (fallback)", version, path.display()),
                        fix: None,
                    });
                }
                Err(_) => {
                    results.push(CheckResult {
                        category: "Runtime".to_string(),
                        name: "runtime_binary".to_string(),
                        status: CheckStatus::Fail,
                        message: "No OCI runtime found (youki or runc)".to_string(),
                        fix: Some(FixCommand {
                            command: "cargo install youki".to_string(),
                            persist: None,
                        }),
                    });
                }
            }
        }
    }

    // Check configured runtime
    if cli.runtime != "youki" && cli.runtime != "runc" {
        if which::which(&cli.runtime).is_ok() {
            results.push(CheckResult {
                category: "Runtime".to_string(),
                name: "configured_runtime".to_string(),
                status: CheckStatus::Pass,
                message: format!("Configured runtime '{}' found", cli.runtime),
                fix: None,
            });
        } else {
            results.push(CheckResult {
                category: "Runtime".to_string(),
                name: "configured_runtime".to_string(),
                status: CheckStatus::Fail,
                message: format!("Configured runtime '{}' not found", cli.runtime),
                fix: Some(FixCommand {
                    command: format!("Install {} or change VORDR_RUNTIME", cli.runtime),
                    persist: None,
                }),
            });
        }
    }

    results
}

fn check_networking() -> Vec<CheckResult> {
    let mut results = Vec::new();

    // Check for netavark
    match which::which("netavark") {
        Ok(path) => {
            let version = get_command_version("netavark", &["--version"]);
            results.push(CheckResult {
                category: "Networking".to_string(),
                name: "netavark_binary".to_string(),
                status: CheckStatus::Pass,
                message: format!("netavark {} found at {}", version, path.display()),
                fix: None,
            });
        }
        Err(_) => {
            results.push(CheckResult {
                category: "Networking".to_string(),
                name: "netavark_binary".to_string(),
                status: CheckStatus::Warn,
                message: "netavark not found (container networking will be limited)".to_string(),
                fix: Some(FixCommand {
                    command: "cargo install netavark".to_string(),
                    persist: None,
                }),
            });
        }
    }

    // Check for aardvark-dns (optional)
    match which::which("aardvark-dns") {
        Ok(path) => {
            results.push(CheckResult {
                category: "Networking".to_string(),
                name: "aardvark_dns".to_string(),
                status: CheckStatus::Pass,
                message: format!("aardvark-dns found at {}", path.display()),
                fix: None,
            });
        }
        Err(_) => {
            results.push(CheckResult {
                category: "Networking".to_string(),
                name: "aardvark_dns".to_string(),
                status: CheckStatus::Warn,
                message: "aardvark-dns not found (DNS resolution will be limited)".to_string(),
                fix: Some(FixCommand {
                    command: "cargo install aardvark-dns".to_string(),
                    persist: None,
                }),
            });
        }
    }

    results
}

fn check_kernel() -> Vec<CheckResult> {
    let mut results = Vec::new();

    // Check kernel version
    if let Ok(output) = Command::new("uname").arg("-r").output() {
        let version = String::from_utf8_lossy(&output.stdout).trim().to_string();
        let major_minor: Vec<u32> = version
            .split('.')
            .take(2)
            .filter_map(|s| s.split('-').next()?.parse().ok())
            .collect();

        if major_minor.len() >= 2 && (major_minor[0] > 5 || (major_minor[0] == 5 && major_minor[1] >= 10)) {
            results.push(CheckResult {
                category: "Kernel/Rootless".to_string(),
                name: "kernel_version".to_string(),
                status: CheckStatus::Pass,
                message: format!("Kernel {} (recommended: 5.10+)", version),
                fix: None,
            });
        } else {
            results.push(CheckResult {
                category: "Kernel/Rootless".to_string(),
                name: "kernel_version".to_string(),
                status: CheckStatus::Warn,
                message: format!("Kernel {} (recommended: 5.10+)", version),
                fix: None,
            });
        }
    }

    // Check cgroup v2
    if Path::new("/sys/fs/cgroup/cgroup.controllers").exists() {
        results.push(CheckResult {
            category: "Kernel/Rootless".to_string(),
            name: "cgroup_v2".to_string(),
            status: CheckStatus::Pass,
            message: "cgroup v2 unified hierarchy".to_string(),
            fix: None,
        });
    } else {
        results.push(CheckResult {
            category: "Kernel/Rootless".to_string(),
            name: "cgroup_v2".to_string(),
            status: CheckStatus::Warn,
            message: "cgroup v2 not detected (using v1)".to_string(),
            fix: Some(FixCommand {
                command: "Add 'systemd.unified_cgroup_hierarchy=1' to kernel cmdline".to_string(),
                persist: None,
            }),
        });
    }

    // Check user namespaces
    let userns_path = Path::new("/proc/sys/kernel/unprivileged_userns_clone");
    if userns_path.exists() {
        if let Ok(content) = std::fs::read_to_string(userns_path) {
            if content.trim() == "1" {
                results.push(CheckResult {
                    category: "Kernel/Rootless".to_string(),
                    name: "user_namespaces".to_string(),
                    status: CheckStatus::Pass,
                    message: "User namespaces enabled".to_string(),
                    fix: None,
                });
            } else {
                results.push(CheckResult {
                    category: "Kernel/Rootless".to_string(),
                    name: "user_namespaces".to_string(),
                    status: CheckStatus::Fail,
                    message: "Unprivileged user namespaces disabled".to_string(),
                    fix: Some(FixCommand {
                        command: "sudo sysctl -w kernel.unprivileged_userns_clone=1".to_string(),
                        persist: Some("echo 'kernel.unprivileged_userns_clone=1' | sudo tee /etc/sysctl.d/99-userns.conf".to_string()),
                    }),
                });
            }
        }
    } else {
        // Kernel doesn't have the sysctl, assume enabled
        results.push(CheckResult {
            category: "Kernel/Rootless".to_string(),
            name: "user_namespaces".to_string(),
            status: CheckStatus::Pass,
            message: "User namespaces available".to_string(),
            fix: None,
        });
    }

    // Check overlay fs
    if let Ok(fs_content) = std::fs::read_to_string("/proc/filesystems") {
        if fs_content.contains("overlay") {
            results.push(CheckResult {
                category: "Kernel/Rootless".to_string(),
                name: "overlay_fs".to_string(),
                status: CheckStatus::Pass,
                message: "Overlay filesystem available".to_string(),
                fix: None,
            });
        } else {
            results.push(CheckResult {
                category: "Kernel/Rootless".to_string(),
                name: "overlay_fs".to_string(),
                status: CheckStatus::Warn,
                message: "Overlay filesystem not available".to_string(),
                fix: Some(FixCommand {
                    command: "sudo modprobe overlay".to_string(),
                    persist: Some("echo 'overlay' | sudo tee /etc/modules-load.d/overlay.conf".to_string()),
                }),
            });
        }
    }

    results
}

fn check_state(cli: &Cli) -> Vec<CheckResult> {
    let mut results = Vec::new();

    let root_path = Path::new(&cli.root);
    let db_path = Path::new(&cli.db_path);

    // Check root directory
    if root_path.exists() {
        if root_path.is_dir() {
            // Check if writable
            let test_file = root_path.join(".vordr_test");
            if std::fs::write(&test_file, "test").is_ok() {
                let _ = std::fs::remove_file(&test_file);
                results.push(CheckResult {
                    category: "State Database".to_string(),
                    name: "root_directory".to_string(),
                    status: CheckStatus::Pass,
                    message: format!("{} exists (writable)", root_path.display()),
                    fix: None,
                });
            } else {
                results.push(CheckResult {
                    category: "State Database".to_string(),
                    name: "root_directory".to_string(),
                    status: CheckStatus::Fail,
                    message: format!("{} not writable", root_path.display()),
                    fix: Some(FixCommand {
                        command: format!("sudo chown $USER:$USER {}", root_path.display()),
                        persist: None,
                    }),
                });
            }
        } else {
            results.push(CheckResult {
                category: "State Database".to_string(),
                name: "root_directory".to_string(),
                status: CheckStatus::Fail,
                message: format!("{} is not a directory", root_path.display()),
                fix: None,
            });
        }
    } else {
        results.push(CheckResult {
            category: "State Database".to_string(),
            name: "root_directory".to_string(),
            status: CheckStatus::Warn,
            message: format!("{} does not exist (will be created)", root_path.display()),
            fix: Some(FixCommand {
                command: format!("sudo mkdir -p {} && sudo chown $USER:$USER {}", root_path.display(), root_path.display()),
                persist: None,
            }),
        });
    }

    // Check database
    if db_path.exists() {
        // Try to open it
        match rusqlite::Connection::open(db_path) {
            Ok(conn) => {
                // Check WAL mode
                match conn.pragma_query_value(None, "journal_mode", |row| row.get::<_, String>(0)) {
                    Ok(mode) => {
                        let msg = format!("Database initialized ({} mode)", mode);
                        results.push(CheckResult {
                            category: "State Database".to_string(),
                            name: "database".to_string(),
                            status: CheckStatus::Pass,
                            message: msg,
                            fix: None,
                        });
                    }
                    Err(_) => {
                        results.push(CheckResult {
                            category: "State Database".to_string(),
                            name: "database".to_string(),
                            status: CheckStatus::Warn,
                            message: "Database exists but could not query journal mode".to_string(),
                            fix: None,
                        });
                    }
                }
            }
            Err(e) => {
                results.push(CheckResult {
                    category: "State Database".to_string(),
                    name: "database".to_string(),
                    status: CheckStatus::Fail,
                    message: format!("Could not open database: {}", e),
                    fix: Some(FixCommand {
                        command: format!("rm {} && vordr ps", db_path.display()),
                        persist: None,
                    }),
                });
            }
        }
    } else {
        results.push(CheckResult {
            category: "State Database".to_string(),
            name: "database".to_string(),
            status: CheckStatus::Pass,
            message: "Database will be created on first use".to_string(),
            fix: None,
        });
    }

    // Check for NFS (WAL mode issue)
    #[cfg(target_os = "linux")]
    {
        if let Ok(mounts) = std::fs::read_to_string("/proc/mounts") {
            let root_str = root_path.to_string_lossy();
            for line in mounts.lines() {
                let parts: Vec<_> = line.split_whitespace().collect();
                if parts.len() >= 3 && parts[2] == "nfs" || parts[2] == "nfs4" {
                    if root_str.starts_with(parts[1]) {
                        results.push(CheckResult {
                            category: "State Database".to_string(),
                            name: "nfs_detection".to_string(),
                            status: CheckStatus::Warn,
                            message: "State directory is on NFS (WAL mode may not work)".to_string(),
                            fix: Some(FixCommand {
                                command: "Set journal_mode = 'delete' in config".to_string(),
                                persist: None,
                            }),
                        });
                    }
                }
            }
        }
    }

    results
}

fn check_gatekeeper() -> Vec<CheckResult> {
    let mut results = Vec::new();

    let version = crate::ffi::gatekeeper_version();
    if version.contains("stub") || version.contains("unavailable") {
        results.push(CheckResult {
            category: "Gatekeeper".to_string(),
            name: "gatekeeper_loaded".to_string(),
            status: CheckStatus::Warn,
            message: "Gatekeeper stub loaded (SPARK verification unavailable)".to_string(),
            fix: Some(FixCommand {
                command: "Install GNAT/SPARK and rebuild: just build-vordr".to_string(),
                persist: None,
            }),
        });
    } else {
        results.push(CheckResult {
            category: "Gatekeeper".to_string(),
            name: "gatekeeper_loaded".to_string(),
            status: CheckStatus::Pass,
            message: format!("Gatekeeper {} (SPARK verification enabled)", version),
            fix: None,
        });
    }

    // Check for gnatprove (optional)
    match which::which("gnatprove") {
        Ok(_) => {
            results.push(CheckResult {
                category: "Gatekeeper".to_string(),
                name: "spark_toolchain".to_string(),
                status: CheckStatus::Pass,
                message: "SPARK toolchain available for policy development".to_string(),
                fix: None,
            });
        }
        Err(_) => {
            results.push(CheckResult {
                category: "Gatekeeper".to_string(),
                name: "spark_toolchain".to_string(),
                status: CheckStatus::Warn,
                message: "SPARK toolchain not found (optional, for policy development)".to_string(),
                fix: None,
            });
        }
    }

    results
}

fn get_command_version(cmd: &str, args: &[&str]) -> String {
    Command::new(cmd)
        .args(args)
        .output()
        .ok()
        .and_then(|o| {
            String::from_utf8_lossy(&o.stdout)
                .lines()
                .next()
                .map(|s| s.trim().to_string())
        })
        .unwrap_or_else(|| "unknown".to_string())
}

fn is_safe_fix(command: &str) -> bool {
    // Only auto-fix directory creation and similar safe operations
    command.starts_with("mkdir") || command.starts_with("sudo mkdir")
}

fn chrono_lite_now() -> String {
    // Simple timestamp without chrono dependency
    use std::time::{SystemTime, UNIX_EPOCH};
    let duration = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default();
    format!("{}Z", duration.as_secs())
}
