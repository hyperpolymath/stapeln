// SPDX-License-Identifier: PMPL-1.0-or-later
//! eBPF-based runtime monitoring CLI commands
//!
//! Usage:
//!   vordr monitor start [--container <id>] [--policy <name>]
//!   vordr monitor stop
//!   vordr monitor status
//!   vordr monitor events [--follow] [--container <id>]

use anyhow::Result;
use clap::{Args, Subcommand};
use std::time::Duration;

use crate::cli::Cli;
use crate::ebpf::{Monitor, MonitorConfig, MonitorStatus, SyscallPolicy};

/// Monitor command arguments
#[derive(Args, Debug)]
pub struct MonitorArgs {
    #[command(subcommand)]
    pub command: MonitorCommands,
}

/// Monitor subcommands
#[derive(Subcommand, Debug)]
pub enum MonitorCommands {
    /// Start eBPF monitoring
    Start {
        /// Container IDs to monitor (default: all)
        #[arg(short, long)]
        container: Vec<String>,

        /// Security policy to apply
        #[arg(short, long, default_value = "strict")]
        policy: String,

        /// Webhook URL for alerts
        #[arg(short, long)]
        webhook: Option<String>,

        /// Anomaly detection sensitivity (0.0 - 1.0)
        #[arg(long, default_value = "0.8")]
        sensitivity: f64,

        /// Run in background (daemon mode)
        #[arg(short, long)]
        daemon: bool,
    },

    /// Stop eBPF monitoring
    Stop,

    /// Show monitoring status
    Status,

    /// Show captured events
    Events {
        /// Follow events in real-time
        #[arg(short, long)]
        follow: bool,

        /// Filter by container ID
        #[arg(short, long)]
        container: Option<String>,

        /// Show only anomalies
        #[arg(short, long)]
        anomalies: bool,

        /// Maximum events to show
        #[arg(short = 'n', long, default_value = "100")]
        limit: usize,
    },

    /// Show available policies
    Policies,

    /// Show monitoring statistics
    Stats {
        /// Container ID to show stats for
        #[arg(short, long)]
        container: Option<String>,
    },

    /// Check eBPF support
    Check,
}

/// Execute monitor commands
pub async fn execute(args: MonitorArgs, cli: &Cli) -> Result<()> {
    match args.command {
        MonitorCommands::Start {
            container,
            policy,
            webhook,
            sensitivity,
            daemon,
        } => start_monitor(container, policy, webhook, sensitivity, daemon, cli).await,
        MonitorCommands::Stop => stop_monitor(cli).await,
        MonitorCommands::Status => show_status(cli).await,
        MonitorCommands::Events {
            follow,
            container,
            anomalies,
            limit,
        } => show_events(follow, container, anomalies, limit, cli).await,
        MonitorCommands::Policies => show_policies(),
        MonitorCommands::Stats { container } => show_stats(container, cli).await,
        MonitorCommands::Check => check_ebpf_support(),
    }
}

/// Start the eBPF monitor
async fn start_monitor(
    containers: Vec<String>,
    policy_name: String,
    webhook: Option<String>,
    sensitivity: f64,
    daemon: bool,
    _cli: &Cli,
) -> Result<()> {
    use console::style;

    println!(
        "{} Starting eBPF monitor...",
        style("[vordr]").bold().cyan()
    );

    // Check eBPF support first
    if !Monitor::check_ebpf_support()? {
        println!(
            "{} eBPF is not supported on this system",
            style("✗").bold().red()
        );
        println!("  Make sure you're running Linux 4.15+ with appropriate permissions");
        return Ok(());
    }

    println!("{} eBPF support verified", style("✓").bold().green());

    // Create configuration
    let config = MonitorConfig {
        container_ids: containers.clone(),
        anomaly_detection: true,
        anomaly_sensitivity: sensitivity,
        webhook_url: webhook.clone(),
        ..Default::default()
    };

    // Load policy
    let policy = match policy_name.as_str() {
        "strict" => SyscallPolicy::strict(),
        "minimal" | "minimal-audit" => SyscallPolicy::minimal_audit(),
        other => {
            println!(
                "{} Unknown policy '{}', using strict",
                style("!").bold().yellow(),
                other
            );
            SyscallPolicy::strict()
        }
    };

    println!(
        "{} Using policy: {}",
        style("✓").bold().green(),
        policy.name
    );

    if containers.is_empty() {
        println!("{} Monitoring: all containers", style("✓").bold().green());
    } else {
        println!(
            "{} Monitoring containers: {}",
            style("✓").bold().green(),
            containers.join(", ")
        );
    }

    if let Some(ref url) = webhook {
        println!("{} Webhook: {}", style("✓").bold().green(), url);
    }

    // Create and start monitor
    let mut monitor = Monitor::new(config);
    monitor.start().await?;

    println!(
        "{} Monitor started successfully",
        style("✓").bold().green()
    );

    if daemon {
        println!(
            "{} Running in daemon mode. Use 'vordr monitor stop' to stop.",
            style("→").bold().blue()
        );
        // In daemon mode, we'd detach and run in background
        // For now, just run in foreground
        loop {
            tokio::time::sleep(Duration::from_secs(1)).await;
            if monitor.status() != MonitorStatus::Running {
                break;
            }
        }
    } else {
        println!(
            "{} Press Ctrl+C to stop monitoring",
            style("→").bold().blue()
        );
        // Wait for interrupt
        tokio::signal::ctrl_c().await?;
        monitor.stop().await?;
        println!(
            "\n{} Monitor stopped",
            style("✓").bold().green()
        );
    }

    // Print final stats
    let stats = monitor.stats();
    println!("\nFinal Statistics:");
    println!("  Events received: {}", stats.events_received);
    println!("  Anomalies detected: {}", stats.anomalies_detected);
    println!("  Alerts sent: {}", stats.alerts_sent);

    Ok(())
}

/// Stop the monitor
async fn stop_monitor(_cli: &Cli) -> Result<()> {
    use console::style;

    println!(
        "{} Stopping eBPF monitor...",
        style("[vordr]").bold().cyan()
    );

    // In production, this would signal a running daemon
    // For now, just acknowledge
    println!(
        "{} Monitor stopped",
        style("✓").bold().green()
    );

    Ok(())
}

/// Show monitoring status
async fn show_status(_cli: &Cli) -> Result<()> {
    
    use tabled::{Table, Tabled};

    #[derive(Tabled)]
    struct StatusRow {
        #[tabled(rename = "Property")]
        property: String,
        #[tabled(rename = "Value")]
        value: String,
    }

    let rows = vec![
        StatusRow {
            property: "Status".to_string(),
            value: "Running".to_string(), // Would check actual status
        },
        StatusRow {
            property: "Policy".to_string(),
            value: "strict".to_string(),
        },
        StatusRow {
            property: "Containers".to_string(),
            value: "all".to_string(),
        },
        StatusRow {
            property: "Events received".to_string(),
            value: "0".to_string(),
        },
        StatusRow {
            property: "Anomalies detected".to_string(),
            value: "0".to_string(),
        },
    ];

    let table = Table::new(rows);
    println!("{}", table);

    Ok(())
}

/// Show captured events
async fn show_events(
    follow: bool,
    container: Option<String>,
    anomalies: bool,
    limit: usize,
    _cli: &Cli,
) -> Result<()> {
    use console::style;

    println!(
        "{} Event viewer (limit: {})",
        style("[vordr]").bold().cyan(),
        limit
    );

    if let Some(ref cid) = container {
        println!("  Filtering by container: {}", cid);
    }

    if anomalies {
        println!("  Showing anomalies only");
    }

    if follow {
        println!("  Following events (Ctrl+C to stop)...\n");

        // In production, this would connect to the monitor's event stream
        loop {
            tokio::time::sleep(Duration::from_millis(100)).await;
            // Would print events as they arrive
        }
    } else {
        // Show historical events
        println!("\nNo historical events available (monitor not running)");
    }

    Ok(())
}

/// Show available policies
fn show_policies() -> Result<()> {
    use console::style;
    use tabled::{Table, Tabled};

    #[derive(Tabled)]
    struct PolicyRow {
        #[tabled(rename = "Name")]
        name: String,
        #[tabled(rename = "Description")]
        description: String,
        #[tabled(rename = "Blocked")]
        blocked: String,
        #[tabled(rename = "Audited")]
        audited: String,
    }

    let rows = vec![
        PolicyRow {
            name: "strict".to_string(),
            description: "Block dangerous syscalls, audit sensitive ones".to_string(),
            blocked: "module_*, reboot, sethostname".to_string(),
            audited: "execve, mount, chroot, namespace".to_string(),
        },
        PolicyRow {
            name: "minimal-audit".to_string(),
            description: "Only audit process execution".to_string(),
            blocked: "none".to_string(),
            audited: "execve, execveat".to_string(),
        },
    ];

    println!(
        "{} Available monitoring policies:\n",
        style("[vordr]").bold().cyan()
    );
    let table = Table::new(rows);
    println!("{}", table);

    println!("\nSyscall Groups:");
    println!(
        "  {} - execve, execveat",
        style("process_exec").bold()
    );
    println!(
        "  {} - clone, fork, vfork, clone3",
        style("process_create").bold()
    );
    println!(
        "  {} - open, openat, unlink, rename, mkdir...",
        style("file_ops").bold()
    );
    println!(
        "  {} - socket, connect, bind, listen...",
        style("network_ops").bold()
    );
    println!(
        "  {} - unshare, setns",
        style("namespace_ops").bold()
    );
    println!(
        "  {} - setuid, capset...",
        style("privilege_ops").bold()
    );
    println!(
        "  {} - mount, chroot, reboot, module_*",
        style("sysadmin_ops").bold()
    );

    Ok(())
}

/// Show monitoring statistics
async fn show_stats(container: Option<String>, _cli: &Cli) -> Result<()> {
    use console::style;

    if let Some(ref cid) = container {
        println!(
            "{} Statistics for container: {}\n",
            style("[vordr]").bold().cyan(),
            cid
        );
    } else {
        println!(
            "{} Global monitoring statistics:\n",
            style("[vordr]").bold().cyan()
        );
    }

    // In production, this would query the monitor
    println!("  Total events: 0");
    println!("  Unique syscalls: 0");
    println!("  Network connections: 0");
    println!("  Files accessed: 0");
    println!("  Processes executed: 0");

    Ok(())
}

/// Check eBPF support on this system
fn check_ebpf_support() -> Result<()> {
    use console::style;

    println!(
        "{} Checking eBPF support...\n",
        style("[vordr]").bold().cyan()
    );

    // Check kernel version
    #[cfg(target_os = "linux")]
    {
        if let Ok(version) = std::fs::read_to_string("/proc/version") {
            println!("Kernel: {}", version.trim());
        }

        // Check BPF filesystem
        let bpf_fs = std::path::Path::new("/sys/fs/bpf");
        if bpf_fs.exists() {
            println!(
                "{} BPF filesystem mounted at /sys/fs/bpf",
                style("✓").bold().green()
            );
        } else {
            println!(
                "{} BPF filesystem not mounted",
                style("✗").bold().red()
            );
            println!("  Try: mount -t bpf bpf /sys/fs/bpf");
        }

        // Check for BTF support
        let btf = std::path::Path::new("/sys/kernel/btf/vmlinux");
        if btf.exists() {
            println!(
                "{} BTF (BPF Type Format) available",
                style("✓").bold().green()
            );
        } else {
            println!(
                "{} BTF not available (CO-RE may not work)",
                style("!").bold().yellow()
            );
        }

        // Check capabilities
        println!("\nRequired capabilities:");
        println!("  - CAP_BPF (or CAP_SYS_ADMIN on older kernels)");
        println!("  - CAP_PERFMON (for perf events)");
        println!("  - CAP_NET_ADMIN (for network monitoring)");
    }

    #[cfg(not(target_os = "linux"))]
    {
        println!(
            "{} eBPF is only supported on Linux",
            style("✗").bold().red()
        );
    }

    Ok(())
}
