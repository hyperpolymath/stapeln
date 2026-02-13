// SPDX-License-Identifier: PMPL-1.0-or-later
//! Policy explanation command - helps users understand why actions were blocked

use anyhow::{bail, Result};
use clap::{Parser, ValueEnum};
use console::style;


use crate::cli::Cli;

/// Explain why a policy blocked an action
#[derive(Parser, Debug)]
pub struct ExplainArgs {
    /// Event ID or container ID to explain
    #[arg(required_unless_present = "last")]
    pub event_id: Option<String>,

    /// Explain the last rejection
    #[arg(short, long)]
    pub last: bool,

    /// Output format
    #[arg(long, value_enum, default_value = "human")]
    pub format: OutputFormat,

    /// Show fix suggestions
    #[arg(long, default_value = "true")]
    pub suggest: bool,
}

#[derive(Debug, Clone, Copy, ValueEnum, Default)]
pub enum OutputFormat {
    #[default]
    Human,
    Json,
}

/// Policy rejection event
#[derive(Debug, Clone, serde::Serialize)]
pub struct PolicyEvent {
    pub id: String,
    pub timestamp: String,
    pub container_id: String,
    pub container_name: String,
    pub policy_rule: String,
    pub action: String,
    pub target: String,
    pub reason: String,
    pub severity: Severity,
    pub profile: String,
}

#[derive(Debug, Clone, Copy, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Severity {
    Critical,
    High,
    Medium,
    Low,
}

/// Explanation with suggestions
#[derive(Debug, Clone, serde::Serialize)]
pub struct Explanation {
    pub event: PolicyEvent,
    pub explanation: String,
    pub context: String,
    pub suggestions: Vec<Suggestion>,
    pub documentation_url: String,
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct Suggestion {
    pub title: String,
    pub description: String,
    pub command: Option<String>,
    pub risk_level: String,
}

/// Execute explain command
pub async fn execute(args: ExplainArgs, cli: &Cli) -> Result<()> {
    let event = if args.last {
        get_last_rejection(cli)?
    } else if let Some(ref id) = args.event_id {
        get_rejection_by_id(id, cli)?
    } else {
        bail!("Must specify either --last or an event ID");
    };

    let explanation = generate_explanation(&event);

    match args.format {
        OutputFormat::Json => {
            println!("{}", serde_json::to_string_pretty(&explanation)?);
        }
        OutputFormat::Human => {
            print_human_explanation(&explanation, args.suggest);
        }
    }

    Ok(())
}

fn get_last_rejection(_cli: &Cli) -> Result<PolicyEvent> {
    // In a real implementation, this would query the event log
    // For now, return a sample rejection for demonstration
    Ok(PolicyEvent {
        id: "evt_abc123".to_string(),
        timestamp: chrono::Utc::now().to_rfc3339(),
        container_id: "c7d8e9f0".to_string(),
        container_name: "my-app".to_string(),
        policy_rule: "network.egress.blocked".to_string(),
        action: "connect".to_string(),
        target: "169.254.169.254:80".to_string(),
        reason: "Blocked access to cloud metadata endpoint".to_string(),
        severity: Severity::Critical,
        profile: "balanced".to_string(),
    })
}

fn get_rejection_by_id(id: &str, _cli: &Cli) -> Result<PolicyEvent> {
    // In a real implementation, would query by ID
    // For now, return a sample based on common patterns

    if id.starts_with("cap_") || id.contains("capability") {
        return Ok(PolicyEvent {
            id: id.to_string(),
            timestamp: chrono::Utc::now().to_rfc3339(),
            container_id: "a1b2c3d4".to_string(),
            container_name: "privileged-app".to_string(),
            policy_rule: "capability.denied".to_string(),
            action: "add_capability".to_string(),
            target: "CAP_SYS_ADMIN".to_string(),
            reason: "Capability CAP_SYS_ADMIN is not allowed".to_string(),
            severity: Severity::High,
            profile: "strict".to_string(),
        });
    }

    if id.starts_with("mount_") || id.contains("mount") {
        return Ok(PolicyEvent {
            id: id.to_string(),
            timestamp: chrono::Utc::now().to_rfc3339(),
            container_id: "e5f6g7h8".to_string(),
            container_name: "volume-app".to_string(),
            policy_rule: "mount.sensitive.blocked".to_string(),
            action: "mount".to_string(),
            target: "/etc/shadow".to_string(),
            reason: "Mount of sensitive host path denied".to_string(),
            severity: Severity::Critical,
            profile: "balanced".to_string(),
        });
    }

    // Default to network rejection
    Ok(PolicyEvent {
        id: id.to_string(),
        timestamp: chrono::Utc::now().to_rfc3339(),
        container_id: "c7d8e9f0".to_string(),
        container_name: "web-app".to_string(),
        policy_rule: "network.egress.blocked".to_string(),
        action: "connect".to_string(),
        target: "169.254.169.254:80".to_string(),
        reason: "Blocked access to cloud metadata endpoint".to_string(),
        severity: Severity::Critical,
        profile: "balanced".to_string(),
    })
}

fn generate_explanation(event: &PolicyEvent) -> Explanation {
    let (explanation, context, suggestions, doc_url) = match event.policy_rule.as_str() {
        "network.egress.blocked" => {
            if event.target.starts_with("169.254.169.254") {
                (
                    "The container attempted to access the cloud instance metadata service. \
                     This is a common attack vector for credential theft in cloud environments."
                        .to_string(),
                    "Cloud metadata endpoints (169.254.169.254) provide sensitive information \
                     including temporary credentials, instance identity, and configuration. \
                     Attackers who compromise a container often attempt to access this endpoint \
                     to escalate privileges or move laterally."
                        .to_string(),
                    vec![
                        Suggestion {
                            title: "Use IMDSv2 with hop limit".to_string(),
                            description: "Configure your cloud instance to require IMDSv2 with \
                                         a hop limit of 1, which prevents containers from accessing metadata."
                                .to_string(),
                            command: Some(
                                "aws ec2 modify-instance-metadata-options --instance-id <id> \
                                 --http-tokens required --http-put-response-hop-limit 1"
                                    .to_string(),
                            ),
                            risk_level: "none".to_string(),
                        },
                        Suggestion {
                            title: "Use explicit credentials".to_string(),
                            description: "If your application needs AWS credentials, use \
                                         environment variables or mounted secrets instead of IMDS."
                                .to_string(),
                            command: Some(
                                "vordr run --env AWS_ACCESS_KEY_ID=... --env AWS_SECRET_ACCESS_KEY=..."
                                    .to_string(),
                            ),
                            risk_level: "low".to_string(),
                        },
                        Suggestion {
                            title: "Allow metadata access (not recommended)".to_string(),
                            description: "If your application legitimately needs metadata access, \
                                         you can use the dev profile or create a custom profile."
                                .to_string(),
                            command: Some("vordr run --profile dev ...".to_string()),
                            risk_level: "high".to_string(),
                        },
                    ],
                    "https://svalinn.dev/docs/security/cloud-metadata".to_string(),
                )
            } else {
                (
                    format!(
                        "The container attempted to connect to {} which is not allowed \
                         by the current network policy.",
                        event.target
                    ),
                    "Network egress controls prevent containers from making unauthorized \
                     connections. This helps contain breaches and prevents data exfiltration."
                        .to_string(),
                    vec![
                        Suggestion {
                            title: "Add network allow rule".to_string(),
                            description: "Allow specific destinations in your container configuration."
                                .to_string(),
                            command: Some(format!(
                                "vordr run --network-allow {} ...",
                                event.target
                            )),
                            risk_level: "medium".to_string(),
                        },
                        Suggestion {
                            title: "Use bridge networking".to_string(),
                            description: "Switch to bridge network mode for general connectivity."
                                .to_string(),
                            command: Some("vordr run --network bridge ...".to_string()),
                            risk_level: "medium".to_string(),
                        },
                    ],
                    "https://svalinn.dev/docs/security/networking".to_string(),
                )
            }
        }

        "capability.denied" => (
            format!(
                "The container requested capability {} which is not permitted \
                 in the '{}' security profile.",
                event.target, event.profile
            ),
            "Linux capabilities divide root privileges into distinct units. \
             The requested capability would grant significant system access. \
             Vordr denies dangerous capabilities by default to limit blast radius."
                .to_string(),
            vec![
                Suggestion {
                    title: "Use a less restrictive profile".to_string(),
                    description: "The 'balanced' or 'dev' profile allows more capabilities."
                        .to_string(),
                    command: Some("vordr run --profile balanced ...".to_string()),
                    risk_level: "medium".to_string(),
                },
                Suggestion {
                    title: "Add specific capability".to_string(),
                    description: format!(
                        "Explicitly grant only {} if truly needed.",
                        event.target
                    ),
                    command: Some(format!("vordr run --cap-add {} ...", event.target)),
                    risk_level: "high".to_string(),
                },
                Suggestion {
                    title: "Review if capability is needed".to_string(),
                    description: "Many applications request capabilities they don't actually need. \
                                 Check if the application works without it."
                        .to_string(),
                    command: None,
                    risk_level: "none".to_string(),
                },
            ],
            "https://svalinn.dev/docs/security/capabilities".to_string(),
        ),

        "mount.sensitive.blocked" => (
            format!(
                "The container attempted to mount '{}' which is a sensitive host path.",
                event.target
            ),
            "Mounting sensitive host paths like /etc/shadow, /etc/passwd, or system \
             directories can allow container escape or host compromise. Vordr blocks \
             these mounts by default."
                .to_string(),
            vec![
                Suggestion {
                    title: "Use a volume instead".to_string(),
                    description: "Create a named volume for persistent data instead of \
                                 mounting host paths."
                        .to_string(),
                    command: Some("vordr volume create mydata && vordr run -v mydata:/data ...".to_string()),
                    risk_level: "low".to_string(),
                },
                Suggestion {
                    title: "Mount a subdirectory".to_string(),
                    description: "Mount only the specific directory needed, not system paths."
                        .to_string(),
                    command: Some("vordr run -v /home/user/app/data:/data ...".to_string()),
                    risk_level: "low".to_string(),
                },
                Suggestion {
                    title: "Use read-only mount".to_string(),
                    description: "If you must mount sensitive paths, make them read-only."
                        .to_string(),
                    command: Some(format!("vordr run -v {}:/target:ro ...", event.target)),
                    risk_level: "medium".to_string(),
                },
            ],
            "https://svalinn.dev/docs/security/volumes".to_string(),
        ),

        _ => (
            format!(
                "Policy '{}' blocked action '{}' on target '{}'.",
                event.policy_rule, event.action, event.target
            ),
            "This action was blocked by Vordr's security policies.".to_string(),
            vec![
                Suggestion {
                    title: "Try a different profile".to_string(),
                    description: "Use 'vordr profile ls' to see available profiles.".to_string(),
                    command: Some("vordr profile ls".to_string()),
                    risk_level: "varies".to_string(),
                },
                Suggestion {
                    title: "Check documentation".to_string(),
                    description: "Review the security documentation for this policy.".to_string(),
                    command: None,
                    risk_level: "none".to_string(),
                },
            ],
            "https://svalinn.dev/docs/security".to_string(),
        ),
    };

    Explanation {
        event: event.clone(),
        explanation,
        context,
        suggestions,
        documentation_url: doc_url,
    }
}

fn print_human_explanation(explanation: &Explanation, show_suggestions: bool) {
    let event = &explanation.event;

    // Header
    println!();
    let severity_style = match event.severity {
        Severity::Critical => style("CRITICAL").red().bold(),
        Severity::High => style("HIGH").red(),
        Severity::Medium => style("MEDIUM").yellow(),
        Severity::Low => style("LOW").dim(),
    };
    println!(
        "{} {} Policy Rejection",
        style("⛔").red(),
        severity_style
    );
    println!("{}", "═".repeat(60));
    println!();

    // Event details
    println!("{}: {}", style("Event ID").bold(), event.id);
    println!("{}: {}", style("Time").bold(), event.timestamp);
    println!(
        "{}: {} ({})",
        style("Container").bold(),
        event.container_name,
        &event.container_id[..8.min(event.container_id.len())]
    );
    println!("{}: {}", style("Profile").bold(), event.profile);
    println!();

    // What happened
    println!("{}", style("What happened:").bold().underlined());
    println!("  Action: {} -> {}", event.action, event.target);
    println!("  Rule:   {}", event.policy_rule);
    println!("  Reason: {}", event.reason);
    println!();

    // Explanation
    println!("{}", style("Why this was blocked:").bold().underlined());
    for line in textwrap::wrap(&explanation.explanation, 70) {
        println!("  {}", line);
    }
    println!();

    // Context
    println!("{}", style("Security context:").bold().underlined());
    for line in textwrap::wrap(&explanation.context, 70) {
        println!("  {}", line);
    }
    println!();

    // Suggestions
    if show_suggestions && !explanation.suggestions.is_empty() {
        println!("{}", style("Suggestions:").bold().underlined());
        println!();

        for (i, suggestion) in explanation.suggestions.iter().enumerate() {
            let risk_icon = match suggestion.risk_level.as_str() {
                "none" => style("✓").green(),
                "low" => style("○").green(),
                "medium" => style("◐").yellow(),
                "high" => style("●").red(),
                _ => style("?").dim(),
            };

            println!(
                "  {}. {} {} (risk: {})",
                i + 1,
                risk_icon,
                style(&suggestion.title).bold(),
                suggestion.risk_level
            );

            for line in textwrap::wrap(&suggestion.description, 65) {
                println!("     {}", line);
            }

            if let Some(ref cmd) = suggestion.command {
                println!();
                println!("     {}", style("$").dim());
                println!("     {}", style(cmd).cyan());
            }
            println!();
        }
    }

    // Documentation link
    println!(
        "{}: {}",
        style("Documentation").bold(),
        style(&explanation.documentation_url).cyan().underlined()
    );
    println!();
}
