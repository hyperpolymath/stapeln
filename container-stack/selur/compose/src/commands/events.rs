// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose events` command implementation

use anyhow::Result;
use tokio::time::{sleep, Duration};

use crate::vordr::VordrClient;

/// Run `events` command - stream real-time events
pub async fn events(project_name: &str) -> Result<()> {
    tracing::info!("Streaming events for project '{}'", project_name);

    // Initialize Vörðr client
    let vordr_url = std::env::var("VORDR_URL")
        .unwrap_or_else(|_| "http://localhost:9090".to_string());
    let vordr_client = VordrClient::new(vordr_url);

    println!("Streaming events (Ctrl+C to stop)...");
    println!();

    // Poll for events (in production, this would be a WebSocket or SSE stream)
    let mut last_event_id = 0;

    loop {
        match vordr_client.get_events(project_name, last_event_id).await {
            Ok(events) => {
                for event in events {
                    // Format: TIMESTAMP TYPE CONTAINER ACTION [ATTRIBUTES]
                    print_event(&event);

                    if event.id > last_event_id {
                        last_event_id = event.id;
                    }
                }
            }
            Err(e) => {
                tracing::warn!("Error fetching events: {}", e);
            }
        }

        // Poll every second (in production, use WebSocket/SSE)
        sleep(Duration::from_secs(1)).await;
    }
}

/// Print a formatted event
fn print_event(event: &crate::vordr::Event) {
    let timestamp = &event.timestamp;
    let event_type = &event.event_type;
    let container = &event.container_id;
    let action = &event.action;

    print!("{} {} {} {}", timestamp, event_type, container, action);

    // Add attributes if present
    if !event.attributes.is_empty() {
        print!(" ");
        for (key, value) in &event.attributes {
            print!("{}={} ", key, value);
        }
    }

    println!();
}
