// SPDX-License-Identifier: PMPL-1.0-or-later
//! `selur-compose images` command implementation
//!
//! Lists images used by services defined in the compose file.

use anyhow::Result;
use prettytable::{Cell, Row, Table, format};

use crate::compose::ComposeFile;
use crate::vordr::VordrClient;

/// Run `images` command - list images used by compose services
pub async fn images(compose: &ComposeFile) -> Result<()> {
    tracing::info!("Listing images for compose services");

    let vordr_url = std::env::var("VORDR_URL")
        .unwrap_or_else(|_| "http://localhost:9090".to_string());
    let vordr_client = VordrClient::new(vordr_url);

    // Fetch all images from the runtime
    let all_images = match vordr_client.list_images().await {
        Ok(imgs) => imgs,
        Err(e) => {
            tracing::warn!("Could not fetch images from Vordr: {}", e);
            Vec::new()
        }
    };

    let mut table = Table::new();
    table.set_format(*format::consts::FORMAT_BOX_CHARS);

    table.add_row(Row::new(vec![
        Cell::new("SERVICE"),
        Cell::new("IMAGE"),
        Cell::new("TAG"),
        Cell::new("IMAGE ID"),
        Cell::new("SIZE"),
    ]));

    for (service_name, service) in &compose.services {
        // Parse image reference into repository:tag
        let (repo, tag) = parse_image_ref(&service.image);

        // Try to find matching image in runtime
        let (image_id, size) = match all_images
            .iter()
            .find(|img| img.repository == repo && img.tag == tag)
        {
            Some(img) => {
                let short_id = if img.id.len() >= 12 {
                    &img.id[..12]
                } else {
                    &img.id
                };
                let size_str = format_size(img.size);
                (short_id.to_string(), size_str)
            }
            None => ("N/A".to_string(), "N/A".to_string()),
        };

        table.add_row(Row::new(vec![
            Cell::new(service_name),
            Cell::new(&repo),
            Cell::new(&tag),
            Cell::new(&image_id),
            Cell::new(&size),
        ]));
    }

    table.printstd();
    println!();
    println!("Total: {} service image(s)", compose.services.len());

    Ok(())
}

/// Parse an image reference into (repository, tag)
/// Examples:
///   "nginx:latest.ctp" -> ("nginx", "latest.ctp")
///   "registry.example.com/app:v1.0.ctp" -> ("registry.example.com/app", "v1.0.ctp")
///   "myapp.ctp" -> ("myapp.ctp", "latest")
fn parse_image_ref(image: &str) -> (String, String) {
    // Split on the last colon that is not part of a port number
    // Simple heuristic: if the part after the last colon contains '/' it is a port
    if let Some(colon_pos) = image.rfind(':') {
        let after_colon = &image[colon_pos + 1..];
        if !after_colon.contains('/') {
            return (
                image[..colon_pos].to_string(),
                after_colon.to_string(),
            );
        }
    }

    (image.to_string(), "latest".to_string())
}

/// Format a byte size into a human-readable string
fn format_size(bytes: u64) -> String {
    const KB: u64 = 1024;
    const MB: u64 = KB * 1024;
    const GB: u64 = MB * 1024;

    if bytes >= GB {
        format!("{:.1} GB", bytes as f64 / GB as f64)
    } else if bytes >= MB {
        format!("{:.1} MB", bytes as f64 / MB as f64)
    } else if bytes >= KB {
        format!("{:.1} KB", bytes as f64 / KB as f64)
    } else {
        format!("{} B", bytes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_image_ref() {
        assert_eq!(
            parse_image_ref("nginx:latest.ctp"),
            ("nginx".to_string(), "latest.ctp".to_string())
        );
        assert_eq!(
            parse_image_ref("myapp.ctp"),
            ("myapp.ctp".to_string(), "latest".to_string())
        );
        assert_eq!(
            parse_image_ref("registry.example.com/app:v1.0"),
            ("registry.example.com/app".to_string(), "v1.0".to_string())
        );
    }

    #[test]
    fn test_format_size() {
        assert_eq!(format_size(500), "500 B");
        assert_eq!(format_size(1024), "1.0 KB");
        assert_eq!(format_size(1_048_576), "1.0 MB");
        assert_eq!(format_size(1_073_741_824), "1.0 GB");
    }
}
