// SPDX-License-Identifier: PMPL-1.0-or-later
//! Image management commands

use anyhow::{Context, Result};
use clap::Subcommand;
use std::path::Path;

use crate::cli::Cli;
use crate::engine::StateManager;

#[derive(Subcommand, Debug)]
pub enum ImageCommands {
    /// List images
    Ls {
        /// Show all images (default hides intermediate images)
        #[arg(short, long)]
        all: bool,

        /// Only show image IDs
        #[arg(short, long)]
        quiet: bool,
    },

    /// Remove an image
    Rm {
        /// Image ID or name
        image: String,

        /// Force removal
        #[arg(short, long)]
        force: bool,
    },

    /// Show image details
    Inspect {
        /// Image ID or name
        image: String,
    },

    /// Remove unused images
    Prune {
        /// Remove all unused images, not just dangling ones
        #[arg(short, long)]
        all: bool,

        /// Do not prompt for confirmation
        #[arg(short, long)]
        force: bool,
    },
}

pub async fn execute(cmd: ImageCommands, cli: &Cli) -> Result<()> {
    match cmd {
        ImageCommands::Ls { all: _, quiet } => list_images(quiet, cli).await,
        ImageCommands::Rm { image, force: _ } => remove_image(&image, cli).await,
        ImageCommands::Inspect { image } => inspect_image(&image, cli).await,
        ImageCommands::Prune { all: _, force: _ } => prune_images(cli).await,
    }
}

async fn list_images(quiet: bool, cli: &Cli) -> Result<()> {
    let db_path = Path::new(&cli.db_path);

    if !db_path.exists() {
        if quiet {
            return Ok(());
        }
        println!("REPOSITORY          TAG                 IMAGE ID            SIZE");
        return Ok(());
    }

    let state = StateManager::open(db_path)
        .context("Failed to open state database")?;

    let images = state.list_images()?;

    if quiet {
        for image in &images {
            println!("{}", &image.id[..12]);
        }
    } else {
        println!("{:<20} {:<20} {:<20} {:<10}",
            "REPOSITORY", "TAG", "IMAGE ID", "SIZE");

        for image in &images {
            let repo = image.repository.as_deref().unwrap_or("<none>");
            let tag = image.tags.first().map(|s| s.as_str()).unwrap_or("latest");
            let size = format_size(image.size);

            println!("{:<20} {:<20} {:<20} {:<10}",
                truncate(repo, 20),
                truncate(tag, 20),
                &image.id[..12.min(image.id.len())],
                size);
        }
    }

    Ok(())
}

async fn remove_image(image_id: &str, cli: &Cli) -> Result<()> {
    let state = StateManager::open(Path::new(&cli.db_path))
        .context("Failed to open state database")?;

    let image = state.get_image(image_id)?;
    state.delete_image(&image.id)?;

    println!("Removed: {}", &image.id[..12]);
    Ok(())
}

async fn inspect_image(image_id: &str, cli: &Cli) -> Result<()> {
    let state = StateManager::open(Path::new(&cli.db_path))
        .context("Failed to open state database")?;

    let image = state.get_image(image_id)?;

    let output = serde_json::json!({
        "Id": image.id,
        "Digest": image.digest,
        "Repository": image.repository,
        "Tags": image.tags,
        "Size": image.size,
        "Created": image.created_at,
    });

    println!("{}", serde_json::to_string_pretty(&output)?);
    Ok(())
}

async fn prune_images(_cli: &Cli) -> Result<()> {
    println!("Image pruning not yet implemented");
    Ok(())
}

fn format_size(bytes: i64) -> String {
    const KB: i64 = 1024;
    const MB: i64 = KB * 1024;
    const GB: i64 = MB * 1024;

    if bytes >= GB {
        format!("{:.1}GB", bytes as f64 / GB as f64)
    } else if bytes >= MB {
        format!("{:.1}MB", bytes as f64 / MB as f64)
    } else if bytes >= KB {
        format!("{:.1}KB", bytes as f64 / KB as f64)
    } else {
        format!("{}B", bytes)
    }
}

fn truncate(s: &str, max: usize) -> String {
    if s.len() <= max {
        s.to_string()
    } else {
        format!("{}...", &s[..max - 3])
    }
}
