// SPDX-License-Identifier: PMPL-1.0-or-later
//! Cerro Torre (ct) CLI integration

use anyhow::Result;
use sha2::{Digest, Sha256};
use std::process::Command;
use std::{fs::File, io::Read};

/// Wrapper for ct CLI operations
pub struct CtClient {
    ct_path: String,
}

impl CtClient {
    /// Create new ct client
    pub fn new() -> Self {
        let ct_path = which::which("ct")
            .map(|p| p.display().to_string())
            .unwrap_or_else(|_| "ct".to_string());

        Self { ct_path }
    }

    /// Pack image into .ctp bundle
    pub async fn pack(&self, image: &str, output: &str) -> Result<()> {
        tracing::info!("Packing {} -> {}", image, output);

        let status = Command::new(&self.ct_path)
            .args(["pack", image, "-o", output])
            .status()?;

        if !status.success() {
            anyhow::bail!("ct pack failed for {}", image);
        }

        Ok(())
    }

    /// Verify .ctp bundle signature
    pub async fn verify(&self, bundle: &str) -> Result<()> {
        tracing::info!("Verifying {}", bundle);

        let status = Command::new(&self.ct_path)
            .args(["verify", bundle])
            .status()?;

        if !status.success() {
            anyhow::bail!("ct verify failed for {}", bundle);
        }

        Ok(())
    }

    /// Push .ctp bundle to registry
    pub async fn push(&self, bundle: &str, destination: &str) -> Result<()> {
        tracing::info!("Pushing {} -> {}", bundle, destination);

        let status = Command::new(&self.ct_path)
            .args(["push", bundle, destination])
            .status()?;

        if !status.success() {
            anyhow::bail!("ct push failed for {}", bundle);
        }

        Ok(())
    }

    /// Pull .ctp bundle from registry
    pub async fn pull(&self, reference: &str, output: &str) -> Result<()> {
        tracing::info!("Pulling {} -> {}", reference, output);

        let status = Command::new(&self.ct_path)
            .args(["fetch", reference, "-o", output])
            .status()?;

        if !status.success() {
            anyhow::bail!("ct fetch failed for {}", reference);
        }

        Ok(())
    }

    /// Run arbitrary ct command with given arguments
    pub async fn run_command(&self, args: &[String]) -> Result<()> {
        tracing::debug!("Running ct with args: {:?}", args);

        let status = Command::new(&self.ct_path)
            .args(args)
            .status()?;

        if !status.success() {
            anyhow::bail!("ct command failed: {:?}", args);
        }

        Ok(())
    }

    /// Calculate sha256 digest for a bundle file
    pub fn bundle_digest(&self, bundle_path: &str) -> Result<String> {
        let mut file = File::open(bundle_path)?;
        let mut hasher = Sha256::new();
        let mut buf = [0u8; 8192];
        loop {
            let read = file.read(&mut buf)?;
            if read == 0 {
                break;
            }
            hasher.update(&buf[..read]);
        }
        let digest = hasher.finalize();
        Ok(format!("sha256:{:x}", digest))
    }
}
