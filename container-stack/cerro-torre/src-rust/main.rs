// SPDX-License-Identifier: PMPL-1.0-or-later
// Cerro Torre Cryptographic Signing CLI
// Replaces shell scripts with native Ed25519 operations

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use ed25519_dalek::{Signature, Signer, SigningKey, VerifyingKey};
use rand::rngs::OsRng;
use sha2::{Digest, Sha256};
use std::fs;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "cerro-sign")]
#[command(about = "Ed25519 signing utilities for Cerro Torre", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Generate a new Ed25519 keypair
    Keygen {
        /// Output path for private key
        #[arg(long)]
        priv_key: PathBuf,

        /// Output path for public key
        #[arg(long)]
        pub_key: PathBuf,
    },

    /// Sign a message with Ed25519
    Sign {
        /// Path to private key (hex format)
        #[arg(long)]
        key: PathBuf,

        /// Message to sign (hex hash)
        #[arg(long)]
        message: String,

        /// Output path for signature (hex format)
        #[arg(long)]
        output: PathBuf,
    },

    /// Verify an Ed25519 signature
    Verify {
        /// Path to public key (hex format)
        #[arg(long)]
        pub_key: PathBuf,

        /// Message that was signed (hex hash)
        #[arg(long)]
        message: String,

        /// Signature to verify (hex format)
        #[arg(long)]
        signature: String,
    },

    /// Get fingerprint of a public key
    Fingerprint {
        /// Path to public key (hex format)
        #[arg(long)]
        pub_key: PathBuf,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Keygen { priv_key, pub_key } => keygen(priv_key, pub_key),
        Commands::Sign {
            key,
            message,
            output,
        } => sign(key, message, output),
        Commands::Verify {
            pub_key,
            message,
            signature,
        } => verify(pub_key, message, signature),
        Commands::Fingerprint { pub_key } => fingerprint(pub_key),
    }
}

/// Generate a new Ed25519 keypair
fn keygen(priv_path: PathBuf, pub_path: PathBuf) -> Result<()> {
    // Generate keypair
    let mut csprng = OsRng;
    let signing_key = SigningKey::generate(&mut csprng);
    let verifying_key = signing_key.verifying_key();

    // Save private key (64 hex chars = 32 bytes)
    let priv_hex = hex::encode(signing_key.to_bytes());
    fs::write(&priv_path, &priv_hex).context("Failed to write private key")?;

    // Save public key (64 hex chars = 32 bytes)
    let pub_hex = hex::encode(verifying_key.to_bytes());
    fs::write(&pub_path, &pub_hex).context("Failed to write public key")?;

    eprintln!("✓ Keypair generated");
    eprintln!("  Private: {}", priv_path.display());
    eprintln!("  Public:  {}", pub_path.display());

    Ok(())
}

/// Sign a message with Ed25519
fn sign(key_path: PathBuf, message_hex: String, output_path: PathBuf) -> Result<()> {
    // Load private key from hex file
    let priv_hex = fs::read_to_string(&key_path).context("Failed to read private key")?;
    let priv_hex = priv_hex.trim();

    // Convert hex to bytes (first 64 hex chars = 32-byte seed)
    let seed_hex = if priv_hex.len() >= 64 {
        &priv_hex[..64]
    } else {
        priv_hex
    };

    let seed_bytes = hex::decode(seed_hex).context("Invalid private key hex format")?;
    if seed_bytes.len() != 32 {
        anyhow::bail!("Private key must be 32 bytes (64 hex chars)");
    }

    // Reconstruct signing key from seed
    let mut seed_array = [0u8; 32];
    seed_array.copy_from_slice(&seed_bytes);
    let signing_key = SigningKey::from_bytes(&seed_array);

    // Decode message hex to bytes
    let message_bytes =
        hex::decode(message_hex.trim()).context("Invalid message hex format")?;

    // Sign message
    let signature: Signature = signing_key.sign(&message_bytes);

    // Write signature as hex (128 hex chars = 64 bytes)
    let sig_hex = hex::encode(signature.to_bytes());
    fs::write(&output_path, sig_hex).context("Failed to write signature")?;

    eprintln!("✓ Signature generated: {}", output_path.display());

    Ok(())
}

/// Verify an Ed25519 signature
fn verify(pub_key_path: PathBuf, message_hex: String, signature_hex: String) -> Result<()> {
    use ed25519_dalek::Verifier;

    // Load public key from hex file
    let pub_hex = fs::read_to_string(&pub_key_path).context("Failed to read public key")?;
    let pub_hex = pub_hex.trim();

    let pub_bytes = hex::decode(pub_hex).context("Invalid public key hex format")?;
    if pub_bytes.len() != 32 {
        anyhow::bail!("Public key must be 32 bytes (64 hex chars)");
    }

    let mut pub_array = [0u8; 32];
    pub_array.copy_from_slice(&pub_bytes);
    let verifying_key = VerifyingKey::from_bytes(&pub_array)
        .context("Invalid public key")?;

    // Decode message and signature
    let message_bytes =
        hex::decode(message_hex.trim()).context("Invalid message hex format")?;

    let sig_bytes =
        hex::decode(signature_hex.trim()).context("Invalid signature hex format")?;
    if sig_bytes.len() != 64 {
        anyhow::bail!("Signature must be 64 bytes (128 hex chars)");
    }

    let mut sig_array = [0u8; 64];
    sig_array.copy_from_slice(&sig_bytes);
    let signature =
        Signature::from_bytes(&sig_array);

    // Verify signature
    verifying_key
        .verify(&message_bytes, &signature)
        .context("Signature verification failed")?;

    eprintln!("✓ Signature valid");

    Ok(())
}

/// Compute SHA-256 fingerprint of a public key
fn fingerprint(pub_key_path: PathBuf) -> Result<()> {
    // Load public key from hex file
    let pub_hex = fs::read_to_string(&pub_key_path).context("Failed to read public key")?;
    let pub_hex = pub_hex.trim();

    let pub_bytes = hex::decode(pub_hex).context("Invalid public key hex format")?;
    if pub_bytes.len() != 32 {
        anyhow::bail!("Public key must be 32 bytes (64 hex chars)");
    }

    // Compute SHA-256 hash of public key
    let mut hasher = Sha256::new();
    hasher.update(&pub_bytes);
    let hash = hasher.finalize();

    // Output fingerprint as hex
    println!("{}", hex::encode(hash));

    Ok(())
}
