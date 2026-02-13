// SPDX-License-Identifier: PMPL-1.0-or-later
//! OCI Distribution Specification client for image pull/push

use oci_spec::image::{ImageConfiguration, ImageManifest};
use reqwest::header::{HeaderMap, HeaderValue, ACCEPT, AUTHORIZATION};
use serde::Deserialize;
use sha2::{Digest, Sha256};
use std::path::Path;
use thiserror::Error;
use tracing::{debug, info};

#[derive(Error, Debug)]
pub enum RegistryError {
    #[error("Authentication required for {0}")]
    AuthRequired(String),
    #[error("Authentication failed: {0}")]
    AuthFailed(String),
    #[error("Image not found: {0}")]
    NotFound(String),
    #[error("Registry error: {0}")]
    RegistryError(String),
    #[error("Network error: {0}")]
    NetworkError(String),
    #[error("Invalid image reference: {0}")]
    InvalidReference(String),
    #[error("Digest mismatch: expected {expected}, got {actual}")]
    DigestMismatch { expected: String, actual: String },
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),
    #[error("HTTP error: {0}")]
    Http(#[from] reqwest::Error),
}

/// Parsed image reference
#[derive(Debug, Clone)]
pub struct ImageReference {
    pub registry: String,
    pub repository: String,
    pub tag: Option<String>,
    pub digest: Option<String>,
}

impl ImageReference {
    /// Parse an image reference string
    ///
    /// Examples:
    /// - "alpine" -> docker.io/library/alpine:latest
    /// - "myregistry.com/myimage:v1" -> myregistry.com/myimage:v1
    /// - "ghcr.io/owner/repo@sha256:..." -> ghcr.io/owner/repo@sha256:...
    pub fn parse(reference: &str) -> Result<Self, RegistryError> {
        let reference = reference.trim();

        if reference.is_empty() {
            return Err(RegistryError::InvalidReference(
                "empty reference".to_string(),
            ));
        }

        // Check for digest
        let (ref_without_digest, digest) = if let Some(pos) = reference.rfind('@') {
            let digest = &reference[pos + 1..];
            if !digest.starts_with("sha256:") {
                return Err(RegistryError::InvalidReference(format!(
                    "invalid digest format: {}",
                    digest
                )));
            }
            (&reference[..pos], Some(digest.to_string()))
        } else {
            (reference, None)
        };

        // Check for tag
        let (ref_without_tag, tag) = if let Some(pos) = ref_without_digest.rfind(':') {
            // Make sure this isn't a port number
            let after_colon = &ref_without_digest[pos + 1..];
            if after_colon.chars().all(|c| c.is_ascii_alphanumeric() || c == '.' || c == '-' || c == '_') {
                (&ref_without_digest[..pos], Some(after_colon.to_string()))
            } else {
                (ref_without_digest, None)
            }
        } else {
            (ref_without_digest, None)
        };

        // Parse registry and repository
        let (registry, repository) = if ref_without_tag.contains('/') {
            let first_slash = ref_without_tag.find('/').expect("checked contains('/') above");
            let first_part = &ref_without_tag[..first_slash];

            // Check if first part looks like a registry (contains . or :)
            if first_part.contains('.') || first_part.contains(':') || first_part == "localhost" {
                (
                    first_part.to_string(),
                    ref_without_tag[first_slash + 1..].to_string(),
                )
            } else {
                // Docker Hub with organization
                ("docker.io".to_string(), ref_without_tag.to_string())
            }
        } else {
            // Docker Hub official image
            (
                "docker.io".to_string(),
                format!("library/{}", ref_without_tag),
            )
        };

        Ok(ImageReference {
            registry,
            repository,
            tag: tag.or(Some("latest".to_string())),
            digest,
        })
    }

    /// Get the full reference string
    pub fn full_reference(&self) -> String {
        let mut ref_str = format!("{}/{}", self.registry, self.repository);

        if let Some(ref digest) = self.digest {
            ref_str.push('@');
            ref_str.push_str(digest);
        } else if let Some(ref tag) = self.tag {
            ref_str.push(':');
            ref_str.push_str(tag);
        }

        ref_str
    }
}

/// Authentication token response
#[derive(Debug, Deserialize)]
struct AuthResponse {
    token: Option<String>,
    access_token: Option<String>,
}

/// OCI registry client
pub struct RegistryClient {
    http_client: reqwest::Client,
    auth_cache: std::collections::HashMap<String, String>,
}

impl RegistryClient {
    /// Create a new registry client
    pub fn new() -> Self {
        Self {
            http_client: reqwest::Client::builder()
                .user_agent("vordr/0.1.0")
                .build()
                .expect("Failed to create HTTP client"),
            auth_cache: std::collections::HashMap::new(),
        }
    }

    /// Get authentication token for a registry
    async fn get_token(&mut self, registry: &str, repository: &str) -> Result<Option<String>, RegistryError> {
        // Check cache
        let cache_key = format!("{}/{}", registry, repository);
        if let Some(token) = self.auth_cache.get(&cache_key) {
            return Ok(Some(token.clone()));
        }

        // Try to access without auth first
        let url = format!("https://{}/v2/", registry);
        let response = self.http_client.get(&url).send().await?;

        if response.status() == 401 {
            // Need authentication
            if let Some(www_auth) = response.headers().get("www-authenticate") {
                let auth_str = www_auth.to_str().unwrap_or("");
                return self.do_token_auth(auth_str, repository).await;
            }
        }

        Ok(None)
    }

    /// Perform token authentication
    async fn do_token_auth(
        &mut self,
        www_auth: &str,
        repository: &str,
    ) -> Result<Option<String>, RegistryError> {
        // Parse Bearer realm="...",service="...",scope="..."
        let parts: std::collections::HashMap<&str, &str> = www_auth
            .strip_prefix("Bearer ")
            .unwrap_or("")
            .split(',')
            .filter_map(|part| {
                let mut kv = part.splitn(2, '=');
                let key = kv.next()?.trim();
                let value = kv.next()?.trim().trim_matches('"');
                Some((key, value))
            })
            .collect();

        let realm = parts.get("realm").ok_or_else(|| {
            RegistryError::AuthFailed("missing realm in www-authenticate".to_string())
        })?;

        let service = parts.get("service").map(|s| s.to_string());
        let scope = format!("repository:{}:pull", repository);

        // Build auth URL
        let mut auth_url = format!("{}?scope={}", realm, scope);
        if let Some(svc) = service {
            auth_url.push_str(&format!("&service={}", svc));
        }

        debug!("Authenticating at: {}", auth_url);

        let response: AuthResponse = self
            .http_client
            .get(&auth_url)
            .send()
            .await?
            .json()
            .await?;

        let token = response.token.or(response.access_token);
        Ok(token)
    }

    /// Build headers with authentication
    fn build_headers(&self, token: Option<&str>) -> HeaderMap {
        let mut headers = HeaderMap::new();
        headers.insert(
            ACCEPT,
            HeaderValue::from_static(
                "application/vnd.oci.image.manifest.v1+json, application/vnd.docker.distribution.manifest.v2+json",
            ),
        );

        if let Some(token) = token {
            if let Ok(value) = HeaderValue::from_str(&format!("Bearer {}", token)) {
                headers.insert(AUTHORIZATION, value);
            }
        }

        headers
    }

    /// Pull an image manifest
    pub async fn get_manifest(&mut self, reference: &ImageReference) -> Result<ImageManifest, RegistryError> {
        let token = self.get_token(&reference.registry, &reference.repository).await?;

        let tag_or_digest = reference
            .digest
            .as_ref()
            .or(reference.tag.as_ref())
            .ok_or_else(|| RegistryError::InvalidReference("no tag or digest".to_string()))?;

        let url = format!(
            "https://{}/v2/{}/manifests/{}",
            reference.registry, reference.repository, tag_or_digest
        );

        info!("Fetching manifest from: {}", url);

        let response = self
            .http_client
            .get(&url)
            .headers(self.build_headers(token.as_deref()))
            .send()
            .await?;

        if response.status() == 404 {
            return Err(RegistryError::NotFound(reference.full_reference()));
        }

        if !response.status().is_success() {
            return Err(RegistryError::RegistryError(format!(
                "HTTP {}: {}",
                response.status(),
                response.text().await.unwrap_or_default()
            )));
        }

        let manifest: ImageManifest = response.json().await?;
        Ok(manifest)
    }

    /// Pull an image configuration
    pub async fn get_config(
        &mut self,
        reference: &ImageReference,
        config_digest: &str,
    ) -> Result<ImageConfiguration, RegistryError> {
        let blob = self.get_blob(reference, config_digest).await?;
        let config: ImageConfiguration = serde_json::from_slice(&blob)?;
        Ok(config)
    }

    /// Pull a blob by digest
    pub async fn get_blob(
        &mut self,
        reference: &ImageReference,
        digest: &str,
    ) -> Result<Vec<u8>, RegistryError> {
        let token = self.get_token(&reference.registry, &reference.repository).await?;

        let url = format!(
            "https://{}/v2/{}/blobs/{}",
            reference.registry, reference.repository, digest
        );

        debug!("Fetching blob: {}", digest);

        let response = self
            .http_client
            .get(&url)
            .headers(self.build_headers(token.as_deref()))
            .send()
            .await?;

        if response.status() == 404 {
            return Err(RegistryError::NotFound(digest.to_string()));
        }

        if !response.status().is_success() {
            return Err(RegistryError::RegistryError(format!(
                "HTTP {}: {}",
                response.status(),
                response.text().await.unwrap_or_default()
            )));
        }

        let bytes = response.bytes().await?.to_vec();

        // Verify digest
        let computed_digest = format!("sha256:{}", hex::encode(Sha256::digest(&bytes)));
        if computed_digest != digest {
            return Err(RegistryError::DigestMismatch {
                expected: digest.to_string(),
                actual: computed_digest,
            });
        }

        Ok(bytes)
    }

    /// Download a blob to a file
    pub async fn download_blob(
        &mut self,
        reference: &ImageReference,
        digest: &str,
        path: &Path,
    ) -> Result<u64, RegistryError> {
        let token = self.get_token(&reference.registry, &reference.repository).await?;

        let url = format!(
            "https://{}/v2/{}/blobs/{}",
            reference.registry, reference.repository, digest
        );

        debug!("Downloading blob {} to {}", digest, path.display());

        let response = self
            .http_client
            .get(&url)
            .headers(self.build_headers(token.as_deref()))
            .send()
            .await?;

        if response.status() == 404 {
            return Err(RegistryError::NotFound(digest.to_string()));
        }

        if !response.status().is_success() {
            return Err(RegistryError::RegistryError(format!(
                "HTTP {}",
                response.status()
            )));
        }

        let mut file = std::fs::File::create(path)?;
        let bytes = response.bytes().await?;

        use std::io::Write;
        file.write_all(&bytes)?;

        Ok(bytes.len() as u64)
    }
}

impl Default for RegistryClient {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_image() {
        let ref1 = ImageReference::parse("alpine").unwrap();
        assert_eq!(ref1.registry, "docker.io");
        assert_eq!(ref1.repository, "library/alpine");
        assert_eq!(ref1.tag, Some("latest".to_string()));
    }

    #[test]
    fn test_parse_image_with_tag() {
        let ref1 = ImageReference::parse("alpine:3.19").unwrap();
        assert_eq!(ref1.registry, "docker.io");
        assert_eq!(ref1.repository, "library/alpine");
        assert_eq!(ref1.tag, Some("3.19".to_string()));
    }

    #[test]
    fn test_parse_image_with_org() {
        let ref1 = ImageReference::parse("nginx/nginx:latest").unwrap();
        assert_eq!(ref1.registry, "docker.io");
        assert_eq!(ref1.repository, "nginx/nginx");
        assert_eq!(ref1.tag, Some("latest".to_string()));
    }

    #[test]
    fn test_parse_image_with_registry() {
        let ref1 = ImageReference::parse("ghcr.io/owner/repo:v1.0").unwrap();
        assert_eq!(ref1.registry, "ghcr.io");
        assert_eq!(ref1.repository, "owner/repo");
        assert_eq!(ref1.tag, Some("v1.0".to_string()));
    }

    #[test]
    fn test_parse_image_with_digest() {
        let ref1 = ImageReference::parse(
            "alpine@sha256:c5b1261d6d3e43071626931fc004f70149baeba2c8ec672bd4f27761f8e1ad6b",
        )
        .unwrap();
        assert_eq!(ref1.registry, "docker.io");
        assert_eq!(ref1.repository, "library/alpine");
        assert!(ref1.digest.is_some());
    }
}
