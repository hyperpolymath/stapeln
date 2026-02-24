// SPDX-License-Identifier: PMPL-1.0-or-later
//! SQLite state management with WAL mode for concurrent access

use rusqlite::{params, Connection, OpenFlags};
use std::path::Path;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum StateError {
    #[error("Database error: {0}")]
    Database(#[from] rusqlite::Error),
    #[error("Container not found: {0}")]
    ContainerNotFound(String),
    #[error("Container already exists: {0}")]
    ContainerAlreadyExists(String),
    #[error("Image not found: {0}")]
    ImageNotFound(String),
    #[error("Image already exists: {0}")]
    ImageAlreadyExists(String),
    #[error("Network not found: {0}")]
    NetworkNotFound(String),
    #[error("Volume not found: {0}")]
    VolumeNotFound(String),
    #[allow(dead_code)]
    #[error("Lock acquisition failed: {0}")]
    LockFailed(String),
}

/// Container state as stored in database
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ContainerState {
    Created,
    Running,
    Paused,
    Stopped,
}

impl ContainerState {
    pub fn as_str(&self) -> &'static str {
        match self {
            ContainerState::Created => "created",
            ContainerState::Running => "running",
            ContainerState::Paused => "paused",
            ContainerState::Stopped => "stopped",
        }
    }

    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "created" => Some(ContainerState::Created),
            "running" => Some(ContainerState::Running),
            "paused" => Some(ContainerState::Paused),
            "stopped" => Some(ContainerState::Stopped),
            _ => None,
        }
    }
}

/// Container information
#[derive(Debug, Clone)]
pub struct ContainerInfo {
    pub id: String,
    pub name: String,
    pub image_id: String,
    pub bundle_path: String,
    pub state: ContainerState,
    pub pid: Option<i32>,
    pub exit_code: Option<i32>,
    pub created_at: String,
    pub started_at: Option<String>,
    pub finished_at: Option<String>,
    pub config: Option<String>,
}

/// Image information
#[derive(Debug, Clone)]
pub struct ImageInfo {
    pub id: String,
    pub digest: String,
    pub repository: Option<String>,
    pub tags: Vec<String>,
    pub size: i64,
    #[allow(dead_code)]
    pub path: Option<String>,
    pub created_at: String,
}

/// Network information
#[derive(Debug, Clone)]
pub struct NetworkInfo {
    pub id: String,
    pub name: String,
    pub driver: String,
    pub subnet: Option<String>,
    pub gateway: Option<String>,
    #[allow(dead_code)]
    pub options: Option<String>,
    pub created_at: String,
}

/// Volume information
#[derive(Debug, Clone)]
pub struct VolumeInfo {
    pub id: String,
    pub name: String,
    pub driver: String,
    pub mountpoint: String,
    pub options: Option<String>,
    pub labels: Option<String>,
    pub created_at: String,
}

pub struct StateManager {
    conn: Connection,
}

impl StateManager {
    /// Open or create the state database.
    /// Automatically detects filesystem type and configures journal mode.
    pub fn open(db_path: &Path) -> Result<Self, StateError> {
        let conn = Connection::open_with_flags(
            db_path,
            OpenFlags::SQLITE_OPEN_READ_WRITE
                | OpenFlags::SQLITE_OPEN_CREATE
                | OpenFlags::SQLITE_OPEN_FULL_MUTEX,
        )?;

        // Detect filesystem and configure journal mode
        let journal_mode = if Self::supports_wal(db_path) {
            "WAL"
        } else {
            eprintln!("Warning: Filesystem does not support WAL mode, using DELETE");
            "DELETE"
        };

        conn.pragma_update(None, "journal_mode", journal_mode)?;
        conn.pragma_update(None, "synchronous", "NORMAL")?;
        conn.pragma_update(None, "foreign_keys", "ON")?;

        // Initialise schema
        conn.execute_batch(include_str!("../schema.sql"))?;

        Ok(Self { conn })
    }

    /// Open an in-memory database (for testing)
    #[cfg(test)]
    pub fn open_in_memory() -> Result<Self, StateError> {
        let conn = Connection::open_in_memory()?;
        conn.pragma_update(None, "foreign_keys", "ON")?;
        conn.execute_batch(include_str!("../schema.sql"))?;
        Ok(Self { conn })
    }

    /// Check if the filesystem supports WAL mode (shared memory).
    fn supports_wal(path: &Path) -> bool {
        // WAL requires mmap support - NFS/CIFS typically don't provide this
        #[cfg(target_os = "linux")]
        {
            use std::process::Command;
            if let Some(parent) = path.parent() {
                if let Ok(output) = Command::new("stat")
                    .args(["-f", "-c", "%T"])
                    .arg(parent)
                    .output()
                {
                    let fstype = String::from_utf8_lossy(&output.stdout);
                    let fstype = fstype.trim();
                    // These filesystems don't support proper mmap locking
                    return !matches!(fstype, "nfs" | "cifs" | "smb" | "9p" | "fuse");
                }
            }
        }
        true // Assume local filesystem on other platforms
    }

    // === IMAGE OPERATIONS ===

    /// Create a new image record.
    pub fn create_image(
        &self,
        id: &str,
        digest: &str,
        repository: Option<&str>,
        tags: &[String],
        size: i64,
        image_path: Option<&str>, // Added parameter
    ) -> Result<(), StateError> {
        let tags_json = serde_json::to_string(tags).unwrap_or_else(|_| "[]".to_string());

        self.conn
            .execute(
                "INSERT INTO images (id, digest, repository, tags, size, path)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
                params![id, digest, repository, tags_json, size, image_path],
            )
            .map_err(|e| {
                if let rusqlite::Error::SqliteFailure(ref err, _) = e {
                    if err.code == rusqlite::ffi::ErrorCode::ConstraintViolation {
                        return StateError::ImageAlreadyExists(id.to_string());
                    }
                }
                StateError::Database(e)
            })?;
        Ok(())
    }

    /// Get an image by ID or digest.
    pub fn get_image(&self, id_or_digest: &str) -> Result<ImageInfo, StateError> {
        let mut stmt = self.conn.prepare(
            "SELECT id, digest, repository, tags, size, path, created_at
             FROM images WHERE id = ?1 OR digest = ?1",
        )?;

        stmt.query_row([id_or_digest], |row| {
            let tags_json: String = row.get(3)?;
            let tags: Vec<String> =
                serde_json::from_str(&tags_json).unwrap_or_default();

            Ok(ImageInfo {
                id: row.get(0)?,
                digest: row.get(1)?,
                repository: row.get(2)?,
                tags,
                size: row.get(4)?,
                path: row.get(5)?, // Retrieve new path field
                created_at: row.get(6)?,
            })
        })
        .map_err(|e| match e {
            rusqlite::Error::QueryReturnedNoRows => {
                StateError::ImageNotFound(id_or_digest.to_string())
            }
            _ => StateError::Database(e),
        })
    }

    /// List all images.
    pub fn list_images(&self) -> Result<Vec<ImageInfo>, StateError> {
        let mut stmt = self.conn.prepare(
            "SELECT id, digest, repository, tags, size, path, created_at FROM images
             ORDER BY created_at DESC",
        )?;

        let images = stmt
            .query_map([], |row| {
                let tags_json: String = row.get(3)?;
                let tags: Vec<String> =
                    serde_json::from_str(&tags_json).unwrap_or_default();

                Ok(ImageInfo {
                    id: row.get(0)?,
                    digest: row.get(1)?,
                    repository: row.get(2)?,
                    tags,
                    size: row.get(4)?,
                    path: row.get(5)?, // Retrieve new path field
                    created_at: row.get(6)?,
                })
            })?
            .collect::<Result<Vec<_>, _>>()?;

        Ok(images)
    }

    /// Delete an image.
    pub fn delete_image(&self, id: &str) -> Result<(), StateError> {
        let rows = self
            .conn
            .execute("DELETE FROM images WHERE id = ?1", params![id])?;

        if rows == 0 {
            return Err(StateError::ImageNotFound(id.to_string()));
        }
        Ok(())
    }

    /// Finds a local image by its reference (e.g., "ubuntu:latest", "alpine:3.19") or by digest.
    #[allow(dead_code)]
    pub fn find_local_image_by_ref(&self, reference: &str) -> Result<Option<ImageInfo>, StateError> {
        // Attempt to parse the reference into repository and tag
        let (repo, tag) = if reference.contains(':') && !reference.starts_with("sha256:") {
            let parts: Vec<&str> = reference.splitn(2, ':').collect();
            (Some(parts[0].to_string()), Some(parts[1].to_string()))
        } else {
            (None, None) // Not a repo:tag format, or it's a digest
        };

        let mut stmt = self.conn.prepare(
            "SELECT id, digest, repository, tags, size, path, created_at
             FROM images WHERE digest = ?1 OR (repository = ?2 AND tags LIKE ?3)"
        )?;

        // If repo and tag are available, construct a LIKE pattern for tags JSON
        let tag_like_pattern = tag.map(|t| format!("%\"{}\"%", t)).unwrap_or_default();

        let image = stmt.query_row(
            params![reference, repo, tag_like_pattern],
            |row| {
                let tags_json: String = row.get(3)?;
                let tags: Vec<String> = serde_json::from_str(&tags_json).unwrap_or_default();

                Ok(ImageInfo {
                    id: row.get(0)?,
                    digest: row.get(1)?,
                    repository: row.get(2)?,
                    tags,
                    size: row.get(4)?,
                    path: row.get(5)?,
                    created_at: row.get(6)?,
                })
            }
        );

        match image {
            Ok(info) => Ok(Some(info)),
            Err(rusqlite::Error::QueryReturnedNoRows) => Ok(None),
            Err(e) => Err(StateError::Database(e)),
        }
    }

    /// Adds a new local image record to the database.
    /// This is a convenience method that calls `create_image`.
    #[allow(dead_code)]
    pub fn add_local_image(
        &self,
        id: &str,       // Unique ID, e.g., image digest
        digest: &str,   // Image content digest
        repository: Option<&str>, // e.g., "alpine"
        tags: &[String], // e.g., ["latest", "3.19"]
        size: i64,      // Size in bytes
        path: &str,     // Local filesystem path to the OCI layout root for this image
    ) -> Result<(), StateError> {
        self.create_image(id, digest, repository, tags, size, Some(path))
    }


    /// Create a new container record.
    pub fn create_container(
        &self,
        id: &str,
        name: &str,
        image_id: &str,
        bundle_path: &str,
        config: Option<&str>,
    ) -> Result<(), StateError> {
        self.conn
            .execute(
                "INSERT INTO containers (id, name, image_id, bundle_path, state, config)
             VALUES (?1, ?2, ?3, ?4, 'created', ?5)",
                params![id, name, image_id, bundle_path, config],
            )
            .map_err(|e| {
                if let rusqlite::Error::SqliteFailure(ref err, _) = e {
                    if err.code == rusqlite::ffi::ErrorCode::ConstraintViolation {
                        return StateError::ContainerAlreadyExists(name.to_string());
                    }
                }
                StateError::Database(e)
            })?;
        Ok(())
    }

    /// Get a container by ID or name.
    pub fn get_container(&self, id_or_name: &str) -> Result<ContainerInfo, StateError> {
        let mut stmt = self.conn.prepare(
            "SELECT id, name, image_id, bundle_path, state, pid, exit_code,
                    created_at, started_at, finished_at, config
             FROM containers WHERE id = ?1 OR name = ?1",
        )?;

        stmt.query_row([id_or_name], |row| {
            let state_str: String = row.get(4)?;
            let state = ContainerState::from_str(&state_str).unwrap_or(ContainerState::Created);

            Ok(ContainerInfo {
                id: row.get(0)?,
                name: row.get(1)?,
                image_id: row.get(2)?,
                bundle_path: row.get(3)?,
                state,
                pid: row.get(5)?,
                exit_code: row.get(6)?,
                created_at: row.get(7)?,
                started_at: row.get(8)?,
                finished_at: row.get(9)?,
                config: row.get(10)?,
            })
        })
        .map_err(|e| match e {
            rusqlite::Error::QueryReturnedNoRows => {
                StateError::ContainerNotFound(id_or_name.to_string())
            }
            _ => StateError::Database(e),
        })
    }

    /// Update container state.
    pub fn set_container_state(
        &self,
        id: &str,
        state: ContainerState,
        pid: Option<i32>,
    ) -> Result<(), StateError> {
        let rows = self.conn.execute(
            "UPDATE containers SET state = ?1, pid = ?2,
             started_at = CASE WHEN ?1 = 'running' THEN CURRENT_TIMESTAMP ELSE started_at END,
             finished_at = CASE WHEN ?1 = 'stopped' THEN CURRENT_TIMESTAMP ELSE finished_at END
             WHERE id = ?3",
            params![state.as_str(), pid, id],
        )?;

        if rows == 0 {
            return Err(StateError::ContainerNotFound(id.to_string()));
        }
        Ok(())
    }

    /// Set container exit code.
    #[allow(dead_code)]
    pub fn set_container_exit_code(&self, id: &str, exit_code: i32) -> Result<(), StateError> {
        let rows = self.conn.execute(
            "UPDATE containers SET exit_code = ?1, state = 'stopped',
             finished_at = CURRENT_TIMESTAMP WHERE id = ?2",
            params![exit_code, id],
        )?;

        if rows == 0 {
            return Err(StateError::ContainerNotFound(id.to_string()));
        }
        Ok(())
    }

    /// List containers with optional state filter.
    pub fn list_containers(
        &self,
        state_filter: Option<ContainerState>,
    ) -> Result<Vec<ContainerInfo>, StateError> {
        let containers = match state_filter {
            Some(state) => {
                let mut stmt = self.conn.prepare(
                    "SELECT id, name, image_id, bundle_path, state, pid, exit_code,
                            created_at, started_at, finished_at, config
                     FROM containers WHERE state = ?1
                     ORDER BY created_at DESC",
                )?;
                let result = stmt.query_map([state.as_str()], Self::row_to_container_info)?
                    .collect::<Result<Vec<_>, _>>()?;
                result
            }
            None => {
                let mut stmt = self.conn.prepare(
                    "SELECT id, name, image_id, bundle_path, state, pid, exit_code,
                            created_at, started_at, finished_at, config
                     FROM containers ORDER BY created_at DESC",
                )?;
                let result = stmt.query_map([], Self::row_to_container_info)?
                    .collect::<Result<Vec<_>, _>>()?;
                result
            }
        };
        Ok(containers)
    }

    fn row_to_container_info(row: &rusqlite::Row) -> Result<ContainerInfo, rusqlite::Error> {
        let state_str: String = row.get(4)?;
        let state = ContainerState::from_str(&state_str).unwrap_or(ContainerState::Created);

        Ok(ContainerInfo {
            id: row.get(0)?,
            name: row.get(1)?,
            image_id: row.get(2)?,
            bundle_path: row.get(3)?,
            state,
            pid: row.get(5)?,
            exit_code: row.get(6)?,
            created_at: row.get(7)?,
            started_at: row.get(8)?,
            finished_at: row.get(9)?,
            config: row.get(10)?,
        })
    }

    /// Delete a container.
    pub fn delete_container(&self, id: &str) -> Result<(), StateError> {
        let rows = self
            .conn
            .execute("DELETE FROM containers WHERE id = ?1", params![id])?;

        if rows == 0 {
            return Err(StateError::ContainerNotFound(id.to_string()));
        }
        Ok(())
    }

    // === NETWORK OPERATIONS ===

    /// Create a new network.
    pub fn create_network(
        &self,
        id: &str,
        name: &str,
        driver: &str,
        subnet: Option<&str>,
        gateway: Option<&str>,
        options: Option<&str>,
    ) -> Result<(), StateError> {
        self.conn.execute(
            "INSERT INTO networks (id, name, driver, subnet, gateway, options)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
            params![id, name, driver, subnet, gateway, options],
        )?;
        Ok(())
    }

    /// Get a network by ID or name.
    pub fn get_network(&self, id_or_name: &str) -> Result<NetworkInfo, StateError> {
        let mut stmt = self.conn.prepare(
            "SELECT id, name, driver, subnet, gateway, options, created_at
             FROM networks WHERE id = ?1 OR name = ?1",
        )?;

        stmt.query_row([id_or_name], |row| {
            Ok(NetworkInfo {
                id: row.get(0)?,
                name: row.get(1)?,
                driver: row.get(2)?,
                subnet: row.get(3)?,
                gateway: row.get(4)?,
                options: row.get(5)?,
                created_at: row.get(6)?,
            })
        })
        .map_err(|e| match e {
            rusqlite::Error::QueryReturnedNoRows => {
                StateError::NetworkNotFound(id_or_name.to_string())
            }
            _ => StateError::Database(e),
        })
    }

    /// List all networks.
    pub fn list_networks(&self) -> Result<Vec<NetworkInfo>, StateError> {
        let mut stmt = self.conn.prepare(
            "SELECT id, name, driver, subnet, gateway, options, created_at
             FROM networks ORDER BY created_at DESC",
        )?;

        let networks = stmt
            .query_map([], |row| {
                Ok(NetworkInfo {
                    id: row.get(0)?,
                    name: row.get(1)?,
                    driver: row.get(2)?,
                    subnet: row.get(3)?,
                    gateway: row.get(4)?,
                    options: row.get(5)?,
                    created_at: row.get(6)?,
                })
            })?
            .collect::<Result<Vec<_>, _>>()?;

        Ok(networks)
    }

    /// Delete a network.
    pub fn delete_network(&self, id: &str) -> Result<(), StateError> {
        let rows = self
            .conn
            .execute("DELETE FROM networks WHERE id = ?1", params![id])?;

        if rows == 0 {
            return Err(StateError::NetworkNotFound(id.to_string()));
        }
        Ok(())
    }

    /// Connect a container to a network.
    pub fn connect_container_network(
        &self,
        container_id: &str,
        network_id: &str,
        ip_address: Option<&str>,
        mac_address: Option<&str>,
        aliases: &[String],
    ) -> Result<(), StateError> {
        let aliases_json = serde_json::to_string(aliases).unwrap_or_else(|_| "[]".to_string());

        self.conn.execute(
            "INSERT INTO container_networks (container_id, network_id, ip_address, mac_address, aliases)
             VALUES (?1, ?2, ?3, ?4, ?5)",
            params![container_id, network_id, ip_address, mac_address, aliases_json],
        )?;
        Ok(())
    }

    /// Disconnect a container from a network.
    pub fn disconnect_container_network(
        &self,
        container_id: &str,
        network_id: &str,
    ) -> Result<(), StateError> {
        self.conn.execute(
            "DELETE FROM container_networks WHERE container_id = ?1 AND network_id = ?2",
            params![container_id, network_id],
        )?;
        Ok(())
    }

    // === VOLUME OPERATIONS ===

    /// Create a new volume.
    pub fn create_volume(
        &self,
        id: &str,
        name: &str,
        driver: &str,
        mountpoint: &str,
        options: Option<&str>,
        labels: Option<&str>,
    ) -> Result<(), StateError> {
        self.conn.execute(
            "INSERT INTO volumes (id, name, driver, mountpoint, options, labels)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
            params![id, name, driver, mountpoint, options, labels],
        )?;
        Ok(())
    }

    /// Get a volume by ID or name.
    pub fn get_volume(&self, id_or_name: &str) -> Result<VolumeInfo, StateError> {
        let mut stmt = self.conn.prepare(
            "SELECT id, name, driver, mountpoint, options, labels, created_at
             FROM volumes WHERE id = ?1 OR name = ?1",
        )?;

        stmt.query_row([id_or_name], |row| {
            Ok(VolumeInfo {
                id: row.get(0)?,
                name: row.get(1)?,
                driver: row.get(2)?,
                mountpoint: row.get(3)?,
                options: row.get(4)?,
                labels: row.get(5)?,
                created_at: row.get(6)?,
            })
        })
        .map_err(|e| match e {
            rusqlite::Error::QueryReturnedNoRows => {
                StateError::VolumeNotFound(id_or_name.to_string())
            }
            _ => StateError::Database(e),
        })
    }

    /// List all volumes.
    pub fn list_volumes(&self) -> Result<Vec<VolumeInfo>, StateError> {
        let mut stmt = self.conn.prepare(
            "SELECT id, name, driver, mountpoint, options, labels, created_at
             FROM volumes ORDER BY created_at DESC",
        )?;

        let volumes = stmt
            .query_map([], |row| {
                Ok(VolumeInfo {
                    id: row.get(0)?,
                    name: row.get(1)?,
                    driver: row.get(2)?,
                    mountpoint: row.get(3)?,
                    options: row.get(4)?,
                    labels: row.get(5)?,
                    created_at: row.get(6)?,
                })
            })?
            .collect::<Result<Vec<_>, _>>()?;

        Ok(volumes)
    }

    /// Delete a volume.
    pub fn delete_volume(&self, id: &str) -> Result<(), StateError> {
        let rows = self
            .conn
            .execute("DELETE FROM volumes WHERE id = ?1", params![id])?;

        if rows == 0 {
            return Err(StateError::VolumeNotFound(id.to_string()));
        }
        Ok(())
    }

    // === LOCK OPERATIONS ===

    /// Acquire an advisory lock.
    #[allow(dead_code)]
    pub fn acquire_lock(&self, resource_type: &str, resource_id: &str) -> Result<(), StateError> {
        let pid = std::process::id() as i32;

        // First, clean up stale locks from dead processes
        self.cleanup_stale_locks()?;

        self.conn
            .execute(
                "INSERT INTO locks (resource_type, resource_id, owner_pid)
             VALUES (?1, ?2, ?3)",
                params![resource_type, resource_id, pid],
            )
            .map_err(|e| {
                if let rusqlite::Error::SqliteFailure(ref err, _) = e {
                    if err.code == rusqlite::ffi::ErrorCode::ConstraintViolation {
                        return StateError::LockFailed(format!(
                            "{}:{}",
                            resource_type, resource_id
                        ));
                    }
                }
                StateError::Database(e)
            })?;
        Ok(())
    }

    /// Release an advisory lock.
    #[allow(dead_code)]
    pub fn release_lock(&self, resource_type: &str, resource_id: &str) -> Result<(), StateError> {
        let pid = std::process::id() as i32;

        self.conn.execute(
            "DELETE FROM locks WHERE resource_type = ?1 AND resource_id = ?2 AND owner_pid = ?3",
            params![resource_type, resource_id, pid],
        )?;
        Ok(())
    }

    /// Clean up locks from dead processes.
    #[allow(dead_code)]
    fn cleanup_stale_locks(&self) -> Result<(), StateError> {
        let mut stmt = self
            .conn
            .prepare("SELECT resource_type, resource_id, owner_pid FROM locks")?;

        let locks: Vec<(String, String, i32)> = stmt
            .query_map([], |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)))?
            .filter_map(Result::ok)
            .collect();

        for (resource_type, resource_id, pid) in locks {
            // Check if process is still alive
            if !Self::process_exists(pid) {
                self.conn.execute(
                    "DELETE FROM locks WHERE resource_type = ?1 AND resource_id = ?2",
                    params![resource_type, resource_id],
                )?;
            }
        }

        Ok(())
    }

    /// Check if a process exists.
    #[allow(dead_code)]
    fn process_exists(pid: i32) -> bool {
        #[cfg(unix)]
        {
            // Sending signal 0 checks if process exists without affecting it
            unsafe { libc::kill(pid, 0) == 0 }
        }
        #[cfg(not(unix))]
        {
            // On non-Unix, assume process exists
            true
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_container_lifecycle() {
        let state = StateManager::open_in_memory().unwrap();

        // Create an image first
        state
            .create_image(
                "img-123",
                "sha256:abc123",
                Some("alpine"),
                &["latest".to_string()],
                1024,
            )
            .unwrap();

        // Create container
        state
            .create_container("ctr-456", "my-container", "img-123", "/bundles/ctr-456", None)
            .unwrap();

        // Get container
        let container = state.get_container("my-container").unwrap();
        assert_eq!(container.name, "my-container");
        assert_eq!(container.state, ContainerState::Created);
        assert!(container.pid.is_none());

        // Start container
        state
            .set_container_state("ctr-456", ContainerState::Running, Some(12345))
            .unwrap();

        let container = state.get_container("ctr-456").unwrap();
        assert_eq!(container.state, ContainerState::Running);
        assert_eq!(container.pid, Some(12345));

        // Stop container
        state.set_container_exit_code("ctr-456", 0).unwrap();

        let container = state.get_container("ctr-456").unwrap();
        assert_eq!(container.state, ContainerState::Stopped);
        assert_eq!(container.exit_code, Some(0));

        // Delete container
        state.delete_container("ctr-456").unwrap();
        assert!(state.get_container("ctr-456").is_err());
    }

    #[test]
    fn test_network_operations() {
        let state = StateManager::open_in_memory().unwrap();

        // Create network
        state
            .create_network(
                "net-123",
                "my-network",
                "bridge",
                Some("172.28.0.0/16"),
                Some("172.28.0.1"),
                None,
            )
            .unwrap();

        // Get network
        let network = state.get_network("my-network").unwrap();
        assert_eq!(network.name, "my-network");
        assert_eq!(network.driver, "bridge");

        // List networks
        let networks = state.list_networks().unwrap();
        assert_eq!(networks.len(), 1);

        // Delete network
        state.delete_network("net-123").unwrap();
        assert!(state.get_network("net-123").is_err());
    }

    #[test]
    fn test_image_operations() {
        let state = StateManager::open_in_memory().unwrap();

        // Create image
        state
            .create_image(
                "img-123",
                "sha256:abc123def456",
                Some("alpine"),
                &["latest".to_string(), "3.19".to_string()],
                5 * 1024 * 1024,
                Some("/var/lib/vordr/images/alpine:latest"), // Added path for test
            )
            .unwrap();

        // Get by ID
        let image = state.get_image("img-123").unwrap();
        assert_eq!(image.repository, Some("alpine".to_string()));
        assert_eq!(image.tags.len(), 2);

        // Get by digest
        let image = state.get_image("sha256:abc123def456").unwrap();
        assert_eq!(image.id, "img-123");

        // List images
        let images = state.list_images().unwrap();
        assert_eq!(images.len(), 1);
    }
}
