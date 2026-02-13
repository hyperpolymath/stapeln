-- SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
-- Svalinn Project - Vordr container state database

PRAGMA journal_mode = WAL;
PRAGMA synchronous = NORMAL;
PRAGMA foreign_keys = ON;

-- Container images
CREATE TABLE IF NOT EXISTS images (
    id TEXT PRIMARY KEY,
    digest TEXT NOT NULL UNIQUE,
    repository TEXT,
    tags TEXT,  -- JSON array
    size INTEGER NOT NULL,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_images_digest ON images(digest);
CREATE INDEX IF NOT EXISTS idx_images_repository ON images(repository);

-- Container definitions
CREATE TABLE IF NOT EXISTS containers (
    id TEXT PRIMARY KEY,
    name TEXT UNIQUE NOT NULL,
    image_id TEXT NOT NULL,
    bundle_path TEXT NOT NULL,
    state TEXT CHECK(state IN ('created', 'running', 'paused', 'stopped')) NOT NULL DEFAULT 'created',
    pid INTEGER,
    exit_code INTEGER,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    started_at DATETIME,
    finished_at DATETIME,
    config TEXT,  -- JSON OCI runtime config
    FOREIGN KEY(image_id) REFERENCES images(id)
);

CREATE INDEX IF NOT EXISTS idx_containers_state ON containers(state);
CREATE INDEX IF NOT EXISTS idx_containers_image ON containers(image_id);
CREATE INDEX IF NOT EXISTS idx_containers_name ON containers(name);

-- Network configurations
CREATE TABLE IF NOT EXISTS networks (
    id TEXT PRIMARY KEY,
    name TEXT UNIQUE NOT NULL,
    driver TEXT NOT NULL DEFAULT 'bridge',
    subnet TEXT,
    gateway TEXT,
    options TEXT,  -- JSON object
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_networks_name ON networks(name);

-- Container-network associations
CREATE TABLE IF NOT EXISTS container_networks (
    container_id TEXT NOT NULL,
    network_id TEXT NOT NULL,
    ip_address TEXT,
    mac_address TEXT,
    aliases TEXT,  -- JSON array
    PRIMARY KEY (container_id, network_id),
    FOREIGN KEY(container_id) REFERENCES containers(id) ON DELETE CASCADE,
    FOREIGN KEY(network_id) REFERENCES networks(id) ON DELETE CASCADE
);

-- Volume mounts
CREATE TABLE IF NOT EXISTS volumes (
    id TEXT PRIMARY KEY,
    name TEXT UNIQUE NOT NULL,
    driver TEXT NOT NULL DEFAULT 'local',
    mountpoint TEXT NOT NULL,
    options TEXT,  -- JSON object
    labels TEXT,   -- JSON object
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_volumes_name ON volumes(name);

-- Container-volume associations
CREATE TABLE IF NOT EXISTS container_volumes (
    container_id TEXT NOT NULL,
    volume_id TEXT NOT NULL,
    destination TEXT NOT NULL,  -- Mount point inside container
    read_only INTEGER NOT NULL DEFAULT 0,
    PRIMARY KEY (container_id, volume_id, destination),
    FOREIGN KEY(container_id) REFERENCES containers(id) ON DELETE CASCADE,
    FOREIGN KEY(volume_id) REFERENCES volumes(id) ON DELETE CASCADE
);

-- Advisory locks for multi-process coordination
CREATE TABLE IF NOT EXISTS locks (
    resource_type TEXT NOT NULL,
    resource_id TEXT NOT NULL,
    owner_pid INTEGER NOT NULL,
    acquired_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (resource_type, resource_id)
);

-- Exec sessions (for vordr exec)
CREATE TABLE IF NOT EXISTS exec_sessions (
    id TEXT PRIMARY KEY,
    container_id TEXT NOT NULL,
    command TEXT NOT NULL,  -- JSON array
    pid INTEGER,
    exit_code INTEGER,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    finished_at DATETIME,
    FOREIGN KEY(container_id) REFERENCES containers(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_exec_sessions_container ON exec_sessions(container_id);
