// SPDX-License-Identifier: PMPL-1.0-or-later
//! Command implementations for selur-compose

// Existing commands
pub mod up;
pub mod down;
pub mod ps;
pub mod verify;
pub mod logs;
pub mod exec;
pub mod scale;
pub mod build;
pub mod start;
pub mod stop;
pub mod restart;
pub mod run;
pub mod top;
pub mod events;
pub mod inspect;
pub mod pull;
pub mod push;
pub mod policy;

// Missing command implementations (previously fell through to "not yet implemented")
pub mod config;
pub mod sbom;
pub mod provenance;
pub mod network;
pub mod volume;

// New power commands
pub mod watch;
pub mod cp;
pub mod pause;
pub mod create;
pub mod images;
pub mod wait_cmd;
pub mod health;

// Re-export existing command entry points
pub use up::up;
pub use down::down;
pub use ps::ps;
pub use verify::verify;
pub use logs::logs;
pub use exec::exec;
pub use scale::scale;
pub use build::build;
pub use start::start;
pub use stop::stop;
pub use restart::restart;
pub use run::run;
pub use top::top;
pub use events::events;
pub use inspect::inspect;
pub use pull::pull;
pub use push::push;
pub use policy::policy;

// Re-export new command entry points
pub use config::config;
pub use sbom::sbom;
pub use provenance::provenance;
pub use network::{network_ls, network_create, network_rm};
pub use volume::{volume_ls, volume_create, volume_rm};
pub use watch::watch;
pub use cp::cp;
pub use pause::{pause, unpause};
pub use create::create;
pub use images::images;
pub use wait_cmd::wait_cmd;
pub use health::health;
