# Wiki Update: Recent Changes in Vörðr Project

This document outlines significant recent changes to the Vörðr project, focusing on improvements to the Rust build process and the removal of Ada/SPARK integration. These updates aim to streamline development, reduce build complexity, and clarify the project's technological stack.

---

## For Users

### Improved Build Reliability
The Vörðr project's Rust components now have a more stable and reliable build process. Several underlying compilation issues have been addressed, leading to smoother compilation and fewer build-related errors for users.

### Simplified Toolchain
The dependency on Ada/SPARK and its associated GNAT toolchain has been removed. This simplifies the development environment setup for Vörðr, as users no longer need to install specialized Ada compilers or verification tools for the core Rust components. The project now exclusively uses a C stub for the gatekeeper functionality, which is automatically handled by the Rust build system.

---

## For Platform Maintainers & Developers

### Rust Build Process Streamlined
All previous compilation errors in the Rust project, including unresolved imports and duplicate definitions, have been fixed. This ensures a cleaner build environment and reduces friction for contributors.

**Key fixes include:**
*   **`cli/explain.rs`**: Missing `console::style` import resolved.
*   **`ebpf/events.rs`**: Duplicate function definitions and incorrect placement of `syscall_name` function addressed.
*   **`ebpf/anomaly.rs`**: Missing definitions for `AnomalyDetector`, `AnomalyReport`, and `AnomalyLevel` added, resolving unresolved import issues.

### Removal of Ada/SPARK Integration
The build process (`build.rs`) has been refactored to completely remove any integration with Ada/SPARK and GNATprove. The project now exclusively uses a C stub implementation for the gatekeeper component, which is automatically compiled and linked by the Rust build script.

**Impact:**
*   Eliminates the need for Ada/SPARK toolchain installation.
*   Removes GNATprove warnings during the Rust build.
*   Simplifies `build.rs` logic by removing conditional compilation based on Ada/SPARK tool availability.

This change is reflected in the updated `ROADMAP.adoc` (Language Discipline) and `README.adoc` (Languages, Architecture Diagram, Components).

### eBPF Monitor Status Update
The eBPF Monitor, responsible for runtime syscall monitoring and anomaly detection, has progressed from 'Planned' to 'In Development'. Initial Rust eBPF probes and anomaly detection capabilities have been implemented and integrated into the build process.

---

## Action Items for Wiki Maintainers:

*   Integrate the information from this document into the appropriate sections of the Vörðr project wiki (e.g., "Getting Started," "Development Setup," "Technical Overview," "Release Notes").
*   Ensure that any outdated references to Ada/SPARK or GNATprove are removed from the wiki.
*   Update any setup guides or dependency lists to reflect the simplified toolchain.
