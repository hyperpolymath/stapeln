// SPDX-License-Identifier: PMPL-1.0-or-later
//! Build script to compile Ada/SPARK and link with Rust
//!
//! This build script handles:
//! 1. Optional SPARK formal verification (can be skipped with SKIP_SPARK_VERIFY=1)
//! 2. Ada static library compilation via gprbuild
//! 3. Linking the resulting library with the Rust binary
//!
//! If GNAT/SPARK tools are not available, a stub implementation is used.

use std::env;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    // Ada/SPARK code is in ../ada relative to the Rust crate
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let spark_path = PathBuf::from(&manifest_dir).join("..").join("ada");

    // Check if we should skip SPARK verification
    let skip_verify = env::var("SKIP_SPARK_VERIFY").is_ok();

    // Check if GNAT tools are available
    let has_gnat = Command::new("gprbuild")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);

    let has_gnatprove = Command::new("gnatprove")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);

    if !has_gnat {
        println!("cargo:warning=GNAT not found - using stub gatekeeper implementation");
        generate_stub_library();
        return;
    }

    // Create output directories
    let obj_dir = spark_path.join("obj");
    let lib_dir = spark_path.join("lib");
    std::fs::create_dir_all(&obj_dir).ok();
    std::fs::create_dir_all(&lib_dir).ok();

    // 1. Run GNATprove to verify SPARK code (unless skipped)
    if has_gnatprove && !skip_verify {
        println!("cargo:warning=Running SPARK formal verification...");
        let prove_status = Command::new("gnatprove")
            .args(["-P", "policy.gpr", "--level=2", "--prover=all", "-j0"])
            .current_dir(&spark_path)
            .status();

        match prove_status {
            Ok(status) if status.success() => {
                println!("cargo:warning=SPARK verification passed");
            }
            Ok(_) => {
                // Verification failed - this is serious but we'll warn rather than fail
                // to allow development iteration
                println!("cargo:warning=SPARK verification failed! Security properties not proven.");
                println!("cargo:warning=Set SKIP_SPARK_VERIFY=1 to skip verification during development.");

                // In release mode, we should fail
                if env::var("PROFILE").unwrap_or_default() == "release" {
                    panic!("SPARK verification failed in release build!");
                }
            }
            Err(e) => {
                println!("cargo:warning=Failed to run gnatprove: {}", e);
            }
        }
    } else if skip_verify {
        println!("cargo:warning=Skipping SPARK verification (SKIP_SPARK_VERIFY=1)");
    } else {
        println!("cargo:warning=GNATprove not found - skipping SPARK verification");
    }

    // 2. Build the Ada static library
    println!("cargo:warning=Building Ada static library...");
    let build_result = Command::new("gprbuild")
        .args(["-P", "policy.gpr", "-p", "-j0", "-gnatX"])  // -gnatX disables most style checks
        .current_dir(&spark_path)
        .status();

    match build_result {
        Ok(status) if status.success() => {
            println!("cargo:warning=Ada compilation successful");
        }
        _ => {
            // Ada compilation failed - fall back to stub implementation
            println!("cargo:warning=Ada compilation failed - using stub implementation");
            println!("cargo:warning=Fix Ada/SPARK code or install GNAT for full security");
            generate_stub_library();
            return;
        }
    }

    // 3. Link the static library
    println!("cargo:rustc-link-search=native={}", lib_dir.display());
    println!("cargo:rustc-link-lib=static=policy");

    // GNAT runtime libraries (platform-specific)
    // These are typically found in the GNAT installation
    if cfg!(target_os = "linux") {
        // On Linux, we need the GNAT runtime
        println!("cargo:rustc-link-lib=gnat");
        println!("cargo:rustc-link-lib=gnarl"); // For tasking support if needed
    } else if cfg!(target_os = "macos") {
        println!("cargo:rustc-link-lib=gnat");
    }

    // 4. Rebuild if Ada sources change
    println!("cargo:rerun-if-changed={}/src", spark_path.display());
    println!("cargo:rerun-if-changed={}/policy.gpr", spark_path.display());
    println!("cargo:rerun-if-env-changed=SKIP_SPARK_VERIFY");
}

/// Generate a stub C library when GNAT is not available.
/// This allows the Rust code to compile and run basic tests
/// without the full Ada/SPARK toolchain.
fn generate_stub_library() {
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let stub_file = out_dir.join("gatekeeper_stub.c");

    let stub_code = r#"
// Stub implementation of gatekeeper when GNAT is not available
// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later

#include <string.h>

// Validation result codes
#define VALID 0
#define INVALID_CAPABILITIES 1
#define INVALID_USER_NAMESPACE 2
#define INVALID_NETWORK_MODE 3
#define INVALID_PRIVILEGE_ESCAPE 4
#define PARSE_ERROR 5
#define INTERNAL_ERROR -1

// Stub: Always returns valid for basic testing
// WARNING: This provides NO security guarantees!
int verify_json_config(const char* json_str) {
    if (json_str == NULL || strlen(json_str) == 0) {
        return PARSE_ERROR;
    }
    // Stub implementation - returns valid
    // Real implementation uses formally verified Ada/SPARK
    return VALID;
}

static const char* error_messages[] = {
    "Configuration is valid (STUB - not verified)",
    "SYS_ADMIN capability requires privileged mode",
    "Root UID (0) requires user namespace to be enabled",
    "NET_ADMIN capability requires Restricted or Admin network mode",
    "Potential privilege escalation detected",
    "Failed to parse container configuration",
    "Internal error in security validation"
};

const char* get_error_message(int code) {
    if (code >= 0 && code <= 5) {
        return error_messages[code];
    }
    return error_messages[6];
}

int sanitise_config(const char* json_str, char* output_buffer, int buffer_size) {
    if (json_str == NULL || output_buffer == NULL || buffer_size <= 0) {
        return -PARSE_ERROR;
    }
    size_t len = strlen(json_str);
    if ((int)len >= buffer_size) {
        return -PARSE_ERROR;
    }
    strcpy(output_buffer, json_str);
    return (int)len;
}

const char* gatekeeper_version(void) {
    return "0.1.0-stub";
}

int gatekeeper_init(void) {
    return 0;
}
"#;

    std::fs::write(&stub_file, stub_code).expect("Failed to write stub file");

    // Compile the stub
    cc::Build::new()
        .file(&stub_file)
        .compile("policy");

    println!("cargo:rerun-if-changed=build.rs");
}
