// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

//! Example demonstrating error handling with selur Bridge

use selur::{Bridge, ErrorCode};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("selur Error Handling Example");
    println!("============================\n");

    // 1. Load selur.wasm
    println!("1. Loading selur.wasm...");
    let wasm_path = "../../zig-out/bin/selur.wasm";

    let mut bridge = match Bridge::new(wasm_path) {
        Ok(b) => {
            println!("   ✓ Successfully loaded WASM module\n");
            b
        }
        Err(e) => {
            eprintln!("   ✗ Failed to load WASM module: {}", e);
            eprintln!("\nNote: Run 'just build' from the selur root directory first");
            return Err(e.into());
        }
    };

    // 2. Check memory size
    println!("2. Checking WASM memory...");
    match bridge.memory_size() {
        Ok(size) => println!("   Memory size: {} bytes\n", size),
        Err(e) => eprintln!("   ✗ Failed to get memory size: {}\n", e),
    }

    // 3. Test with valid request
    println!("3. Sending valid request...");
    let valid_request = create_valid_request();
    match bridge.send_request(&valid_request) {
        Ok(response) => {
            println!("   ✓ Received response: {} bytes", response.len());
            if let Some(status) = response.first() {
                if let Some(error_code) = ErrorCode::from_u32(*status as u32) {
                    println!("   Status: {}", error_code);
                }
            }
        }
        Err(e) => {
            eprintln!("   ✗ Request failed: {}", e);
        }
    }

    // 4. Test with oversized request
    println!("\n4. Sending oversized request (should fail)...");
    let oversized_request = vec![0x42; 2_000_000]; // 2MB request
    match bridge.send_request(&oversized_request) {
        Ok(_) => println!("   ! Unexpected success"),
        Err(e) => println!("   ✓ Correctly rejected: {}", e),
    }

    // 5. Test with malformed request
    println!("\n5. Sending malformed request (should fail)...");
    let malformed_request = vec![0xFF]; // Invalid command
    match bridge.send_request(&malformed_request) {
        Ok(_) => println!("   ! Unexpected success"),
        Err(e) => println!("   ✓ Correctly rejected: {}", e),
    }

    println!("\nError handling demonstration complete!");

    Ok(())
}

fn create_valid_request() -> Vec<u8> {
    let mut request = Vec::new();

    // Command: CREATE_CONTAINER (0x01)
    request.push(0x01);

    // Payload length (4 bytes, little-endian)
    let payload = b"test-container";
    request.extend_from_slice(&(payload.len() as u32).to_le_bytes());

    // Payload
    request.extend_from_slice(payload);

    request
}
