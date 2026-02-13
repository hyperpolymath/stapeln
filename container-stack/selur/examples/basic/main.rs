// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

//! Basic example of using selur Bridge for Svalinn ↔ Vörðr communication

use selur::Bridge;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("selur Basic Example: Svalinn → Vörðr communication via WASM");
    println!("============================================================\n");

    // 1. Load selur.wasm
    println!("1. Loading selur.wasm...");
    let wasm_path = "../../zig-out/bin/selur.wasm";
    let mut bridge = Bridge::new(wasm_path)?;
    println!("   ✓ Loaded WASM module\n");

    // 2. Create a sample request
    println!("2. Creating sample request...");
    let request = create_sample_request();
    println!("   Request: {} bytes", request.len());
    println!("   Command: CREATE_CONTAINER (0x01)");
    println!("   Payload: container_config.json\n");

    // 3. Send request through bridge
    println!("3. Sending request through selur bridge...");
    match bridge.send_request(&request) {
        Ok(response) => {
            println!("   ✓ Received response: {} bytes", response.len());
            println!("   Status: {:?}", parse_response(&response));
        }
        Err(e) => {
            eprintln!("   ✗ Bridge error: {}", e);
        }
    }

    println!("\n4. Example complete!");
    println!("   Zero-copy IPC demonstrated via Ephapax-linear types.");

    Ok(())
}

/// Create a sample IPC request for Vörðr
/// Format: [command: 1 byte] [payload_len: 4 bytes] [payload]
fn create_sample_request() -> Vec<u8> {
    let mut request = Vec::new();

    // Command: CREATE_CONTAINER (0x01)
    request.push(0x01);

    // Payload: container configuration (simplified JSON)
    let payload = br#"{"image":"nginx:latest","name":"web-server"}"#;
    request.extend_from_slice(&(payload.len() as u32).to_le_bytes());
    request.extend_from_slice(payload);

    request
}

/// Parse response from Vörðr
/// Format: [status: 1 byte] [payload_len: 4 bytes] [payload]
fn parse_response(response: &[u8]) -> String {
    if response.is_empty() {
        return "Empty response".to_string();
    }

    let status = response[0];
    match status {
        0 => "SUCCESS",
        1 => "INVALID_REQUEST",
        2 => "CONTAINER_NOT_FOUND",
        3 => "PERMISSION_DENIED",
        _ => "UNKNOWN_STATUS",
    }
    .to_string()
}
