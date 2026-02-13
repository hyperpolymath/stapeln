// SPDX-License-Identifier: PMPL-1.0-or-later
// Example ReScript code showing selur integration

open Selur

// Initialize the FFI library
let () = init()

// Example: Create and manage a container
let example = async () => {
  // Create bridge to WASM module
  let bridgeResult = newBridge("../../zig/zig-out/bin/selur.wasm")

  switch bridgeResult {
  | Ok(bridge) => {
      Console.log("✓ Bridge created successfully")
      Console.log(`Memory size: ${Int.toString(memorySize(bridge))} bytes`)

      // Create container
      switch createContainer(bridge, "nginx:latest") {
      | Ok(containerId) => {
          Console.log(`✓ Container created: ${containerId}`)

          // Start container
          switch startContainer(bridge, containerId) {
          | Ok() => {
              Console.log(`✓ Container started: ${containerId}`)

              // List containers
              switch listContainers(bridge) {
              | Ok(containers) => {
                  Console.log(`✓ Running containers: ${Int.toString(Array.length(containers))}`)
                  Array.forEach(containers, id => Console.log(`  - ${id}`))
                }
              | Error(e) => Console.error(`✗ List failed: ${ErrorCode.toString(e)}`)
              }

              // Stop container
              switch stopContainer(bridge, containerId) {
              | Ok() => Console.log(`✓ Container stopped: ${containerId}`)
              | Error(e) => Console.error(`✗ Stop failed: ${ErrorCode.toString(e)}`)
              }

              // Delete container
              switch deleteContainer(bridge, containerId) {
              | Ok() => Console.log(`✓ Container deleted: ${containerId}`)
              | Error(e) => Console.error(`✗ Delete failed: ${ErrorCode.toString(e)}`)
              }
            }
          | Error(e) => Console.error(`✗ Start failed: ${ErrorCode.toString(e)}`)
          }
        }
      | Error(e) => Console.error(`✗ Create failed: ${ErrorCode.toString(e)}`)
      }

      // Cleanup
      freeBridge(bridge)
      Console.log("✓ Bridge freed")
    }
  | Error(e) => Console.error(`✗ Bridge creation failed: ${ErrorCode.toString(e)}`)
  }

  cleanup()
}

// Run example
let () = {
  let _ = example()
}
