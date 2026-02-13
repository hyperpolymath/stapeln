// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
//
// Zig build configuration for selur WASM compilation

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .freestanding,
    });

    const optimize = b.standardOptimizeOption(.{});

    // Create root module for selur
    const root_module = b.createModule(.{
        .root_source_file = b.path("runtime.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Build selur.wasm
    const wasm = b.addExecutable(.{
        .name = "selur",
        .root_module = root_module,
    });

    // WASM-specific settings
    wasm.entry = .disabled;  // No _start function
    wasm.rdynamic = true;     // Export all symbols

    b.installArtifact(wasm);

    // Add build step
    const build_step = b.step("wasm", "Build WASM module");
    build_step.dependOn(&wasm.step);
}
