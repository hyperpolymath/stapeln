// SPDX-License-Identifier: PMPL-1.0-or-later
// build.zig - Build script for DOM mounter FFI

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Build as a static library (for FFI linking)
    const lib = b.addStaticLibrary(.{
        .name = "dom_mounter",
        .root_source_file = b.path("src/dom_mounter.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Install the library
    b.installArtifact(lib);

    // Create test step
    const tests = b.addTest(.{
        .root_source_file = b.path("src/dom_mounter.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_tests = b.addRunArtifact(tests);

    const test_step = b.step("test", "Run FFI tests");
    test_step.dependOn(&run_tests.step);
}
