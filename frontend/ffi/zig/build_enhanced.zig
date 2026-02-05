// SPDX-License-Identifier: PMPL-1.0-or-later
// build_enhanced.zig - Build script for enhanced DOM mounter

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Build enhanced DOM mounter library
    const lib = b.addStaticLibrary(.{
        .name = "dom_mounter_enhanced",
        .root_source_file = b.path("src/dom_mounter_enhanced.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(lib);

    // Tests for enhanced features
    const tests = b.addTest(.{
        .root_source_file = b.path("src/dom_mounter_enhanced.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_tests = b.addRunArtifact(tests);

    const test_step = b.step("test", "Run Phase 1 enhancement tests");
    test_step.dependOn(&run_tests.step);
}
