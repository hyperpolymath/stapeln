// SPDX-License-Identifier: PMPL-1.0-or-later
// Vordr Zig FFI Build Configuration

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // -----------------------------------------------------------------------
    // Root module (shared between library and tests)
    // -----------------------------------------------------------------------
    const root_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });

    // -----------------------------------------------------------------------
    // Shared library for Idris2 FFI
    // -----------------------------------------------------------------------
    const lib = b.addLibrary(.{
        .name = "vordr",
        .root_module = root_module,
        .linkage = .dynamic,
    });

    // Install library
    b.installArtifact(lib);

    // -----------------------------------------------------------------------
    // Unit tests (compiled into the library source)
    // -----------------------------------------------------------------------
    const test_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });

    const main_tests = b.addTest(.{
        .root_module = test_module,
    });

    const run_main_tests = b.addRunArtifact(main_tests);
    const test_step = b.step("test", "Run library unit tests");
    test_step.dependOn(&run_main_tests.step);

    // -----------------------------------------------------------------------
    // Integration tests (link against the shared library)
    // -----------------------------------------------------------------------
    const integration_module = b.createModule(.{
        .root_source_file = b.path("test/integration_test.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });

    const integration_tests = b.addTest(.{
        .root_module = integration_module,
    });
    integration_tests.linkLibrary(lib);

    const run_integration_tests = b.addRunArtifact(integration_tests);
    const integration_step = b.step("test-integration", "Run integration tests");
    integration_step.dependOn(&run_integration_tests.step);

    // "test-all" runs both unit and integration tests
    const test_all_step = b.step("test-all", "Run all tests (unit + integration)");
    test_all_step.dependOn(&run_main_tests.step);
    test_all_step.dependOn(&run_integration_tests.step);
}
