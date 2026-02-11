// SPDX-License-Identifier: AGPL-3.0-or-later
// Build script for Stapeln Zig FFI library.

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const shared_root_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const lib = b.addLibrary(.{
        .linkage = .dynamic,
        .name = "stapeln_ffi",
        .root_module = shared_root_module,
    });
    b.installArtifact(lib);

    const static_root_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const static_lib = b.addLibrary(.{
        .linkage = .static,
        .name = "stapeln_ffi",
        .root_module = static_root_module,
    });
    b.installArtifact(static_lib);

    const tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run Zig unit tests");
    test_step.dependOn(&run_tests.step);
}
