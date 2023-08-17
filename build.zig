const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const test_step = b.step("test", "Run library tests");

    const asm_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/asm.zig" },
        .target = target,
        .optimize = optimize,
    });
    test_step.dependOn(&b.addRunArtifact(asm_tests).step);

    const ssa_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/ssa.zig" },
        .target = target,
        .optimize = optimize,
    });
    test_step.dependOn(&b.addRunArtifact(ssa_tests).step);
}
