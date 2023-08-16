const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const asm_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/asm.zig" },
        .target = target,
        .optimize = optimize,
    });
    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&b.addRunArtifact(asm_tests).step);
}
