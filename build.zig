const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const util_mod = b.createModule(.{
        .source_file = .{ .path = "src/util.zig" },
    });
    const asm_mod = b.addModule("asm", .{
        .source_file = .{ .path = "src/asm.zig" },
    });
    const ssa_mod = b.addModule("ssa", .{
        .source_file = .{ .path = "src/ssa.zig" },
        .dependencies = &.{
            .{
                .name = "util",
                .module = util_mod,
            },
        },
    });
    const codegen_mod = b.addModule("codegen", .{
        .source_file = .{ .path = "src/codegen.zig" },
        .dependencies = &.{
            .{
                .name = "util",
                .module = util_mod,
            },
            .{
                .name = "asm",
                .module = asm_mod,
            },
            .{
                .name = "ssa",
                .module = ssa_mod,
            },
        },
    });
    _ = codegen_mod;

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
    ssa_tests.addModule("util", util_mod);
    test_step.dependOn(&b.addRunArtifact(ssa_tests).step);

    // const codegen_tests = b.addTest(.{
    //     .root_source_file = .{ .path = "src/codegen.zig" },
    //     .target = target,
    //     .optimize = optimize,
    // });
    // codegen_tests.addModule("util", util_mod);
    // codegen_tests.addModule("asm", asm_mod);
    // codegen_tests.addModule("ssa", ssa_mod);
    // test_step.dependOn(&b.addRunArtifact(codegen_tests).step);
}
