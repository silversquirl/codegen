const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const util_mod = b.createModule(.{
        .root_source_file = .{ .path = "src/util.zig" },
    });

    const asm_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/asm.zig" },
        .target = target,
        .optimize = optimize,
    });
    try b.modules.put("asm", &asm_tests.root_module);

    const ssa_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/ssa.zig" },
        .target = target,
        .optimize = optimize,
    });
    ssa_tests.root_module.addImport("util", util_mod);
    try b.modules.put("ssa", &ssa_tests.root_module);

    const codegen_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/codegen.zig" },
        .target = target,
        .optimize = optimize,
    });
    codegen_tests.root_module.addImport("util", util_mod);
    codegen_tests.root_module.addImport("asm", &asm_tests.root_module);
    codegen_tests.root_module.addImport("ssa", &ssa_tests.root_module);

    const disable_x86 = b.option(bool, "disable_xed", "Disable all x86 codegen (avoids linking Intel XED, which requires libc)") orelse false;
    if (!disable_x86) {
        const xed = b.dependency("xed", .{
            .target = target,
            .optimize = optimize,
        });
        codegen_tests.linkLibC();
        codegen_tests.linkLibrary(xed.artifact("xed"));

        codegen_tests.addCSourceFile(.{
            .file = .{ .path = "src/codegen/x86_64.c" },
            .flags = &.{ "-Wall", "-Wextra", "-Werror" },
        });
    }

    try b.modules.put("codegen", &codegen_tests.root_module);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&b.addRunArtifact(asm_tests).step);
    test_step.dependOn(&b.addRunArtifact(ssa_tests).step);
    test_step.dependOn(&b.addRunArtifact(codegen_tests).step);
}
