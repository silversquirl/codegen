const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const util_mod = b.createModule(.{
        .root_source_file = .{ .path = "src/util.zig" },
    });

    const test_step = b.step("test", "Run library tests");

    const asm_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/asm.zig" },
        .target = target,
        .optimize = optimize,
    });
    test_step.dependOn(&b.addRunArtifact(asm_tests).step);
    try b.modules.put("asm", &asm_tests.root_module);

    const ssa_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/ssa.zig" },
        .target = target,
        .optimize = optimize,
    });
    ssa_tests.root_module.addImport("util", util_mod);
    test_step.dependOn(&b.addRunArtifact(ssa_tests).step);
    try b.modules.put("ssa", &ssa_tests.root_module);

    const codegen_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/codegen.zig" },
        .target = target,
        .optimize = optimize,
    });
    codegen_tests.root_module.addImport("util", util_mod);
    codegen_tests.root_module.addImport("asm", &asm_tests.root_module);
    codegen_tests.root_module.addImport("ssa", &ssa_tests.root_module);
    test_step.dependOn(&b.addRunArtifact(codegen_tests).step);
    try b.modules.put("codegen", &codegen_tests.root_module);
}
