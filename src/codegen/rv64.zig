//! RV64G code generation
const std = @import("std");
const rv64 = @import("asm").rv64;
const ssa = @import("ssa");
const util = @import("util");
const regalloc = @import("regalloc.zig");

pub fn compile(
    allocator: std.mem.Allocator,
    code_allocator: std.mem.Allocator,
    func: ssa.Function,
    liveness: ssa.liveness.Info,
) ![]const u8 {
    const vregs = try regalloc.virtualAlloc(allocator, func, liveness);
    defer vregs.deinit(allocator);

    const regs = try regalloc.physicalAlloc(rv64.Register, rv64.mutable_regs, allocator, func, liveness, vregs);
    defer regs.deinit(allocator);

    var comp = try Compiler.init(allocator, func, liveness, regs);
    defer comp.deinit();

    // Count required instructions and generate label data
    for (0..func.blocks.count()) |blk_idx| {
        const blk_ref: ssa.Block.Ref = @enumFromInt(blk_idx);
        comp.labels.getPtr(blk_ref).* = comp.offset;
        try comp.block(blk_ref);
    }

    // Allocate code buffer
    comp.code = try code_allocator.alloc(u8, @as(usize, @sizeOf(u32)) * comp.offset);
    errdefer code_allocator.free(comp.code.?);

    // Generate code
    comp.offset = 0;
    for (0..func.blocks.count()) |blk_idx| {
        const blk_ref: ssa.Block.Ref = @enumFromInt(blk_idx);
        try comp.block(blk_ref);
    }

    return comp.code.?;
}

const Compiler = struct {
    allocator: std.mem.Allocator,
    func: ssa.Function,
    liveness: ssa.liveness.Info,
    regs: ssa.InstructionStore(rv64.Register),
    labels: util.IndexedStore(ssa.Block.Ref, u32).Mutable = .{},

    offset: u32 = 0,
    code: ?[]u8 = null,

    fn init(allocator: std.mem.Allocator, func: ssa.Function, liveness: ssa.liveness.Info, regs: ssa.InstructionStore(rv64.Register)) !Compiler {
        var comp: Compiler = .{
            .allocator = allocator,
            .func = func,
            .liveness = liveness,
            .regs = regs,
        };

        try comp.labels.resize(allocator, func.blocks.count());
        errdefer comp.labels.deinit(comp.allocator);

        return comp;
    }
    fn deinit(comp: *Compiler) void {
        comp.labels.deinit(comp.allocator);
    }

    fn block(comp: *Compiler, blk_ref: ssa.Block.Ref) !void {
        const blk = comp.func.blocks.getPtr(blk_ref);
        for (blk.slice(comp.func.insns), blk.slice(comp.func.types), @intFromEnum(blk.start)..) |insn, ty, insn_idx| {
            const insn_ref: ssa.Instruction.Ref = @enumFromInt(insn_idx);
            const reg = comp.regs.get(blk.start, insn_ref);

            switch (insn) {
                .param => {}, // TODO
                .expect => {},

                .void => {},

                .i_const => |value| {
                    // TODO: immediates
                    switch (ty.kind()) {
                        .unsigned_int => {
                            if (std.math.cast(u12, value)) |v12| {
                                try comp.emit(.{ .addi = .{ reg, .x0, v12 } });
                            } else if (std.math.cast(u31, value)) |v31| {
                                const top = v31 & 0x7ffff000;
                                const bottom: u12 = @truncate(v31);
                                try comp.emit(.{ .lui = .{ reg, top } });
                                try comp.emit(.{ .addi = .{ reg, reg, bottom } });
                            } else if (std.math.cast(u32, value)) |v32| {
                                const lsb = value & 1 != 0;

                                const top = v32 >> 1 & 0x7ffff000;
                                const bottom: u12 = @truncate(v32 >> 1);
                                try comp.emit(.{ .lui = .{ reg, top } });
                                try comp.emit(.{ .addi = .{ reg, reg, bottom } });
                                try comp.emit(.{ .slli = .{ reg, reg, 1 } });
                                if (lsb) {
                                    try comp.emit(.{ .addi = .{ reg, reg, 1 } });
                                }
                            } else {
                                @panic("TODO");
                            }
                        },

                        else => @panic("TODO"),
                    }
                },

                .true => try comp.emit(.{ .addi = .{ reg, .x0, 1 } }),
                .false => try comp.emit(.{ .addi = .{ reg, .x0, 0 } }),

                .add => |i| {
                    const lhs = comp.regs.get(blk.start, i.lhs);
                    const rhs = comp.regs.get(blk.start, i.rhs);
                    try comp.emit(.{ .add = .{ reg, lhs, rhs } });
                },
                .sub => |i| {
                    const lhs = comp.regs.get(blk.start, i.lhs);
                    const rhs = comp.regs.get(blk.start, i.rhs);
                    try comp.emit(.{ .sub = .{ reg, lhs, rhs } });
                },
                .mul => |i| {
                    const lhs = comp.regs.get(blk.start, i.lhs);
                    const rhs = comp.regs.get(blk.start, i.rhs);
                    try comp.emit(.{ .mul = .{ reg, lhs, rhs } });
                },
                .div => |i| {
                    const lhs = comp.regs.get(blk.start, i.lhs);
                    const rhs = comp.regs.get(blk.start, i.rhs);
                    try comp.emit(.{ .div = .{ reg, lhs, rhs } });
                },

                .eq => {},
                .ne => {},
                .lt => |i| {
                    const lhs = comp.regs.get(blk.start, i.lhs);
                    const rhs = comp.regs.get(blk.start, i.rhs);
                    try comp.emit(.{ .slt = .{ reg, lhs, rhs } });
                },
                .le => {},
                .gt => |i| {
                    const lhs = comp.regs.get(blk.start, i.lhs);
                    const rhs = comp.regs.get(blk.start, i.rhs);
                    try comp.emit(.{ .slt = .{ reg, rhs, lhs } });
                },
                .ge => {},

                .call => {},
            }
        }
    }

    fn emit(comp: *Compiler, insn: rv64.Instruction) !void {
        if (comp.code) |code| {
            const op = try rv64.assembleInstruction(insn);
            std.mem.writeInt(u32, code[@sizeOf(u32) * comp.offset ..][0..@sizeOf(u32)], op, .little);
        }
        comp.offset += 1;
    }
};

comptime {
    std.testing.refAllDeclsRecursive(@This());
}

test "basic arithmetic" {
    var b = ssa.Builder.init(std.testing.allocator);
    defer b.deinit();

    var blk = try b.block(&.{});
    const c_13 = try blk.i(.u32, .{ .i_const = 13 });
    const c_7 = try blk.i(.u32, .{ .i_const = 7 });
    const add = try blk.i(.u32, .{ .add = .{ .lhs = c_13, .rhs = c_7 } });
    const c_4 = try blk.i(.u32, .{ .i_const = 4 });
    const sub = try blk.i(.u32, .{ .sub = .{ .lhs = add, .rhs = c_4 } });
    const c_3 = try blk.i(.u32, .{ .i_const = 3 });
    const mul = try blk.i(.u32, .{ .mul = .{ .lhs = sub, .rhs = c_3 } });
    const div = try blk.i(.u32, .{ .div = .{ .lhs = mul, .rhs = c_4 } });
    try blk.ret(div);

    const func = try b.finish();
    defer func.deinit(std.testing.allocator);

    try testGen(func, &.{
        .{ .addi = .{ .x1, .x0, 13 } },
        .{ .addi = .{ .x2, .x0, 7 } },
        .{ .add = .{ .x1, .x1, .x2 } },
        .{ .addi = .{ .x2, .x0, 4 } },
        .{ .sub = .{ .x1, .x1, .x2 } },
        .{ .addi = .{ .x3, .x0, 3 } },
        .{ .mul = .{ .x1, .x1, .x3 } },
        .{ .div = .{ .x1, .x1, .x2 } },
    });
}

fn testGen(func: ssa.Function, expected: []const rv64.Instruction) !void {
    const liveness = try ssa.liveness.analyze(std.testing.allocator, func);
    defer liveness.deinit(std.testing.allocator);

    const code = try compile(std.testing.allocator, std.testing.allocator, func, liveness);
    defer std.testing.allocator.free(code);

    const exp_assembled = try std.testing.allocator.alloc(u32, expected.len);
    defer std.testing.allocator.free(exp_assembled);
    for (exp_assembled, expected) |*word, insn| {
        word.* = try rv64.assembleInstruction(insn);
        if (@import("builtin").cpu.arch.endian() != .little) {
            word.* = @byteSwap(word.*);
        }
    }

    try std.testing.expectEqualSlices(u8, std.mem.sliceAsBytes(exp_assembled), code);
}
