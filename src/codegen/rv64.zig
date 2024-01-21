//! RV64G code generation
// TODO: register allocation
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

    for (func.blocks.items) |blk| {
        for (blk.slice(func.insns), blk.slice(vregs), blk.slice(regs), 0..) |ins, vreg, reg, insn_i| {
            const live = liveness.single(func.insns, blk.start, @enumFromInt(insn_i));
            std.debug.print("{} {} {}\n", .{
                ins.fmtWithLiveness(live),
                vreg,
                reg,
            });
        }
    }

    var comp = try Compiler.init(allocator, func, liveness, regs);
    defer comp.deinit();

    // Count required instructions and generate label data
    var count: u32 = 0;
    for (0..func.blocks.count()) |blk_idx| {
        const blk_ref: ssa.Block.Ref = @enumFromInt(blk_idx);
        comp.labels.getPtr(blk_ref).* = count;
        count += try comp.block(false, blk_ref, count);
    }

    // Allocate code buffer
    comp.code = try code_allocator.alloc(u8, @as(usize, @sizeOf(u32)) * count);
    errdefer code_allocator.free(comp.code);

    // Generate code
    count = 0;
    for (0..func.blocks.count()) |blk_idx| {
        const blk_ref: ssa.Block.Ref = @enumFromInt(blk_idx);
        count += try comp.block(true, blk_ref, count);
    }

    return comp.code;
}

const Compiler = struct {
    allocator: std.mem.Allocator,
    func: ssa.Function,
    liveness: ssa.liveness.Info,
    regs: ssa.InstructionStore(rv64.Register),
    labels: util.IndexedStore(ssa.Block.Ref, u32).Mutable = .{},
    code: []u8 = undefined,

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

    fn block(comp: *Compiler, comptime do_emit: bool, blk_ref: ssa.Block.Ref, start_offset: usize) !u32 {
        const blk = comp.func.blocks.getPtr(blk_ref);
        var count: u32 = 0;
        for (blk.slice(comp.func.insns), blk.slice(comp.func.types), @intFromEnum(blk.start)..) |insn, ty, insn_idx| {
            const offset = start_offset + count;
            const insn_ref: ssa.Instruction.Ref = @enumFromInt(insn_idx);
            switch (insn) {
                .param => {}, // TODO
                .expect => {},

                .void => {},

                .i_const => |value| {
                    // TODO: immediates
                    const reg = comp.regs.get(blk.start, insn_ref);
                    switch (ty.kind()) {
                        .unsigned_int => {
                            if (std.math.cast(u12, value)) |v12| {
                                if (do_emit) {
                                    try comp.emit(offset, .{ .addi = .{ reg, .x0, v12 } });
                                }
                                count += 1;
                            } else if (std.math.cast(u31, value)) |v31| {
                                if (do_emit) {
                                    const top = v31 & 0x7ffff000;
                                    const bottom: u12 = @truncate(v31);
                                    try comp.emit(offset, .{ .lui = .{ reg, top } });
                                    try comp.emit(offset + 1, .{ .addi = .{ reg, reg, bottom } });
                                }
                                count += 2;
                            } else if (std.math.cast(u32, value)) |v32| {
                                const lsb = value & 1 != 0;

                                if (do_emit) {
                                    const top = v32 >> 1 & 0x7ffff000;
                                    const bottom: u12 = @truncate(v32 >> 1);
                                    try comp.emit(offset, .{ .lui = .{ reg, top } });
                                    try comp.emit(offset + 1, .{ .addi = .{ reg, reg, bottom } });
                                    try comp.emit(offset + 2, .{ .slli = .{ reg, reg, 1 } });
                                    if (lsb) {
                                        try comp.emit(offset + 3, .{ .addi = .{ reg, reg, 1 } });
                                    }
                                }

                                count += if (lsb) 4 else 3;
                            } else {
                                @panic("TODO");
                            }
                        },

                        else => @panic("TODO"),
                    }
                },

                .true => {},
                .false => {},

                .add => {},
                .sub => {},
                .mul => {},
                .div => {},

                .eq => {},
                .ne => {},
                .lt => {},
                .le => {},
                .gt => {},
                .ge => {},

                .call => {},
            }
        }
        return count;
    }

    fn emit(comp: *Compiler, offset: usize, insn: rv64.Instruction) !void {
        const op = try rv64.assembleInstruction(insn);
        std.mem.writeInt(u32, comp.code[@sizeOf(u32) * offset ..][0..@sizeOf(u32)], op, .little);
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
        // .{ .add = .{ ..., .x3, .x4 } },
        .{ .addi = .{ .x2, .x0, 4 } },
        // .{ .sub = .{ ..., ..., .x5 } },
        .{ .addi = .{ .x3, .x0, 3 } },
        // .{ .mul = .{ ..., ..., .x6 } },
        // .{ .div = .{ ..., ..., .x5 } },
    });
}

fn testGen(func: ssa.Function, expected: []const rv64.Instruction) !void {
    const liveness = try ssa.liveness.analyze(std.testing.allocator, func);
    defer liveness.deinit(std.testing.allocator);

    std.debug.print("{}\n", .{func.fmtWithAnnotations(.{liveness})});

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
