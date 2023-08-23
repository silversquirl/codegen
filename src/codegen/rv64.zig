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
    var comp = try Compiler.init(allocator, func, liveness);
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
    labels: util.IndexedStore(ssa.Block.Ref, u32).Mutable = .{},
    // one less because x0 cannot be allocated (it is always zero)
    register_lifetimes: [nreg - 1]ssa.Instruction.Ref = .{.invalid} ** (nreg - 1),
    code: []u8 = undefined,

    const nreg = std.enums.values(rv64.Register).len;

    fn init(allocator: std.mem.Allocator, func: ssa.Function, liveness: ssa.liveness.Info) !Compiler {
        var comp: Compiler = .{
            .allocator = allocator,
            .func = func,
            .liveness = liveness,
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
        for (blk.insns(comp.func.insns), blk.types(comp.func.types), @intFromEnum(blk.start)..) |insn, ty, insn_idx| {
            const offset = start_offset + count;
            const insn_ref: ssa.Instruction.Ref = @enumFromInt(insn_idx);
            switch (insn) {
                .param => {}, // TODO

                .i_const => |value| {
                    // TODO: immediates
                    const reg = try comp.allocRegister(insn_ref);
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
        std.mem.writeIntLittle(u32, comp.code[@sizeOf(u32) * offset ..][0..@sizeOf(u32)], op);
    }

    fn allocRegister(comp: *Compiler, current_insn: ssa.Instruction.Ref) !rv64.Register {
        for (&comp.register_lifetimes, std.enums.values(rv64.Register)[1..]) |*lifetime, reg| {
            if (lifetime.* == .invalid or @intFromEnum(lifetime.*) < @intFromEnum(current_insn)) {
                // This register is unused at the current location; allocate it
                lifetime.* = comp.liveness.get(current_insn);
                return reg;
            }
        }
        return error.OutOfRegisters;
    }
};

comptime {
    std.testing.refAllDeclsRecursive(@This());
    std.testing.refAllDeclsRecursive(regalloc);
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
        .{ .addi = .{ .x3, .x0, 13 } },
        .{ .addi = .{ .x4, .x0, 7 } },
        // .{ .add = .{ ..., .x3, .x4 } },
        .{ .addi = .{ .x3, .x0, 4 } },
        // .{ .sub = .{ ..., ..., .x5 },
        .{ .addi = .{ .x4, .x0, 3 } },
        // .{ .mul = .{ ..., ..., .x6 },
        // .{ .div = .{ ..., ..., .x5 },
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
        if (@import("builtin").cpu.arch.endian() != .Little) {
            word.* = @byteSwap(word.*);
        }
    }

    try std.testing.expectEqualSlices(u8, std.mem.sliceAsBytes(exp_assembled), code);
}
