//! RV64G code generation
// TODO: register allocation
const std = @import("std");
const rv64 = @import("asm").rv64;
const ssa = @import("ssa");
const util = @import("util");

pub fn compile(
    allocator: std.mem.Allocator,
    code_allocator: std.mem.Allocator,
    func: ssa.Function,
    liveness: ssa.liveness.LivenessInfo,
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
    liveness: ssa.liveness.LivenessInfo,
    labels: util.IndexedStore(u32, ssa.Block.Ref).Mutable = .{},
    // one less because x0 cannot be allocated (it is always zero)
    register_lifetimes: [nreg - 1]ssa.Instruction.Ref = .{.invalid} ** (nreg - 1),
    code: []u8 = undefined,

    const nreg = std.enums.values(rv64.Register).len;

    fn init(allocator: std.mem.Allocator, func: ssa.Function, liveness: ssa.liveness.LivenessInfo) !Compiler {
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
                    switch (ty) {
                        .u8 => {
                            if (do_emit) {
                                const v8: u8 = @intCast(value);
                                try comp.emit(offset, .{ .addi = .{ reg, .x0, v8 } });
                            }
                            count += 1;
                        },
                        .u16 => {
                            if (do_emit) {
                                const v16: u16 = @intCast(value);
                                const v12: u12 = @truncate(v16);
                                try comp.emit(offset, .{ .lui = .{ reg, v16 & 0xf000 } });
                                try comp.emit(offset + 1, .{ .addi = .{ reg, reg, v12 } });
                            }
                            count += 2;
                        },
                        .u32 => {
                            const lsb = value & 1 != 0;
                            const msb = value & (1 << 31) != 0;

                            if (do_emit) {
                                const v32: u32 = @intCast(value);
                                if (msb) {
                                    const top = v32 >> 1 & 0x7ffff000;
                                    const bottom: u12 = @truncate(v32 >> 1);
                                    try comp.emit(offset, .{ .lui = .{ reg, top } });
                                    try comp.emit(offset + 1, .{ .addi = .{ reg, reg, bottom } });
                                    try comp.emit(offset + 2, .{ .slli = .{ reg, reg, 1 } });
                                    if (lsb) {
                                        try comp.emit(offset + 3, .{ .addi = .{ reg, reg, 1 } });
                                    }
                                } else {
                                    const top = v32 & 0x7ffff000;
                                    const bottom: u12 = @truncate(v32);
                                    try comp.emit(offset, .{ .lui = .{ reg, top } });
                                    try comp.emit(offset + 1, .{ .addi = .{ reg, reg, bottom } });
                                }
                            }

                            count += if (msb and lsb)
                                4
                            else if (msb)
                                3
                            else
                                2;
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
}

test "basic arithmetic" {
    var b = ssa.Builder.init(std.testing.allocator);
    defer b.deinit();

    var blk = try b.block(&.{});
    const c0 = try blk.i(.u32, .{ .i_const = 13 });
    const c1 = try blk.i(.u32, .{ .i_const = 7 });
    const add = try blk.i(.u32, .{ .add = .{ .lhs = c0, .rhs = c1 } });
    const c2 = try blk.i(.u32, .{ .i_const = 4 });
    const sub = try blk.i(.u32, .{ .sub = .{ .lhs = add, .rhs = c2 } });
    try blk.ret(sub);

    const func = try b.finish();
    defer func.deinit(std.testing.allocator);

    const liveness = try ssa.liveness.analyze(std.testing.allocator, func);
    defer liveness.deinit(std.testing.allocator);

    const code = try compile(std.testing.allocator, std.testing.allocator, func, liveness);
    defer std.testing.allocator.free(code);

    // TODO: check generated code
    std.debug.print("{}\n", .{std.fmt.fmtSliceHexLower(code)});
}
