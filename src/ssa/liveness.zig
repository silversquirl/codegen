//! Liveness analysis
const std = @import("std");
const util = @import("util");
const ssa = @import("../ssa.zig");

pub const LivenessInfo = util.IndexedStore(ssa.Instruction.Ref, ssa.Instruction.Ref);

pub fn analyze(allocator: std.mem.Allocator, func: ssa.Function) !LivenessInfo {
    var ana = Analyzer{
        .func = func,
    };
    errdefer ana.liveness.deinit(allocator);

    try ana.liveness.resize(allocator, func.insns.count());
    @memset(ana.liveness.items.items, .invalid);

    // TODO: properly handle loops
    // Anything that comes from outside a loop and is referenced within the loop must exist until the end of the loop, rather than simply its last reference within the loop
    for (0..func.blocks.count()) |blk_idx| {
        try ana.block(@enumFromInt(blk_idx));
    }

    return ana.liveness.toConst(allocator);
}

const Analyzer = struct {
    func: ssa.Function,
    liveness: LivenessInfo.Mutable = .{},

    fn block(ana: Analyzer, blk_ref: ssa.Block.Ref) !void {
        const blk = ana.func.blocks.getPtr(blk_ref);

        for (blk.insns(ana.func.insns), @intFromEnum(blk.start)..) |insn, insn_idx| {
            const ref: ssa.Instruction.Ref = @enumFromInt(insn_idx);
            switch (insn) {
                .phi => |phi| {
                    var i: usize = 0;
                    while (phi.values[i] != .invalid) : (i += 1) {
                        ana.updateRef(phi.values[i], ref);
                    }
                },

                .i_const => {},

                .call => |call| for (call.args) |arg| {
                    ana.updateRef(arg, ref);
                },

                inline else => |value, name| switch (@TypeOf(value)) {
                    ssa.Instruction.Ref => ana.updateRef(value, ref),
                    ssa.Instruction.Binary => {
                        ana.updateRef(value.lhs, ref);
                        ana.updateRef(value.rhs, ref);
                    },

                    else => @compileError("Cannot handle instruction " ++ @tagName(name) ++ " of type " ++ @typeName(@TypeOf(value))),
                },
            }
        }

        const end: ssa.Instruction.Ref = @enumFromInt(@intFromEnum(blk.start) + blk.count);
        switch (blk.term) {
            .ret => |value| ana.updateRef(value, end),
            .jump => {},
            .branch => |branch| ana.updateRef(branch.cond, end),
        }
    }

    fn updateRef(ana: Analyzer, value: ssa.Instruction.Ref, usage: ssa.Instruction.Ref) void {
        const ln = ana.liveness.getPtr(value);
        if (ln.* == .invalid or @intFromEnum(ln.*) < @intFromEnum(usage)) {
            ln.* = usage;
        }
    }
};

test "convoluted" {
    var b = ssa.Builder.init(std.testing.allocator);
    defer b.deinit();

    var blk0 = try b.block();
    const c_7 = try blk0.i(.u32, .{ .i_const = 7 });
    const c_13 = try blk0.i(.u32, .{ .i_const = 13 });
    const sum = try blk0.i(.u32, .{ .add = .{
        .lhs = c_7,
        .rhs = c_13,
    } });
    const c_20 = try blk0.i(.u32, .{ .i_const = 20 });
    const cond = try blk0.i(.bool, .{ .lt = .{
        .lhs = sum,
        .rhs = c_20,
    } });

    var blk1 = try b.block();
    var blk2 = try b.block();
    try blk0.finish(.{ .branch = .{
        .cond = cond,
        .true = blk1.ref,
        .false = blk2.ref,
    } });

    const call = try blk1.i(.u32, .{ .call = &.{
        .name = "add2",
        .args = &.{sum},
    } });
    try blk1.finish(.{ .jump = blk2.ref });

    var phi = try blk2.phi(.u32);
    {
        defer phi.deinit();
        try phi.add(sum);
        try phi.add(call);
        try phi.finish();
    }
    const c_3 = try blk2.i(.u32, .{ .i_const = 3 });
    const add3 = try blk2.i(.u32, .{ .add = .{
        .lhs = phi.ref,
        .rhs = c_3,
    } });
    const c_0 = try blk2.i(.u32, .{ .i_const = 0 });

    var blk3 = try b.block();
    try blk2.finish(.{ .jump = blk3.ref });

    var loop_value = try blk3.phi(.u32);
    defer loop_value.deinit();
    try loop_value.add(add3);

    var mul_value = try blk3.phi(.u32);
    defer mul_value.deinit();
    try mul_value.add(c_0);

    const c_1 = try blk3.i(.u32, .{ .i_const = 1 });
    const next_loop_value = try blk3.i(.u32, .{ .add = .{
        .lhs = loop_value.ref,
        .rhs = c_1,
    } });

    const c_5 = try blk3.i(.u32, .{ .i_const = 5 });
    const next_mul_value = try blk3.i(.u32, .{ .mul = .{
        .lhs = mul_value.ref,
        .rhs = c_5,
    } });

    const c_10 = try blk3.i(.u32, .{ .i_const = 10 });
    const loop_check = try blk3.i(.bool, .{ .lt = .{
        .lhs = next_loop_value,
        .rhs = c_10,
    } });

    var blk4 = try b.block();
    var blk5 = try b.block();
    try blk3.finish(.{ .branch = .{
        .cond = loop_check,
        .true = blk4.ref,
        .false = blk5.ref,
    } });

    const c_500 = try blk4.i(.u32, .{ .i_const = 500 });
    const mul_check = try blk4.i(.bool, .{ .lt = .{
        .lhs = next_mul_value,
        .rhs = c_500,
    } });

    try loop_value.add(try blk4.i(.u32, .{ .copy = next_loop_value }));
    try loop_value.finish();

    try mul_value.add(try blk4.i(.u32, .{ .copy = next_mul_value }));
    try mul_value.finish();

    var blk6 = try b.block();
    try blk4.finish(.{ .branch = .{
        .cond = mul_check,
        .true = blk3.ref,
        .false = blk6.ref,
    } });

    const adjusted_mul_value = try blk6.i(.u32, .{ .div = .{
        .lhs = next_mul_value,
        .rhs = c_5,
    } });
    try blk6.finish(.{ .jump = blk5.ref });

    var final_mul_value = try blk5.phi(.u32);
    defer final_mul_value.deinit();
    try final_mul_value.add(next_mul_value);
    try final_mul_value.add(adjusted_mul_value);
    try final_mul_value.finish();
    try blk5.finish(.{ .ret = final_mul_value.ref });

    const func = try b.finish();
    defer func.deinit(std.testing.allocator);
    try std.testing.expectFmt(
        \\@0:
        \\  %0: u32 = i_const 7
        \\  %1: u32 = i_const 13
        \\  %2: u32 = add %0, %1
        \\  %3: u32 = i_const 20
        \\  %4: bool = lt %2, %3
        \\  branch %4, @1, @2
        \\@1:
        \\  %5: u32 = call add2(%2)
        \\  jump @2
        \\@2:
        \\  %6: u32 = phi %2, %5
        \\  %7: u32 = i_const 3
        \\  %8: u32 = add %6, %7
        \\  %9: u32 = i_const 0
        \\  jump @3
        \\@3:
        \\  %10: u32 = phi %8, %20
        \\  %11: u32 = phi %9, %21
        \\  %12: u32 = i_const 1
        \\  %13: u32 = add %10, %12
        \\  %14: u32 = i_const 5
        \\  %15: u32 = mul %11, %14
        \\  %16: u32 = i_const 10
        \\  %17: bool = lt %13, %16
        \\  branch %17, @4, @5
        \\@4:
        \\  %18: u32 = i_const 500
        \\  %19: bool = lt %15, %18
        \\  %20: u32 = copy %13
        \\  %21: u32 = copy %15
        \\  branch %19, @3, @6
        \\@5:
        \\  %23: u32 = phi %15, %22
        \\  ret %23
        \\@6:
        \\  %22: u32 = div %15, %14
        \\  jump @5
        \\
    , "{}", .{func});

    // TODO: check liveness data
}
