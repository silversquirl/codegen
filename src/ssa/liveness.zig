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
                .i_const => {},

                .call => |call| for (call.args) |arg| {
                    ana.updateRef(arg, ref);
                },

                inline else => |value, name| switch (@TypeOf(value)) {
                    void => {},

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

    var blk0 = try b.block(&.{});
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

    var t0 = try blk0.branch(cond);
    defer t0.deinit();

    var blk1 = try b.block(&.{.u32});
    t0.true(blk1.ref);
    try t0.addArg(sum);

    const call = try blk1.i(.u32, .{ .call = &.{
        .name = "add2",
        .args = &.{blk1.arg(0)},
    } });
    var t1 = try blk1.jump();
    defer t1.deinit();

    var blk2 = try b.block(&.{.u32});

    try t0.false(blk2.ref);
    try t0.addArg(sum);
    try t0.finish();

    t1.to(blk2.ref);
    try t1.addArg(call);
    try t1.finish();

    const c_3 = try blk2.i(.u32, .{ .i_const = 3 });
    const add3 = try blk2.i(.u32, .{ .add = .{
        .lhs = blk2.arg(0),
        .rhs = c_3,
    } });
    const c_0 = try blk2.i(.u32, .{ .i_const = 0 });

    var t2 = try blk2.jump();
    defer t2.deinit();

    var blk3 = try b.block(&.{ .u32, .u32 });

    t2.to(blk3.ref);
    try t2.addArg(add3);
    try t2.addArg(c_0);
    try t2.finish();

    const c_1 = try blk3.i(.u32, .{ .i_const = 1 });
    const next_loop_value = try blk3.i(.u32, .{ .add = .{
        .lhs = blk3.arg(0),
        .rhs = c_1,
    } });

    const c_5 = try blk3.i(.u32, .{ .i_const = 5 });
    const next_mul_value = try blk3.i(.u32, .{ .mul = .{
        .lhs = blk3.arg(1),
        .rhs = c_5,
    } });

    const c_10 = try blk3.i(.u32, .{ .i_const = 10 });
    const loop_check = try blk3.i(.bool, .{ .lt = .{
        .lhs = next_loop_value,
        .rhs = c_10,
    } });

    var t3 = try blk3.branch(loop_check);
    defer t3.deinit();

    var blk4 = try b.block(&.{ .u32, .u32 });
    t3.true(blk4.ref);
    try t3.addArg(next_loop_value);
    try t3.addArg(next_mul_value);

    const c_500 = try blk4.i(.u32, .{ .i_const = 500 });
    const mul_check = try blk4.i(.bool, .{ .lt = .{
        .lhs = blk4.arg(1),
        .rhs = c_500,
    } });

    var t4 = try blk4.branch(mul_check);
    defer t4.deinit();
    t4.true(blk3.ref);
    try t4.addArg(blk4.arg(0));
    try t4.addArg(blk4.arg(1));

    var blk5 = try b.block(&.{.u32});
    try t3.false(blk5.ref);
    try t3.addArg(next_mul_value);
    try t3.finish();

    try blk5.ret(blk5.arg(0));

    var blk6 = try b.block(&.{.u32});
    try t4.false(blk6.ref);
    try t4.addArg(blk4.arg(1));
    try t4.finish();

    const c_5_2 = try blk6.i(.u32, .{ .i_const = 5 });
    const adjusted_mul_value = try blk6.i(.u32, .{ .div = .{
        .lhs = blk6.arg(0),
        .rhs = c_5_2,
    } });
    {
        var j = try blk6.jump();
        defer j.deinit();
        j.to(blk5.ref);
        try j.addArg(adjusted_mul_value);
        try j.finish();
    }

    const func = try b.finish();
    defer func.deinit(std.testing.allocator);
    try std.testing.expectFmt(
        \\@0():
        \\  %0: u32 = i_const 7
        \\  %1: u32 = i_const 13
        \\  %2: u32 = add %0, %1
        \\  %3: u32 = i_const 20
        \\  %4: bool = lt %2, %3
        \\  branch %4, @1(%2), @2(%2)
        \\@1(%0: u32):
        \\  %1: u32 = call add2(%0)
        \\  jump @2(%1)
        \\@2(%0: u32):
        \\  %1: u32 = i_const 3
        \\  %2: u32 = add %0, %1
        \\  %3: u32 = i_const 0
        \\  jump @3(%2, %3)
        \\@3(%0: u32, %1: u32):
        \\  %2: u32 = i_const 1
        \\  %3: u32 = add %0, %2
        \\  %4: u32 = i_const 5
        \\  %5: u32 = mul %1, %4
        \\  %6: u32 = i_const 10
        \\  %7: bool = lt %3, %6
        \\  branch %7, @4(%3, %5), @5(%5)
        \\@4(%0: u32, %1: u32):
        \\  %2: u32 = i_const 500
        \\  %3: bool = lt %1, %2
        \\  branch %3, @3(%0, %1), @6(%1)
        \\@5(%0: u32):
        \\  ret %0
        \\@6(%0: u32):
        \\  %1: u32 = i_const 5
        \\  %2: u32 = div %0, %1
        \\  jump @5(%2)
        \\
    , "{}", .{func});

    // TODO: check liveness data
}
