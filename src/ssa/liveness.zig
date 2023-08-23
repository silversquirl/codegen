//! Liveness analysis
const std = @import("std");
const util = @import("util");
const ssa = @import("../ssa.zig");

pub const Info = struct {
    small: ssa.InstructionStore(Small),
    large: LargeMap,

    pub const Small = std.bit_set.IntegerBitSet(8);
    pub const Large = std.DynamicBitSetUnmanaged;
    const LargeMap = std.ArrayHashMapUnmanaged(
        ScopedInsnRef,
        Large,
        std.array_hash_map.AutoContext(ScopedInsnRef),
        false,
    );

    const ScopedInsnRef = packed struct {
        base: ssa.Block.Start,
        insn: ssa.Instruction.Ref,
    };

    comptime {
        std.debug.assert(@sizeOf(Small) == 1);
    }

    pub fn deinit(liveness: Info, allocator: std.mem.Allocator) void {
        liveness.small.deinit(allocator);
        for (liveness.large.values()) |*l| {
            l.deinit(allocator);
        }
        var large = liveness.large;
        large.deinit(allocator);
    }

    pub fn diesImmediately(
        liveness: Info,
        base: ssa.Block.Start,
        insn_ref: ssa.Instruction.Ref,
    ) bool {
        return liveness.small.get(base, insn_ref).isSet(0);
    }

    pub fn operandDies(
        liveness: Info,
        base: ssa.Block.Start,
        insn_ref: ssa.Instruction.Ref,
        operand: usize,
    ) bool {
        const idx = operand + 1;
        if (idx < Small.bit_length) {
            return liveness.small.get(base, insn_ref).isSet(operand + 1);
        } else {
            return liveness.large.get(.{ .base = base, .insn = insn_ref }).?.isSet(operand + 1 - Small.bit_length);
        }
    }

    pub fn single(
        liveness: Info,
        insns: ssa.InstructionStore(ssa.Instruction),
        base: ssa.Block.Start,
        insn_ref: ssa.Instruction.Ref,
    ) Single {
        return .{
            .small = liveness.small.get(base, insn_ref),
            .large = if (insns.get(base, insn_ref).arity() < Small.bit_length)
                undefined
            else
                liveness.large.get(.{ .base = base, .insn = insn_ref }).?,
        };
    }

    pub fn operandDeaths(
        liveness: Info,
        insns: ssa.InstructionStore(ssa.Instruction),
        base: ssa.Block.Start,
        insn_ref: ssa.Instruction.Ref,
    ) OperandDeathIterator {
        const insn = insns.get(base, insn_ref);
        return .{
            .insn = insn,
            .small = liveness.small.get(base, insn_ref).iterator(.{}),
            .large = if (insn.arity() < Small.bit_length)
                undefined
            else
                liveness.large.get(.{ .base = base, .insn = insn_ref }).?.iterator(.{}),
        };
    }
};

// Liveness record for a single instruction
pub const Single = struct {
    small: Info.Small,
    large: Info.Large,

    pub fn diesImmediately(liveness: Single) bool {
        return liveness.small.isSet(0);
    }

    pub fn operandDies(liveness: Single, operand: usize) bool {
        const idx = operand + 1;
        if (idx < Info.Small.bit_length) {
            return liveness.small.isSet(operand + 1);
        } else {
            return liveness.large.isSet(operand + 1 - Info.Small.bit_length);
        }
    }
};

pub const OperandDeathIterator = struct {
    insn: ssa.Instruction,
    small: Info.Small.Iterator(.{}),
    large: Info.Large.Iterator(.{}), // undefined if insn.arity() < Info.Small.bit_length

    pub fn next(it: *OperandDeathIterator) ?ssa.Instruction.Ref {
        if (it.small.next()) |idx| {
            if (idx == 0) return it.next();
            return it.insn.operand(idx - 1);
        } else if (it.insn.arity() >= Info.Small.bit_length) {
            if (it.large.next()) |i| {
                const idx = i + Info.Small.bit_length;
                return it.insn.operand(idx - 1);
            }
        }
        return null;
    }
};

pub fn analyze(allocator: std.mem.Allocator, func: ssa.Function) !Info {
    var small: ssa.InstructionStore(Info.Small).Mutable = .{};
    errdefer small.deinit(allocator);
    try small.resize(allocator, func.insns.count());

    var large: Info.LargeMap = .{};
    errdefer {
        for (large.values()) |*l| {
            l.deinit(allocator);
        }
        large.deinit(allocator);
    }

    var alive = try std.DynamicBitSet.initEmpty(allocator, 256);
    defer alive.deinit();

    for (func.blocks.items) |blk| {
        alive.setRangeValue(.{ .start = 0, .end = alive.capacity() }, false);
        if (blk.count > alive.capacity()) {
            try alive.resize(alive.capacity() + alive.capacity() / 2, false);
        }

        switch (blk.term) {
            .ret => |t| alive.set(@intFromEnum(t)),

            .jump => |t| {
                var i: usize = 0;
                while (t.args[i] != .invalid) : (i += 1) {
                    alive.set(@intFromEnum(t.args[i]));
                }
            },

            .branch => |t| {
                alive.set(@intFromEnum(t.cond));
                var i: usize = 0;
                while (t.args[i] != .invalid) : (i += 1) {
                    alive.set(@intFromEnum(t.args[i]));
                }
                i += 1;
                while (t.args[i] != .invalid) : (i += 1) {
                    alive.set(@intFromEnum(t.args[i]));
                }
            },
        }

        var insn_idx: u32 = blk.count;
        while (insn_idx > 0) {
            insn_idx -= 1;
            const insn_ref: ssa.Instruction.Ref = @enumFromInt(insn_idx);
            const insn = func.insns.get(blk.start, insn_ref);

            // Slots for all operands, plus one for the result temporary
            const slot_count = insn.arity() + 1;

            // Set small death record
            {
                const s = small.getPtr(blk.start, insn_ref);
                s.* = Info.Small.initEmpty();
                if (!alive.isSet(insn_idx)) {
                    s.set(0); // Dies immediately
                }

                for (1..@min(Info.Small.bit_length, slot_count)) |idx| {
                    const operand_idx = @intFromEnum(insn.operand(idx - 1));
                    if (!alive.isSet(operand_idx)) {
                        alive.set(operand_idx);
                        s.set(idx);
                    }
                }
            }

            // If there's more operands, create a large death record as well
            if (slot_count > Info.Small.bit_length) {
                var l = try Info.Large.initEmpty(allocator, slot_count - Info.Small.bit_length);

                for (Info.Small.bit_length..slot_count) |idx| {
                    const operand_idx = @intFromEnum(insn.operand(idx - 1));
                    if (!alive.isSet(operand_idx)) {
                        alive.set(operand_idx);
                        l.set(idx);
                    }
                }

                try large.put(allocator, .{ .base = blk.start, .insn = insn_ref }, l);
            }
        }
    }

    return .{
        .small = try small.toConst(allocator),
        .large = large,
    };
}

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

    const c_1337 = try blk2.i(.u32, .{ .i_const = 1337 });
    _ = c_1337;

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

    const live = try analyze(std.testing.allocator, func);
    defer live.deinit(std.testing.allocator);

    try std.testing.expectFmt(
        \\@0():
        \\  %0: u32 = i_const 7
        \\  %1: u32 = i_const 13
        \\  %2: u32 = add %0!, %1!
        \\  %3: u32 = i_const 20
        \\  %4: bool = lt %2, %3!
        \\  branch %4, @1(%2), @2(%2)
        \\@1(
        \\    %0: u32
        \\):
        \\  %1: u32 = call add2(%0!)
        \\  jump @2(%1)
        \\@2(
        \\    %0: u32
        \\):
        \\  %1: u32 = i_const 3
        \\  %2: u32 = add %0!, %1!
        \\  %3: u32 = i_const 0
        \\  %4!: u32 = i_const 1337
        \\  jump @3(%2, %3)
        \\@3(
        \\    %0: u32
        \\    %1: u32
        \\):
        \\  %2: u32 = i_const 1
        \\  %3: u32 = add %0!, %2!
        \\  %4: u32 = i_const 5
        \\  %5: u32 = mul %1!, %4!
        \\  %6: u32 = i_const 10
        \\  %7: bool = lt %3, %6!
        \\  branch %7, @4(%3, %5), @5(%5)
        \\@4(
        \\    %0: u32
        \\    %1: u32
        \\):
        \\  %2: u32 = i_const 500
        \\  %3: bool = lt %1, %2!
        \\  branch %3, @3(%0, %1), @6(%1)
        \\@5(
        \\    %0: u32
        \\):
        \\  ret %0
        \\@6(
        \\    %0: u32
        \\):
        \\  %1: u32 = i_const 5
        \\  %2: u32 = div %0!, %1!
        \\  jump @5(%2)
        \\
    , "{}", .{func.fmtWithAnnotations(.{live})});
}
