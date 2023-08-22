//! Register allocator
const std = @import("std");
const ssa = @import("ssa");
const util = @import("util");

/// A virtual register, assuming near infinite available registers
pub const VirtualRegister = enum(u32) {
    _,

    pub fn format(reg: VirtualRegister, _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        try w.print("v${X}", .{@intFromEnum(reg)});
    }
};

pub fn virtualAlloc(
    allocator: std.mem.Allocator,
    func: ssa.Function,
) !ssa.InstructionStore(VirtualRegister) {
    var regs: ssa.InstructionStore(VirtualRegister).Mutable = .{};
    errdefer regs.deinit(allocator);
    try regs.resize(allocator, func.insns.count());

    var finished = try std.DynamicBitSet.initEmpty(allocator, func.blocks.count());
    defer finished.deinit();

    // Used to detect register conflicts
    // Only parameter or argument registers are inserted in here, as all others are always fresh
    // TODO: use liveness information to improve the accuracy of this set
    var reg_set = std.AutoHashMap(VirtualRegister, void).init(allocator);
    defer reg_set.deinit();
    try reg_set.ensureTotalCapacity(64);

    // Used to track temporaries that are arguments to finished blocks
    var finished_arg_temps = try std.DynamicBitSet.initEmpty(allocator, 64);
    defer finished_arg_temps.deinit();

    var stack = std.ArrayList(Jump).init(allocator);
    defer stack.deinit();
    try stack.append(.{
        .to = @enumFromInt(0),
        .from = .invalid,
    });

    var n_regs: u32 = 0;
    while (stack.popOrNull()) |j| {
        if (finished.isSet(@intFromEnum(j.to))) continue;

        std.debug.assert(j.from == .invalid or finished.isSet(@intFromEnum(j.from)));

        const blk = func.blocks.get(j.to);
        const from = if (j.from == .invalid) undefined else func.blocks.get(j.from);
        const args = j.args(func.blocks);

        reg_set.clearRetainingCapacity();

        // Track which temporaries are used as arguments to finished blocks called by this block
        // We can reuse their arguments' registers for these temporaries
        {
            finished_arg_temps.setRangeValue(.{ .start = 0, .end = finished_arg_temps.capacity() }, false);
            if (finished_arg_temps.capacity() < blk.count) {
                try finished_arg_temps.resize(blk.count, false);
            }
            var it = FinishedArgIterator{ .term = blk.term, .finished = finished };
            while (it.next()) |arg| {
                finished_arg_temps.set(@intFromEnum(arg.arg));
            }
        }

        // Allocate registers for all instructions
        for (blk.insns(func.insns), 0..) |insn, insn_i| {
            const insn_ref: ssa.Instruction.Ref = @enumFromInt(insn_i);
            const reg = regs.getPtr(blk.start, insn_ref);

            if (insn == .param and insn_i < args.len) {
                // Attempt to reuse argument register
                const arg_reg = regs.get(from.start, args[insn_i]);
                if (null == try reg_set.fetchPut(arg_reg, {})) {
                    // No conflict, use this register
                    reg.* = arg_reg;
                    continue;
                }
            }

            if (finished_arg_temps.isSet(insn_i)) {
                // This is an argument to a finished block, attempt to reuse that block's parameter register
                // OPTIM: this is O(n^2)
                var it = FinishedArgIterator{ .term = blk.term, .finished = finished };
                const param_reg = while (it.next()) |arg| {
                    if (arg.arg != insn_ref) continue;

                    const target = func.blocks.get(arg.target);
                    break regs.get(target.start, arg.param);
                } else unreachable; // Somehow finished_arg_temps is corrupted

                if (null == try reg_set.fetchPut(param_reg, {})) {
                    // No conflict, use this register
                    reg.* = param_reg;
                    continue;
                }
            }

            // No specialized strategies worked; allocate a new register
            reg.* = @enumFromInt(n_regs);
            n_regs += 1;
        }

        finished.set(@intFromEnum(j.to));

        // OPTIM: early pruning of finished blocks
        switch (blk.term) {
            .ret => {},
            .jump => |t| try stack.append(.{
                .to = t.to,
                .from = j.to,
            }),
            .branch => |t| {
                try stack.append(.{
                    .to = t.false,
                    .from = j.to,
                });
                try stack.append(.{
                    .to = t.true,
                    .from = j.to,
                });
            },
        }
    }

    return regs.toConst(allocator);
}

const Jump = struct {
    to: ssa.Block.Ref,
    from: ssa.Block.Ref,

    // Get the arguments passed from the jump source
    fn args(j: Jump, blocks: util.IndexedStore(ssa.Block.Ref, ssa.Block)) []const ssa.Instruction.Ref {
        if (j.from == .invalid) return &.{};
        switch (blocks.get(j.from).term) {
            .ret => return &.{},
            .jump => |t| return std.mem.span(t.args),
            .branch => |t| {
                const true_args = std.mem.span(t.args);
                if (t.true == j.to) {
                    return true_args;
                } else {
                    return std.mem.span(t.args + true_args.len + 1);
                }
            },
        }
    }
};

const FinishedArgIterator = struct {
    term: ssa.Block.Terminal,
    finished: std.DynamicBitSet,
    i: u32 = 0,
    end_of_true: ?u32 = null,

    fn next(it: *FinishedArgIterator) ?Arg {
        switch (it.term) {
            .ret => {},
            .jump => |t| if (it.finished.isSet(@intFromEnum(t.to))) {
                const ref = t.args[it.i];
                if (ref == .invalid) return null;
                it.i += 1;
                return .{ .arg = ref, .target = t.to, .param = @enumFromInt(it.i - 1) };
            },
            .branch => |t| {
                if (it.end_of_true == null) {
                    if (it.finished.isSet(@intFromEnum(t.true))) {
                        const ref = t.args[it.i];
                        it.i += 1;
                        if (ref != .invalid) {
                            return .{ .arg = ref, .target = t.true, .param = @enumFromInt(it.i - 1) };
                        }
                    }
                    if (it.finished.isSet(@intFromEnum(t.false))) {
                        while (t.args[it.i] != .invalid) : (it.i += 1) {}
                        it.i += 1;
                        it.end_of_true = it.i;
                    }
                }

                if (it.end_of_true != null and it.finished.isSet(@intFromEnum(t.false))) {
                    const ref = t.args[it.i];
                    if (ref == .invalid) return null;
                    it.i += 1;
                    return .{ .arg = ref, .target = t.true, .param = @enumFromInt(it.i - 1 - it.end_of_true.?) };
                }

                return null;
            },
        }
        return null;
    }

    const Arg = struct {
        /// Argument within the source block
        arg: ssa.Instruction.Ref,
        /// Target block
        target: ssa.Block.Ref,
        /// Parameter within the target block
        param: ssa.Instruction.Ref,
    };
};

test "allocate virtual registers" {
    var b = ssa.Builder.init(std.testing.allocator);
    defer b.deinit();

    var blk0 = try b.block(&.{ .u32, .u32 });
    const x0 = try blk0.i(.u32, .{ .add = .{
        .lhs = blk0.arg(0),
        .rhs = blk0.arg(1),
    } });
    const y0 = try blk0.i(.u32, .{ .mul = .{
        .lhs = blk0.arg(0),
        .rhs = blk0.arg(1),
    } });

    var t0 = try blk0.jump();
    defer t0.deinit();

    var blk1 = try b.block(&.{ .u32, .u32, .u32 });
    t0.to(blk1.ref);
    try t0.addArg(blk0.arg(1));
    try t0.addArg(x0);
    try t0.addArg(y0);
    try t0.finish();

    const zero = try blk1.i(.u32, .{ .i_const = 0 });
    const gt = try blk1.i(.bool, .{ .gt = .{
        .lhs = blk1.arg(2),
        .rhs = zero,
    } });

    var t1 = try blk1.branch(gt);
    defer t1.deinit();

    var blk2 = try b.block(&.{ .u32, .u32, .u32 });
    t1.true(blk2.ref);
    try t1.addArg(blk1.arg(0));
    try t1.addArg(blk1.arg(1));
    try t1.addArg(blk1.arg(2));

    const one = try blk2.i(.u32, .{ .i_const = 1 });
    const x2 = try blk2.i(.u32, .{ .sub = .{
        .lhs = blk2.arg(1),
        .rhs = one,
    } });
    const y2 = try blk2.i(.u32, .{ .mul = .{
        .lhs = blk2.arg(2),
        .rhs = blk2.arg(0),
    } });

    var t2 = try blk2.jump();
    defer t2.deinit();
    t2.to(blk1.ref);
    try t2.addArg(blk2.arg(0));
    try t2.addArg(x2);
    try t2.addArg(y2);
    try t2.finish();

    var blk3 = try b.block(&.{.u32});
    try t1.false(blk3.ref);
    try t1.addArg(blk1.arg(2));
    try t1.finish();

    try blk3.ret(blk3.arg(0));

    const func = try b.finish();
    defer func.deinit(std.testing.allocator);

    const regs = try virtualAlloc(std.testing.allocator, func);
    defer regs.deinit(std.testing.allocator);

    try std.testing.expectFmt(
        \\@0(
        \\    %0: u32    v$0
        \\    %1: u32    v$1
        \\):
        \\  %2: u32 = add %0, %1    v$2
        \\  %3: u32 = mul %0, %1    v$3
        \\  jump @1(%1, %2, %3)
        \\@1(
        \\    %0: u32    v$1
        \\    %1: u32    v$2
        \\    %2: u32    v$3
        \\):
        \\  %3: u32 = i_const 0    v$4
        \\  %4: bool = gt %2, %3    v$5
        \\  branch %4, @2(%0, %1, %2), @3(%2)
        \\@2(
        \\    %0: u32    v$1
        \\    %1: u32    v$2
        \\    %2: u32    v$3
        \\):
        \\  %3: u32 = i_const 1    v$6
        \\  %4: u32 = sub %1, %3    v$7
        \\  %5: u32 = mul %2, %0    v$8
        \\  jump @1(%0, %4, %5)
        \\@3(
        \\    %0: u32    v$3
        \\):
        \\  ret %0
        \\
    , "{}", .{func.fmtWithAnnotations(.{regs})});
}

test "allocate virtual registers 2" {
    var b = ssa.Builder.init(std.testing.allocator);
    defer b.deinit();

    var blk0 = try b.block(&.{ .u32, .u32 });
    const cond0 = try blk0.i(.bool, .{ .lt = .{
        .lhs = blk0.arg(0),
        .rhs = blk0.arg(1),
    } });
    var t0 = try blk0.branch(cond0);
    defer t0.deinit();

    var blk1 = try b.block(&.{ .u32, .u32 });
    t0.true(blk1.ref);
    try t0.addArg(blk0.arg(0));
    try t0.addArg(blk0.arg(1));

    const x1 = try blk1.i(.u32, .{ .add = .{
        .lhs = blk1.arg(1),
        .rhs = try blk1.i(.u32, .{ .i_const = 1 }),
    } });
    var t1 = try blk1.jump();
    defer t1.deinit();

    const blk2 = try b.block(&.{.u32});
    try t0.false(blk2.ref);
    try t0.addArg(blk0.arg(0));
    try t0.finish();

    const x2 = try blk2.i(.u32, .{ .add = .{
        .lhs = blk2.arg(0),
        .rhs = try blk2.i(.u32, .{ .i_const = 1 }),
    } });
    var t2 = try blk2.jump();
    defer t2.deinit();
    t2.to(blk1.ref);
    try t2.addArg(x2);
    try t2.addArg(blk2.arg(0));
    try t2.finish();

    const blk3 = try b.block(&.{.u32});
    t1.to(blk3.ref);
    try t1.addArg(blk1.arg(0));
    try t1.addArg(x1);
    try t1.finish();

    try blk3.ret(blk3.arg(0));

    const func = try b.finish();
    defer func.deinit(std.testing.allocator);

    const regs = try virtualAlloc(std.testing.allocator, func);
    defer regs.deinit(std.testing.allocator);

    try std.testing.expectFmt(
        \\@0(
        \\    %0: u32    v$0
        \\    %1: u32    v$1
        \\):
        \\  %2: bool = lt %0, %1    v$2
        \\  branch %2, @1(%0, %1), @2(%0)
        \\@1(
        \\    %0: u32    v$0
        \\    %1: u32    v$1
        \\):
        \\  %2: u32 = i_const 1    v$3
        \\  %3: u32 = add %1, %2    v$4
        \\  jump @3(%0, %3)
        \\@2(
        \\    %0: u32    v$0
        \\):
        \\  %1: u32 = i_const 1    v$5
        \\  %2: u32 = add %0, %1    v$6
        \\  jump @1(%2, %0)
        \\@3(
        \\    %0: u32    v$0
        \\):
        \\  ret %0
        \\
    , "{}", .{func.fmtWithAnnotations(.{regs})});
}
