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
