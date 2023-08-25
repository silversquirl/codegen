//! Validation of SSA integrity and types
const std = @import("std");
const ssa = @import("../ssa.zig");
const log = std.log.scoped(.ssa_validate);

pub fn validate(func: ssa.Function) !void {
    for (func.blocks.items) |blk| {
        for (blk.slice(func.insns), blk.slice(func.types), 0..) |insn, ty, insn_idx| {
            // Validate structure
            for (0..insn.arity()) |i| {
                const operand = insn.operand(i);
                if (@intFromEnum(operand) >= insn_idx) {
                    return error.ForwardReference;
                }
            }

            // Validate types
            switch (insn) {
                .param => {},
                .expect => |e| {
                    try checkType(.void, ty);
                    try checkType(.bool, func.types.get(blk.start, e.value));
                },

                .void => try checkType(.void, ty),
                .true, .false => try checkType(.bool, ty),
                .i_const => try checkTypeIsNumeric(ty),

                .add, .sub, .div, .mul => |bin| {
                    try checkTypeIsNumeric(ty);
                    try checkType(ty, func.types.get(blk.start, bin.lhs));
                    try checkType(ty, func.types.get(blk.start, bin.rhs));
                },

                .eq, .ne, .lt, .le, .gt, .ge => |bin| {
                    try checkType(.bool, ty);
                    const lhs_ty = func.types.get(blk.start, bin.lhs);
                    try checkTypeIsNumeric(lhs_ty);
                    try checkType(lhs_ty, func.types.get(blk.start, bin.rhs));
                },

                .call => |call| {
                    // TODO: type checking for function calls
                    _ = call;
                },
            }
        }

        switch (blk.term) {
            .ret => |t| try boundsCheck(blk.count, t),

            .jump => |t| _ = try validateArgs(func, blk, t.to, t.args),

            .branch => |t| {
                try boundsCheck(blk.count, t.cond);

                const i = try validateArgs(func, blk, t.true, t.args);
                _ = try validateArgs(func, blk, t.false, t.args + i + 1);
            },
        }
    }
}

fn validateArgs(
    func: ssa.Function,
    source: ssa.Block,
    target_ref: ssa.Block.Ref,
    args: [*:.invalid]ssa.Instruction.Ref,
) !usize {
    try boundsCheck(func.blocks.count(), target_ref);
    const target = func.blocks.get(target_ref);

    var i: usize = 0;
    while (args[i] != .invalid) : (i += 1) {
        try boundsCheck(source.count, args[i]);

        // Check arity
        if (i >= target.count or
            func.insns.get(target.start, @enumFromInt(i)) != .param)
        {
            return error.TooManyArguments;
        }

        // Check types
        const arg_ty = func.types.get(source.start, args[i]);
        const param_ty = func.types.get(target.start, @enumFromInt(i));
        try checkType(param_ty, arg_ty);
    }

    if (i < target.count) {
        if (func.insns.get(target.start, @enumFromInt(i)) == .param) {
            log.err("block {} expects more than {} parameters", .{ target_ref, i });
            return error.NotEnoughArguments;
        }
    }

    return i;
}

fn boundsCheck(count: u32, ref: anytype) !void {
    if (@intFromEnum(ref) >= count) {
        return error.OutOfBounds;
    }
}

fn checkType(expect: ssa.Type, actual: ssa.Type) !void {
    if (expect != actual) {
        return error.InvalidType;
    }
}
fn checkTypeKind(expect: ssa.Type.Kind, actual: ssa.Type) !void {
    if (expect != actual.kind()) {
        return error.InvalidType;
    }
}
fn checkTypeIsNumeric(ty: ssa.Type) !void {
    return checkTypeKind(.unsigned_int, ty) catch
        checkTypeKind(.signed_int, ty);
}
