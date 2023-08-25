//! Dead code elimination
const std = @import("std");
const util = @import("util");
const ssa = @import("../ssa.zig");

/// Remove dead code from a function.
/// Deletes unused instructions and blocks. May reorder both instructions and blocks, but will not change behaviour.
/// TODO: eliminate unneeded parameters
pub fn apply(allocator: std.mem.Allocator, func: *ssa.Function, options: Options) !void {
    var rb = try Rebuilder.init(allocator, func);
    defer rb.deinit();

    const new_zero = try rb.mapBlock(@enumFromInt(0));
    std.debug.assert(@intFromEnum(new_zero) == 0);

    while (try rb.next()) |blk| {
        for (blk.old.slice(func.insns), 0..) |insn, insn_idx| {
            if (insn == .param or
                insn.hasSideEffect() or
                (!options.remove_metadata and insn.isMetadata()))
            {
                _ = try rb.mapInsn(@enumFromInt(insn_idx));
            }
        }

        blk.new.term = switch (blk.old.term) {
            .ret => |t| .{ .ret = try rb.mapInsn(t) },

            .jump => |t| a: {
                var i: usize = 0;
                while (t.args[i] != .invalid) : (i += 1) {
                    t.args[i] = try rb.mapInsn(t.args[i]);
                }
                break :a .{ .jump = .{
                    .to = try rb.mapBlock(t.to),
                    .args = t.args,
                } };
            },

            .branch => |t| a: {
                var i: usize = 0;
                while (t.args[i] != .invalid) : (i += 1) {
                    t.args[i] = try rb.mapInsn(t.args[i]);
                }
                i += 1;
                while (t.args[i] != .invalid) : (i += 1) {
                    t.args[i] = try rb.mapInsn(t.args[i]);
                }

                break :a .{ .branch = .{
                    .cond = try rb.mapInsn(t.cond),
                    .true = try rb.mapBlock(t.true),
                    .false = try rb.mapBlock(t.false),
                    .args = t.args,
                } };
            },
        };

        blk.new.count = rb.insns.count() - @intFromEnum(blk.new.start);
    }

    try rb.finish();
}

pub const Options = struct {
    remove_metadata: bool = true,
};

const Rebuilder = struct {
    allocator: std.mem.Allocator,
    func: *ssa.Function,

    // Blocks to be processed
    block_stack: std.ArrayListUnmanaged(ssa.Block.Ref) = .{},
    // Instructions to be processed
    insn_stack: std.ArrayListUnmanaged(ssa.Instruction.Ref) = .{},

    // Current blocks being processed
    current: Blocks = undefined,

    insns: ssa.InstructionStore(ssa.Instruction).Mutable = .{},
    types: ssa.InstructionStore(ssa.Type).Mutable = .{},
    blocks: util.IndexedStore(ssa.Block.Ref, ssa.Block).Mutable = .{},

    // Mapping from old to new blocks
    block_mapping: util.IndexedStore(ssa.Block.Ref, ssa.Block.Ref).Mutable = .{},
    // Block-local mapping from old to new instructions
    insn_mapping: util.IndexedStore(ssa.Instruction.Ref, ssa.Instruction.Ref).Mutable = .{},

    pub const Blocks = struct {
        old_ref: ssa.Block.Ref,
        old: ssa.Block,
        new_ref: ssa.Block.Ref,
        new: *ssa.Block,
    };

    pub fn init(allocator: std.mem.Allocator, func: *ssa.Function) !Rebuilder {
        var rb = Rebuilder{
            .allocator = allocator,
            .func = func,
        };

        try rb.block_mapping.resize(allocator, func.blocks.count());
        errdefer rb.block_mapping.deinit(allocator);
        @memset(rb.block_mapping.items.items, .invalid);

        return rb;
    }

    pub fn deinit(rb: *Rebuilder) void {
        rb.block_stack.deinit(rb.allocator);
        rb.insn_stack.deinit(rb.allocator);

        rb.insns.deinit(rb.allocator);
        rb.types.deinit(rb.allocator);
        rb.blocks.deinit(rb.allocator);

        rb.block_mapping.deinit(rb.allocator);
        rb.insn_mapping.deinit(rb.allocator);
    }

    pub fn finish(rb: *Rebuilder) !void {
        const insns_const = try rb.insns.toConst(rb.allocator);
        const types_const = try rb.types.toConst(rb.allocator);
        const blocks_const = try rb.blocks.toConst(rb.allocator);

        rb.func.insns.deinit(rb.allocator);
        rb.func.types.deinit(rb.allocator);
        rb.func.blocks.deinit(rb.allocator);

        rb.func.insns = insns_const;
        rb.func.types = types_const;
        rb.func.blocks = blocks_const;
    }

    pub fn next(rb: *Rebuilder) !?Blocks {
        const old_ref = rb.block_stack.popOrNull() orelse return null;
        const old = rb.func.blocks.get(old_ref);

        const new_ref = rb.block_mapping.get(old_ref);
        const new = rb.blocks.getPtr(new_ref);
        new.start = @enumFromInt(rb.insns.count());

        try rb.insn_mapping.resize(rb.allocator, old.count);
        @memset(rb.insn_mapping.items.items, .invalid);

        std.debug.assert(rb.insn_stack.items.len == 0);

        rb.current = .{
            .old_ref = old_ref,
            .old = old,
            .new_ref = new_ref,
            .new = new,
        };
        return rb.current;
    }

    pub fn mapBlock(rb: *Rebuilder, old_ref: ssa.Block.Ref) !ssa.Block.Ref {
        const new_ref = rb.block_mapping.getPtr(old_ref);
        if (new_ref.* == .invalid) {
            // Allocate a new block and queue it for processing
            new_ref.* = try rb.blocks.appendUndefined(rb.allocator);
            try rb.block_stack.append(rb.allocator, old_ref);
        }
        return new_ref.*;
    }

    pub fn mapInsn(
        rb: *Rebuilder,
        old_ref: ssa.Instruction.Ref,
    ) !ssa.Instruction.Ref {
        const new_ref = rb.insn_mapping.get(old_ref);
        if (new_ref == .invalid) {
            // Recursively copy to new function
            try rb.copyInsns(old_ref);
            return rb.insn_mapping.get(old_ref);
        } else {
            return new_ref;
        }
    }

    fn copyInsns(
        rb: *Rebuilder,
        root: ssa.Instruction.Ref,
    ) !void {
        std.debug.assert(rb.insn_stack.items.len == 0);
        try rb.insn_stack.append(rb.allocator, root);
        while (rb.insn_stack.getLastOrNull()) |old_ref| {
            const insn = rb.func.insns.get(rb.current.old.start, old_ref);

            // Map all operands
            var all_mapped = true;
            for (0..insn.arity()) |i| {
                const operand = insn.operand(i);
                if (rb.insn_mapping.get(operand) == .invalid) {
                    try rb.insn_stack.append(rb.allocator, operand);
                    all_mapped = false;
                }
            }
            if (!all_mapped) continue;

            // Pop
            std.debug.assert(old_ref == rb.insn_stack.pop());

            // Copy instruction
            const new_ref = rb.insn_mapping.getPtr(old_ref);
            const ty = rb.func.types.get(rb.current.old.start, old_ref);
            new_ref.* = try rb.copyInsn(ty, insn);
        }
    }

    pub fn copyInsn(
        rb: *Rebuilder,
        ty: ssa.Type,
        insn: ssa.Instruction,
    ) !ssa.Instruction.Ref {
        const new_insn: ssa.Instruction = switch (insn) {
            .param, .void, .i_const, .true, .false => insn,

            .expect => |e| .{ .expect = .{
                .value = rb.insn_mapping.get(e.value),
                .probability = e.probability,
            } },

            .call => |call| a: {
                // Safe to modify in-place since we know it's allocated in the function's arena
                for (@constCast(call.args)) |*arg| {
                    arg.* = rb.insn_mapping.get(arg.*);
                }
                break :a insn;
            },

            inline else => |i, name| switch (@TypeOf(i)) {
                ssa.Instruction.Binary => @unionInit(ssa.Instruction, @tagName(name), .{
                    .lhs = rb.insn_mapping.get(i.lhs),
                    .rhs = rb.insn_mapping.get(i.rhs),
                }),

                else => |t| @compileError("Unknown instruction type " ++ @typeName(t)),
            },
        };

        const new_start = rb.current.new.start;
        const new_ref = try rb.insns.append(rb.allocator, new_start, new_insn);
        std.debug.assert(new_ref == try rb.types.append(rb.allocator, new_start, ty));
        return new_ref;
    }
};

comptime {
    std.testing.refAllDecls(@This());
}

test "delete dead blocks" {
    var func = try ssa.parse(std.testing.allocator,
        \\@start(%arg: bool):
        \\  branch %arg, @used0(), @used1()
        \\@used0():
        \\  jump @used2()
        \\@used2():
        \\  %void: void = void
        \\  ret %void
        \\
        \\@used1():
        \\  jump @used3()
        \\@used3():
        \\  jump @used1()
        \\
        \\@unused0():
        \\  jump @used0()
        \\@unused1():
        \\  jump @used1()
        \\@unused2():
        \\  jump @used2()
        \\@unused3():
        \\  %1: bool = true
        \\  jump @start(%1)
        \\@unused4():
        \\  jump @unused5()
        \\@unused5():
        \\  jump @unused4()
        \\@unused6():
        \\  %void: void = void
        \\  ret %void
        \\
    );
    defer func.deinit(std.testing.allocator);
    try ssa.validate(func);

    try apply(std.testing.allocator, &func, .{});
    try ssa.validate(func);

    try std.testing.expectFmt(
        \\@0(%0: bool):
        \\  branch %0, @1(), @2()
        \\@1():
        \\  jump @4()
        \\@2():
        \\  jump @3()
        \\@3():
        \\  jump @2()
        \\@4():
        \\  %0: void = void
        \\  ret %0
        \\
    , "{}", .{func});
}

test "delete dead instructions" {
    var func = try ssa.parse(std.testing.allocator,
        \\@0(%0: u32):
        \\  %1: u32 = add %0, %0
        \\  %2: u32 = mul %1, %0
        \\  %3: bool = lt %2, %1
        \\  %4: u32 = mul %0, %0
        \\  %5: u32 = sub %2, %4
        \\  %6: u32 = add %0, %1
        \\  %7: u32 = div %4, %2
        \\  ret %4
        \\
    );
    defer func.deinit(std.testing.allocator);
    try ssa.validate(func);

    try apply(std.testing.allocator, &func, .{});
    try ssa.validate(func);

    try std.testing.expectFmt(
        \\@0(%0: u32):
        \\  %1: u32 = mul %0, %0
        \\  ret %1
        \\
    , "{}", .{func});
}

test "delete metadata" {
    var func = try ssa.parse(std.testing.allocator,
        \\@0(%0: u32):
        \\  %1: u32 = add %0, %0
        \\  %2: u32 = mul %1, %0
        \\  %3: bool = lt %2, %1
        \\  %4: u32 = mul %0, %0
        \\  %5: u32 = sub %2, %4
        \\  %6: u32 = add %0, %1
        \\  %7: u32 = div %4, %2
        \\  %8: bool = eq %6, %7
        \\  %9: void = expect %8, 0.5
        \\  ret %4
        \\
    );
    defer func.deinit(std.testing.allocator);
    try ssa.validate(func);

    try apply(std.testing.allocator, &func, .{});
    try ssa.validate(func);

    try std.testing.expectFmt(
        \\@0(%0: u32):
        \\  %1: u32 = mul %0, %0
        \\  ret %1
        \\
    , "{}", .{func});
}

test "keep metadata" {
    var func = try ssa.parse(std.testing.allocator,
        \\@0(%0: u32):
        \\  %1: u32 = add %0, %0
        \\  %2: u32 = mul %1, %0
        \\  %3: bool = lt %2, %1
        \\  %4: u32 = mul %0, %0
        \\  %5: u32 = sub %2, %4
        \\  %6: u32 = add %0, %1
        \\  %7: u32 = div %4, %2
        \\  %8: bool = eq %6, %7
        \\  %9: void = expect %8, 0.5
        \\  ret %4
        \\
    );
    defer func.deinit(std.testing.allocator);
    try ssa.validate(func);

    try apply(std.testing.allocator, &func, .{ .remove_metadata = false });

    try std.testing.expectFmt(
        \\@0(%0: u32):
        \\  %1: u32 = add %0, %0
        \\  %2: u32 = mul %1, %0
        \\  %3: u32 = mul %0, %0
        \\  %4: u32 = div %3, %2
        \\  %5: u32 = add %0, %1
        \\  %6: bool = eq %5, %4
        \\  %7: void = expect %6, 0.5
        \\  ret %3
        \\
    , "{}", .{func});
}
