//! Dead code elimination
const std = @import("std");
const util = @import("util");
const ssa = @import("../ssa.zig");

/// Remove dead code from a function.
/// Deletes unused instructions and blocks. May reorder blocks, but will not reorder instructions within them.
/// Does not modify the passed-in liveness information, which must be recomputed.
/// FIXME: this currently relies too heavily on liveness and requires multiple passes for full effectiveness
pub fn apply(allocator: std.mem.Allocator, func: *ssa.Function, live: ssa.liveness.Info) !void {
    var insns: ssa.InstructionStore(ssa.Instruction).Mutable = .{};
    errdefer insns.deinit(allocator);

    var types: ssa.InstructionStore(ssa.Type).Mutable = .{};
    errdefer types.deinit(allocator);

    var blocks: util.IndexedStore(ssa.Block.Ref, ssa.Block).Mutable = .{};
    errdefer blocks.deinit(allocator);

    // Mapping from old blocks to new blocks
    var block_mapping: util.IndexedStore(ssa.Block.Ref, ssa.Block.Ref).Mutable = .{};
    defer block_mapping.deinit(allocator);
    try block_mapping.resize(allocator, func.blocks.count());
    @memset(block_mapping.items.items, .invalid);

    // Block-local mapping from old instructions to new instructions
    var insn_mapping: util.IndexedStore(ssa.Instruction.Ref, ssa.Instruction.Ref).Mutable = .{};
    defer insn_mapping.deinit(allocator);

    var stack = std.ArrayList(ssa.Block.Ref).init(allocator);
    defer stack.deinit();
    const new_zero = try mapBlock(&stack, &blocks, block_mapping, @enumFromInt(0));
    std.debug.assert(@intFromEnum(new_zero) == 0);
    while (stack.popOrNull()) |old_blk_ref| {
        const new_blk_ref = block_mapping.get(old_blk_ref);
        const old_blk = func.blocks.get(old_blk_ref);
        const new_blk = blocks.getPtr(new_blk_ref);

        insn_mapping.items.clearRetainingCapacity();
        new_blk.start = @enumFromInt(insns.count());
        for (old_blk.insns(func.insns), old_blk.types(func.types), 0..) |insn, ty, insn_idx| {
            const old_ref: ssa.Instruction.Ref = @enumFromInt(insn_idx);
            if (live.diesImmediately(old_blk.start, old_ref)) {
                // Not used, don't include
                std.debug.assert(old_ref == try insn_mapping.append(allocator, .invalid));
            } else {
                const new_insn = switch (insn) {
                    .param, .void, .i_const => insn,

                    .call => |call| a: {
                        // Safe to modify in-place since we know it's allocated in the function's arena
                        for (@constCast(call.args)) |*arg| {
                            arg.* = insn_mapping.get(arg.*);
                        }
                        break :a insn;
                    },

                    inline else => |i, name| switch (@TypeOf(i)) {
                        ssa.Instruction.Binary => @unionInit(ssa.Instruction, @tagName(name), .{
                            .lhs = insn_mapping.get(i.lhs),
                            .rhs = insn_mapping.get(i.rhs),
                        }),

                        else => |t| @compileError("Unknown instruction type " ++ @typeName(t)),
                    },
                };

                const new_ref = try insns.append(allocator, new_blk.start, new_insn);
                std.debug.assert(new_ref == try types.append(allocator, new_blk.start, ty));
                std.debug.assert(old_ref == try insn_mapping.append(allocator, new_ref));
            }
        }
        new_blk.count = insns.count() - @intFromEnum(new_blk.start);

        new_blk.term = switch (old_blk.term) {
            .ret => |t| .{ .ret = insn_mapping.get(t) },

            .jump => |t| a: {
                var i: usize = 0;
                while (t.args[i] != .invalid) : (i += 1) {
                    t.args[i] = insn_mapping.get(t.args[i]);
                }
                break :a .{ .jump = .{
                    .to = try mapBlock(&stack, &blocks, block_mapping, t.to),
                    .args = t.args,
                } };
            },

            .branch => |t| a: {
                var i: usize = 0;
                while (t.args[i] != .invalid) : (i += 1) {
                    t.args[i] = insn_mapping.get(t.args[i]);
                }
                i += 1;
                while (t.args[i] != .invalid) : (i += 1) {
                    t.args[i] = insn_mapping.get(t.args[i]);
                }

                break :a .{ .branch = .{
                    .cond = insn_mapping.get(t.cond),
                    .true = try mapBlock(&stack, &blocks, block_mapping, t.true),
                    .false = try mapBlock(&stack, &blocks, block_mapping, t.false),
                    .args = t.args,
                } };
            },
        };
    }

    const insns_const = try insns.toConst(allocator);
    const types_const = try types.toConst(allocator);
    const blocks_const = try blocks.toConst(allocator);

    func.insns.deinit(allocator);
    func.types.deinit(allocator);
    func.blocks.deinit(allocator);

    func.insns = insns_const;
    func.types = types_const;
    func.blocks = blocks_const;
}

fn mapBlock(
    stack: *std.ArrayList(ssa.Block.Ref),
    blocks: *util.IndexedStore(ssa.Block.Ref, ssa.Block).Mutable,
    block_mapping: util.IndexedStore(ssa.Block.Ref, ssa.Block.Ref).Mutable,
    old_ref: ssa.Block.Ref,
) !ssa.Block.Ref {
    const new_ref = block_mapping.getPtr(old_ref);
    if (new_ref.* != .invalid) return new_ref.*;

    // Allocate a new block and queue it for processing
    new_ref.* = try blocks.appendUndefined(stack.allocator);
    try stack.append(old_ref);
    return new_ref.*;
}

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
        \\  jump @start()
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

    const live = try ssa.liveness.analyze(std.testing.allocator, func);
    defer live.deinit(std.testing.allocator);

    try apply(std.testing.allocator, &func, live);

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

    const live = try ssa.liveness.analyze(std.testing.allocator, func);
    defer live.deinit(std.testing.allocator);

    try apply(std.testing.allocator, &func, live);

    // FIXME: this should eliminate a lot more instructions
    try std.testing.expectFmt(
        \\@0(%0: u32):
        \\  %1: u32 = add %0, %0
        \\  %2: u32 = mul %1, %0
        \\  %3: u32 = mul %0, %0
        \\  ret %3
        \\
    , "{}", .{func});
}
