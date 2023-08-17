//! High-level SSA IR
const std = @import("std");
const util = @import("util.zig");

pub const Instruction = union(enum) {
    phi: Binary,

    i_const: u64,

    i_add: Binary,
    i_sub: Binary,
    i_mul: Binary,
    i_div: Binary,

    i_eq: Binary,
    i_ne: Binary,
    i_lt: Binary,
    i_le: Binary,
    i_gt: Binary,
    i_ge: Binary,

    call: *const Call,

    pub const Binary = struct {
        lhs: Ref,
        rhs: Ref,

        pub fn format(op: Binary, _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
            try w.print("{}, {}", .{ op.lhs, op.rhs });
        }
    };

    pub const Call = struct {
        name: []const u8,
        args: []const Ref,

        pub fn format(call: Call, _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
            try w.print("{s}(", .{call.name});
            for (call.args, 0..) |arg, i| {
                if (i > 0) {
                    try w.writeAll(", ");
                }
                try w.print("{}", .{arg});
            }
            try w.writeAll(")");
        }

        fn clone(call: Call, allocator: std.mem.Allocator) !*Call {
            const name = try allocator.dupe(u8, call.name);
            errdefer allocator.free(name);

            const args = try allocator.dupe(Ref, call.args);
            errdefer allocator.free(args);

            const new = try allocator.create(Call);
            errdefer allocator.free(new);
            new.* = .{
                .name = name,
                .args = args,
            };
            return new;
        }
    };

    pub fn format(insn: Instruction, _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        switch (insn) {
            inline else => |operands, name| {
                try w.print("{s} {}", .{ @tagName(name), operands });
            },
        }
    }

    pub const Ref = enum(u32) {
        _,

        pub fn format(ref: Ref, _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
            try w.print("%{}", .{@intFromEnum(ref)});
        }
    };
};

pub const Block = struct {
    start: Instruction.Ref,
    count: u32,

    term: Terminal,

    pub const Terminal = union(enum) {
        ret: Instruction.Ref,
        jump: Block.Ref,
        branch: struct {
            cond: Instruction.Ref,
            true: Block.Ref,
            false: Block.Ref,
        },

        pub fn format(term: Terminal, _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
            try w.print("{s} ", .{@tagName(term)});
            switch (term) {
                inline .ret, .jump => |arg| try w.print("{}", .{arg}),
                .branch => |br| try w.print("{[cond]}, {[true]}, {[false]}", br),
            }
        }
    };

    pub const Ref = enum(u32) {
        _,

        pub fn format(ref: Ref, _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
            try w.print("@{}", .{@intFromEnum(ref)});
        }
    };

    pub fn body(blk: Block, insns: util.IndexedStore(Instruction, Instruction.Ref)) []const Instruction {
        return insns.items[@intFromEnum(blk.start)..][0..blk.count];
    }
};

pub const Function = struct {
    arena: std.heap.ArenaAllocator.State,
    insns: util.IndexedStore(Instruction, Instruction.Ref),
    blocks: util.IndexedStore(Block, Block.Ref),

    pub fn deinit(func: Function, allocator: std.mem.Allocator) void {
        func.insns.deinit(allocator);
        func.blocks.deinit(allocator);
        func.arena.promote(allocator).deinit();
    }

    pub fn format(func: Function, _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        for (func.blocks.items, 0..) |blk, blk_i| {
            try w.print("@{}:\n", .{blk_i});
            for (blk.body(func.insns), @intFromEnum(blk.start)..) |insn, insn_i| {
                try w.print("  %{} = {}\n", .{ insn_i, insn });
            }
            try w.print("  {}\n", .{blk.term});
        }
    }
};

pub const Builder = struct {
    arena: std.heap.ArenaAllocator,
    insns: util.IndexedStore(Instruction, Instruction.Ref).Mutable = .{},
    blocks: util.IndexedStore(Block, Block.Ref).Mutable = .{},
    current_block: ?Block.Ref = null, // Used to ensure blocks are constructed one at a time

    pub fn init(allocator: std.mem.Allocator) Builder {
        return .{ .arena = std.heap.ArenaAllocator.init(allocator) };
    }
    pub fn deinit(b: *Builder) void {
        const allocator = b.arena.child_allocator;
        b.insns.deinit(allocator);
        b.blocks.deinit(allocator);
        b.arena.deinit();
    }

    pub fn block(b: *Builder) !BlockBuilder {
        const blk = try b.blocks.appendUndefined(b.arena.child_allocator);
        return .{ .b = b, .ref = blk };
    }

    pub fn finish(b: *Builder) !Function {
        std.debug.assert(b.current_block == null); // Check for unfinished blocks
        const allocator = b.arena.child_allocator;
        const func = Function{
            .arena = b.arena.state,
            .insns = try b.insns.toConst(allocator),
            .blocks = try b.blocks.toConst(allocator),
        };
        b.arena = std.heap.ArenaAllocator.init(allocator); // Reset arena so builder can be reused
        return func;
    }

    pub const BlockBuilder = struct {
        b: *Builder,
        ref: Block.Ref,

        pub fn i(b: *BlockBuilder, insn: Instruction) !Instruction.Ref {
            b.checkOrInitCurrentBlock();

            const arena = b.b.arena.allocator();
            const copy: Instruction = switch (insn) {
                .call => |data| .{ .call = try data.clone(arena) },

                // No allocated data
                else => insn,
            };

            return b.b.insns.append(b.b.arena.child_allocator, copy);
        }

        pub fn finish(b: *BlockBuilder, terminal: Block.Terminal) !void {
            b.checkOrInitCurrentBlock();

            const blk = b.b.blocks.getPtr(b.ref);
            blk.count = b.b.insns.count() - @intFromEnum(blk.start);
            blk.term = terminal;

            b.b.current_block = null;
        }

        fn checkOrInitCurrentBlock(b: BlockBuilder) void {
            if (b.b.current_block) |blk| {
                std.debug.assert(blk == b.ref); // Ensure blocks are constructed one at a time
            } else {
                b.b.current_block = b.ref;
                b.b.blocks.getPtr(b.ref).start = @enumFromInt(b.b.insns.count());
            }
        }
    };
};

comptime {
    std.testing.refAllDecls(@This());
}

test "format" {
    try std.testing.expectFmt(
        \\i_const 7
        \\i_add %0, %1
        \\call foo()
        \\call bar(%7, %4)
    ,
        "{}\n{}\n{}\n{}",
        .{
            Instruction{ .i_const = 7 },
            Instruction{ .i_add = .{
                .lhs = @enumFromInt(0),
                .rhs = @enumFromInt(1),
            } },
            Instruction{ .call = &.{
                .name = "foo",
                .args = &.{},
            } },
            Instruction{ .call = &.{
                .name = "bar",
                .args = &[_]Instruction.Ref{
                    @enumFromInt(7),
                    @enumFromInt(4),
                },
            } },
        },
    );
}

test "builder" {
    var b = Builder.init(std.testing.allocator);
    defer b.deinit();

    var blk0 = try b.block();
    const c0 = try blk0.i(.{ .i_const = 7 });
    const c1 = try blk0.i(.{ .i_const = 13 });
    const sum = try blk0.i(.{ .i_add = .{
        .lhs = c0,
        .rhs = c1,
    } });
    const c2 = try blk0.i(.{ .i_const = 20 });
    const cond = try blk0.i(.{ .i_lt = .{
        .lhs = sum,
        .rhs = c2,
    } });

    var blk1 = try b.block();
    var blk2 = try b.block();
    try blk0.finish(.{ .branch = .{
        .cond = cond,
        .true = blk1.ref,
        .false = blk2.ref,
    } });

    const call = try blk1.i(.{ .call = &.{
        .name = "add2",
        .args = &.{sum},
    } });
    try blk1.finish(.{ .jump = blk2.ref });

    const phi = try blk2.i(.{ .phi = .{
        .lhs = sum,
        .rhs = call,
    } });
    try blk2.finish(.{ .ret = phi });

    const func = try b.finish();
    defer func.deinit(std.testing.allocator);
    try std.testing.expectFmt(
        \\@0:
        \\  %0 = i_const 7
        \\  %1 = i_const 13
        \\  %2 = i_add %0, %1
        \\  %3 = i_const 20
        \\  %4 = i_lt %2, %3
        \\  branch %4, @1, @2
        \\@1:
        \\  %5 = call add2(%2)
        \\  jump @2
        \\@2:
        \\  %6 = phi %2, %5
        \\  ret %6
        \\
    , "{}", .{func});
}
