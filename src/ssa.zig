//! High-level SSA IR
const std = @import("std");
const util = @import("util");
pub const liveness = @import("ssa/liveness.zig");

pub const Instruction = union(enum) {
    phi: Phi,
    copy: Ref,

    i_const: u64,

    add: Binary,
    sub: Binary,
    mul: Binary,
    div: Binary,

    eq: Binary,
    ne: Binary,
    lt: Binary,
    le: Binary,
    gt: Binary,
    ge: Binary,

    call: *const Call,

    pub const Phi = struct {
        values: [*:.invalid]const Ref,

        pub fn format(phi: Phi, _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
            var i: usize = 0;
            while (phi.values[i] != .invalid) : (i += 1) {
                if (i > 0) {
                    try w.writeAll(", ");
                }
                try w.print("{}", .{phi.values[i]});
            }
        }

        fn clone(phi: Phi, allocator: std.mem.Allocator) !Phi {
            const old_values = std.mem.span(phi.values);
            const values = try allocator.allocSentinel(Ref, old_values.len, .invalid);
            @memcpy(values, old_values);
            return .{ .values = values };
        }
    };

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
        invalid = std.math.maxInt(u32),
        _,

        pub fn format(ref: Ref, _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
            if (ref == .invalid) {
                try w.writeAll("%invalid");
            } else {
                try w.print("%{}", .{@intFromEnum(ref)});
            }
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
        invalid = std.math.maxInt(u32),
        _,

        pub fn format(ref: Ref, _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
            if (ref == .invalid) {
                try w.writeAll("@invalid");
            } else {
                try w.print("@{}", .{@intFromEnum(ref)});
            }
        }
    };

    pub fn insns(blk: Block, i: util.IndexedStore(Instruction, Instruction.Ref)) []const Instruction {
        return i.items[@intFromEnum(blk.start)..][0..blk.count];
    }
    pub fn types(blk: Block, t: util.IndexedStore(Type, Instruction.Ref)) []const Type {
        return t.items[@intFromEnum(blk.start)..][0..blk.count];
    }
};

pub const Function = struct {
    arena: std.heap.ArenaAllocator.State,
    insns: util.IndexedStore(Instruction, Instruction.Ref),
    types: util.IndexedStore(Type, Instruction.Ref),
    blocks: util.IndexedStore(Block, Block.Ref),

    pub fn deinit(func: Function, allocator: std.mem.Allocator) void {
        func.insns.deinit(allocator);
        func.types.deinit(allocator);
        func.blocks.deinit(allocator);
        func.arena.promote(allocator).deinit();
    }

    pub fn format(func: Function, _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        for (func.blocks.items, 0..) |blk, blk_i| {
            try w.print("@{}:\n", .{blk_i});
            for (blk.insns(func.insns), blk.types(func.types), @intFromEnum(blk.start)..) |insn, ty, insn_i| {
                try w.print("  %{}: {} = {}\n", .{ insn_i, ty, insn });
            }
            try w.print("  {}\n", .{blk.term});
        }
    }
};

pub const Type = enum {
    // Unsigned integers
    u8,
    u16,
    u32,
    u64,

    // Signed integers
    s8,
    s16,
    s32,
    s64,

    bool,

    pub fn format(ty: Type, _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        try w.writeAll(@tagName(ty));
    }
};

pub const Builder = struct {
    arena: std.heap.ArenaAllocator,
    insns: util.IndexedStore(Instruction, Instruction.Ref).Mutable = .{},
    types: util.IndexedStore(Type, Instruction.Ref).Mutable = .{},
    blocks: util.IndexedStore(Block, Block.Ref).Mutable = .{},
    current_block: Block.Ref = .invalid, // Used to ensure blocks are constructed one at a time
    unfinished_phis: usize = 0,

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
        std.debug.assert(b.current_block == .invalid); // Check for unfinished blocks
        std.debug.assert(b.unfinished_phis == 0); // Check for unfinished phis
        const allocator = b.arena.child_allocator;
        const func = Function{
            .arena = b.arena.state,
            .insns = try b.insns.toConst(allocator),
            .types = try b.types.toConst(allocator),
            .blocks = try b.blocks.toConst(allocator),
        };
        b.arena = std.heap.ArenaAllocator.init(allocator); // Reset arena so builder can be reused
        return func;
    }

    pub const BlockBuilder = struct {
        b: *Builder,
        ref: Block.Ref,

        pub fn i(b: *BlockBuilder, ty: Type, insn: Instruction) !Instruction.Ref {
            b.checkOrInitCurrentBlock();

            const arena = b.b.arena.allocator();
            const copy: Instruction = switch (insn) {
                inline .phi, .call => |data, name| @unionInit(
                    Instruction,
                    @tagName(name),
                    try data.clone(arena),
                ),

                // No allocated data
                else => insn,
            };

            const ty_ref = try b.b.types.append(b.b.arena.child_allocator, ty);
            errdefer b.b.types.popLast();
            const insn_ref = try b.b.insns.append(b.b.arena.child_allocator, copy);
            std.debug.assert(ty_ref == insn_ref);
            return insn_ref;
        }

        pub fn phi(b: *BlockBuilder, ty: Type) !PhiBuilder {
            b.checkOrInitCurrentBlock();

            const ty_ref = try b.b.types.append(b.b.arena.child_allocator, ty);
            errdefer b.b.types.popLast();
            const insn_ref = try b.b.insns.appendUndefined(b.b.arena.child_allocator);
            std.debug.assert(ty_ref == insn_ref);

            b.b.unfinished_phis += 1;
            return .{ .b = b.b, .ref = insn_ref };
        }

        pub fn finish(b: *BlockBuilder, terminal: Block.Terminal) !void {
            b.checkOrInitCurrentBlock();

            const blk = b.b.blocks.getPtr(b.ref);
            blk.count = b.b.insns.count() - @intFromEnum(blk.start);
            blk.term = terminal;

            b.b.current_block = .invalid;
        }

        fn checkOrInitCurrentBlock(b: BlockBuilder) void {
            if (b.b.current_block == .invalid) {
                b.b.current_block = b.ref;
                b.b.blocks.getPtr(b.ref).start = @enumFromInt(b.b.insns.count());
            } else {
                std.debug.assert(b.b.current_block == b.ref); // Ensure blocks are constructed one at a time
            }
        }
    };

    pub const PhiBuilder = struct {
        b: *Builder,
        ref: Instruction.Ref,
        values: std.ArrayListUnmanaged(Instruction.Ref) = .{},
        finished: bool = false,

        pub fn deinit(b: *PhiBuilder) void {
            const allocator = b.b.arena.child_allocator;
            b.values.deinit(allocator);
        }

        pub fn add(b: *PhiBuilder, value: Instruction.Ref) !void {
            std.debug.assert(!b.finished);
            try b.values.append(b.b.arena.child_allocator, value);
        }

        pub fn finish(b: *PhiBuilder) !void {
            const values = try b.b.arena.allocator().allocSentinel(Instruction.Ref, b.values.items.len, .invalid);
            @memcpy(values, b.values.items);
            b.b.insns.getPtr(b.ref).* = .{ .phi = .{ .values = values } };

            b.finished = true;
            b.b.unfinished_phis -= 1;
        }
    };
};

comptime {
    std.testing.refAllDecls(@This());
}

test "format" {
    try std.testing.expectFmt(
        \\i_const 7
        \\add %0, %1
        \\call foo()
        \\call bar(%7, %4)
    ,
        "{}\n{}\n{}\n{}",
        .{
            Instruction{ .i_const = 7 },
            Instruction{ .add = .{
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
    const c0 = try blk0.i(.u32, .{ .i_const = 7 });
    const c1 = try blk0.i(.u32, .{ .i_const = 13 });
    const sum = try blk0.i(.u32, .{ .add = .{
        .lhs = c0,
        .rhs = c1,
    } });
    const c2 = try blk0.i(.u32, .{ .i_const = 20 });
    const cond = try blk0.i(.bool, .{ .lt = .{
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
    try blk2.finish(.{ .ret = phi.ref });

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
        \\  ret %6
        \\
    , "{}", .{func});
}
