//! High-level SSA IR
const std = @import("std");
const util = @import("util");
pub const dce = @import("ssa/dce.zig");
pub const parse = @import("ssa/parse.zig").parse;
pub const liveness = @import("ssa/liveness.zig");

pub const Instruction = union(enum) {
    param: void,

    void: void,
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

    pub const Binary = struct {
        lhs: Ref,
        rhs: Ref,
    };

    pub const Call = struct {
        name: []const u8,
        args: []const Ref,

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

    pub fn arity(insn: Instruction) usize {
        return switch (insn) {
            .param, .void, .i_const => 0,
            .call => |call| call.args.len,
            inline else => |i| switch (@TypeOf(i)) {
                Binary => 2,
                else => |t| @compileError("Unknown instruction type " ++ @typeName(t)),
            },
        };
    }

    pub fn operand(insn: Instruction, idx: usize) Ref {
        return switch (insn) {
            .param, .void, .i_const => unreachable,
            .call => |call| call.args[idx],

            inline else => |i| switch (@TypeOf(i)) {
                Binary => switch (idx) {
                    0 => i.lhs,
                    1 => i.rhs,
                    else => unreachable,
                },

                else => |t| @compileError("Unknown instruction type " ++ @typeName(t)),
            },
        };
    }

    pub fn format(insn: Instruction, comptime fmt: []const u8, opts: std.fmt.FormatOptions, w: anytype) !void {
        try insn.fmtWithLiveness(FakeLiveness.Single{}).format(fmt, opts, w);
    }
    pub fn fmtWithLiveness(insn: Instruction, live: anytype) Formatter(@TypeOf(live)) {
        return .{ .insn = insn, .live = live };
    }

    pub const Ref = enum(u16) {
        invalid = std.math.maxInt(u16),
        _,

        pub fn format(ref: Ref, _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
            if (ref == .invalid) {
                try w.writeAll("%invalid");
            } else {
                try w.print("%{}", .{@intFromEnum(ref)});
            }
        }
    };

    pub fn Formatter(comptime Liveness: type) type {
        return struct {
            insn: Instruction,
            live: Liveness,

            pub fn format(fmt: @This(), _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
                try w.print("{s}", .{@tagName(fmt.insn)});
                switch (fmt.insn) {
                    .i_const => |v| try w.print(" {}", .{v}),

                    .call => |call| {
                        try w.print(" {s}(", .{call.name});
                        for (call.args, 0..) |arg, i| {
                            if (i > 0) {
                                try w.writeAll(", ");
                            }
                            try w.print("{}{}", .{ arg, fmtDeath(fmt.live.operandDies(i)) });
                        }
                        try w.writeAll(")");
                    },

                    else => for (0..fmt.insn.arity()) |i| {
                        if (i > 0) {
                            try w.writeAll(",");
                        }
                        try w.print(" {}{}", .{ fmt.insn.operand(i), fmtDeath(fmt.live.operandDies(i)) });
                    },
                }
            }
        };
    }
};

pub const Block = struct {
    start: Start,
    count: u32,

    term: Terminal,

    pub const Start = enum(u32) { _ };

    pub const Terminal = union(enum) {
        ret: Instruction.Ref,
        jump: struct {
            to: Block.Ref,
            args: [*:.invalid]Instruction.Ref,
        },
        branch: struct {
            cond: Instruction.Ref,
            true: Block.Ref,
            false: Block.Ref,
            // Up until the first sentinel, args for true. After the first sentinel, args for false
            args: [*:.invalid]Instruction.Ref,
        },

        pub fn format(term: Terminal, _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
            try w.print("{s} ", .{@tagName(term)});
            switch (term) {
                .ret => |v| try w.print("{}", .{v}),

                .jump => |j| {
                    try w.print("{}(", .{j.to});
                    var i: usize = 0;
                    while (j.args[i] != .invalid) : (i += 1) {
                        if (i > 0) try w.writeAll(", ");
                        try w.print("{}", .{j.args[i]});
                    }
                    try w.writeAll(")");
                },

                .branch => |br| {
                    try w.print("{}, {}(", .{ br.cond, br.true });
                    var i: usize = 0;
                    while (br.args[i] != .invalid) : (i += 1) {
                        if (i > 0) try w.writeAll(", ");
                        try w.print("{}", .{br.args[i]});
                    }

                    try w.print("), {}(", .{br.false});
                    i += 1;
                    const false_start = i;
                    while (br.args[i] != .invalid) : (i += 1) {
                        if (i > false_start) try w.writeAll(", ");
                        try w.print("{}", .{br.args[i]});
                    }
                    try w.writeAll(")");
                },
            }
        }
    };

    pub const Ref = enum(u16) {
        invalid = std.math.maxInt(u16),
        _,

        pub fn format(ref: Ref, _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
            if (ref == .invalid) {
                try w.writeAll("@invalid");
            } else {
                try w.print("@{}", .{@intFromEnum(ref)});
            }
        }
    };

    pub fn insns(blk: Block, i: InstructionStore(Instruction)) []const Instruction {
        return i.items[@intFromEnum(blk.start)..][0..blk.count];
    }
    pub fn types(blk: Block, t: InstructionStore(Type)) []const Type {
        return t.items[@intFromEnum(blk.start)..][0..blk.count];
    }
};

pub const Function = struct {
    arena: std.heap.ArenaAllocator.State,
    insns: InstructionStore(Instruction),
    types: InstructionStore(Type),
    blocks: util.IndexedStore(Block.Ref, Block),

    pub fn deinit(func: Function, allocator: std.mem.Allocator) void {
        func.insns.deinit(allocator);
        func.types.deinit(allocator);
        func.blocks.deinit(allocator);
        func.arena.promote(allocator).deinit();
    }

    pub fn format(func: Function, comptime fmt: []const u8, opts: std.fmt.FormatOptions, w: anytype) !void {
        try func.fmtWithAnnotations(.{}).format(fmt, opts, w);
    }

    pub fn fmtWithAnnotations(func: *const Function, annotations: anytype) AnnotatedFormatter(@TypeOf(annotations)) {
        return .{ .func = func, .annotations = annotations };
    }

    pub fn AnnotatedFormatter(comptime AnnotationsTuple: type) type {
        return struct {
            func: *const Function,
            annotations: AnnotationsTuple,

            pub fn format(fmt: @This(), _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
                const live = inline for (fmt.annotations) |annot| {
                    if (@TypeOf(annot) == liveness.Info) {
                        break annot;
                    }
                } else FakeLiveness{};

                for (fmt.func.blocks.items, 0..) |blk, blk_i| {
                    try w.print("@{}(", .{blk_i});
                    if (fmt.annotations.len > 0 and blk.insns(fmt.func.insns)[0] == .param) {
                        try w.writeAll("\n");
                    }

                    var params = true;
                    for (blk.insns(fmt.func.insns), blk.types(fmt.func.types), 0..) |insn, ty, insn_i| {
                        const insn_ref: Instruction.Ref = @enumFromInt(insn_i);
                        const insn_live = live.single(fmt.func.insns, blk.start, insn_ref);

                        // Handle parameters
                        if (insn == .param) {
                            std.debug.assert(params);
                            if (fmt.annotations.len == 0) {
                                if (insn_i > 0) try w.writeAll(", ");
                            } else {
                                try w.writeAll("    ");
                            }
                            try w.print("{}{}: {}", .{ insn_ref, fmtDeath(insn_live.diesImmediately()), ty });

                            inline for (fmt.annotations) |annot| {
                                const A = @TypeOf(annot);
                                if (@hasDecl(A, "OffsetIndex") and A.OffsetIndex == Instruction.Ref) {
                                    try w.print("    {}", .{annot.get(blk.start, insn_ref)});
                                }
                            }

                            if (fmt.annotations.len > 0) {
                                try w.writeAll("\n");
                            }
                            continue;
                        } else if (params) {
                            try w.writeAll("):\n");
                            params = false;
                        }

                        // Print instruction
                        try w.print("  {}{}: {} = {}", .{
                            insn_ref,
                            fmtDeath(insn_live.diesImmediately()),
                            ty,
                            insn.fmtWithLiveness(insn_live),
                        });

                        inline for (fmt.annotations) |annot| {
                            const A = @TypeOf(annot);
                            if (@hasDecl(A, "OffsetIndex") and A.OffsetIndex == Instruction.Ref) {
                                try w.print("    {}", .{annot.get(blk.start, insn_ref)});
                            }
                        }

                        try w.writeAll("\n");
                    }
                    if (params) {
                        try w.writeAll("):\n");
                    }
                    try w.print("  {}\n", .{blk.term});
                }
            }
        };
    }
};

pub const Type = enum {
    void,
    bool,

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

    pub fn kind(ty: Type) Kind {
        return switch (ty) {
            .void => .void,
            .bool => .boolean,
            .u8, .u16, .u32, .u64 => .unsigned_int,
            .s8, .s16, .s32, .s64 => .signed_int,
        };
    }
    pub const Kind = enum {
        void,
        boolean,
        unsigned_int,
        signed_int,
    };

    pub fn format(ty: Type, _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        try w.writeAll(@tagName(ty));
    }
};

pub const Builder = struct {
    arena: std.heap.ArenaAllocator,
    insns: InstructionStore(Instruction).Mutable = .{},
    types: InstructionStore(Type).Mutable = .{},
    blocks: util.IndexedStore(Block.Ref, Block).Mutable = .{},
    current_block: Block.Ref = .invalid, // Used to ensure blocks are constructed one at a time
    unfinished_blocks: u32 = 0,

    pub fn init(allocator: std.mem.Allocator) Builder {
        return .{ .arena = std.heap.ArenaAllocator.init(allocator) };
    }
    pub fn deinit(b: *Builder) void {
        const allocator = b.arena.child_allocator;
        b.insns.deinit(allocator);
        b.types.deinit(allocator);
        b.blocks.deinit(allocator);
        b.arena.deinit();
    }

    pub fn block(b: *Builder, params: []const Type) !BlockBuilder {
        std.debug.assert(b.current_block == .invalid); // Ensure no block is already being built

        const ref = try b.blocks.appendUndefined(b.arena.child_allocator);
        b.current_block = ref;
        b.unfinished_blocks += 1;

        const blk = b.blocks.getPtr(ref);
        blk.start = @enumFromInt(b.insns.count());

        var bb: BlockBuilder = .{ .b = b, .ref = ref };
        for (params) |ty| {
            _ = try bb.i(ty, .param);
        }

        return bb;
    }

    pub fn finish(b: *Builder) !Function {
        std.debug.assert(b.current_block == .invalid); // Check for in-progress blocks
        std.debug.assert(b.unfinished_blocks == 0); // Check for unfinished blocks

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

        pub fn i(b: BlockBuilder, ty: Type, insn: Instruction) !Instruction.Ref {
            b.checkCurrentBlock();

            const arena = b.b.arena.allocator();
            const copy: Instruction = switch (insn) {
                inline .call => |data, name| @unionInit(
                    Instruction,
                    @tagName(name),
                    try data.clone(arena),
                ),

                // No allocated data
                else => insn,
            };

            const start = b.b.blocks.get(b.ref).start;
            const ty_ref = try b.b.types.append(b.b.arena.child_allocator, start, ty);
            errdefer b.b.types.popLast();
            const insn_ref = try b.b.insns.append(b.b.arena.child_allocator, start, copy);
            std.debug.assert(ty_ref == insn_ref);
            return insn_ref;
        }

        pub fn arg(b: BlockBuilder, index: u32) Instruction.Ref {
            const ref: Instruction.Ref = @enumFromInt(index);
            if (std.debug.runtime_safety) {
                const start = b.b.blocks.get(b.ref).start;
                std.debug.assert(b.b.insns.get(start, ref) == .param);
            }
            return ref;
        }

        /// Finish the block by returning
        pub fn ret(b: BlockBuilder, value: Instruction.Ref) !void {
            try b.finish();
            b.b.blocks.getPtr(b.ref).term = .{ .ret = value };
            b.b.unfinished_blocks -= 1;
        }

        /// Finish the block by jumping
        pub fn jump(b: BlockBuilder) !JumpBuilder {
            try b.finish();
            return .{ .b = b.b, .blk = b.ref };
        }

        /// Finish the block by branching
        pub fn branch(b: BlockBuilder, cond: Instruction.Ref) !BranchBuilder {
            try b.finish();
            return .{ .b = b.b, .blk = b.ref, .cond = cond };
        }

        fn finish(b: BlockBuilder) !void {
            b.checkCurrentBlock();

            const blk = b.b.blocks.getPtr(b.ref);
            blk.count = b.b.insns.count() - @intFromEnum(blk.start);

            b.b.current_block = .invalid;
        }

        fn checkCurrentBlock(b: BlockBuilder) void {
            std.debug.assert(b.b.current_block == b.ref); // Ensure blocks are constructed one at a time
        }
    };

    pub const JumpBuilder = struct {
        b: *Builder,
        blk: Block.Ref,

        target: Block.Ref = .invalid,
        args: std.ArrayListUnmanaged(Instruction.Ref) = .{},

        pub fn deinit(b: *JumpBuilder) void {
            const allocator = b.b.arena.child_allocator;
            b.args.deinit(allocator);
        }

        pub fn to(b: *JumpBuilder, target: Block.Ref) void {
            std.debug.assert(b.target == .invalid); // Cannot jump to more than one location
            b.target = target;
        }

        pub fn addArg(b: *JumpBuilder, value: Instruction.Ref) !void {
            const allocator = b.b.arena.child_allocator;
            try b.args.append(allocator, value);
        }

        pub fn finish(b: *JumpBuilder) !void {
            std.debug.assert(b.target != .invalid); // Must set target before finishing

            const arena = b.b.arena.allocator();
            const args = try arena.alloc(Instruction.Ref, b.args.items.len + 1);
            errdefer arena.free(args);
            @memcpy(args[0 .. args.len - 1], b.args.items);
            args[args.len - 1] = .invalid;

            b.b.blocks.getPtr(b.blk).term = .{ .jump = .{
                .to = b.target,
                .args = args[0 .. args.len - 1 :.invalid],
            } };
            b.b.unfinished_blocks -= 1;
        }
    };

    pub const BranchBuilder = struct {
        b: *Builder,
        blk: Block.Ref,

        cond: Instruction.Ref,
        true_target: Block.Ref = .invalid,
        false_target: Block.Ref = .invalid,
        args: std.ArrayListUnmanaged(Instruction.Ref) = .{},

        pub fn deinit(b: *BranchBuilder) void {
            const allocator = b.b.arena.child_allocator;
            b.args.deinit(allocator);
        }

        pub fn @"true"(b: *BranchBuilder, target: Block.Ref) void {
            std.debug.assert(b.true_target == .invalid); // Cannot call `true` multiple times
            std.debug.assert(b.false_target == .invalid); // `true` must be called before `false`
            b.true_target = target;
        }
        pub fn @"false"(b: *BranchBuilder, target: Block.Ref) !void {
            std.debug.assert(b.true_target != .invalid); // `true` must be called before `false`
            std.debug.assert(b.false_target == .invalid); // Cannot call `false` multiple times
            const allocator = b.b.arena.child_allocator;
            try b.args.append(allocator, .invalid);
            b.false_target = target;
        }

        pub fn addArg(b: *BranchBuilder, value: Instruction.Ref) !void {
            std.debug.assert(b.true_target != .invalid); // Must call `true` and/or `false` before `addArg`
            const allocator = b.b.arena.child_allocator;
            try b.args.append(allocator, value);
        }

        pub fn finish(b: *BranchBuilder) !void {
            std.debug.assert(b.true_target != .invalid and b.false_target != .invalid); // Must call both `true` and `false` before `finish`

            const arena = b.b.arena.allocator();
            const args = try arena.alloc(Instruction.Ref, b.args.items.len + 1);
            errdefer arena.free(args);
            @memcpy(args[0 .. args.len - 1], b.args.items);
            args[args.len - 1] = .invalid;

            b.b.blocks.getPtr(b.blk).term = .{ .branch = .{
                .cond = b.cond,
                .true = b.true_target,
                .false = b.false_target,
                .args = args[0 .. args.len - 1 :.invalid],
            } };
            b.b.unfinished_blocks -= 1;
        }
    };
};

pub fn InstructionStore(comptime Value: type) type {
    return util.SectionIndexedStore(Block.Start, Instruction.Ref, Value);
}

/// Used in formatting when liveness is unavailable, returns "no deaths" for every query
const FakeLiveness = struct {
    inline fn diesImmediately(_: FakeLiveness, _: Block.Start, _: Instruction.Ref) bool {
        return false;
    }
    inline fn operandDies(_: FakeLiveness, _: Block.Start, _: Instruction.Ref, _: usize) bool {
        return false;
    }
    inline fn single(_: FakeLiveness, _: InstructionStore(Instruction), _: Block.Start, _: Instruction.Ref) Single {
        return .{};
    }

    const Single = struct {
        inline fn diesImmediately(_: Single) bool {
            return false;
        }
        inline fn operandDies(_: Single, _: usize) bool {
            return false;
        }
    };
};

fn fmtDeath(dies: bool) std.fmt.Formatter(formatDeath) {
    return .{ .data = dies };
}
fn formatDeath(dies: bool, _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
    if (dies) {
        try w.writeAll("!");
    }
}

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

    var blk0 = try b.block(&.{});
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

    var br = try blk0.branch(cond);
    defer br.deinit();

    var blk1 = try b.block(&.{.u32});
    br.true(blk1.ref);
    try br.addArg(sum);

    const call = try blk1.i(.u32, .{ .call = &.{
        .name = "add2",
        .args = &.{blk1.arg(0)},
    } });
    var j = try blk1.jump();
    defer j.deinit();

    var blk2 = try b.block(&.{.u32});

    try br.false(blk2.ref);
    try br.addArg(sum);
    try br.finish();

    j.to(blk2.ref);
    try j.addArg(call);
    try j.finish();

    try blk2.ret(blk2.arg(0));

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
        \\  ret %0
        \\
    , "{}", .{func});
}
