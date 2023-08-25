//! SSA parser
const std = @import("std");
const ssa = @import("../ssa.zig");

const log = std.log.scoped(.ssa_parser);

pub fn parse(allocator: std.mem.Allocator, src: []const u8) !ssa.Function {
    var p = Parser{
        .b = ssa.Builder.init(allocator),
        .toks = Tokenizer{ .src = src },
    };
    defer p.deinit();

    try p.func();

    return p.b.finish();
}
const Parser = struct {
    b: ssa.Builder,
    toks: Tokenizer,
    labels: std.StringArrayHashMapUnmanaged(ssa.Block.Ref) = .{},
    temps: std.StringArrayHashMapUnmanaged(ssa.Instruction.Ref) = .{},

    unresolved_jumps: std.ArrayListUnmanaged(struct {
        t: ssa.Builder.JumpBuilder,
        label: []const u8,
    }) = .{},
    unresolved_branches: std.ArrayListUnmanaged(struct {
        t: ssa.Builder.BranchBuilder,
        true: []const u8,
        false: []const u8,
    }) = .{},

    fn deinit(p: *Parser) void {
        const alloc = p.allocator();

        p.labels.deinit(alloc);
        p.temps.deinit(alloc);

        for (p.unresolved_jumps.items) |*j| {
            j.t.deinit();
        }
        p.unresolved_jumps.deinit(alloc);

        for (p.unresolved_branches.items) |*br| {
            br.t.deinit();
        }
        p.unresolved_branches.deinit(alloc);

        p.b.deinit();
    }

    fn func(p: *Parser) !void {
        defer {
            p.labels.clearRetainingCapacity();

            for (p.unresolved_jumps.items) |*j| {
                j.t.deinit();
            }
            p.unresolved_jumps.clearRetainingCapacity();

            for (p.unresolved_branches.items) |*br| {
                br.t.deinit();
            }
            p.unresolved_branches.clearRetainingCapacity();
        }

        while (try p.block()) {}

        for (p.unresolved_jumps.items) |*j| {
            j.t.to(try p.getLabel(j.label));
            try j.t.finish();
        }

        for (p.unresolved_branches.items) |*br| {
            br.t.true_target = try p.getLabel(br.true);
            br.t.false_target = try p.getLabel(br.false);
            try br.t.finish();
        }
    }

    fn block(p: *Parser) !bool {
        p.temps.clearRetainingCapacity();

        const label = while (p.maybeNext()) |tok| {
            switch (tok.tag) {
                .newline => {},
                .label => break tok.text,
                else => return error.Parse,
            }
        } else return false;

        // Parse parameters
        _ = try p.exact(.lparen);
        var params = std.ArrayList(ssa.Type).init(p.allocator());
        defer params.deinit();
        while (true) {
            const temp = try p.next();
            switch (temp.tag) {
                .newline => continue,
                .rparen => break,
                .temp => {},
                else => return error.Parse,
            }
            try p.addTemp(temp.text, @enumFromInt(params.items.len));

            _ = try p.exact(.colon);
            const type_name = try p.exact(.word);
            const ty = std.meta.stringToEnum(ssa.Type, type_name) orelse return error.InvalidTypeName;
            try params.append(ty);

            switch ((try p.next()).tag) {
                .comma => {},
                .rparen => break,
                .newline => {
                    _ = try p.exact(.rparen);
                    break;
                },
                else => return error.Parse,
            }
        }
        _ = try p.exact(.colon);

        var blk = try p.b.block(params.items);
        try p.addLabel(label, blk.ref);

        while (true) {
            _ = try p.exact(.newline);
            const tok = try p.next();
            switch (tok.tag) {
                .temp => {
                    const ref = try p.insn(&blk);
                    try p.addTemp(tok.text, ref);
                },

                .word => {
                    try p.term(&blk, tok.text);
                    break;
                },

                else => return error.Parse,
            }
        }

        return true;
    }

    fn insn(p: *Parser, blk: *ssa.Builder.BlockBuilder) !ssa.Instruction.Ref {
        _ = try p.exact(.colon);
        const type_name = try p.exact(.word);
        const ty = std.meta.stringToEnum(ssa.Type, type_name) orelse return error.InvalidTypeName;

        _ = try p.exact(.equals);
        const insn_name = try p.exact(.word);
        const insn_tag = std.meta.stringToEnum(
            std.meta.Tag(ssa.Instruction),
            insn_name,
        ) orelse return error.InvalidInstruction;

        switch (insn_tag) {
            .param => return error.InvalidInstruction,
            inline .void, .true, .false => |tag| return blk.i(ty, tag),

            .expect => {
                const value = try p.getTemp(try p.exact(.temp));
                _ = try p.exact(.comma);

                const prob_str = try p.exact(.float);
                const prob = try std.fmt.parseFloat(f16, prob_str);

                return blk.i(ty, .{ .expect = .{
                    .value = value,
                    .probability = prob,
                } });
            },

            .i_const => {
                const num_str = try p.exact(.unsigned);
                const num = try std.fmt.parseUnsigned(u64, num_str, 10);
                return blk.i(ty, .{ .i_const = num });
            },

            .call => {
                const name = try p.exact(.word);

                _ = try p.exact(.lparen);
                var args = std.ArrayList(ssa.Instruction.Ref).init(p.allocator());
                defer args.deinit();
                while (true) {
                    const temp = try p.next();
                    switch (temp.tag) {
                        .newline => continue,
                        .rparen => break,
                        .temp => {},
                        else => return error.Parse,
                    }

                    const ref = try p.getTemp(temp.text);
                    try args.append(ref);

                    switch ((try p.next()).tag) {
                        .comma => {},
                        .rparen => break,
                        .newline => {
                            _ = try p.exact(.rparen);
                            break;
                        },
                        else => return error.Parse,
                    }
                }

                return blk.i(ty, .{ .call = &.{
                    .name = name,
                    .args = args.items,
                } });
            },

            inline else => |tag| switch (std.meta.fieldInfo(ssa.Instruction, tag).type) {
                ssa.Instruction.Binary => {
                    const lhs = try p.exact(.temp);
                    _ = try p.exact(.comma);
                    const rhs = try p.exact(.temp);

                    return blk.i(ty, @unionInit(ssa.Instruction, @tagName(tag), .{
                        .lhs = try p.getTemp(lhs),
                        .rhs = try p.getTemp(rhs),
                    }));
                },

                else => |t| @compileError("Unknown instruction type " ++ @typeName(t)),
            },
        }
    }

    fn term(p: *Parser, blk: *ssa.Builder.BlockBuilder, term_name: []const u8) !void {
        const term_tag = std.meta.stringToEnum(
            std.meta.Tag(ssa.Block.Terminal),
            term_name,
        ) orelse return error.InvalidTerminal;

        switch (term_tag) {
            .ret => {
                const temp = try p.getTemp(try p.exact(.temp));
                try blk.ret(temp);
            },

            .jump => {
                var t = try blk.jump();
                errdefer t.deinit();
                const label = try p.exact(.label);
                try p.terminalArgs(&t);

                try p.unresolved_jumps.append(p.allocator(), .{
                    .t = t,
                    .label = label,
                });
            },

            .branch => {
                const cond = try p.getTemp(try p.exact(.temp));
                var t = try blk.branch(cond);
                errdefer t.deinit();

                _ = try p.exact(.comma);
                const true_lab = try p.exact(.label);
                t.true(blk.ref); // Just need a valid ref, we'll patch the correct one in later
                try p.terminalArgs(&t);

                _ = try p.exact(.comma);
                const false_lab = try p.exact(.label);
                try t.false(blk.ref);
                try p.terminalArgs(&t);

                try p.unresolved_branches.append(p.allocator(), .{
                    .t = t,
                    .true = true_lab,
                    .false = false_lab,
                });
            },
        }
    }

    fn terminalArgs(p: *Parser, t: anytype) !void {
        _ = try p.exact(.lparen);
        while (true) {
            const temp = try p.next();
            switch (temp.tag) {
                .newline => continue,
                .rparen => break,
                .temp => {},
                else => return error.Parse,
            }

            const ref = try p.getTemp(temp.text);
            try t.addArg(ref);

            switch ((try p.next()).tag) {
                .comma => {},
                .rparen => break,
                .newline => {
                    _ = try p.exact(.rparen);
                    break;
                },
                else => return error.Parse,
            }
        }
    }

    fn addLabel(p: *Parser, name: []const u8, ref: ssa.Block.Ref) !void {
        const result = try p.labels.getOrPutValue(p.allocator(), name, ref);
        if (result.found_existing) return error.DuplicateLabel;
    }
    fn getLabel(p: *Parser, name: []const u8) !ssa.Block.Ref {
        return p.labels.get(name) orelse error.UndefinedLabel;
    }

    fn addTemp(p: *Parser, name: []const u8, ref: ssa.Instruction.Ref) !void {
        const result = try p.temps.getOrPutValue(p.allocator(), name, ref);
        if (result.found_existing) return error.DuplicateTemporary;
    }
    fn getTemp(p: *Parser, name: []const u8) !ssa.Instruction.Ref {
        return p.temps.get(name) orelse error.UndefinedTemporary;
    }

    fn maybeNext(p: *Parser) ?Token {
        return p.toks.next();
    }
    fn next(p: *Parser) !Token {
        return p.maybeNext() orelse error.Parse;
    }
    fn exact(p: *Parser, tag: Token.Tag) ![]const u8 {
        const tok = try p.next();
        if (tok.tag != tag) {
            log.err("expected {s} token, got {s}", .{ @tagName(tag), @tagName(tok.tag) });
            return error.Parse;
        }
        return tok.text;
    }

    fn allocator(p: Parser) std.mem.Allocator {
        return p.b.arena.child_allocator;
    }
};

const Token = struct {
    tag: Tag,
    text: []const u8,

    const Tag = enum {
        invalid,

        newline,
        comma,
        colon,
        equals,
        lparen,
        rparen,

        temp,
        label,
        word,
        unsigned,
        float,
    };
};

const Tokenizer = struct {
    src: []const u8,
    i: usize = 0,

    fn next(toks: *Tokenizer) ?Token {
        var tag: ?Token.Tag = null;
        var start: usize = undefined;
        while (toks.i < toks.src.len) : (toks.i += 1) {
            const c = toks.src[toks.i];
            if (tag) |t| switch (t) {
                .newline => switch (c) {
                    ' ', '\t', '\n' => {},
                    else => break,
                },

                .comma, .colon, .equals, .lparen, .rparen, .invalid => break,

                .temp, .label, .word => switch (c) {
                    '0'...'9', 'a'...'z', 'A'...'Z', '_' => {},
                    else => break,
                },

                .unsigned => switch (c) {
                    '0'...'9' => {},
                    '.' => tag = .float,
                    else => break,
                },

                .float => switch (c) {
                    '0'...'9' => {},
                    else => break,
                },
            } else {
                tag = switch (c) {
                    ' ', '\t' => null,
                    '\n' => .newline,
                    ',' => .comma,
                    ':' => .colon,
                    '=' => .equals,
                    '(' => .lparen,
                    ')' => .rparen,

                    '%' => .temp,
                    '@' => .label,
                    'a'...'z', 'A'...'Z', '_' => .word,
                    '0'...'9' => .unsigned,
                    '.' => .float,

                    else => .invalid,
                };
                if (tag != null) {
                    start = toks.i;
                }
            }
        }

        if (tag) |t| {
            return .{
                .tag = t,
                .text = toks.src[start..toks.i],
            };
        } else {
            return null;
        }
    }
};

comptime {
    std.testing.refAllDecls(@This());
}

test "simple parse" {
    const src =
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
    ;

    const func = try parse(std.testing.allocator, src);
    defer func.deinit(std.testing.allocator);
    try ssa.validate(func);

    try std.testing.expectFmt(src, "{}", .{func});
}

test "named temporaries/blocks" {
    const src =
        \\@start():
        \\  %_7: u32 = i_const 7
        \\  %_13: u32 = i_const 13
        \\  %sum: u32 = add %_7, %_13
        \\  %_20: u32 = i_const 20
        \\  %less: bool = lt %sum, %_20
        \\  branch %less, @modify(%sum), @return(%sum)
        \\@modify(%arg: u32):
        \\  %result: u32 = call add2(%arg)
        \\  jump @return(%result)
        \\@return(%result: u32):
        \\  ret %result
        \\
    ;

    const func = try parse(std.testing.allocator, src);
    defer func.deinit(std.testing.allocator);
    try ssa.validate(func);

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
