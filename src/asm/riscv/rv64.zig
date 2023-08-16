//! Assembler for RV64G (IMAFD)
const std = @import("std");
const opcodes = @import("opcodes.zig");

pub const Register = enum(u5) {
    x0, // Always 0
    x1,
    x2,
    x3,
    x4,
    x5,
    x6,
    x7,

    x8,
    x9,
    x10,
    x11,
    x12,
    x13,
    x14,
    x15,

    x16,
    x17,
    x18,
    x19,
    x20,
    x21,
    x22,
    x23,

    x24,
    x25,
    x26,
    x27,
    x28,
    x29,
    x30,
    x31,
};

pub const RoundingMode = enum(u3) {
    nearest_even = 0b000, // Towards nearest, ties to even
    nearest_max = 0b100, // Towards nearest, ties to max magnitude

    towards_zero = 0b001, // Towards 0

    down = 0b010, // Towards -inf
    up = 0b011, // Towards +inf

    dynamic = 0b111, // Dynamic rounding mode
};

pub fn assembleInstruction(insn: Instruction) error{InvalidOperand}!u32 {
    @setEvalBranchQuota(3000);
    switch (insn) {
        inline else => |operands, name| {
            const desc = opcodes.opcodes[@intFromEnum(name)];
            comptime std.debug.assert(std.mem.eql(u8, @tagName(name), desc.name));

            comptime var shift = 0;
            comptime var mask = 0;
            var word: u32 = 0;
            inline for (desc.variable_fields) |field| {
                shift += @ctz(~(desc.mask >> shift));

                const value = switch (field) {
                    // Immediates
                    // NOTE: 'hi' and 'lo' are actually swapped for some reason, and refer to the low and high bits respectively
                    .bimm12lo => try B12.from(getOperand(desc, .imm13, operands)).hi(),
                    .bimm12hi => try B12.from(getOperand(desc, .imm13, operands)).lo(),

                    .imm12 => getOperand(desc, .imm12, operands),
                    .imm12lo => @as(u7, @intCast(getOperand(desc, .imm12, operands) >> 5)),
                    .imm12hi => @as(u5, @truncate(getOperand(desc, .imm12, operands))),

                    .imm20 => a: {
                        const x = getOperand(desc, .imm32, operands);
                        if (x & 0xfff != 0) return error.InvalidOperand;
                        const v: u20 = @intCast(@shrExact(x, 12));
                        break :a v;
                    },
                    .jimm20 => try J20.from(getOperand(desc, .imm21, operands)).pack(),

                    // Registers
                    .rd, .rs1, .rs2, .rs3 => |f| @intFromEnum(getOperand(desc, std.enums.nameCast(opcodes.Operand, f), operands)),

                    // Shift amounts
                    .shamtd, .shamtw => |f| getOperand(desc, std.enums.nameCast(opcodes.Operand, f), operands),

                    // Rounding mode
                    .rm => @intFromEnum(getOperand(desc, .rm, operands)),

                    // Atomics
                    .aq => @panic("TODO: atomics"),
                    .rl => @panic("TODO: atomics"),

                    // Fences
                    .fm => @panic("TODO: fence"),
                    .pred => @panic("TODO: fence"),
                    .succ => @panic("TODO: fence"),
                };

                word |= @as(u32, value) << shift;
                mask |= @as(u32, ~@as(@TypeOf(value), 0)) << shift;

                shift += @bitSizeOf(@TypeOf(value));
            }
            if (shift < 32) {
                shift += @ctz(~(desc.mask >> shift));
            }
            comptime std.debug.assert(shift == 32);
            comptime std.debug.assert(mask == ~desc.mask);
            word |= desc.match;

            return word;
        },
    }
}

const B12 = packed struct(u13) {
    b0: u1,
    b1_4: u4,
    b5_10: u6,
    b11: u1,
    b12: u1,

    const Lo = packed struct(u5) { f0: u1, f1: u4 };
    const Hi = packed struct(u7) { f0: u6, f1: u1 };

    fn from(v: u13) B12 {
        return @bitCast(v);
    }
    fn lo(x: B12) !u5 {
        if (x.b0 != 0) return error.InvalidOperand;
        return @bitCast(Lo{ .f0 = x.b11, .f1 = x.b1_4 });
    }
    fn hi(x: B12) !u7 {
        if (x.b0 != 0) return error.InvalidOperand;
        return @bitCast(Hi{ .f0 = x.b5_10, .f1 = x.b12 });
    }
};
const J20 = packed struct(u21) {
    b0: u1,
    b1_10: u10,
    b11: u1,
    b12_19: u8,
    b20: u1,

    const P = packed struct(u20) { f0: u8, f1: u1, f2: u10, f3: u1 };

    fn from(v: u21) J20 {
        return @bitCast(v);
    }
    fn pack(x: J20) !u20 {
        if (x.b0 != 0) return error.InvalidOperand;
        return @bitCast(P{ .f0 = x.b12_19, .f1 = x.b11, .f2 = x.b1_10, .f3 = x.b20 });
    }
};

fn getOperand(
    comptime desc: opcodes.OpcodeDesc,
    comptime operand: opcodes.Operand,
    operands: anytype,
) OperandType(operand) {
    inline for (desc.operands, operands) |name, value| {
        if (comptime name == operand) {
            return value;
        }
    }
}

const Opcode = a: {
    var fields: [opcodes.opcodes.len]std.builtin.Type.EnumField = undefined;
    for (&fields, opcodes.opcodes, 0..) |*field, opcode, i| {
        field.* = .{
            .name = opcode.name,
            .value = i,
        };
    }
    break :a @Type(.{ .Enum = .{
        .tag_type = std.math.IntFittingRange(0, fields.len - 1),
        .fields = &fields,
        .decls = &.{},
        .is_exhaustive = true,
    } });
};

pub const Instruction = a: {
    var fields: [opcodes.opcodes.len]std.builtin.Type.UnionField = undefined;
    for (&fields, opcodes.opcodes) |*field, opcode| {
        const T = Operands(opcode.operands);
        field.* = .{
            .name = opcode.name,
            .type = T,
            .alignment = @alignOf(T),
        };
    }
    break :a @Type(.{ .Union = .{
        .layout = .Auto,
        .tag_type = Opcode,
        .fields = &fields,
        .decls = &.{},
    } });
};

fn Operands(comptime operands: []const opcodes.Operand) type {
    var types: [operands.len]type = undefined;
    for (&types, operands) |*t, operand| {
        t.* = OperandType(operand);
    }
    return std.meta.Tuple(&types);
}
fn OperandType(comptime operand: opcodes.Operand) type {
    return switch (operand) {
        // Immediates
        .imm13 => u13,
        .imm12 => u12,
        .imm21 => u21,
        .imm32 => u32,

        // Registers
        .rd, .rs1, .rs2, .rs3 => Register,

        // Shift amounts
        .shamtd => u6,
        .shamtw => u5,

        // Rounding mode
        .rm => RoundingMode,

        // Atomics
        .aq => TODO,
        .rl => TODO,

        // Fences
        .fm => TODO,
        .pred => TODO,
        .succ => TODO,
    };
}

const TODO = struct {};

comptime {
    std.testing.refAllDecls(@This());
}

test "lui" {
    // LUI is a U-type instruction
    try std.testing.expectEqual(
        @as(u32, 0xaaaa_a3b7),
        try assembleInstruction(.{ .lui = .{
            .x7, 0xaaaa_a000,
        } }),
    );
}

test "jal" {
    // JAL is a J-type instruction
    try std.testing.expectEqual(
        @as(u32, 0xaaba_a3ef),
        try assembleInstruction(.{ .jal = .{
            .x7, 0x001a_aaaa,
        } }),
    );
}

test "jalr" {
    // JALR is an I-type instruction
    try std.testing.expectEqual(
        @as(u32, 0xaaaa_8fe7),
        try assembleInstruction(.{ .jalr = .{
            .x31, .x21, 0x0aaa,
        } }),
    );
}

test "beq" {
    // BEQ is a B-type instruction
    try std.testing.expectEqual(
        @as(u32, 0xab5f_85e3),
        try assembleInstruction(.{ .beq = .{
            .x31, .x21, 0x1aaa,
        } }),
    );
}

test "sb" {
    // SB is an S-type instruction
    try std.testing.expectEqual(
        @as(u32, 0xab5f_8523),
        try assembleInstruction(.{ .sb = .{
            .x31, .x21, 0x0aaa,
        } }),
    );
}

test "add" {
    // ADD is an R-type instruction
    try std.testing.expectEqual(
        @as(u32, 0x01aa_8fb3),
        try assembleInstruction(.{ .add = .{
            .x31, .x21, .x26,
        } }),
    );
}

test "slli" {
    // SLLI is an I-type instruction, but its immediate is limited to 6 bits rather than the normal 12
    try std.testing.expectEqual(
        @as(u32, 0x02aa_9f93),
        try assembleInstruction(.{ .slli = .{
            .x31, .x21, 0x2a,
        } }),
    );
}
