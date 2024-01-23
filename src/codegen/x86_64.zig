//! x86_64 code generation, using Intel XED
const std = @import("std");
const ssa = @import("ssa");
const util = @import("util");
const regalloc = @import("regalloc.zig");

const c = @cImport({
    @cInclude("xed/xed-interface.h");
});

pub fn compile(
    allocator: std.mem.Allocator,
    code_allocator: std.mem.Allocator,
    func: ssa.Function,
    liveness: ssa.liveness.Info,
) ![]const u8 {
    const vregs = try regalloc.virtualAlloc(allocator, func, liveness);
    defer vregs.deinit(allocator);

    const regs = try regalloc.physicalAlloc(Register, mutable_regs, allocator, func, liveness, vregs);
    defer regs.deinit(allocator);

    var comp = try Compiler.init(allocator, code_allocator, func, liveness, regs);
    defer comp.deinit();

    // Generate code
    for (0..func.blocks.count()) |blk_idx| {
        const blk_ref: ssa.Block.Ref = @enumFromInt(blk_idx);
        try comp.block(blk_ref);
    }

    return comp.code.toOwnedSlice();
}

const x86_64_state: c.xed_state_t = .{
    .mmode = c.XED_MACHINE_MODE_LONG_64,
    .stack_addr_width = c.XED_ADDRESS_WIDTH_64b,
};

const Register = enum(c.xed_reg_enum_t) {
    rax = c.XED_REG_RAX,
    rcx = c.XED_REG_RCX,
    rdx = c.XED_REG_RDX,
    rbx = c.XED_REG_RBX,
    rsp = c.XED_REG_RSP,
    rbp = c.XED_REG_RBP,
    rsi = c.XED_REG_RSI,
    rdi = c.XED_REG_RDI,

    r8 = c.XED_REG_R8,
    r9 = c.XED_REG_R9,
    r10 = c.XED_REG_R10,
    r11 = c.XED_REG_R11,
    r12 = c.XED_REG_R12,
    r13 = c.XED_REG_R13,
    r14 = c.XED_REG_R14,
    r15 = c.XED_REG_R15,
};
const mutable_regs = std.EnumSet(Register).initFull();

var init_once = std.once(initX86);
fn initX86() void {
    c.xed_tables_init();
}

const Compiler = struct {
    allocator: std.mem.Allocator,
    func: ssa.Function,
    liveness: ssa.liveness.Info,
    regs: ssa.InstructionStore(Register),
    labels: util.IndexedStore(ssa.Block.Ref, u32).Mutable = .{},

    code: std.ArrayList(u8),

    fn init(
        allocator: std.mem.Allocator,
        code_allocator: std.mem.Allocator,
        func: ssa.Function,
        liveness: ssa.liveness.Info,
        regs: ssa.InstructionStore(Register),
    ) !Compiler {
        init_once.call();

        var comp: Compiler = .{
            .allocator = allocator,
            .func = func,
            .liveness = liveness,
            .regs = regs,
            .code = std.ArrayList(u8).init(code_allocator),
        };

        try comp.labels.resize(allocator, func.blocks.count());
        errdefer comp.labels.deinit(comp.allocator);

        return comp;
    }
    fn deinit(comp: *Compiler) void {
        comp.labels.deinit(comp.allocator);
        comp.code.deinit();
    }

    fn block(comp: *Compiler, blk_ref: ssa.Block.Ref) !void {
        const blk = comp.func.blocks.getPtr(blk_ref);
        for (blk.slice(comp.func.insns), blk.slice(comp.func.types), @intFromEnum(blk.start)..) |insn, ty, insn_idx| {
            const insn_ref: ssa.Instruction.Ref = @enumFromInt(insn_idx);
            const reg = comp.regs.get(blk.start, insn_ref);

            switch (insn) {
                .param => {}, // TODO
                .expect => {},

                .void => {},

                .i_const => |value| {
                    // TODO: immediates
                    switch (ty.kind()) {
                        .unsigned_int => try comp.emit(c.XED_ICLASS_MOV, 64, &.{
                            c.xed_reg(@intFromEnum(reg)),
                            c.xed_imm0(value, @max(32, immWidth(value))),
                        }),

                        else => @panic("TODO"),
                    }
                },

                .true => try comp.emit(c.XED_ICLASS_MOV, 64, &.{
                    c.xed_reg(@intFromEnum(reg)),
                    c.xed_imm0(1, 32),
                }),
                .false => try comp.emit(c.XED_ICLASS_MOV, 64, &.{
                    c.xed_reg(@intFromEnum(reg)),
                    c.xed_imm0(0, 32),
                }),

                .add => |i| {
                    // TODO: inform register allocator that src1 and dest should be the same
                    const lhs = comp.regs.get(blk.start, i.lhs);
                    const rhs = comp.regs.get(blk.start, i.rhs);
                    try comp.mov(reg, lhs);
                    try comp.emit(c.XED_ICLASS_ADD, 64, &.{
                        c.xed_reg(@intFromEnum(reg)),
                        c.xed_reg(@intFromEnum(rhs)),
                    });
                },
                .sub => |i| {
                    // TODO: inform register allocator that src1 and dest should be the same
                    const lhs = comp.regs.get(blk.start, i.lhs);
                    const rhs = comp.regs.get(blk.start, i.rhs);
                    try comp.mov(reg, lhs);
                    try comp.emit(c.XED_ICLASS_SUB, 64, &.{
                        c.xed_reg(@intFromEnum(reg)),
                        c.xed_reg(@intFromEnum(rhs)),
                    });
                },

                .mul => |i| {
                    // TODO: inform register allocator of how this instruction works
                    const lhs = comp.regs.get(blk.start, i.lhs);
                    const rhs = comp.regs.get(blk.start, i.rhs);

                    try comp.mov(reg, .rax); // Save rax
                    try comp.push(.rdx); // Save rdx

                    try comp.mov(.rax, lhs);
                    // TODO: check type
                    try comp.emit(c.XED_ICLASS_MUL, 64, &.{
                        c.xed_reg(@intFromEnum(rhs)),
                    });

                    try comp.xchg(reg, .rax); // Restore rax
                    try comp.pop(.rdx); // Restore rdx
                },

                .div => |i| {
                    // TODO: inform register allocator of how this instruction works
                    const lhs = comp.regs.get(blk.start, i.lhs);
                    const rhs = comp.regs.get(blk.start, i.rhs);

                    try comp.mov(reg, .rax); // Save rax
                    try comp.push(.rdx); //Save rdx

                    try comp.mov(.rax, lhs);
                    try comp.zero(.rdx);
                    // TODO: check type
                    try comp.emit(c.XED_ICLASS_DIV, 64, &.{
                        c.xed_reg(@intFromEnum(rhs)),
                    });

                    try comp.xchg(reg, .rax); // Restore rax
                    try comp.pop(.rdx); // Restore rdx
                },

                .eq => {},
                .ne => {},
                .lt => {},
                .le => {},
                .gt => {},
                .ge => {},

                .call => {},
            }
        }
    }

    fn immWidth(value: u64) u7 {
        return if (value > std.math.maxInt(u32))
            64
        else if (value > std.math.maxInt(u16))
            32
        else if (value > std.math.maxInt(u8))
            16
        else
            8;
    }

    // Helpers for common register operations
    fn zero(comp: *Compiler, reg: Register) !void {
        try comp.emit(c.XED_ICLASS_XOR, 64, &.{
            c.xed_reg(@intFromEnum(reg)),
            c.xed_reg(@intFromEnum(reg)),
        });
    }
    fn mov(comp: *Compiler, to: Register, from: Register) !void {
        if (to != from) {
            try comp.emit(c.XED_ICLASS_MOV, 64, &.{
                c.xed_reg(@intFromEnum(to)),
                c.xed_reg(@intFromEnum(from)),
            });
        }
    }
    fn xchg(comp: *Compiler, to: Register, from: Register) !void {
        if (to != from) {
            try comp.emit(c.XED_ICLASS_XCHG, 64, &.{
                c.xed_reg(@intFromEnum(to)),
                c.xed_reg(@intFromEnum(from)),
            });
        }
    }
    fn push(comp: *Compiler, reg: Register) !void {
        try comp.emit(c.XED_ICLASS_PUSH, 64, &.{
            c.xed_reg(@intFromEnum(reg)),
        });
    }
    fn pop(comp: *Compiler, reg: Register) !void {
        try comp.emit(c.XED_ICLASS_POP, 64, &.{
            c.xed_reg(@intFromEnum(reg)),
        });
    }

    fn emit(comp: *Compiler, iclass: c.xed_iclass_enum_t, operand_width: u32, ops: []const c.xed_encoder_operand_t) !void {
        var req: c.xed_encoder_request_t = undefined;
        c.xed_encoder_request_zero_set_mode(&req, &x86_64_state);
        std.debug.assert(_codegen_xed_build(&req, iclass, operand_width, x86_64_state, ops.len, ops.ptr));

        const start = comp.code.items.len;
        const buf = try comp.code.addManyAsArray(c.XED_MAX_INSTRUCTION_BYTES);
        var count: c_uint = undefined;
        const err = c.xed_encode(&req, buf.ptr, buf.len, &count);
        try xedErr(err);
        comp.code.shrinkRetainingCapacity(start + count);
    }
    extern fn _codegen_xed_build(
        req: *c.xed_encoder_request_t,
        iclass: c.xed_iclass_enum_t,
        operand_width: c.xed_uint_t,
        mode: c.xed_state_t,
        noperand: usize,
        operands: [*]const c.xed_encoder_operand_t,
    ) bool;
};

fn xedErr(err: c.xed_error_enum_t) !void {
    switch (err) {
        c.XED_ERROR_NONE => {}, // There was no error
        c.XED_ERROR_BUFFER_TOO_SHORT => return error.BufferTooShort, // There were not enough bytes in the given buffer
        c.XED_ERROR_GENERAL_ERROR => return error.GeneralError, // XED could not decode the given instruction
        c.XED_ERROR_INVALID_FOR_CHIP => return error.InvalidForChip, // The instruction is not valid for the specified chip
        c.XED_ERROR_BAD_REGISTER => return error.BadRegister, // XED could not decode the given instruction because an invalid register encoding was used.
        c.XED_ERROR_BAD_LOCK_PREFIX => return error.BadLockPrefix, // A lock prefix was found where none is allowed.
        c.XED_ERROR_BAD_REP_PREFIX => return error.BadRepPrefix, // An F2 or F3 prefix was found where none is allowed.
        c.XED_ERROR_BAD_LEGACY_PREFIX => return error.BadLegacyPrefix, // A 66, F2 or F3 prefix was found where none is allowed.
        c.XED_ERROR_BAD_REX_PREFIX => return error.BadRexPrefix, // A REX prefix was found where none is allowed.
        c.XED_ERROR_BAD_MAP => return error.BadMap, // An illegal value for the MAP field was detected in the instruction.
        c.XED_ERROR_BAD_EVEX_V_PRIME => return error.BadEvexVPrime, // EVEX.V'=0 was detected in a non-64b mode instruction.
        c.XED_ERROR_BAD_EVEX_Z_NO_MASKING => return error.BadEvexZNoMasking, // EVEX.Z!=0 when EVEX.aaa==0
        c.XED_ERROR_NO_OUTPUT_POINTER => return error.NoOutputPointer, // The output pointer for xed_agen was zero
        c.XED_ERROR_NO_AGEN_CALL_BACK_REGISTERED => return error.NoAgenCallBackRegistered, // One or both of the callbacks for xed_agen were missing.
        c.XED_ERROR_BAD_MEMOP_INDEX => return error.BadMemopIndex, // Memop indices must be 0 or 1.
        c.XED_ERROR_CALLBACK_PROBLEM => return error.CallbackProblem, // The register or segment callback for xed_agen experienced a problem
        c.XED_ERROR_GATHER_REGS => return error.GatherRegs, // The index, dest and mask regs for AVX2 gathers must be different.
        c.XED_ERROR_INSTR_TOO_LONG => return error.InstrTooLong, // Full decode of instruction would exeed 15B.
        c.XED_ERROR_INVALID_MODE => return error.InvalidMode, // The instruction was not valid for the specified mode
        c.XED_ERROR_BAD_EVEX_LL => return error.BadEvexLl, // EVEX.LL must not ==3 unless using embedded rounding
        c.XED_ERROR_BAD_REG_MATCH => return error.BadRegMatch, // Some registers must not match for this instruction (e.g. source with dest or dest with dest).
        else => unreachable,
    }
}

comptime {
    std.testing.refAllDeclsRecursive(@This());
}

test "basic arithmetic" {
    var b = ssa.Builder.init(std.testing.allocator);
    defer b.deinit();

    var blk = try b.block(&.{});
    const c_13 = try blk.i(.u32, .{ .i_const = 13 });
    const c_7 = try blk.i(.u32, .{ .i_const = 7 });
    const add = try blk.i(.u32, .{ .add = .{ .lhs = c_13, .rhs = c_7 } });
    const c_4 = try blk.i(.u32, .{ .i_const = 4 });
    const sub = try blk.i(.u32, .{ .sub = .{ .lhs = add, .rhs = c_4 } });
    const c_3 = try blk.i(.u32, .{ .i_const = 3 });
    const mul = try blk.i(.u32, .{ .mul = .{ .lhs = sub, .rhs = c_3 } });
    const div = try blk.i(.u32, .{ .div = .{ .lhs = mul, .rhs = c_4 } });
    try blk.ret(div);

    const func = try b.finish();
    defer func.deinit(std.testing.allocator);

    try testGen(func,
        \\mov rax, 0xd
        \\mov rcx, 0x7
        \\add rax, rcx
        \\mov rcx, 0x4
        \\sub rax, rcx
        \\mov rdx, 0x3
        \\push rdx
        \\mul rdx
        \\pop rdx
        \\push rdx
        \\xor rdx, rdx
        \\div rcx
        \\pop rdx
        \\
    );
}

fn testGen(func: ssa.Function, expected: []const u8) !void {
    const liveness = try ssa.liveness.analyze(std.testing.allocator, func);
    defer liveness.deinit(std.testing.allocator);

    const code = try compile(std.testing.allocator, std.testing.allocator, func, liveness);
    defer std.testing.allocator.free(code);

    const buffer = try std.testing.allocator.allocSentinel(u8, 25 * code.len, 0);
    defer std.testing.allocator.free(buffer);

    var code_offset: usize = 0;
    var out_offset: usize = 0;
    while (code_offset < code.len) {
        var decoded: c.xed_decoded_inst_t = undefined;
        c.xed_decoded_inst_zero_set_mode(&decoded, &x86_64_state);
        const err = c.xed_decode(&decoded, code.ptr + code_offset, @intCast(code.len - code_offset));
        try xedErr(err);

        code_offset += c.xed_decoded_inst_get_length(&decoded);

        const buf = buffer[out_offset..];
        try std.testing.expect(c.xed_format_context(
            c.XED_SYNTAX_INTEL,
            &decoded,
            buf.ptr,
            @intCast(buf.len),
            0,
            null,
            null,
        ) != 0);
        const end = std.mem.indexOfScalar(u8, buf, 0).?;
        buf[end] = '\n';

        out_offset += end + 1;
    }

    try std.testing.expectEqualStrings(expected, buffer[0..out_offset]);
}
