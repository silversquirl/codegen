pub const rv64 = @import("codegen/rv64.zig");
pub const x86_64 = @import("codegen/x86_64.zig");

comptime {
    _ = rv64;
    _ = x86_64;
}
